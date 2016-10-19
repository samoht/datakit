open Result
open Lwt.Infix
open Datakit_github

let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

let ( >>*= ) x f =
  x >>= function
  | Ok x         -> f x
  | Error _ as e -> Lwt.return e

let ok x = Lwt.return (Ok x)

module Make (API: API) (DK: Datakit_S.CLIENT) = struct

  module State = Datakit_github.State(API)
  module Conv  = Datakit_github_conv.Make(DK)

  (*              [bridge]     [datakit]
      [in memory Snapshot.t] [9p/datakit endpoint]
                      |            |
      GH --events-->  |            | <--commits-- Users
                      |            |
                      | <--watch-- |
                      |            |
      GH --API GET--> |            |
      GH <--API SET-- |            |
                      | --write--> |
                      |            |
  *)

  type state = {
    bridge : Snapshot.t;     (* in-memory representation of the bridge state. *)
    datakit: Conv.t;                                        (* datakit state. *)
  }

  let pp_state ppf t =
    Fmt.pf ppf "@[datakit:@;@[<2>%a@];@[bridge:@;<2>%a]@]"
      Conv.pp t.datakit Snapshot.pp t.bridge

  let is_open tr = DK.Transaction.closed tr = false
  let is_closed tr = DK.Transaction.closed tr

  let create ~debug ?old br =
    let bridge = match old with
      | None   -> Snapshot.empty
      | Some o -> o.bridge
    in
    let old = match old with
      | None   -> None
      | Some o -> Some o.datakit
    in
    Conv.create ~debug ?old br >|= fun (tr, datakit) ->
    tr, { datakit; bridge }

  let safe_abort tr =
    if DK.Transaction.closed tr then Lwt.return_unit
    else DK.Transaction.abort tr

  let rec safe_commit ?(retry=5) tr ~message =
    DK.Transaction.commit tr ~message >>= function
    | Ok ()   -> Lwt.return true
    | Error e ->
      if retry <> 0 then safe_commit ~retry:(retry-1) tr ~message
      else (
        Log.info (fun l -> l "Abort: %a" DK.pp_error e);
        DK.Transaction.abort tr >|= fun () ->
        false
      )

  (* Create and init [br] if it doesn't exist. *)
  let init_sync br =
    Log.debug (fun l -> l "init_sync %s" @@ DK.Branch.name br);
    let init =
      DK.Branch.head br  >>*= function
      | Some _ -> ok ()
      | None   ->
        DK.Branch.with_transaction br (fun tr ->
            let dir  = Datakit_path.empty in
            let data = Cstruct.of_string "### DataKit -- GitHub bridge\n" in
            DK.Transaction.create_or_replace_file tr ~dir "README.md" data
            >>= function
            | Ok ()   -> DK.Transaction.commit tr ~message:"Initial commit"
            | Error e ->
              DK.Transaction.abort tr >>= fun () ->
              Lwt.fail_with @@ Fmt.strf "init_sync: %a" DK.pp_error e
          )
    in
    init >>= function
    | Ok () -> Lwt.return_unit
    | Error e ->
      Log.err (fun l -> l "init_sync: %a" DK.pp_error e);
      Lwt.fail_with "init_sync"

  type webhook = {
    watch : Repo.t -> unit Lwt.t;
    events: unit -> Event.t list;
  }

  let commit t tr =
    let diff = Snapshot.diff t.bridge (Conv.snapshot t.datakit) in
    if Snapshot.is_diff_empty diff then
      safe_abort tr >|= fun () -> true
    else
      let message = Fmt.to_to_string Snapshot.pp_diff diff in
      safe_commit tr ~message

  let sync ~token ~webhook t tr repos =
    assert (is_open tr);
    let bridge = match webhook with
      | None                   -> Lwt.return t.bridge
      | Some { watch; events } ->
        State.add_webhooks token ~watch repos >>= fun () ->
        State.import_webhook_events token ~events t.bridge
    in
    bridge >>= fun bridge ->
    State.import token bridge repos >>= fun bridge ->
    commit t tr >|= fun commited ->
    assert (is_closed tr);
    if not commited then t else { t with bridge }

  (* On startup, build the initial state by looking at the active
     repository in datakit. Import the new repositories and call the
     GitHub API with the diff between the GitHub state and datakit. *)
  let first_sync ~token ~webhook br =
    create ~debug:"first-sync" ?old:None br >>= fun (tr, t) ->
    Log.debug (fun l -> l "[first_sync]@;@[<2>%a@]" pp_state t);
    let repos = Snapshot.repos (Conv.snapshot t.datakit) in
    if Repo.Set.is_empty repos then safe_abort tr >|= fun _ -> t
    else sync ~token ~webhook t tr repos >|= fun t -> t

  (* The main synchonisation function: it is called on every change in
     the datakit branch and when new webhook events are received. *)
  let sync_once ~token ~webhook old br =
    create ~debug:"sync-once" ~old br >>= fun (tr, t) ->
    Log.debug (fun l -> l "[sync_once]@;old:%a@;new:%a" pp_state old pp_state t);
    let datakit = Conv.snapshot t.datakit in
    let diff = Snapshot.diff datakit t.bridge in
    State.apply token diff >>= fun () ->
    let repos =
      Repo.Set.diff (Snapshot.repos datakit) (Snapshot.repos t.bridge)
    in
    assert (is_open tr);
    sync ~token ~webhook t tr repos >|= fun t ->
    assert (is_closed tr);
    t

  type t = State of state | Starting

  let empty = Starting

  let continue = function
    | Some s -> Lwt_switch.is_on s
    | None   -> true

  let process_webhook = function
    | None   -> None, fun _ -> fst (Lwt.task ())
    | Some w ->
      let watch r = API.Webhook.watch w r in
      let events () =
        let e = API.Webhook.events w in
        API.Webhook.clear w;
        e
      in
      let rec wait s =
        API.Webhook.wait w >>= fun () ->
        s ();
        wait s
      in
      let run s =
        Lwt.pick [
          API.Webhook.run w;
          wait s
        ]
      in
      Some {watch; events}, run

  let run ~webhook ?switch ~token br t policy =
    let webhook, run_webhook = process_webhook webhook in
    let sync_once = function
      | Starting -> first_sync ~token ~webhook br
      | State t  -> sync_once ~token ~webhook t br
    in
    match policy with
    | `Once   -> sync_once t >|= fun t -> State t
    | `Repeat ->
      let t = ref t in
      let updates = ref false in
      let cond = Lwt_condition.create () in
      let pp ppf = function
        | Starting -> Fmt.string ppf "<starting>"
        | State t  ->
          let repos = Snapshot.repos t.bridge in
          Fmt.pf ppf "active repos: %a" Repo.Set.pp repos
      in
      let rec react () =
        if not (continue switch) then Lwt.return_unit
        else
          (if not !updates then Lwt_condition.wait cond else Lwt.return_unit)
          >>= fun () ->
          updates := false;
          Log.info (fun l -> l "Processing new entry -- %a" pp !t);
          Lwt.catch
            (fun () ->
               sync_once !t >|= fun s ->
               t := State s)
            (fun e ->
               Log.err (fun l -> l "error: %s" (Printexc.to_string e));
               Lwt.return_unit)
          >>=
          react
      in
      let notify () =
        Log.debug (fun l -> l "webhook event received!");
        updates := true;
        Lwt_condition.signal cond ()
      in
      let watch br =
        let notify _ =
          Log.info (fun l -> l "Change detected in %s" @@ DK.Branch.name br);
          updates := true;
          Lwt_condition.signal cond ();
          ok `Again
        in
        DK.Branch.wait_for_head ?switch br notify >>= function
        | Ok _    -> Lwt.return_unit
        | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
      in
      Lwt.choose [ react () ; watch br; run_webhook notify ]
      >|= fun () ->
      !t

  let sync ~token ?webhook ?switch ?(policy=`Repeat)
      ?(cap=Capabilities.all) br t =
    Log.debug (fun l -> l "[sync] %s" @@ DK.Branch.name br);
    let token = State.token token cap in
    init_sync br >>= fun () ->
    run ~webhook ?switch ~token br t policy

end
