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
  module Conv = Datakit_github_conv.Make(DK)

  let error fmt = Fmt.kstrf (fun str -> DK.error "sync: %s" str) fmt

  (*             [snapshot]      [tr]
      [in memory Snapshot.t]  [9p/datakit endpoint]
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
    snapshot: Snapshot.t;    (* in-memory representation of the bridge state. *)
    tr      : DK.Transaction.t;                (* open transaction to datakit *)
    head    : DK.Commit.t;   (* commit where the transaction has been created *)
    name    : string;                        (* name of the branch in datakit *)
  }

  let pp ppf t =
    Fmt.pf ppf "@[%a@;%a@]" DK.Commit.pp t.head Snapshot.pp t.snapshot

  let tr_head tr =
    DK.Transaction.parents tr >>*= function
    | []  -> error "no parents!"
    | [p] -> ok p
    | _   -> error "too many parents!"

  let is_open t = DK.Transaction.closed t.tr = false
  let is_closed t = DK.Transaction.closed t.tr

  let create ~debug ?old b =
    begin
      DK.Branch.transaction b >>*= fun tr ->
      tr_head tr >>*= fun head ->
      ok (tr, head)
    end >>= function
    | Error e ->
      Log.err (fun l -> l "create %s: %a" debug DK.pp_error e);
      Lwt.fail_with debug
    | Ok (tr, head) ->
      Conv.snapshot ~debug ?old tr >|= fun snapshot ->
      let name = DK.Branch.name b in
      { snapshot; tr; head; name }

  let safe_abort t =
    if DK.Transaction.closed t.tr then Lwt.return_unit
    else DK.Transaction.abort t.tr

  let rec safe_commit ?(retry=5) t ~message =
    DK.Transaction.commit t.tr ~message >>= function
    | Ok ()   -> Lwt.return true
    | Error e ->
      if retry <> 0 then safe_commit ~retry:(retry-1) t ~message
      else (
        Log.info (fun l -> l "Abort: %a" DK.pp_error e);
        DK.Transaction.abort t.tr >|= fun () ->
        false
      )

  (* Create [github-metadata] if it doesn't exist. *)
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

  let commit t snapshot =
    let diff = Snapshot.diff snapshot t.snapshot in
    if Snapshot.is_diff_empty diff then
      safe_abort t >|= fun () -> true
    else
      let message = Fmt.to_to_string Snapshot.pp_diff diff in
      safe_commit t ~message

  let sync ~token ~webhook t repos =
    assert (is_open t);
    let snapshot = match webhook with
      | None                   -> Lwt.return t.snapshot
      | Some { watch; events } ->
        State.add_webhooks token ~watch repos >>= fun () ->
        State.import_webhook_events token ~events t.snapshot
    in
    snapshot >>= fun snapshot ->
    State.import token snapshot repos >>= fun snapshot ->
    commit t snapshot >|= fun commited ->
    assert (is_closed t);
    if not commited then
      t
    else
      assert false

  (* On startup, build the initial state by looking at the active
     repository in datakit. Import the new repositories and call the
     GitHub API with the diff between the GitHub state and datakit. *)
  let first_sync ~token ~webhook b =
    create ~debug:"first-sync" ?old:None b >>= fun t ->
    Log.debug (fun l -> l "[first_sync]@;@[<2>%a@]" pp t);
    let repos = Snapshot.repos t.snapshot in
    if Repo.Set.is_empty repos then safe_abort t >|= fun _ -> (t, true) (* FIXME *)
    else sync ~token ~webhook t repos >>= fun _ -> assert false

  let sync_once ~token:_ ~webhook:_ _t = assert false

  (*
  (* The main synchonisation function: it is called on every change in
     the public or private branch. *)
  let sync_once ~webhook ~cap ~token ~pub ~priv ~old =
    assert (is_closed old);
    state "sync-once" ~old:(Some old) ~pub ~priv >>*= fun t ->
    Log.debug (fun l -> l "[sync_once]@;old:%a@;new:%a" pp old pp t);
    call_github_api ~cap ~token ~old:old.pub.snapshot t >>*= fun t ->
    let repos = Repo.Set.union (repos old.pub t.pub) (repos old.priv t.priv) in
    sync ~webhook ~cap ~token ~pub ~priv t repos >>*= fun t ->
    assert (is_closed t);
    ok t
*)

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
      | Starting -> first_sync ~token ~webhook br >|= fun (s, _) -> s
      | State t  -> sync_once ~token ~webhook t
    in
    match policy with
    | `Once   -> sync_once t >|= fun _fixme -> t
    | `Repeat ->
      let t = ref t in
      let updates = ref false in
      let cond = Lwt_condition.create () in
      let pp ppf = function
        | Starting -> Fmt.string ppf "<starting>"
        | State t  ->
          let repos = Snapshot.repos t.snapshot in
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
