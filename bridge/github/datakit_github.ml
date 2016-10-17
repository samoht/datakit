open Result
open Astring
open Lwt.Infix
open Datakit_github_types

let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

module Capabilities = struct

  type op = [`Read | `Write]
  type resource = [`PR | `Status | `Ref | `Webhook]

  module X = struct

    type t = { read: bool; write: bool }
    let none = { read = false; write = false }
    let all = { read = true; write = true }

    let allow t = function
      | `Read  -> { t with read  = true }
      | `Write -> { t with write = true }

    let disallow t = function
      | `Read  -> { t with read  = false }
      | `Write -> { t with write = false }

    let check t = function
      | `Read  -> t.read
      | `Write -> t.write

    let pp ppf = function
      | { read = true ; write = true  } -> Fmt.string ppf "rw"
      | { read = true ; write = false } -> Fmt.string ppf "r"
      | { read = false; write = true  } -> Fmt.string ppf "w"
      | { read = false; write = false } -> Fmt.string ppf ""

  end

  type t = { pr: X.t; status: X.t; ref: X.t; webhook: X.t }

  let none = { pr = X.none; status = X.none; ref = X.none; webhook = X.none }
  let all = { pr = X.all; status = X.all; ref = X.all; webhook = X.all }

  let pp ppf t =
    if t = all then Fmt.string ppf "*:rw"
    else if t = none then Fmt.string ppf "*:"
    else if t.pr = t.status && t.pr = t.ref && t.pr = t.webhook then
      Fmt.pf ppf "*:%a" X.pp t.pr
    else
      Fmt.pf ppf "pr:%a,status:%a,ref:%a,webhook:%a"
        X.pp t.pr X.pp t.status X.pp t.ref X.pp t.webhook

  let pp_resource ppf = function
    | `PR      -> Fmt.string ppf "pr"
    | `Status  -> Fmt.string ppf "status"
    | `Ref     -> Fmt.string ppf "ref"
    | `Webhook -> Fmt.string ppf "webhook"

  let pp_op ppf = function
    | `Read  -> Fmt.string ppf "read"
    | `Write -> Fmt.string ppf "write"

  let apply f t op = function
    | `PR      -> { t with pr      = f t.pr op }
    | `Status  -> { t with status  = f t.status op }
    | `Ref     -> { t with ref     = f t.ref op }
    | `Webhook -> { t with webhook = f t.webhook op }
    | `All     ->
      { pr      = f t.pr op;
        status  = f t.status op;
        ref     = f t.ref op;
        webhook = f t.webhook op }

  let allow = apply X.allow
  let disallow = apply X.disallow

  let x t = function
    | `PR      -> t.pr
    | `Status  -> t.status
    | `Ref     -> t.ref
    | `Webhook -> t.webhook

  let check t op r =
    let allowed = X.check (x t r) op in
    if not allowed then
      Log.info (fun l ->
          l "%a: %a is denied (current policy is %a)"
            pp_resource r pp_op op pp t
        );
    allowed

  let resource_of_string = function
    | "pr"      -> Some `PR
    | "status"  -> Some `Status
    | "ref"     -> Some `Ref
    | "webhook" -> Some `Webhook
    | "*"       -> Some `All
    | s         -> Log.err (fun l -> l "%s is not a valid API resource" s); None

  let ops_of_string = function
    | ""   -> Some []
    | "r"  -> Some [`Read]
    | "w"  -> Some [`Write]
    | "rw" -> Some [`Read; `Write]
    | s    -> Log.err (fun l -> l "%s is not a valid operation" s); None

  exception Error of string * string

  let of_string s =
    let aux s = match String.cut ~sep:":" s with
      | None        -> raise (Error (s, "missing ':'"))
      | Some (r, c) -> match resource_of_string r, ops_of_string c with
        | None  , _      -> raise (Error (r, "wrong resource"))
        | _     , None   -> raise (Error (c, "wrong capacity"))
        | Some r, Some c -> r, c
    in
    let allows t cs r = List.fold_left (fun acc c -> allow acc c r) t cs in
    let caps =
      try String.cuts ~sep:"," s |> List.map aux |> fun s -> `Ok s
      with Error (s, r) -> let err = Fmt.strf "%s: %s" s r in `Error err
    in
    match caps with
    | `Error _ as e -> e
    | `Ok caps ->
      let all =
        try List.find (fun (r, _) -> r = `Default) caps |> snd
        with Not_found -> if caps = [] then [`Read; `Write] else []
      in
      List.fold_left
        (fun acc (r, cs) -> allows acc cs r) (allows none all `All) caps
      |> fun t -> `Ok t

end

module API (API: Datakit_github_api.S): sig
  (** [set old new] performs a serie of GitHub API calls to make the
      GitHub state upgrade from [old] to [new]. *)
  val apply: cap:Capabilities.t -> token:API.token -> Snapshot.diff -> unit Lwt.t
end = struct

  let ok x = Lwt.return (Ok x)

  let status_of_commits ~cap ~token commits =
    let api_status token c =
      Log.info (fun l -> l "API.status %a" Commit.pp c);
      if not (Capabilities.check cap `Read `Status) then ok Status.Set.empty
      else
        API.status token c >|= function
        | Error e   -> Error (c, e)
        | Ok status -> Ok (Status.Set.of_list status)
    in
    Lwt_list.map_p (api_status token) (Commit.Set.elements commits)
    >|= fun status ->
    List.fold_left (fun status -> function
        | Ok s         -> Status.Set.union status s
        | Error (c, e) ->
          Log.err (fun l -> l "API.status %a: %s" Commit.pp c e);
          status
      ) Status.Set.empty status

  let new_prs ~cap ~token repos =
    let repos_l = Repo.Set.elements repos in
    Lwt_list.map_p (fun r ->
        Log.info (fun l -> l "API.prs %a" Repo.pp r);
        if not (Capabilities.check cap `Read `PR) then ok PR.Set.empty
        else
          API.prs token r >|= function
          | Error e -> Error (r, e)
          | Ok prs  ->
            List.filter (fun pr -> pr.PR.state = `Open) prs
            |> PR.Set.of_list
            |> fun x -> Ok x
      ) repos_l
    >|= fun new_prs ->
    List.fold_left (fun new_prs -> function
        | Ok prs       -> PR.Set.union prs new_prs
        | Error (r, e) ->
          Log.err (fun l -> l "API.prs %a: %s" Repo.pp r e);
          new_prs
      ) PR.Set.empty new_prs

  let new_refs ~cap ~token repos =
    let repos_l = Repo.Set.elements repos in
    Lwt_list.map_p (fun r ->
        Log.info (fun l -> l "API.refs %a" Repo.pp r);
        if not (Capabilities.check cap `Read `Ref) then ok Ref.Set.empty
        else
          API.refs token r >|= function
          | Error e -> Error (r, e)
          | Ok refs -> Ok (Ref.Set.of_list refs)
      ) repos_l
    >|= fun new_refs ->
    List.fold_left (fun new_refs -> function
        | Ok refs      -> Ref.Set.union refs new_refs
        | Error (r, e) ->
          Log.err (fun l -> l "API.refs %a: %s" Repo.pp r e);
          new_refs
      ) Ref.Set.empty new_refs

  (* Import http://github.com/usr/repo state. *)
  let import_repos t ~cap ~token repos =
    new_prs ~cap ~token repos >>= fun new_prs ->
    new_refs ~cap ~token repos >>= fun new_refs ->
    let new_commits =
      Commit.Set.union (PR.Set.commits new_prs) (Ref.Set.commits new_refs)
    in
    status_of_commits ~cap ~token new_commits >|= fun new_status ->
    let new_t =
      Snapshot.create ~repos ~prs:new_prs ~refs:new_refs ~commits:new_commits
        ~status:new_status
    in
    Log.debug (fun l ->
         l "import_repo %a@;@[<2>new:%a@]" Repo.Set.pp repos Snapshot.pp new_t);
    let base = Snapshot.without_repos repos t in
    let repos = Repo.Set.union (Snapshot.repos t) repos in
    let prs = PR.Set.union (Snapshot.prs base) new_prs in
    let refs = Ref.Set.union (Snapshot.refs base) new_refs in
    let commits = Commit.Set.union (Snapshot.commits base) new_commits in
    let status = Status.Set.union (Snapshot.status base) new_status in
    Snapshot.create ~repos ~prs ~commits ~refs ~status,
    Snapshot.diff new_t t

  let api_set_pr ~cap ~token pr =
    Log.info (fun l -> l "API.set-pr %a" PR.pp pr);
    if not (Capabilities.check cap `Write `PR) then Lwt.return_unit
    else
      API.set_pr token pr >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-pr %a: %s" PR.pp pr e)

  let api_remove_ref ~cap ~token r =
    let repo, name = Ref.id r in
    let pp ppf r = Ref.pp_id ppf (Ref.id r) in
    Log.info (fun l -> l "API.remove-ref %a" pp r);
    if not (Capabilities.check cap `Write `Ref) then Lwt.return_unit
    else
      API.remove_ref token repo name >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.remove-ref %a: %s" pp r e)

  let api_set_ref ~cap ~token r =
    Log.info (fun l -> l "API.set-ref %a" Ref.pp r);
    if not (Capabilities.check cap `Write `Ref) then Lwt.return_unit
    else
      API.set_ref token r >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-ref %a: %s" Ref.pp r e)

  let api_set_status ~cap ~token s =
    Log.info (fun l -> l "API.set-status %a" Status.pp s);
    if not (Capabilities.check cap `Write `Status) then Lwt.return_unit
    else
      API.set_status token s >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-status %a: %s" Status.pp s e)

  (* Read DataKit data and call the GitHub API to sync the world with
     what DataKit think it should be. *)
  let apply ~cap ~token diff =
    Log.debug (fun l -> l "GithubAPI.apply@;@[%a@]" Snapshot.pp_diff diff);
    let prs =
      PR.Set.union
        (match Snapshot.to_update diff with
         | None   -> PR.Set.empty
         | Some t -> Snapshot.prs t)
        (match Snapshot.to_remove diff with
         | None   -> PR.Set.empty
         | Some t -> Snapshot.prs t |> PR.Set.map PR.close)
    in
    Lwt_list.iter_p (api_set_pr ~cap ~token) (PR.Set.elements prs)
    >>= fun () ->
    let closed_refs = match Snapshot.to_remove diff with
      | None   -> Ref.Set.empty
      | Some t -> Snapshot.refs t
    in
    Lwt_list.iter_p (api_remove_ref ~cap ~token) (Ref.Set.elements closed_refs)
    >>= fun () ->
    let refs = match Snapshot.to_update diff with
      | None   -> Ref.Set.empty
      | Some t -> Snapshot.refs t
    in
    Lwt_list.iter_p (api_set_ref ~cap ~token) (Ref.Set.elements refs)
    >>= fun () ->
    (* NOTE: ideally we would also remove status, but the GitHub API doesn't
       support removing status so we just ignore *)
    let status = match Snapshot.to_update diff with
      | None   -> Status.Set.empty
      | Some t -> Snapshot.status t
    in
    Lwt_list.iter_p (api_set_status ~cap ~token) (Status.Set.elements status)

end


module Sync (API: Datakit_github_api.S) (DK: Datakit_S.CLIENT) = struct

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

  type t = {
    snapshot: Snapshot.t;    (* in-memory representation of the bridge state. *)
    tr      : DK.Transaction.t;                (* open transaction to datakit *)
    head    : DK.Commit.t;   (* commit where the transaction has been created *)
    name    : string;                        (* name of the branch in datakit *)
  }

  let pp ppf t =
    Fmt.pf ppf "@[%a@;%a@]" DK.Commit.pp t.head Snapshot.pp t.snapshot

  let compare x y =
    match DK.Commit.compare x.head y.head with
    | 0 -> Snapshot.compare x.snapshot y.snapshot
    | i -> i

  let with_head branch fn =
    DK.Branch.head branch >>*= function
    | None   -> error "empty branch!"
    | Some c -> fn c

  let tr_head tr =
    DK.Transaction.parents tr >>*= function
    | []  -> error "no parents!"
    | [p] -> ok p
    | _   -> error "too many parents!"

  let is_open t = DK.Transaction.closed t.tr = false
  let is_closed t = DK.Transaction.closed t.tr

  let create msg ?old b =
    DK.Branch.transaction b >>*= fun tr ->
    tr_head tr >>*= fun head ->
    Conv.snapshot msg ?old (Conv.tree_of_commit head) >>= fun snapshot ->
    let name = DK.Branch.name b in
    ok { snapshot; tr; head; name }

  (** Merge *)

  let abort t =
    let close tr =
      if DK.Transaction.closed tr then Lwt.return_unit
      else DK.Transaction.abort tr
    in
    close t.pub.tr

  let safe_commit tr ~message =
    DK.Transaction.commit tr ~message >>= function
    | Ok ()   -> Lwt.return_unit
    | Error e ->
      Log.info (fun l -> l "Abort: %a" DK.pp_error e);
      DK.Transaction.abort tr

  (**TODO
  (* Merge the private branch back in the public branch. *)
  let merge t ~pub ~priv =
    assert (is_closed t);
    state "start-merge" ~old:(Some t) ~pub ~priv >>*= fun t ->
    Log.debug (fun l -> l "[merge]@;%a" pp t);
    let to_remove = Snapshot.diff t.pub.snapshot t.priv in
    let to_add = Snapshot.diff t.priv t.pub.snapshot in
    if Snapshot.is_empty to_remove && Snapshot.is_empty to_add then ok t
    else

      DK.Transaction.merge t.pub.tr t.priv.head >>*= fun (m, conflicts) ->
      (if conflicts = [] then ok ""
       else (
         (* usually that means a conflict between what the user
            request and the state of imported events from
            GitHub. *)
         let { DK.Transaction.ours; theirs; _ } = m in
         list_iter_s (fun path ->
             let dir, file =
               match List.rev @@ Datakit_path.unwrap path with
               | [] -> failwith "TODO"
               | base :: dir ->
                 Datakit_path.of_steps_exn (List.rev dir), base
             in
             DK.Tree.read_file ours path   >>= fun ours   ->
             DK.Tree.read_file theirs path >>= fun theirs ->
             match ours, theirs with
             | Error _ , Error _ -> DK.Transaction.remove t.pub.tr dir
             | Ok v    ,  _
             | Error _ , Ok v    ->
               DK.Transaction.create_or_replace_file t.pub.tr ~dir file v
           ) conflicts
         >>*= fun () ->
         ok @@ Fmt.strf "\n\nconflicts:@;@[%a@]"
           Fmt.(list ~sep:(unit "\n") Datakit_path.pp) conflicts)
      ) >>*= fun conflict_msg ->
      DK.Transaction.diff t.pub.tr t.pub.head >>*= function
      | []   -> ok t
      | diff ->
        let diff = Diff.changes diff in
        let pp ppf diff =
          Fmt.(list ~sep:(unit "\n") Diff.pp) ppf (Diff.Set.elements diff)
        in
        let msg =
          Fmt.strf "Merging with %s\n\nChanges:\n%a%s"
            t.priv.name pp diff conflict_msg
        in
        Log.debug (fun l -> l "merge commit: %s" msg);
        safe_commit t.pub.tr ~message:msg >>= fun () ->
        DK.Transaction.abort t.priv.tr  >>= fun () ->
        state "end-merge" ~old:(Some t) ~priv ~pub
  *)

  (** Sync *)

  (* Create [github-metadata] if it doesn't exist. *)
  let init_sync br =
    Log.debug (fun l -> l "init_sync %s" @@ DK.Branch.name br);
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

  let prune_branch s =
    let diff = Snapshot.prune s.snapshot in
    Conv.apply_diff "prune" diff s.tr >>*= fun () ->
    DK.Transaction.diff s.tr s.head >>*= fun diff ->
    (if diff = [] then DK.Transaction.abort s.tr
     else safe_commit s.tr ~message:"Prune") >>= ok

  let prune b =
    (branch "prune" b >>*= prune_branch) >>= function
    | Ok ()   -> Lwt.return_unit
    | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e

  let sync_repos ~cap ~token ~pub ~priv t repos =
    import_repos ~cap ~token t.priv repos >>= fun (priv_s, c) ->
    Conv.apply_diff "import" c t.pub.tr >>*= fun () ->
    let priv_s, remove = Snapshot.prune priv_s in
    cleanup "sync" { remove; update = Some priv_s } t.priv.tr >>*= fun () ->
    DK.Transaction.diff t.priv.tr t.priv.head >>*= fun diff ->
    (if diff = [] then DK.Transaction.abort t.priv.tr else
       let message = Fmt.strf "Sync with %a" Repo.Set.pp repos in
       safe_commit t.priv.tr ~message)
    >>= fun () ->
    DK.Transaction.abort t.pub.tr >>= fun () ->
    merge t ~pub ~priv >>*= fun t ->
    let pub_s, remove = Snapshot.prune t.pub.snapshot in
    cleanup "sync" { remove; update = Some pub_s } t.pub.tr >>*= fun () ->
    DK.Transaction.diff t.pub.tr t.pub.head >>*= fun diff ->
    (if diff = [] then ok t else
       safe_commit t.pub.tr ~message:"Prune" >>= fun () ->
       DK.Transaction.abort t.priv.tr >>= fun () ->
       state "end-sync-repos" ~old:(Some t) ~pub ~priv)

  type webhook = {
    watch: Repo.t -> unit Lwt.t;
    events: unit -> Event.t list;
  }

  let sync_webhooks t ~cap ~token ~webhook ~priv ~pub repos =
    match webhook with
    | None   -> ok t
    | Some w ->
      Log.debug (fun l -> l "[sync_webhook] repos: %a" Repo.Set.pp repos);
      (* register new webhooks *)
      Lwt_list.iter_p (fun r ->
          Log.info (fun l -> l "API.add-webhook %a" Repo.pp r);
          if not (Capabilities.check cap `Write `Webhook) then Lwt.return_unit
          else w.watch r
        ) (Repo.Set.elements repos) >>= fun () ->
      (* apply the webhook events *)
      match w.events () with
      | []     -> ok t
      | events ->
        Log.debug (fun l ->
            l "[sync_webhook] events:@;%a" (Fmt.Dump.list Event.pp) events);
        let priv_s =
          List.fold_left (Snapshot.replace_event) t.priv.snapshot events
        in
        (* Need to resynchronsize build status for new commits *)
        let commits = List.fold_left (fun acc -> function
            | Event.PR pr ->
              if PR.state pr <> `Open then acc
              else Commit.Set.add (PR.commit pr) acc
            | Event.Ref (`Removed, _) -> acc
            | Event.Ref (_, r) -> Commit.Set.add (Ref.commit r) acc
            | Event.Repo _ | Event.Status _  | Event.Other _  -> acc
          ) Commit.Set.empty events
        in
        let new_commits = Commit.Set.diff commits priv_s.Snapshot.commits in
        status_of_commits ~cap ~token commits >>= fun new_status ->
        let status = Status.Set.union new_status priv_s.Snapshot.status in
        let commits = Commit.Set.union new_commits priv_s.Snapshot.commits in
        let priv_s = { priv_s with Snapshot.status; commits } in
        let events =
          events @ List.map Event.status @@ Status.Set.elements new_status
        in
        list_iter_s (Conv.update_event t.priv.tr) events >>*= fun () ->
        let _, remove = Snapshot.prune priv_s in
        cleanup "events" { remove; update = None } t.priv.tr >>*= fun () ->
        let message =
          Fmt.strf "New webhook events\n\n%a"
            Fmt.(list ~sep:(unit "\n\n") Event.pp) events
        in
        safe_commit t.priv.tr ~message >>= fun () ->
        DK.Transaction.abort t.pub.tr >>= fun () ->
        merge t ~pub ~priv

  let sync ~webhook ~cap ~token ~pub ~priv t repos =
    assert (is_open t);
    sync_webhooks t ~cap ~token ~webhook ~priv ~pub repos >>*= fun t ->
    assert (is_open t);
    sync_repos ~cap ~token ~pub ~priv t repos >>*= fun t ->
    assert (is_open t);
    abort t >>= fun () ->
    ok t

  (* On startup, build the initial state by looking at the active
     repository in the public and private branch. Import the new
     repositories in the private branch, then merge it in the public
     branch. Finally call the GitHub API with the diff between the
     public and the private branch. *)
  let first_sync ~webhook ~cap ~token ~pub ~priv =
    state "first-sync" ~old:None ~pub ~priv >>*= fun t ->
    Log.debug (fun l ->
        l "[first_sync]@;@[<2>priv:%a@]@;@[<2>pub=%a@]"
          pp_branch t.priv
          pp_branch t.pub
      );
    let repos =
      let r t = t.snapshot.Snapshot.repos in
      Repo.Set.union (r t.priv) (r t.pub)
    in
    begin
      if Repo.Set.is_empty repos then abort t >>= fun () -> ok t
      else sync ~webhook ~cap ~token ~pub ~priv t repos
    end >>*= fun t ->
    assert (is_closed t);
    let old = t in
    state "api-calls" ~old:(Some t) ~pub ~priv >>*= fun t ->
    abort t >>= fun () ->
    call_github_api ~cap ~token ~old:old.priv.snapshot t >>*= fun t ->
    ok t

  let repos old t =
    let old = Snapshot.repos old.snapshot in
    let t   = Snapshot.repos t.snapshot in
    Repo.Set.(union (diff old t) (diff t old))

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

  let run ~webhook ?switch ~cap ~token ~priv ~pub t policy =
    let webhook, run_webhook = process_webhook webhook in
    let sync_once = function
      | Starting -> first_sync ~webhook ~cap ~token ~priv ~pub
      | State t  -> sync_once ~webhook ~cap ~token ~priv ~pub ~old:t
    in
    match policy with
    | `Once   -> sync_once t >>*= fun t -> ok (`Finish (State t))
    | `Repeat ->
      let t = ref t in
      let updates = ref false in
      let cond = Lwt_condition.create () in
      let pp ppf = function
        | Starting -> Fmt.string ppf "<starting>"
        | State t  ->
          let repos = Snapshot.repos t.priv.snapshot in
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
            (fun () -> sync_once !t >|= function
               | Ok s    -> t := State s
               | Error e -> Log.err (fun l -> l "sync error: %a" DK.pp_error e))
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
      Lwt.choose [ react () ; watch priv; watch pub; run_webhook notify ]
      >>= fun () ->
      ok (`Finish !t)

  let sync ?webhook ?switch ?(policy=`Repeat)
      ?(cap=Capabilities.all) ~pub ~priv ~token t =
    Log.debug (fun l ->
        l "[sync] pub:%s priv:%s" (DK.Branch.name pub) (DK.Branch.name priv)
      );
    (init_sync ~priv ~pub >>*= fun () ->
     run ~webhook ?switch ~cap ~token ~priv ~pub t policy >>*= function
     | `Finish l -> ok l
     | _ -> failwith "TODO")
    >>= function
    | Ok t    -> Lwt.return t
    | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e

end
