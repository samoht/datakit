open Lwt.Infix
open Datakit_path.Infix
open Datakit_github

let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

let ( >>*= ) x f =
  x >>= function
  | Ok x         -> f x
  | Error _ as e -> Lwt.return e

let pp_path = Fmt.(list ~sep:(unit "/") string)

module Make (DK: Datakit_S.CLIENT) = struct

  type tree = DK.Tree.t

  (* conversion between GitHub and DataKit states. *)

  let path s = Datakit_path.of_steps_exn s

  let safe_remove t path =
    DK.Transaction.remove t path >|= function
    | Error _ | Ok () -> ()

  let safe_read_dir t dir =
    DK.Tree.read_dir t dir >|= function
    | Error _ -> []
    | Ok dirs -> dirs

  let safe_exists_dir t dir =
    DK.Tree.exists_dir t dir >|= function
    | Error _ -> false
    | Ok b    -> b

  let safe_exists_file t file =
    DK.Tree.exists_file t file >|= function
    | Error _ -> false
    | Ok b    -> b

  let safe_read_file t file =
    DK.Tree.read_file t file >|= function
    | Error _ -> None
    | Ok b    -> Some (String.trim (Cstruct.to_string b))

  let lift_errors name f = f >>= function
    | Error e -> Lwt.fail_with @@ Fmt.strf "%s: %a" name DK.pp_error e
    | Ok x    -> Lwt.return x

  let path_of_diff = function
    | `Added f | `Removed f | `Updated f -> Datakit_path.unwrap f

  let changes diff =
    let without_last l = List.rev (List.tl (List.rev l)) in
    List.fold_left (fun acc d ->
        let path = path_of_diff d in
        let t = match path with
          | [] | [_]             -> None
          | user :: repo :: path ->
            let repo = { Repo.user; repo } in
            match path with
            | [] | [".monitor"] -> Some (`Repo repo)
            | "pr" :: id :: _   -> Some (`PR (repo, int_of_string id))
            | "commit" :: [id]  -> Some (`Commit { Commit.repo; id })
            | "commit" :: id :: "status" :: (_ :: _ :: _ as tl) ->
              Some (`Status ({ Commit.repo; id }, without_last tl))
            | "ref" :: ( _ :: _ :: _ as tl)  ->
              Some (`Ref (repo, without_last tl))
            |  _ -> None
        in
        match t with
        | None   -> acc
        | Some t -> Elt.IdSet.add t acc
      ) Elt.IdSet.empty diff

  let safe_diff x y =
    DK.Commit.diff x y >|= function
    | Error _ -> Elt.IdSet.empty
    | Ok d    -> changes d

  let walk
      (type elt) (type t) (module Set: SET with type elt = elt and type t = t)
      tree root (file, fn) =
    let rec aux acc = function
      | [] -> Lwt.return acc
      | context :: todo ->
        match Datakit_path.of_steps context with
        | Error e -> Log.err (fun l -> l "%s" e); aux acc todo
        | Ok ctx  ->
          let dir = root /@ ctx in
          safe_read_dir tree dir >>= fun childs ->
          let todo = List.map (fun c -> context @ [c]) childs @ todo in
          safe_exists_file tree (dir / file) >>= function
          | false -> aux acc todo
          | true ->
            fn (Datakit_path.unwrap ctx) >>= function
            | None   -> aux acc todo
            | Some e -> aux (Set.add e acc) todo
    in
    aux Set.empty [ [] ]

  let empty = Datakit_path.empty

  let root r = empty / r.Repo.user / r.Repo.repo

  (* Repos *)

  let repo tree repo =
    safe_read_file tree (root repo / ".monitor") >|= function
    | None   ->
      Log.debug (fun l -> l "repo %a -> false" Repo.pp repo);
      None
    | Some _ ->
      Log.debug (fun l -> l "repo %a -> true" Repo.pp repo);
      Some repo

  let repos tree =
    let root = Datakit_path.empty in
    safe_read_dir tree root >>= fun users ->
    Lwt_list.fold_left_s (fun acc user ->
        safe_read_dir tree (root / user) >>= fun repos ->
        Lwt_list.fold_left_s (fun acc repo ->
            safe_read_file tree (root / user /repo / ".monitor") >|= function
            | None   -> acc
            | Some _ -> Repo.Set.add { Repo.user; repo } acc
          ) acc repos
      ) Repo.Set.empty users >|= fun repos ->
    Log.debug (fun l -> l "repos -> @;@[<2>%a@]" Repo.Set.pp repos);
    repos

  let update_repo_aux tr s r =
    let dir = root r in
    match s with
    | `Ignored   -> safe_remove tr (root r / ".monitor")
    | `Monitored ->
      let remove =
        DK.Transaction.make_dirs tr dir >>*= fun () ->
        let empty = Cstruct.of_string "" in
        DK.Transaction.create_or_replace_file tr ~dir ".monitor" empty
      in
      lift_errors "update_repo" remove

  let update_repo tr r = update_repo_aux tr `Monitored r
  let remove_repo tr r = update_repo_aux tr `Ignored r

  let update_commit tr c =
    let dir = root (Commit.repo c) / "commit" in
    lift_errors "update_commit" @@ DK.Transaction.make_dirs tr dir

  (* PRs *)

  let update_pr t pr =
    let dir = root (PR.repo pr) / "pr" / string_of_int pr.PR.number in
    Log.debug (fun l -> l "update_pr %s" @@ Datakit_path.to_hum dir);
    let update =
      DK.Transaction.make_dirs t dir >>*= fun () ->
      let write k v =
        let v = Cstruct.of_string (v ^ "\n") in
        DK.Transaction.create_or_replace_file t ~dir k v
      in
      write "head"  (PR.commit_id pr)                >>*= fun () ->
      write "state" (PR.string_of_state pr.PR.state) >>*= fun () ->
      write "title" pr.PR.title                      >>*= fun () ->
      write "base"  pr.PR.base
    in
    lift_errors "update_pr" update

  let remove_pr t (repo, num) =
    let dir = root repo / "pr" / string_of_int num in
    Log.debug (fun l -> l "remove_pr %s" @@ Datakit_path.to_hum dir);
    safe_remove t dir

  let pr tree repo number =
    let dir = root repo / "pr" / string_of_int number in
    Log.debug (fun l -> l "pr %a" Datakit_path.pp dir);
    safe_read_file tree (dir / "head")  >>= fun head ->
    safe_read_file tree (dir / "state") >>= fun state ->
    safe_read_file tree (dir / "title") >>= fun title ->
    safe_read_file tree (dir / "base")  >|= fun base ->
    match head, state with
    | None, _ ->
      Log.debug (fun l ->
          l "error: %a/pr/%d/head does not exist" Repo.pp repo number);
      None
    | _, None ->
      Log.debug (fun l ->
          l "error: %a/pr/%d/state does not exist" Repo.pp repo number);
      None
    | Some id, Some state ->
      let base = match base with
        | Some b -> b
        | None   ->
          Log.debug (fun l ->
              l "error: %a/pr/%d/base does not exist, using 'master' instead"
                Repo.pp repo number);
          "master"
      in
      let head = { Commit.repo; id } in
      let title = match title with None -> "" | Some t -> t in
      let state = match PR.state_of_string state with
        | Some s -> s
        | None    ->
          Log.err (fun l ->
              l "%s is not a valid PR state, picking `Closed instead"
                state);
          `Closed
      in
      Some { PR.head; number; state; title; base }

  let prs_of_repo tree repo =
    let dir = root repo / "pr"  in
    safe_read_dir tree dir >>= fun nums ->
    Lwt_list.fold_left_s (fun acc n ->
        pr tree repo (int_of_string n) >|= function
        | None   -> acc
        | Some p -> PR.Set.add p acc
      ) PR.Set.empty nums >|= fun prs ->
    Log.debug (fun l ->
        l "prs_of_repo %a -> @;@[<2>%a@]" Repo.pp repo PR.Set.pp prs);
    prs

  let maybe_repos tree = function
    | None -> repos tree
    | Some rs -> Lwt.return rs

  let prs ?repos:rs tree =
    maybe_repos tree rs >>= fun repos ->
    Lwt_list.fold_left_s (fun acc r ->
        prs_of_repo tree r >|= fun prs ->
        PR.Set.union prs acc
      ) PR.Set.empty (Repo.Set.elements repos)
    >|= fun prs ->
    Log.debug (fun l -> l "prs -> @;@[<2>%a@]" PR.Set.pp prs);
    prs

  (* Commits *)

  let commit tree repo id =
    let dir = root repo / "commit" / id in
    safe_exists_dir tree dir >|= function
    | false ->
      Log.debug (fun l -> l "commit {%a %s} -> false" Repo.pp repo id);
      None
    | true  ->
      Log.debug (fun l -> l "commit {%a %s} -> true" Repo.pp repo id);
      Some { Commit.repo; id }

  let commits_of_repo tree repo =
    let dir = root repo / "commit" in
    safe_read_dir tree dir >|= fun commits ->
    List.fold_left (fun s id ->
        Commit.Set.add { Commit.repo; id } s
      ) Commit.Set.empty commits
    |> fun cs ->
    Log.debug
      (fun l -> l "commits_of_repo %a -> @;@[<2>%a@]" Repo.pp repo
          Commit.Set.pp cs);
    cs

  let commits ?repos:rs tree =
    maybe_repos tree rs >>= fun repos ->
    Lwt_list.fold_left_s (fun acc r ->
        commits_of_repo tree r >|= fun commits ->
        Commit.Set.union commits acc
      ) Commit.Set.empty (Repo.Set.elements repos)
    >|= fun cs ->
    Log.debug (fun l -> l "commits -> @;@[<2>%a@]" Commit.Set.pp cs);
    cs

  (* Status *)

  let update_status t s =
    let dir = root (Status.repo s) / "commit" / (Status.commit_id s)
              / "status" /@ path (Status.context s)
    in
    Log.debug (fun l -> l "update_status %a" Datakit_path.pp dir);
    lift_errors "update_status" (DK.Transaction.make_dirs t dir) >>= fun () ->
    let description = match s.Status.description with
      | None   -> None
      | Some d -> Some (String.trim d)
    in
    let kvs = [
      "description", description;
      "state"      , Some (Status_state.to_string s.Status.state);
      "target_url" , s.Status.url;
    ] in
    Lwt_list.iter_s (fun (k, v) -> match v with
        | None   -> safe_remove t (dir / k)
        | Some v ->
          let v = Cstruct.of_string (v ^ "\n") in
          lift_errors "update_status" @@
          DK.Transaction.create_or_replace_file t ~dir k v
      ) kvs

  let status tree commit context =
    let context = Datakit_path.of_steps_exn context in
    let dir =
      root (Commit.repo commit) / "commit" / Commit.id commit / "status"
      /@ context
    in
    safe_read_file tree (dir / "state") >>= fun state ->
    match state with
    | None     ->
      Log.debug (fun l -> l "status %a -> None" Datakit_path.pp dir);
      Lwt.return_none
    | Some str ->
      let state = match Status_state.of_string str with
        | Some s -> s
        | None   ->
          Log.err (fun l -> l "%s: invalid state, using `Failure instead" str);
          `Failure
      in
      Log.debug (fun l -> l "status %a -> %a"
                    Datakit_path.pp context Status_state.pp state);
      safe_read_file tree (dir / "description") >>= fun description ->
      safe_read_file tree (dir / "target_url")  >|= fun url ->
      let context = Datakit_path.unwrap context in
      Some { Status.state; commit; context; description; url }

  let statuses_of_commits tree commits =
    Lwt_list.fold_left_s (fun acc commit ->
        let dir = root (Commit.repo commit) / "commit" in
        let dir = dir / Commit.id commit / "status" in
        walk (module Status.Set) tree dir ("state", status tree commit)
        >|= fun status ->
        Status.Set.union status acc
      ) Status.Set.empty (Commit.Set.elements commits)
    >|= fun status ->
    Log.debug (fun l -> l "statuses_of_commits %a -> @;@[<2>%a@]"
                  Commit.Set.pp commits Status.Set.pp status);
    status

  let maybe_commits tree = function
    | None   -> commits tree
    | Some c -> Lwt.return c

  let statuses ?commits:cs tree =
    maybe_commits tree cs >>= fun commits ->
    statuses_of_commits tree commits >|= fun status ->
    Log.debug (fun l -> l "statuses -> @;@[<2>%a@]" Status.Set.pp status);
    status

  (* Refs *)

  let ref_ tree repo name =
    let path = Datakit_path.of_steps_exn name in
    let head = root repo / "ref" /@ path / "head" in
    safe_read_file tree head >|= function
    | None    ->
      Log.debug (fun l -> l "ref_ %a:%a -> None" Repo.pp repo pp_path name);
      None
    | Some id ->
      Log.debug (fun l -> l "ref_ %a:%a -> %s" Repo.pp repo pp_path name id);
      let head = { Commit.repo; id } in
      Some { Ref.head; name }

  let refs_of_repo tree repo =
    let dir = root repo / "ref" in
    walk (module Ref.Set) tree dir ("head", ref_ tree repo) >|= fun refs ->
    Log.debug (fun l ->
        l "refs_of_repo %a -> @;@[<2>%a@]" Repo.pp repo Ref.Set.pp refs);
    refs

  let refs ?repos:rs tree =
    maybe_repos tree rs >>= fun repos ->
    Lwt_list.fold_left_s (fun acc r ->
        refs_of_repo tree r >|= fun refs ->
        Ref.Set.union acc refs
      ) Ref.Set.empty (Repo.Set.elements repos)
    >|= fun refs ->
    Log.debug (fun l -> l "refs -> @;@[<2>%a@]" Ref.Set.pp refs);
    refs

  let update_ref tr r =
    let path = Datakit_path.of_steps_exn (Ref.name r) in
    Log.debug (fun l -> l "update_ref %a" Datakit_path.pp path);
    let dir = root (Ref.repo r) / "ref" /@ path in
    let update =
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let head = Cstruct.of_string (Ref.commit_id r ^ "\n") in
      DK.Transaction.create_or_replace_file tr ~dir "head" head
    in
    lift_errors "update_ref" update

  let remove_ref tr (repo, name) =
    let path = Datakit_path.of_steps_exn name in
    Log.debug (fun l -> l "remove_ref %a" Datakit_path.pp path);
    let dir = root repo / "ref" /@ path in
    safe_remove tr dir

  let update_event t = function
    | Event.Repo (s, r) -> update_repo_aux t s r
    | Event.PR pr       -> update_pr t pr
    | Event.Status s    -> update_status t s
    | Event.Ref (`Removed, r) -> remove_ref t (Ref.id r)
    | Event.Ref (_, r)        -> update_ref t r
    | Event.Other o     ->
      Log.debug (fun l  -> l "ignoring event: %s" @@ snd o);
      Lwt.return_unit

  (* Snapshot *)

  let snapshot_of_repos tree repos =
    commits ~repos tree >>= fun commits ->
    prs ~repos tree >>= fun prs ->
    statuses ~commits tree >>= fun status ->
    refs ~repos tree >|= fun refs ->
    Snapshot.create ~repos ~status ~prs ~refs ~commits

  let snapshot_of_commit c =
    let tree = DK.Commit.tree c in
    repos tree >>= fun repos ->
    snapshot_of_repos tree repos

  (* Diffs *)

  let combine_repo t tree r =
    repo tree r >>= function
    | None   -> Lwt.return (Diff.with_remove (`Repo r)  t)
    | Some r ->
      snapshot_of_repos tree (Repo.Set.singleton r) >|= fun s ->
      Elt.Set.fold Diff.with_update (Snapshot.elts s) t

  let combine_commit t tree c =
    commit tree (Commit.repo c) (Commit.id c) >|= function
    | None   -> Diff.with_remove (`Commit c) t
    | Some c -> Diff.with_update (`Commit c) t

  let combine_pr t tree (r, id as x)  =
    pr tree r id >|= function
    | Some pr -> Diff.with_update (`PR pr) t
    | None    -> Diff.with_remove (`PR x) t

  let combine_status t tree (c, context as x) =
    status tree c context >|= function
    | None   -> Diff.with_remove (`Status x) t
    | Some s -> Diff.with_update (`Status s) t

  let combine_ref t tree (r, name as x) =
    ref_ tree r name >|= function
    | None   -> Diff.with_remove (`Ref x) t
    | Some r -> Diff.with_update (`Ref r) t

  let apply_on_commit diff head =
    Log.debug (fun l -> l "apply");
    let tree = DK.Commit.tree head in
    if Elt.IdSet.is_empty diff then Lwt.return Diff.empty
    else Lwt_list.fold_left_s (fun acc -> function
        | `Repo repo -> combine_repo acc tree repo
        | `PR id     -> combine_pr acc tree id
        | `Ref id    -> combine_ref acc tree id
        | `Commit id -> combine_commit acc tree id
        | `Status id ->
          combine_status acc tree id >>= fun acc ->
          combine_commit acc tree (fst id)
      ) Diff.empty (Elt.IdSet.elements diff)
      >|= fun r ->
      Log.debug (fun l ->
          l "apply @[<2>%a@]@;@[<2>->%a@]" Elt.IdSet.pp diff Diff.pp r);
      r

  type t = {
    head    : DK.Commit.t;
    snapshot: Snapshot.t;
  }

  let snapshot t = t.snapshot
  let head t = t.head

  let pp ppf s =
    Fmt.pf ppf "@[%a:@;@[<2>%a@]@]" DK.Commit.pp s.head Snapshot.pp s.snapshot

  let diff x y =
    safe_diff x y >>= fun diff ->
    apply_on_commit diff x

  let tr_head tr =
    DK.Transaction.parents tr >>= function
    | Error e ->
      Log.err (fun l -> l "tr_head: %a" DK.pp_error e);
      Lwt.fail_with "tr_head"
    | Ok  []  -> Lwt.fail_with "no parents!"
    | Ok [p]  -> Lwt.return p
    | Ok _    -> Lwt.fail_with "too many parents!"

  let of_branch ~debug ?old branch =
    DK.Branch.transaction branch >>= function
    | Error e ->
      Log.err
        (fun l -> l "snpshot %s: %a" (DK.Branch.name branch) DK.pp_error e);
      Lwt.fail_with "snapshot"
    | Ok tr ->
      Log.debug (fun l ->
          let c = match old with None -> "*" | Some t -> DK.Commit.id t.head in
          l "snapshot %s old=%s" debug c
        );
      tr_head tr >>= fun head ->
      match old with
      | None ->
        snapshot_of_commit head >|= fun snapshot -> tr, { head; snapshot }
      | Some old ->
        diff head old.head >|= fun diff ->
        let snapshot = Diff.apply diff old.snapshot in
        tr, { head; snapshot }

  let of_commit ~debug ?old head =
    Log.debug (fun l ->
        let c = match old with None -> "*" | Some t -> DK.Commit.id t.head in
        l "snapshot %s old=%s" debug c
      );
    match old with
    | None     -> snapshot_of_commit head >|= fun snapshot -> { head; snapshot }
    | Some old ->
      diff head old.head >|= fun diff ->
      let snapshot = Diff.apply diff old.snapshot in
      { head; snapshot }

  let remove_elt tr = function
    | `Repo repo -> remove_repo tr repo
    | `PR pr     -> remove_pr tr pr
    | `Ref r     -> remove_ref tr r
    | `Status (h, c) ->
      let dir =
        root (Commit.repo h) / "commit" / Commit.id h / "status"
        /@ path c
      in
      safe_remove tr dir
    | `Commit c ->
      let dir = root (Commit.repo c) / "commit" / c.Commit.id in
      safe_remove tr dir

  let update_elt tr = function
    | `Repo r   -> update_repo tr r
    | `Commit c -> update_commit tr c
    | `PR pr    -> update_pr tr pr
    | `Ref r    -> update_ref tr r
    | `Status s -> update_status tr s

  let remove ~debug t =
    if Elt.IdSet.is_empty t then None
    else
      let f tr =
        Log.debug
          (fun l -> l "remove_snapshot (from %s):@;%a" debug Elt.IdSet.pp t);
        Lwt_list.iter_s (remove_elt tr) (Elt.IdSet.elements t)
      in
      Some f

  let update ~debug t =
    if Elt.Set.is_empty t then None
    else
      let f tr =
        Log.debug
          (fun l -> l "update_snapshot (from %s):@;%a" debug Elt.Set.pp t);
        Lwt_list.iter_s (update_elt tr) (Elt.Set.elements t)
      in
      Some f

  let apply ~debug diff tr =
    let clean () = match remove ~debug (Diff.remove diff) with
      | None   -> Lwt.return_unit
      | Some f -> f tr
    in
    let update () = match update ~debug (Diff.update diff) with
      | None   -> Lwt.return_unit
      | Some f -> f tr
    in
    clean () >>= fun () ->
    update ()

end
