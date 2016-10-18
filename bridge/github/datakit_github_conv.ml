open Lwt.Infix
open Datakit_path.Infix
open Datakit_github

let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

let ( >>*= ) x f =
  x >>= function
  | Ok x         -> f x
  | Error _ as e -> Lwt.return e

let ok x = Lwt.return (Ok x)

let list_iter_s f l =
  Lwt_list.map_s f l >|= fun l ->
  List.fold_left (fun acc x -> match acc, x with
      | Ok (), Ok ()            -> Ok ()
      | Error e, _ | _, Error e -> Error e
    ) (Ok ()) (List.rev l)

let pp_path = Fmt.(list ~sep:(unit "/") string)

module Diff = struct

  type t = [
    | `Repo of Repo.t
    | `PR of PR.id
    | `Commit of Commit.t
    | `Status of Status.id
    | `Ref of Ref.id
    | `Unknown of Repo.t
  ]

  let pp ppf = function
    | `Repo r    -> Fmt.pf ppf "{%a}" Repo.pp r
    | `Unknown r -> Fmt.pf ppf "{%a ?}" Repo.pp r
    | `PR id     -> Fmt.pf ppf "%a" PR.pp_id id
    | `Ref id    -> Fmt.pf ppf "%a" Ref.pp_id id
    | `Commit c  -> Fmt.pf ppf "%a" Commit.pp c
    | `Status id -> Fmt.pf ppf "%a" Status.pp_id id

  let compare: t -> t -> int = Pervasives.compare

  module Set = Set(struct
      type nonrec t = t
      let compare = compare
      let pp = pp
    end)

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
            |  _ -> Some (`Unknown repo)
        in
        match t with
        | None   -> acc
        | Some t -> Set.add t acc
      ) Set.empty diff

end

module Make (DK: Datakit_S.CLIENT) = struct

  type nonrec 'a result = ('a, DK.error) result Lwt.t

  (* conversion between GitHub and DataKit states. *)

  let safe_remove t path =
    DK.Transaction.remove t path >>= function
    | Error _ | Ok () -> ok ()

  let safe_read_dir t dir =
    DK.Transaction.read_dir t dir >|= function
    | Error _ -> []
    | Ok dirs -> dirs

  let safe_exists_dir t dir =
    DK.Transaction.exists_dir t dir >|= function
    | Error _ -> false
    | Ok b    -> b

  let safe_exists_file t file =
    DK.Transaction.exists_file t file >|= function
    | Error _ -> false
    | Ok b    -> b

  let safe_read_file t file =
    DK.Transaction.read_file t file >|= function
    | Error _ -> None
    | Ok b    -> Some (String.trim (Cstruct.to_string b))

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

  let update_repo tr s r =
    let dir = root r in
    match s with
    | `Ignored   -> ok ()
    | `Monitored ->
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let empty = Cstruct.of_string "" in
      DK.Transaction.create_or_replace_file tr ~dir ".monitor" empty

  (* PRs *)

  let update_pr t pr =
    let dir = root (PR.repo pr) / "pr" / string_of_int pr.PR.number in
    Log.debug (fun l -> l "update_pr %s" @@ Datakit_path.to_hum dir);
    match pr.PR.state with
    | `Closed -> safe_remove t dir
    | `Open   ->
      DK.Transaction.make_dirs t dir >>*= fun () ->
      let write k v =
        let v = Cstruct.of_string (v ^ "\n") in
        DK.Transaction.create_or_replace_file t ~dir k v
      in
      write "head"  (PR.commit_id pr)                >>*= fun () ->
      write "state" (PR.string_of_state pr.PR.state) >>*= fun () ->
      write "title" pr.PR.title                      >>*= fun () ->
      write "base"  pr.PR.base

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
      (fun l -> l "commits_of_repo %a -> @;@[<2>%a@]" Repo.pp repo Commit.Set.pp cs);
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
              / "status" /@ Status.path s
    in
    Log.debug (fun l -> l "update_status %a" Datakit_path.pp dir);
    DK.Transaction.make_dirs t dir >>*= fun () ->
    let description = match s.Status.description with
      | None   -> None
      | Some d -> Some (String.trim d)
    in
    let kvs = [
      "description", description;
      "state"      , Some (Status_state.to_string s.Status.state);
      "target_url" , s.Status.url;
    ] in
    list_iter_s (fun (k, v) -> match v with
        | None   -> safe_remove t (dir / k)
        | Some v ->
          let v = Cstruct.of_string (v ^ "\n") in
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

  let update_ref tr s r =
    let path = Datakit_path.of_steps_exn (Ref.name r) in
    Log.debug (fun l -> l "update_ref %a" Datakit_path.pp path);
    let dir = root (Ref.repo r) / "ref" /@ path in
    match s with
    | `Removed -> safe_remove tr dir
    | `Created | `Updated ->
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let head = Cstruct.of_string (Ref.commit_id r ^ "\n") in
      DK.Transaction.create_or_replace_file tr ~dir "head" head

  let update_event t = function
    | Event.Repo (s, r) -> update_repo t s r
    | Event.PR pr       -> update_pr t pr
    | Event.Status s    -> update_status t s
    | Event.Ref (s, r)  -> update_ref t s r
    | Event.Other o     ->
      Log.debug (fun l  -> l "ignoring event: %s" @@ snd o);
      ok ()

  (* Snapshot *)

  let snapshot_of_repos tree repos =
    commits ~repos tree >>= fun commits ->
    prs ~repos tree >>= fun prs ->
    statuses ~commits tree >>= fun status ->
    refs ~repos tree >|= fun refs ->
    Snapshot.create ~repos ~status ~prs ~refs ~commits

  let snapshot_of_tree tree =
    repos tree >>= fun repos ->
    snapshot_of_repos tree repos

  (* Diffs *)

  let safe_diff t c =
    DK.Transaction.diff t c >|= function
    | Error _ -> Diff.Set.empty
    | Ok d    -> Diff.changes d

  let combine_repo t tree r =
    repo tree r >>= function
    | None   -> Snapshot.without_repo r t |> Lwt.return
    | Some r ->
      snapshot_of_repos tree (Repo.Set.singleton r) >|= fun s ->
      Snapshot.union s t

  let combine_commit t tree c =
    commit tree (Commit.repo c) (Commit.id c) >|= function
    | None   -> Snapshot.without_commit c t
    | Some c -> Snapshot.with_commit c t

  let combine_pr t tree (r, id as x)  =
    pr tree r id >|= function
    | None    -> Snapshot.without_pr x t
    | Some pr -> Snapshot.with_pr pr t

  let combine_status t tree (c, context as x) =
    status tree c context >|= function
    | None   -> Snapshot.without_status x t
    | Some s -> Snapshot.with_status s t

  let combine_ref t tree (r, name as x) =
    ref_ tree r name >|= function
    | None   -> Snapshot.without_ref x t
    | Some r -> Snapshot.with_ref r t

  let combine init (tree, diff) =
    Log.debug (fun l -> l "apply");
    if Diff.Set.is_empty diff then Lwt.return init
    else Lwt_list.fold_left_s (fun acc -> function
        | `Repo repo -> combine_repo acc tree repo
        | `PR id     -> combine_pr acc tree id
        | `Ref id    -> combine_ref acc tree id
        | `Commit id -> combine_commit acc tree id
        | `Status id ->
          combine_status acc tree id >>= fun acc ->
          combine_commit acc tree (fst id)
        | `Unknown _ -> Lwt.return acc
      ) init (Diff.Set.elements diff)
      >|= fun t ->
      Log.debug (fun l -> l "apply @[<2>(%a)@]@;@[<2>(%a)@]@;@[<2>->(%a)@]"
                    Diff.Set.pp diff Snapshot.pp init Snapshot.pp t);
      t

  let snapshot ~debug ?old tree =
    Log.debug (fun l ->
        let c = match old with None -> "*" | Some (c, _) -> DK.Commit.id c in
        l "snapshot %s old=%s" debug c
      );
    match old with
    | None        -> snapshot_of_tree tree
    | Some (c, s) ->
      safe_diff tree c >>= fun diff ->
      combine s (tree, diff) >|= fun s ->
      s

  let remove_snapshot ~debug = function
    | None   -> ok None
    | Some t ->
      let f tr =
        Log.debug
          (fun l -> l "remove_snapshot (from %s):@;%a" debug Snapshot.pp t);
        let root { Repo.user; repo } = Datakit_path.(empty / user / repo) in
        let prs = Snapshot.prs t in
        let refs = Snapshot.refs t in
        let status = Snapshot.status t in
        let commits = Snapshot.commits t in
        list_iter_s (fun pr ->
            let dir = root (PR.repo pr) / "pr" / string_of_int pr.PR.number in
            safe_remove tr dir
          ) (PR.Set.elements prs)
        >>*= fun () ->
        list_iter_s (fun r ->
            let dir = root (Ref.repo r) / "ref" /@ Ref.path r in
            safe_remove tr dir
          ) (Ref.Set.elements refs)
        >>*= fun () ->
        list_iter_s (fun s ->
            let id = Status.commit_id s in
            let c  = Status.path s in
            let dir = root (Status.repo s) / "commit" / id / "status" /@ c in
            safe_remove tr dir
          ) (Status.Set.elements status)
        >>*= fun () ->
        list_iter_s (fun c ->
            let dir = root (Commit.repo c) / "commit" / c.Commit.id in
            safe_remove tr dir
          ) (Commit.Set.elements commits)
      in
      ok (Some f)

  let update_snapshot ~debug = function
    | None   -> ok None
    | Some t ->
      let f tr =
        Log.debug
          (fun l -> l "update_snapshot (from %s):@;%a" debug Snapshot.pp t);
        let repos = Snapshot.repos t in
        let prs = Snapshot.prs t in
        let refs = Snapshot.refs t in
        let status = Snapshot.status t in
        list_iter_s (update_repo tr `Monitored) (Repo.Set.elements repos)
        >>*= fun () ->
        list_iter_s (update_pr tr) (PR.Set.elements prs)
        >>*= fun () ->
        list_iter_s (update_ref tr `Updated) (Ref.Set.elements refs)
        >>*= fun () ->
        list_iter_s (update_status tr) (Status.Set.elements status)
      in
      ok (Some f)

  let apply ~debug diff tr =
    let remove = Snapshot.to_remove diff in
    let update = Snapshot.to_update diff in
    let clean () =
      remove_snapshot ~debug remove >>*= function
      | None   -> ok ()
      | Some f -> f tr
    in
    let update () =
      update_snapshot ~debug update >>*= function
      | None   -> ok ()
      | Some f -> f tr
    in
    clean () >>*= fun () ->
    update ()

end
