let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

module type ELT = sig
  include Set.OrderedType
  val pp: t Fmt.t
end

module type SET = sig
  include Set.S
  val pp: t Fmt.t
end

module Set (E: ELT) = struct

  include Set.Make(E)

  let pp ppf t = Fmt.(list ~sep:(unit "@;") E.pp) ppf (elements t)

  let map f t = fold (fun x acc -> add (f x) acc) t empty

  let index t f =
    let tbl = Hashtbl.create (cardinal t) in
    iter (fun x ->
        let i = f x in
        let v =
          try Hashtbl.find tbl i
          with Not_found -> []
        in
        Hashtbl.replace tbl i (x :: v)
      ) t;
    tbl

end

let pp_path = Fmt.(list ~sep:(unit "/") string)

module Repo = struct

  type t = { user: string; repo: string }
  let pp ppf t = Fmt.pf ppf "%s/%s" t.user t.repo
  let compare (x:t) (y:t) = Pervasives.compare x y
  type state = [`Monitored | `Ignored]

  let pp_state ppf = function
    | `Monitored -> Fmt.string ppf "+"
    | `Ignored   -> Fmt.string ppf "-"

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)

end

module Status_state = struct

    type t = [ `Error | `Pending | `Success | `Failure ]

    let to_string = function
    | `Error   -> "error"
    | `Failure -> "failure"
    | `Pending -> "pending"
    | `Success -> "success"

  let pp =  Fmt.of_to_string to_string

  let of_string = function
    | "error"   -> Some `Error
    | "failure" -> Some `Failure
    | "pending" -> Some `Pending
    | "success" -> Some `Success
    | _         -> None

end

let compare_fold fs x y =
  List.fold_left (fun acc f ->
      match acc with
      | 0 -> f x y
      | i -> i
    ) 0 (List.rev fs)

module Commit = struct

  type t = { repo: Repo.t; id : string }

  let pp ppf t = Fmt.pf ppf "{%a %s}" Repo.pp t.repo t.id
  let id t = t.id
  let repo t = t.repo
  let compare_repo x y = Repo.compare x.repo y.repo
  let compare_id x y = String.compare x.id y.id
  let equal (x:t) (y:t) = x = y

  let compare = compare_fold [
      compare_repo;
      compare_id;
    ]

  module Set = struct
    include Set(struct
        type nonrec t = t
        let pp = pp
        let compare = compare
      end)
    let repos t =
      fold (fun c acc -> Repo.Set.add (repo c) acc) t Repo.Set.empty
  end

end

module PR = struct

  type t = {
    head: Commit.t;
    number: int;
    state: [`Open | `Closed];
    title: string;
    base: string;
  }

  type id = Repo.t * int

  let string_of_state = function
    | `Open   -> "open"
    | `Closed -> "closed"

  let state_of_string  = function
    | "open"   -> Some `Open
    | "closed" -> Some `Closed
    | _        -> None

  let pp_state ppf = function
    | `Open   -> Fmt.string ppf "open"
    | `Closed -> Fmt.string ppf "closed"

  let repo t = t.head.Commit.repo
  let id t = repo t, t.number
  let commit t = t.head
  let commit_id t = t.head.Commit.id
  let compare_repo x y = Repo.compare (repo x) (repo y)
  let compare_num x y = Pervasives.compare x.number y.number
  let number t = t.number
  let title t = t.title
  let state t = t.state
  let close t = { t with state = `Closed }
  let same_id x y = repo x = repo y && number x = number y

  let compare = compare_fold [
      compare_repo;
      compare_num;
      Pervasives.compare;
    ]

  let pp ppf t =
    Fmt.pf ppf "{%a %d[%s] %s %a %S}"
      Repo.pp (repo t) t.number (commit_id t) t.base pp_state t.state t.title

  let pp_id ppf (r, n) = Fmt.pf ppf "{%a %d}" Repo.pp r n

  module Set = struct
    include Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
      end)
    let repos t =
      fold (fun c acc -> Repo.Set.add (repo c) acc) t Repo.Set.empty
    let commits t =
      fold (fun c acc -> Commit.Set.add (commit c) acc) t Commit.Set.empty
  end

end

module Status = struct

  type t = {
    commit: Commit.t;
    context: string list;
    url: string option;
    description: string option;
    state: Status_state.t;
  }

  type id = Commit.t * string list

  let context t = match t.context with
    | [] -> ["default"]
    | l  -> l

  let id t = t.commit, t.context
  let path s = Datakit_path.of_steps_exn (context s)
  let repo t = t.commit.Commit.repo
  let commit t = t.commit
  let commit_id t = t.commit.Commit.id
  let same_id x y = commit x = commit y && context x = context y
  let compare_repo x y = Repo.compare (repo x) (repo y)
  let compare_commit_id x y = Pervasives.compare (commit_id x) (commit_id y)
  let compare_context x y = Pervasives.compare x.context y.context

  let compare = compare_fold [
      compare_repo;
      compare_commit_id;
      compare_context;
      Pervasives.compare
    ]

  let pp_opt k ppf v = match v with
    | None   -> ()
    | Some v -> Fmt.pf ppf " %s=%s" k v

  let pp ppf t =
    Fmt.pf ppf "{%a %s:%a[%a]%a%a}"
      Repo.pp (repo t) (commit_id t)
      pp_path t.context
      Status_state.pp t.state
      (pp_opt "url") t.url
      (pp_opt "descr") t.description

  let pp_id ppf (c, s) = Fmt.pf ppf "{%a %a}" Commit.pp c pp_path s

  module Set = struct
    include Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
      end)
    let repos t =
      fold (fun c acc -> Repo.Set.add (repo c) acc) t Repo.Set.empty
    let commits t =
      fold (fun c acc -> Commit.Set.add (commit c) acc) t Commit.Set.empty
  end

end

module Ref = struct

  type t = {
    head: Commit.t;
    name: string list;
  }

  type id = Repo.t * string list

  let repo t = t.head.Commit.repo
  let id t = repo t, t.name
  let commit t = t.head
  let commit_id t = t.head.Commit.id
  let name t = t.name
  let compare_repo x y = Repo.compare (repo x) (repo y)
  let compare_name x y = Pervasives.compare x.name y.name
  let same_id x y = repo x = repo y && name x = name y
  let path s = Datakit_path.of_steps_exn s.name

  let compare = compare_fold [
      compare_repo;
      compare_name;
      Pervasives.compare;
    ]

  let pp ppf t =
    Fmt.pf ppf "{%a %a[%s]}" Repo.pp (repo t) pp_path t.name (commit_id t)

  let pp_id ppf (r, p) = Fmt.pf ppf "{%a %a}" Repo.pp r pp_path p

  module Set = struct
    include Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
      end)
    let repos t =
      fold (fun c acc -> Repo.Set.add (repo c) acc) t Repo.Set.empty
    let commits t =
      fold (fun c acc -> Commit.Set.add (commit c) acc) t Commit.Set.empty
  end

  type state = [`Created | `Updated | `Removed]

  let pp_state ppf = function
    | `Created -> Fmt.string ppf "+"
    | `Updated -> Fmt.string ppf "*"
    | `Removed -> Fmt.string ppf "-"

end

module Event = struct

  type t =
    | Repo of (Repo.state * Repo.t)
    | PR of PR.t
    | Status of Status.t
    | Ref of (Ref.state * Ref.t)
    | Other of (Repo.t * string)

  let of_repo s r = Repo (s, r)
  let of_pr x = PR x
  let of_status x = Status x
  let of_ref x y = Ref (x, y)
  let of_other x y = Other (x, y)

  let pp ppf = function
    | Repo(s,r)-> Fmt.pf ppf "Repo: %a%a" Repo.pp_state s Repo.pp r
    | PR pr    -> Fmt.pf ppf "PR: %a" PR.pp pr
    | Status s -> Fmt.pf ppf "Status: %a" Status.pp s
    | Ref(s,r) -> Fmt.pf ppf "Ref: %a%a" Ref.pp_state s Ref.pp r
    | Other o  -> Fmt.pf ppf "Other: %s" @@ snd o

  let repo = function
    | Repo r   -> snd r
    | PR pr    -> PR.repo pr
    | Status s -> Status.repo s
    | Ref r    -> Ref.repo (snd r)
    | Other o  -> fst o

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)

end

module Snapshot = struct

  type t = {
    repos  : Repo.Set.t;
    commits: Commit.Set.t;
    status : Status.Set.t;
    prs    : PR.Set.t;
    refs   : Ref.Set.t;
  }

  let repos t = t.repos
  let status t = t.status
  let prs t = t.prs
  let refs t = t.refs
  let commits t = t.commits

  let empty =
    { repos = Repo.Set.empty;
      commits = Commit.Set.empty;
      status = Status.Set.empty;
      prs = PR.Set.empty;
      refs = Ref.Set.empty }

  let is_empty s =
    Repo.Set.is_empty s.repos &&
    Commit.Set.is_empty s.commits &&
    Status.Set.is_empty s.status &&
    PR.Set.is_empty s.prs &&
    Ref.Set.is_empty s.refs

  let union x y = {
    repos   = Repo.Set.union x.repos y.repos;
    commits = Commit.Set.union x.commits y.commits;
    status  = Status.Set.union x.status y.status;
    prs     = PR.Set.union x.prs y.prs;
    refs    = Ref.Set.union x.refs y.refs;
  }

  let create ~repos ~commits ~status ~prs ~refs =
    { repos; commits; status; prs; refs }

  let compare_repos x y = Repo.Set.compare x.repos y.repos
  let compare_commits x y = Commit.Set.compare x.commits y.commits
  let compare_status x y = Status.Set.compare x.status y.status
  let compare_prs x y = PR.Set.compare x.prs y.prs
  let compare_refs x y = Ref.Set.compare x.refs y.refs

  let compare = compare_fold [
      compare_repos;
      compare_commits;
      compare_status;
      compare_prs;
      compare_refs
    ]

  let pp ppf t =
    if compare t empty = 0 then Fmt.string ppf "empty"
    else
      Fmt.pf ppf "{@[<2>repos:%a@]@;@[<2>prs:%a@]@;@[<2>refs:%a@]@;\
                  @[<2>commits:%a@]@;@[<2>status:%a@]}"
        Repo.Set.pp t.repos PR.Set.pp t.prs Ref.Set.pp t.refs
        Commit.Set.pp t.commits Status.Set.pp t.status

  let without_repo repo t =
    let keep f r = Repo.compare (f r) repo <> 0 in
    let repos = Repo.Set.remove repo t.repos in
    let prs = PR.Set.filter (keep PR.repo) t.prs in
    let refs = Ref.Set.filter (keep Ref.repo) t.refs in
    let commits = Commit.Set.filter (keep Commit.repo) t.commits in
    let status = Status.Set.filter (keep Status.repo) t.status in
    { repos; prs; refs; commits; status }

  let without_repos = Repo.Set.fold without_repo

  let keep_repos t repos =
    let keep f r = Repo.Set.mem (f r) repos in
    let repos = Repo.Set.diff t.repos repos in
    let prs = PR.Set.filter (keep PR.repo) t.prs in
    let refs = Ref.Set.filter (keep Ref.repo) t.refs in
    let commits = Commit.Set.filter (keep Commit.repo) t.commits in
    let status = Status.Set.filter (keep Status.repo) t.status in
    { repos; prs; refs; commits; status }

  let with_repo r t = { t with repos = Repo.Set.add r t.repos }
  let with_repos = Repo.Set.fold with_repo

  let without_commit { Commit.repo; id } t =
    let keep x = repo <> Commit.repo x || id <> Commit.id x in
    { t with commits = Commit.Set.filter keep t.commits }

  let with_commit c t =
    let commits = Commit.Set.add c t.commits in
    { t with commits }

  let without_pr (r, id) t =
    let keep pr = r  <> PR.repo pr || id <>  pr.PR.number in
    { t with prs = PR.Set.filter keep t.prs }

  let add_pr pr t =
    let prs     = PR.Set.add pr t.prs in
    let commits = Commit.Set.add (PR.commit pr) t.commits in
    { t with prs; commits }

  let with_pr pr t =
    if not (Repo.Set.mem (PR.repo pr) t.repos) then t
    else
      let id = PR.repo pr, pr.PR.number in
      add_pr pr (without_pr id t)

  let without_status (s, l) t =
    let keep x = s <> Status.commit x || l <> x.Status.context in
    { t with status = Status.Set.filter keep t.status }

  let add_status t s =
    let status  = Status.Set.add s t.status in
    let commits = Commit.Set.add (Status.commit s) t.commits in
    { t with status; commits }

  let with_status s t =
    if not (Repo.Set.mem (Status.repo s) t.repos) then t
    else
      let cc = s.Status.commit, s.Status.context in
      add_status (without_status cc t) s

  let without_ref (r, l) t =
    let keep x = r <> Ref.repo x || l <> x.Ref.name in
    { t with refs = Ref.Set.filter keep t.refs }

  let add_ref t r =
    let refs = Ref.Set.add r t.refs in
    { t with refs }

  let with_ref r t =
    if not (Repo.Set.mem (Ref.repo r) t.repos) then t
    else
      let name = Ref.repo r, r.Ref.name in
      add_ref (without_ref name t) r

  let with_event = function
    | Event.Repo (`Ignored,r) -> without_repo r
    | Event.Repo (_, r)       -> with_repo r
    | Event.PR pr             -> with_pr pr
    | Event.Ref (`Removed, r) -> without_ref (Ref.repo r, Ref.name r)
    | Event.Ref (_, r)        -> with_ref r
    | Event.Status s          -> with_status s
    | Event.Other _           -> fun t -> t

  type diff = {
    remove: t option;
    update: t option;
  }

  let to_update t = t.update
  let to_remove t = t.remove

  let pp_diff ppf t =
    Fmt.pf ppf "@[remove:%a@;update:%a]"
      Fmt.(option pp) t.remove Fmt.(option pp) t.update

  (* [prune t] is [t] with all the closed PRs pruned. *)
  let prune t =
    let status = Status.Set.index t.status Status.repo in
    let prs = PR.Set.index t.prs PR.repo in
    let refs = Ref.Set.index t.refs Ref.repo in
    let commits = Commit.Set.index t.commits Commit.repo in
    let find r x = try Hashtbl.find x r with Not_found -> []  in
    let aux repo =
      let status  = find repo status  |> Status.Set.of_list in
      let prs     = find repo prs     |> PR.Set.of_list in
      let refs    = find repo refs    |> Ref.Set.of_list in
      let commits = find repo commits |> Commit.Set.of_list in
      let open_prs, closed_prs =
        PR.Set.fold (fun pr (open_prs, closed_prs) ->
            match pr.PR.state with
            | `Open   -> PR.Set.add pr open_prs, closed_prs
            | `Closed -> open_prs, PR.Set.add pr closed_prs
          ) prs (PR.Set.empty, PR.Set.empty)
      in
      Log.debug (fun l -> l "[prune]+prs:@;%a" PR.Set.pp open_prs);
      Log.debug (fun l -> l "[prune]-prs:@;%a" PR.Set.pp closed_prs);
      let is_commit_open c =
        PR.Set.exists (fun pr -> PR.commit pr = c) open_prs
        || Ref.Set.exists (fun r -> Ref.commit r = c) refs
      in
      let open_commits, closed_commits =
        Commit.Set.fold (fun c (open_commit, closed_commit) ->
            match is_commit_open c with
            | false -> open_commit, Commit.Set.add c closed_commit
            | true  -> Commit.Set.add c open_commit, closed_commit
          ) commits (Commit.Set.empty, Commit.Set.empty)
      in
      Log.debug (fun l -> l "[prune]+commits:@;%a" Commit.Set.pp open_commits);
      Log.debug (fun l -> l "[prune]-commits:@;%a" Commit.Set.pp closed_commits);
      let is_status_open s =
        Commit.Set.exists (fun c -> s.Status.commit = c ) open_commits
      in
      let open_status, closed_status =
        Status.Set.fold (fun s (open_status, closed_status) ->
            match is_status_open s with
            | false -> open_status, Status.Set.add s closed_status
            | true  -> Status.Set.add s open_status, closed_status
          ) status (Status.Set.empty, Status.Set.empty)
      in
      let cleanup = {
        repos   = Repo.Set.empty;
        refs    = Ref.Set.empty;
        prs     = closed_prs;
        status  = closed_status;
        commits = closed_commits;
      } in
      Log.debug (fun l -> l "[prune]+status:@;%a" Status.Set.pp open_status);
      Log.debug (fun l -> l "[prune]-status:@;%a" Status.Set.pp closed_status);
      let repos   = Repo.Set.singleton repo in
      let status  = open_status in
      let prs     = open_prs in
      let commits = open_commits in
      let t = { repos; status; prs; refs; commits } in
      let cleanup =
        if PR.Set.is_empty closed_prs && Commit.Set.is_empty closed_commits
        then `Clean
        else `Prune cleanup
      in
      (t, cleanup)
    in
    let result, cleanup =
      Repo.Set.fold (fun r (result, cleanup) ->
          let (x, c) = aux r in
          let result = union result x in
          let cleanup = match c with
            | `Clean   -> cleanup
            | `Prune c -> union cleanup c
          in
          result, cleanup
        ) t.repos (empty, empty)
    in
    if PR.Set.is_empty cleanup.prs && Commit.Set.is_empty cleanup.commits then (
      assert (compare (keep_repos t t.repos) (keep_repos result t.repos)  = 0);
      { update = Some result; remove = None }
    ) else
      { update = Some result; remove = Some cleanup }

  (* Compute the diff between old_s and new_s. *)
  let diff new_s old_s =
    let repos = repos new_s in
    let mk repos keep_pr keep_ref keep_status keep_commit =
      let prs = PR.Set.filter keep_pr old_s.prs in
      let refs = Ref.Set.filter keep_ref old_s.refs in
      let status = Status.Set.filter keep_status old_s.status in
      let commits = Commit.Set.filter keep_commit old_s.commits in
      let t = { repos; prs; refs; commits; status } in
      if compare empty t = 0 then None else Some t
    in
    let remove =
      let keep_pr x =
        Repo.Set.mem (PR.repo x) repos
        && not (PR.Set.exists (PR.same_id x) new_s.prs)
      in
      let keep_ref x =
        Repo.Set.mem (Ref.repo x) repos
        && not (Ref.Set.exists (Ref.same_id x) new_s.refs)
      in
      let keep_status x =
        Repo.Set.mem (Status.repo x) repos
        && not (Status.Set.exists (Status.same_id x) new_s.status)
      in
      let keep_commit x =
        Repo.Set.mem (Commit.repo x) repos
        && not (Commit.Set.exists (Commit.equal x) new_s.commits)
      in
      mk Repo.Set.empty keep_pr keep_ref keep_status keep_commit
    in
    let update =
      let keep_pr x =
        Repo.Set.mem (PR.repo x) repos
        && PR.Set.exists
          (fun y -> PR.same_id x y && PR.compare x y <> 0) new_s.prs
      in
      let keep_ref x =
        Repo.Set.mem (Ref.repo x) repos
        && Ref.Set.exists
          (fun y -> Ref.same_id x y && Ref.compare x y <> 0) new_s.refs
      in
      let keep_status x =
        Repo.Set.mem (Status.repo x) repos
        && Status.Set.exists
          (fun y -> Status.same_id x y && Status.compare x y <> 0) new_s.status
      in
      let keep_commit _ = false in
      mk repos keep_pr keep_ref keep_status keep_commit
    in
    { remove; update }

end
