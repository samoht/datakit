open Astring

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

let pp_set (type a) k (module S: SET with type t = a) ppf (v:a) =
  if S.is_empty v then Fmt.string ppf "" else
    Fmt.pf ppf "@[<2>%s:@;%a@;@]" k S.pp v

let pp_field k pp ppf v = Fmt.pf ppf "@[<2>%s:@;%a@]" k pp v

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

  module IdSet = Set(struct
      type t = id
      let compare_repo x y = Repo.compare (fst x) (fst y)
      let compare_num x y = Pervasives.compare (snd x) (snd y)
      let compare = compare_fold [ compare_repo; compare_num ]
      let pp = pp_id
    end)

  module Set = struct
    include Set(struct
        type nonrec t = t
        let pp = pp
        let compare = compare
      end)
    let ids t = elements t |> List.map id |> IdSet.of_list
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

  module IdSet = Set(struct
      type t = id
      let compare_commit x y = Commit.compare (fst x) (fst y)
      let compare_context x y = Pervasives.compare (snd x) (snd y)
      let compare = compare_fold [ compare_commit; compare_context ]
      let pp = pp_id
    end)

  module Set = struct
    include Set(struct
        type nonrec t = t
        let pp = pp
        let compare = compare
      end)
    let ids t = elements t |> List.map id |> IdSet.of_list
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

  module IdSet = Set (struct
      type t = id
      let pp = pp_id
      let compare_repo x y = Repo.compare (fst x) (fst y)
      let compare_context x y = Pervasives.compare (snd x) (snd y)
      let compare = compare_fold [ compare_repo; compare_context ]
    end)

  module Set = struct
    include Set(struct
        type nonrec t = t
        let pp = pp
        let compare = compare
      end)
    let ids t = elements t |> List.map id |> IdSet.of_list
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
    | Repo(s,r) -> Fmt.pf ppf "Repo: %a%a" Repo.pp_state s Repo.pp r
    | PR pr     -> Fmt.pf ppf "PR: %a" PR.pp pr
    | Status s  -> Fmt.pf ppf "Status: %a" Status.pp s
    | Ref(s,r)  -> Fmt.pf ppf "Ref: %a%a" Ref.pp_state s Ref.pp r
    | Other o   -> Fmt.pf ppf "Other: %s" @@ snd o

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

  let pp ppf t =
    if compare t empty = 0 then Fmt.string ppf "{}"
    else
      Fmt.pf ppf "{%a%a%a%a%a}"
        (pp_set "repos"   (module Repo.Set)) t.repos
        (pp_set "prs"     (module PR.Set)) t.prs
        (pp_set "refs"    (module Ref.Set)) t.refs
        (pp_set "commits" (module Commit.Set)) t.commits
        (pp_set "status"  (module Status.Set)) t.status

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

  type keep = { f: 'a . ('a -> Repo.t) -> 'a -> bool }

  let with_repo r t = { t with repos = Repo.Set.add r t.repos }
  let with_repos = Repo.Set.fold with_repo

  let without_repo_f keep t =
    let repos = Repo.Set.filter (keep.f (fun x -> x)) t.repos in
    let prs = PR.Set.filter (keep.f PR.repo) t.prs in
    let refs = Ref.Set.filter (keep.f Ref.repo) t.refs in
    let commits = Commit.Set.filter (keep.f Commit.repo) t.commits in
    let status = Status.Set.filter (keep.f Status.repo) t.status in
    { repos; prs; refs; commits; status }

  let without_repo repo =
    without_repo_f { f = fun f r -> Repo.compare (f r) repo <> 0 }

  let without_repos repos =
    without_repo_f { f = fun f r -> not (Repo.Set.mem (f r) repos) }

  let without_commit { Commit.repo; id } t =
    let keep x = repo <> Commit.repo x || id <> Commit.id x in
    { t with commits = Commit.Set.filter keep t.commits }

  let with_commit c t =
    let commits = Commit.Set.add c t.commits in
    { t with commits }

  let without_commits = Commit.Set.fold without_commit
  let with_commits = Commit.Set.fold with_commit

  let without_pr (r, id) t =
    let keep pr = r  <> PR.repo pr || id <>  pr.PR.number in
    { t with prs = PR.Set.filter keep t.prs }

  let add_pr pr t = { t with prs = PR.Set.add pr t.prs}

  let with_pr pr t =
    let id = PR.repo pr, pr.PR.number in
    add_pr pr (without_pr id t)

  let with_prs = PR.Set.fold with_pr
  let without_prs = PR.IdSet.fold without_pr

  let without_status (s, l) t =
    let keep x = s <> Status.commit x || l <> x.Status.context in
    { t with status = Status.Set.filter keep t.status }

  let add_status t s = { t with status = Status.Set.add s t.status }

  let with_status s t = add_status (without_status (Status.id s) t) s
  let with_statuses = Status.Set.fold with_status
  let without_statuses = Status.IdSet.fold without_status

  let without_ref (r, l) t =
    let keep x = r <> Ref.repo x || l <> x.Ref.name in
    { t with refs = Ref.Set.filter keep t.refs }

  let add_ref t r = { t with refs = Ref.Set.add r t.refs }
  let with_ref r t = add_ref (without_ref (Ref.id r) t) r
  let with_refs = Ref.Set.fold with_ref
  let without_refs = Ref.IdSet.fold without_ref

  let with_event = function
    | Event.Repo (`Ignored,r) -> without_repo r
    | Event.Repo (_, r)       -> with_repo r
    | Event.PR pr             -> with_pr pr
    | Event.Ref (`Removed, r) -> without_ref (Ref.repo r, Ref.name r)
    | Event.Ref (_, r)        -> with_ref r
    | Event.Status s          -> with_status s
    | Event.Other _           -> fun t -> t

  let with_events es t = List.fold_left (fun acc e -> with_event e acc) t es

  type diff = { remove: t; update: t }

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
      Log.debug (fun l -> l "[prune]+status:@;%a" Status.Set.pp open_status);
      Log.debug (fun l -> l "[prune]-status:@;%a" Status.Set.pp closed_status);
      let repos   = Repo.Set.singleton repo in
      let status  = open_status in
      let prs     = open_prs in
      let commits = open_commits in
      { repos; status; prs; refs; commits }
    in
    Repo.Set.fold (fun r acc -> union acc (aux r)) t.repos empty

  (* Compute the diff between old_s and new_s. *)
  let diff x y =
    Log.debug (fun l -> l "%a@;%a" pp x pp y);
    let mk t repos skip_pr skip_ref skip_status skip_commit =
      let neg f x = not (f x) in
      let prs = PR.Set.filter (neg skip_pr) t.prs in
      let refs = Ref.Set.filter (neg skip_ref) t.refs in
      let status = Status.Set.filter (neg skip_status) t.status in
      let commits = Commit.Set.filter (neg skip_commit) t.commits in
      let t = { repos; prs; refs; commits; status } in
      Log.debug (fun l -> l "XXX YYY %a" pp t);
      t
    in
    let remove =
      let repos = Repo.Set.diff y.repos x.repos in
      let skip_pr pr = PR.Set.exists (PR.same_id pr) x.prs in
      let skip_ref r = Ref.Set.exists (Ref.same_id r) x.refs in
      let skip_status s = Status.Set.exists (Status.same_id s) x.status in
      let skip_commit c = Commit.Set.exists (Commit.equal c) x.commits in
      mk y repos skip_pr skip_ref skip_status skip_commit
    in
    let update =
      let repos = Repo.Set.diff x.repos y.repos in
      let skip_pr pr = PR.Set.exists (fun x -> PR.same_id pr x) y.prs in
      let skip_ref r = Ref.Set.exists (fun x -> Ref.same_id r x) y.refs in
      let skip_status s =
        Status.Set.exists (fun x -> Status.same_id s x) y.status
      in
      let skip_commit _ = true in
      mk x repos skip_pr skip_ref skip_status skip_commit
    in
    { remove; update }

end

module Diff = struct

  open Snapshot

  type t = Snapshot.diff
  let empty = { update = Snapshot.empty; remove = Snapshot.empty }

  let update t = t.update
  let remove t = t.remove

  let cardinal t =
    Repo.Set.cardinal t.repos
    + Status.Set.cardinal t.status
    + Commit.Set.cardinal t.commits
    + PR.Set.cardinal t.prs
    + Ref.Set.cardinal t.refs

  let compare x y =
    match Snapshot.compare x.update y.update with
    | 0 -> Snapshot.compare x.remove y.remove
    | i -> i

  let pp ppf t =
    Fmt.pf ppf "@[[%a@;%a]@]"
      (pp_field "update" Snapshot.pp) t.update
      (pp_field "remove" Snapshot.pp) t.remove

  let commit_message t =
    let updates = cardinal t.update in
    let removes = cardinal t.remove in
    if updates = 0 && removes = 0 then Fmt.strf "No changes!"
    else if updates = 0 && removes = 1 then
      Fmt.strf "1 item removed@;@;@[<2>%a@]" Snapshot.pp t.remove
    else if updates = 0 then
      Fmt.strf "%d items removed@;@;@[<2>%a@]" removes Snapshot.pp t.remove
    else if removes = 0 && updates = 1 then
      Fmt.strf "1 item updated@;@;@[<2>%a@]" Snapshot.pp t.update
    else if removes = 0 then
      Fmt.strf "%d items removed@;@;@[<2>%a@]"
        removes Snapshot.pp t.remove
    else
      Fmt.strf "%d items modified@;@;@[Updated@;<2>%a@]@;@;\
                @[Removed@;<2>%a@]"
        (updates+removes) Snapshot.pp t.update Snapshot.pp t.remove

  let is_empty t = is_empty t.remove && is_empty t.update
  let with_update s t = { t with update = Snapshot.union s t.update }
  let with_remove s t = { t with remove = Snapshot.union s t.remove }

  let apply d t =
    let remove =
      (* remove all the stuff that we don't need. *)
      let repos = Snapshot.repos d.remove in
      let commits = Snapshot.commits d.remove in
      let status = Snapshot.status d.remove |> Status.Set.ids in
      let refs = Snapshot.refs d.remove |> Ref.Set.ids in
      let prs = Snapshot.prs d.remove |> PR.Set.ids in
      Snapshot.without_repos repos t
      |> Snapshot.without_commits commits
      |> Snapshot.without_statuses status
      |> Snapshot.without_refs refs
      |> Snapshot.without_prs prs
    in
    let update =
      (* update new stuff *)
      let repos = Snapshot.repos d.update in
      let commits = Snapshot.commits d.update in
      let status = Snapshot.status d.update in
      let refs = Snapshot.refs d.update in
      let prs = Snapshot.prs d.update in
      Snapshot.with_repos repos remove
      |> Snapshot.with_commits commits
      |> Snapshot.with_statuses status
      |> Snapshot.with_refs refs
      |> Snapshot.with_prs prs
    in
    update

end

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

module type API = sig
  type token
  type 'a result = ('a, string) Result.result Lwt.t
  val user_exists: token -> user:string -> bool result
  val repo_exists: token -> Repo.t -> bool result
  val repos: token -> user:string -> Repo.t list result
  val status: token -> Commit.t -> Status.t list result
  val set_status: token -> Status.t -> unit result
  val set_ref: token -> Ref.t -> unit result
  val remove_ref: token -> Repo.t -> string list -> unit result
  val set_pr: token -> PR.t -> unit result
  val prs: token -> Repo.t -> PR.t list result
  val refs: token -> Repo.t -> Ref.t list result
  val events: token -> Repo.t -> Event.t list result
  module Webhook: sig
    type t
    val create: token -> Uri.t -> t
    val run: t -> unit Lwt.t
    val repos: t -> Repo.Set.t
    val watch: t -> Repo.t -> unit Lwt.t
    val events: t -> Event.t list
    val wait: t -> unit Lwt.t
    val clear: t -> unit
  end
end

module State (API: API) = struct

  open Lwt.Infix

  type token = {
    t: API.token;
    c: Capabilities.t;
  }

  let token t c = { t; c }
  let ok x = Lwt.return (Ok x)

  let status_of_commits token commits =
    let api_status token c =
      Log.info (fun l -> l "API.status %a" Commit.pp c);
      if not (Capabilities.check token.c `Read `Status) then ok Status.Set.empty
      else
        API.status token.t c >|= function
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

  let new_prs token repos =
    let repos_l = Repo.Set.elements repos in
    Lwt_list.map_p (fun r ->
        Log.info (fun l -> l "API.prs %a" Repo.pp r);
        if not (Capabilities.check token.c `Read `PR) then ok PR.Set.empty
        else
          API.prs token.t r >|= function
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

  let new_refs token repos =
    let repos_l = Repo.Set.elements repos in
    Lwt_list.map_p (fun r ->
        Log.info (fun l -> l "API.refs %a" Repo.pp r);
        if not (Capabilities.check token.c `Read `Ref) then ok Ref.Set.empty
        else
          API.refs token.t r >|= function
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
  let import token t repos =
    new_prs token repos >>= fun new_prs ->
    new_refs token repos >>= fun new_refs ->
    let new_commits =
      Commit.Set.union (PR.Set.commits new_prs) (Ref.Set.commits new_refs)
    in
    status_of_commits token new_commits >|= fun new_status ->
    let new_t =
      Snapshot.create ~repos ~prs:new_prs ~refs:new_refs ~commits:new_commits
        ~status:new_status
    in
    Log.debug (fun l -> l "State.import %a@;@[<2>new:%a@]"
                  Repo.Set.pp repos Snapshot.pp new_t);
    let base = Snapshot.without_repos repos t in
    let repos = Repo.Set.union (Snapshot.repos t) repos in
    let prs = PR.Set.union (Snapshot.prs base) new_prs in
    let refs = Ref.Set.union (Snapshot.refs base) new_refs in
    let commits = Commit.Set.union (Snapshot.commits base) new_commits in
    let status = Status.Set.union (Snapshot.status base) new_status in
    Snapshot.create ~repos ~prs ~commits ~refs ~status

  let api_set_pr token pr =
    Log.info (fun l -> l "API.set-pr %a" PR.pp pr);
    if not (Capabilities.check token.c `Write `PR) then Lwt.return_unit
    else
      API.set_pr token.t pr >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-pr %a: %s" PR.pp pr e)

  let api_remove_ref token r =
    let repo, name = Ref.id r in
    let pp ppf r = Ref.pp_id ppf (Ref.id r) in
    Log.info (fun l -> l "API.remove-ref %a" pp r);
    if not (Capabilities.check token.c `Write `Ref) then Lwt.return_unit
    else
      API.remove_ref token.t repo name >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.remove-ref %a: %s" pp r e)

  let api_set_ref token r =
    Log.info (fun l -> l "API.set-ref %a" Ref.pp r);
    if not (Capabilities.check token.c `Write `Ref) then Lwt.return_unit
    else
      API.set_ref token.t r >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-ref %a: %s" Ref.pp r e)

  let api_set_status token s =
    Log.info (fun l -> l "API.set-status %a" Status.pp s);
    if not (Capabilities.check token.c `Write `Status) then Lwt.return_unit
    else
      API.set_status token.t s >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-status %a: %s" Status.pp s e)

  (* Read DataKit data and call the GitHub API to sync the world with
     what DataKit think it should be. *)
  let apply token diff =
    Log.debug (fun l -> l "State.apply@;@[%a@]" Diff.pp diff);
    let prs =
      PR.Set.union
        (Diff.update diff |> Snapshot.prs)
        (Diff.remove diff |> Snapshot.prs |> PR.Set.map PR.close)
    in
    Lwt_list.iter_p (api_set_pr token) (PR.Set.elements prs)
    >>= fun () ->
    let closed_refs = Snapshot.refs (Diff.remove diff) in
    Lwt_list.iter_p (api_remove_ref token) (Ref.Set.elements closed_refs)
    >>= fun () ->
    let refs = Snapshot.refs (Diff.update diff) in
    Lwt_list.iter_p (api_set_ref token) (Ref.Set.elements refs)
    >>= fun () ->
    (* NOTE: ideally we would also remove status, but the GitHub API doesn't
       support removing status so we just ignore *)
    let status = Snapshot.status (Diff.update diff) in
    Lwt_list.iter_p (api_set_status token) (Status.Set.elements status)

  let add_webhooks token ~watch repos =
    Log.debug (fun l -> l "[add_webhooks] repos: %a" Repo.Set.pp repos);
    Lwt_list.iter_p (fun r ->
        Log.info (fun l -> l "API.add-webhook %a" Repo.pp r);
        if not (Capabilities.check token.c `Write `Webhook) then Lwt.return_unit
        else watch r
      ) (Repo.Set.elements repos)

  let import_webhook_events token ~events t =
    match events () with
    | []     -> Lwt.return t
    | events ->
      Log.debug (fun l ->
          l "[sync_webhook] events:@;%a" (Fmt.Dump.list Event.pp) events);
      (* Need to resynchronsize build status for new commits *)
      let commits = List.fold_left (fun acc -> function
          | Event.PR pr ->
            if PR.state pr <> `Open then acc
            else Commit.Set.add (PR.commit pr) acc
          | Event.Ref (`Removed, _) -> acc
          | Event.Ref (_, r) -> Commit.Set.add (Ref.commit r) acc
          | Event.Repo _  | Event.Status _  | Event.Other _  -> acc
        ) Commit.Set.empty events
      in
      let new_commits = Commit.Set.diff commits (Snapshot.commits t) in
      status_of_commits token new_commits >|= fun new_status ->
      let events =
        (List.map Event.of_status @@ Status.Set.elements new_status)
        @ events
      in
      Snapshot.with_events events t

end
