(** Main object types used by the GitHub bridge. *)

(** {1 Printable Sets} *)

(** Pretty-printable {!Set.OrderedType}. *)
module type ELT = sig
  include Set.OrderedType
  val pp: t Fmt.t
end

(** Pretty-printable {!Set.S}. *)
module type SET = sig
  include Set.S
  val pp: t Fmt.t
end

module Set (E: ELT): SET with type elt = E.t
(** [Set] is similar to {!Set.Make} but for pretty-printable sets. *)

(** {1 Data-model} *)

module Status_state: sig

  type t = [ `Error | `Pending | `Success | `Failure ]
  (** The type for status states. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for status states. *)

  val to_string: t -> string
  (** [to_string v] is the string represenation of [v]. *)

  val of_string: string -> t option
  (** [of_string s] is the value v such that [of_string s] is [Some
      v]. *)

end

module Repo: sig

  type t = { user: string; repo: string }
  (** The type for Github repositories. *)

  type state = [`Monitored | `Ignored]
  (** The type for repository state. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for Github repositories. *)

  val pp_state: state Fmt.t
  (** [pp_state] is the pretty-printer for repository state. *)

  module Set: SET with type elt = t
  (** Sets of repositories. *)

end

module Commit: sig

  type t = { repo: Repo.t; id: string }
  (** The type for commits. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for commits. *)

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repository. *)

  val id: t -> string
  (** [id t] is [t]'s SHA1. *)

  val equal: t -> t -> bool
  (** [equal] is the equality functions for commits. *)

  module Set: sig
    include SET with type elt = t
    val repos: t -> Repo.Set.t
  end
  (** Sets of commits. *)

end

module PR: sig

  (** The type for pull-requests values. *)
  type t = {
    head: Commit.t;
    number: int;
    state: [`Open | `Closed];
    title: string;
    base: string;
  }

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for pull-request values. *)

  type id = Repo.t * int
  (** The type for commit ids. *)

  val pp_id: id Fmt.t
  (** [pp_id] is the pretty-printer for PR ids. *)

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repostiory. *)

  val id: t -> id
  (** [id t] is [t]'s ID. *)

  val commit: t -> Commit.t
  (** [commit t] is [t]'s commit. *)

  val commit_id: t -> string
  (** [commit_id t] is the SHA1 of [t]'s commit. *)

  val number: t -> int
  (** [number t] is [t]'s number. *)

  val state: t -> [`Open | `Closed]
  (** [state t] is [t]'s state. *)

  val close: t -> t
  (** [close t] is [t] with [state t] set to [`Closed]. *)

  val state_of_string: string -> [`Open | `Closed] option
  (** [string_of_state str] is [Some s] if there exists a state [s]
      such that [state_of_string s] is [str]. Otherwise it is
      [None]. *)

  val string_of_state: [`Open | `Closed] -> string
  (** [state_of_string s] is [s]'s string representation. *)

  val title: t -> string
  (** [title t] is [t]'s title. *)

  val same_id: t -> t -> bool
  (** [same_id x y] is true if [x] and [y] have the same ID. *)

  module Set: sig
    include SET with type elt = t
    val repos: t -> Repo.Set.t
    val commits: t -> Commit.Set.t
    val map: (elt -> elt) -> t -> t
  end
  (** Sets of pull requests. *)

end

module Status: sig

  (** The type for status values. *)
  type t = {
    commit: Commit.t;
    context: string list;
    url: string option;
    description: string option;
    state: Status_state.t;
  }

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for status values. *)

  type id = Commit.t * string list
  (** The type for build-status IDs. *)

  val pp_id: id Fmt.t
  (** [pp_id] is the pretty-printer for build-status IDs. *)

  val id: t -> id
  (** [id t] is [t]'s ID. *)

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repository. *)

  val commit: t -> Commit.t
  (** [commit t] is [t]'s commit. *)

  val commit_id: t -> string
  (** [commit_id t] is [t]'s commit ID. *)

  val path: t -> Datakit_path.t
  (** [path t] is path corresponding to [t]'s context. The empty list
      is rewritten into ["default"] to match the GitHub
      API. Otherwise, segments are concatenated using ["/"] as a
      separator. *)

  val same_id: t -> t -> bool
  (** [same_id x y] is true if [x] and [y] have the same ID. *)

  val compare: t -> t -> int
  (** [compare] is the comparison function for build status. *)

  module Set: sig
    include SET with type elt = t
    val repos: t -> Repo.Set.t
    val commits: t -> Commit.Set.t
  end
  (** Sets of build status. *)

end

module Ref: sig

  type t = {
    head: Commit.t;
    name: string list;
  }
  (** The type for Git references. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for references. *)

  type id = Repo.t * string list
  (** The type for Git reference IDs. *)

  val pp_id: id Fmt.t
  (** [pp_id] is the pretty-printer for Git reference IDs. *)

  val id: t -> id
  (** [id t] is [t]'s ID. *)

  val name: t -> string list
  (** [name t] is [t]'s name. *)

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repository. *)

  val commit: t -> Commit.t
  (** [commit t] is [t]'s commit. *)

  val commit_id: t -> string
  (** [commit_id t] is [t]'s commit ID. *)

  val same_id: t -> t -> bool
  (** [same_id x y] is true if [x] and [y] have the same ID. *)

  module Set: sig
    include SET with type elt = t
    val repos: t -> Repo.Set.t
    val commits: t -> Commit.Set.t
  end
  (** Sets of Git references. *)

  type state = [`Created | `Updated | `Removed]
  (** The type for reference state. *)

  val path: t -> Datakit_path.t

end

module Event: sig

  (** The type for event values. *)
  type t =
    | Repo of (Repo.state * Repo.t)
    | PR of PR.t
    | Status of Status.t
    | Ref of (Ref.state * Ref.t)
    | Other of (Repo.t * string)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for event values. *)

  val of_repo: Repo.state -> Repo.t -> t
  val of_pr: PR.t -> t
  val of_status: Status.t -> t
  val of_ref: Ref.state -> Ref.t -> t
  val of_other: Repo.t -> string -> t

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repository. *)

end

module Snapshot: sig

  (** {1 GitHub snapshot} *)

  type t
  (** The type for GitHub snapshot. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for snapshots. *)

  val empty: t
  (** The empty snapshot. *)

  val is_empty: t -> bool
  (** [is_empty t] is true if [t] is {!empty}. *)

  val create:
    repos:Repo.Set.t -> commits:Commit.Set.t -> status:Status.Set.t ->
    prs:PR.Set.t -> refs:Ref.Set.t -> t
  (** [create ?repos ~status ~prs] is a new snapshot [t] with
      pull-requests [prs], build status [status] and repositories the
      unions of [repos], the repositories of [status] and [prs]. *)

  val compare: t -> t -> int
  (** [compare] is the comparison function for snapshots. *)

  val union: t -> t -> t
  (** [union x y] is the union of the snapshots [x] and [y]. *)

  (** {1 Diffs} *)

  type diff
  (** The type for snapshot differences. *)

  val pp_diff: diff Fmt.t
  (** [pp_diff] is the pretty-printer for diffs. *)

  val to_update: diff -> t option
  (** [to_update d] is either [Some t], where [t] are the parts of [d]
      which need to be updated, or [None] if everything is already
      up-to-date. *)

  val to_remove: diff -> t option
  (** [to_remove d] is either [Some t], where [t] are the parts of [d]
      which need to be deleted, or [None] if everything is already
      up-to-date. *)

  val diff: t -> t -> diff
  (** [diff x y] is the difference between [x] and [y]. *)

  val prune: t -> diff
  (** [prune t] is either a clean snapshot and an optional snapshot
      representing the the commits and prs entries to remove. *)

  (** {1 Repositories} *)

  val repos: t -> Repo.Set.t
  (** [repos t] are [t]'s repository. *)

  val with_repo: Repo.t -> t -> t
  (** [with_repo r t] it [t] with the repostiory [r] added. *)

  val with_repos: Repo.Set.t -> t -> t
  (** [with_repos] is like {!with_repo} but for a set of
      repostiories. *)

  val without_repo: Repo.t -> t -> t
  (** [without_repo r t] is [t] without the repository [r]. *)

  val without_repos: Repo.Set.t -> t -> t
  (** [without_repos] is like {!without_repo} but for a set of
      repositories. *)

  (** {1 Commits} *)

  val commits: t -> Commit.Set.t
  (** [commits t] are [t]'s commits. *)

  val with_commit: Commit.t -> t -> t
  val without_commit: Commit.t -> t -> t

  (** {1 PRs} *)

  val prs: t -> PR.Set.t
  (** [prs t] are [t]'s pull-requests. *)

  val with_pr: PR.t -> t -> t
  val without_pr: PR.id -> t -> t

  (** {1 Status} *)

  val status: t -> Status.Set.t
  (** [status t] are [t]'s build status. *)

  val with_status: Status.t -> t -> t
  val without_status: Status.id -> t -> t

  (** {1 Refs} *)

  val refs: t -> Ref.Set.t
  (** [refs t] are [t]'s Git references. *)

  val with_ref: Ref.t -> t -> t
  val without_ref: Ref.id -> t -> t

  (** {1 Events} *)

  val with_event: Event.t -> t -> t

end
