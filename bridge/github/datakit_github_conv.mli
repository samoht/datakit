(** Efficient conversions between in-memory snapshots and persistent
    datakit state. *)

open Datakit_github

module Diff: sig

  (** {1 Github diffs} *)

  type t = [
    | `Repo of Repo.t
    | `PR of PR.id
    | `Commit of Commit.t
    | `Status of Status.id
    | `Ref of Ref.id
    | `Unknown of Repo.t
  ]
  (** The type for diff identifiers. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for diff values. *)

  val compare: t -> t -> int
  (** [compare] is the comparison function for diff values. *)

  module Set: SET with type elt = t
  (** Set of changes. *)

  val changes: Datakit_path.t Datakit_S.diff list -> Set.t
  (** [changes d] is the set of GitHub changes carried over in the
      filesystem changes [d]. *)

end

(** Conversion between GitHub and DataKit states. *)
module Make (DK: Datakit_S.CLIENT): sig

  type nonrec 'a result = ('a, DK.error) Result.result Lwt.t
  (** The type for conversion results. *)

  (** The type for trees. *)
  type tree =
    | Tree: DK.Tree.t -> tree
    | Transaction: DK.Transaction.t -> tree

  (** {1 Repositories} *)

  val repos: tree -> Repo.Set.t Lwt.t
  (** [repos t] is the list of repositories stored in [t]. *)

  val update_repo: DK.Transaction.t -> Repo.state -> Repo.t -> unit result
  (** [update_repo t s r] applies the repository [r] to the
      transaction [t]. Depending on the state [s] it can either remove
      the directory or create a [monitored] file. *)

  (** {1 Status} *)

  val status: tree -> Commit.t -> string list -> Status.t option Lwt.t
  (** [status t c s] is the commit's build statuses [s] for the commit
      [c] in the tree [t]. *)

  val statuses: ?commits:Commit.Set.t -> tree -> Status.Set.t Lwt.t
  (** [statuses t] is the list of status stored in [t]. *)

  val update_status: DK.Transaction.t -> Status.t -> unit result
  (** [update_status t s] applies the status [s] to the transaction
      [t]. *)

  (** {1 Pull requests} *)

  val pr: tree -> Repo.t -> int -> PR.t option Lwt.t
  (** [pr t r n] is the [n]'th pull-request of the repostiry [r] in
      [t]. *)

  val prs: ?repos:Repo.Set.t -> tree -> PR.Set.t Lwt.t
  (** [prs t] is the list of pull requests stored in [t]. *)

  val update_pr: DK.Transaction.t -> PR.t -> unit result
  (** [update_pr t pr] applies the pull-request [pr] to the
      transaction [t]. *)

  (** {1 Git References} *)

  val refs: ?repos:Repo.Set.t -> tree -> Ref.Set.t Lwt.t
  (** [refs t] is the list of Git references stored in [t].*)

  val update_ref: DK.Transaction.t -> Ref.state -> Ref.t -> unit result
  (** [update_ref t s r] applies the Git reference [r] to the
      transaction [t]. Depending on the state [s] it can either remove
      the directory or create an [head] file. *)

  (** {1 Events} *)

  val update_event: DK.Transaction.t -> Event.t -> unit result
  (** [update_event t e] applies the (webhook) event [e] to the
      transaction [t]. *)

  (** {1 Snapshots and diffs} *)

  type t
  (** The type for filesystem snapshots. *)

  val snapshot: t -> Snapshot.t
  (** [snapshot t] is [t]'s in-memory snapshot. *)

  val head: t -> DK.Commit.t
  (** [head t] is [t]'s head. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for {!snapshot} values. *)

  type diffable = [`Transaction of DK.Transaction.t | `Commit of DK.Commit.t ]
  (** The type for diffable values. *)

  val safe_diff: diffable -> DK.Commit.t -> Diff.Set.t Lwt.t
  (** [diff t c] computes the Github diff between the diffable object
      [t] (either a transaction or a commit) and the commit [c]. *)

  val of_branch:
    debug:string -> ?old:t -> DK.Branch.t -> (DK.Transaction.t * t) Lwt.t
  (** [snapshot dbg ?old b] is a pair [(t, s)] where [s] is a snapshot
      of the branch [b] and a [t] is a transaction started on [s]'s
      commit. Note: this is expensive, so try to provide a (recent)
      [old] snapshot if possible. In that case, the difference between
      the two snapshot's commits will be computed and only the minimal
      number of filesystem access will be performed to compute the new
      snapshot by updating the old one. *)

  val of_commit: debug:string -> ?old:t -> DK.Commit.t -> t Lwt.t
  (** Same as {!of_branch} but does not allow to update the underlying
      store. *)

  val apply: debug:string -> Snapshot.diff -> DK.Transaction.t -> unit result
  (** [apply d t] applies the snapshot diff [d] into the datakit
      transaction [t]. *)

end
