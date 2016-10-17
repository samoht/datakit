open Datakit_github_types

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

  (** {1 Repositories} *)

  val repos: DK.Transaction.t -> Repo.Set.t Lwt.t
  (** [repos t] is the list of repositories stored in [t]. *)

  val update_repo: DK.Transaction.t -> Repo.state -> Repo.t -> unit result
  (** [update_repo t s r] applies the repository [r] to the
      transaction [t]. Depending on the state [s] it can either remove
      the directory or create a [monitored] file. *)

  (** {1 Status} *)

  val status:
    DK.Transaction.t -> Commit.t -> string list -> Status.t option Lwt.t
  (** [status t c s] is the commit's build status [s] for the commit
      [c] in the transaction [t]. *)

  val statuses: ?commits:Commit.Set.t -> DK.Transaction.t -> Status.Set.t Lwt.t
  (** [statuses t] is the list of status stored in [t].. *)

  val update_status: DK.Transaction.t -> Status.t -> unit result
  (** [update_status t s] applies the status [s] to the transaction
      [t]. *)

  (** {1 Pull requests} *)

  val pr: DK.Transaction.t -> Repo.t -> int -> PR.t option Lwt.t
  (** [pr t r n] is the [n]'th pull-request of the repostiry [r] in
      [t]. *)

  val prs: ?repos:Repo.Set.t -> DK.Transaction.t -> PR.Set.t Lwt.t
  (** [prs t] is the list of pull requests stored in [t]. *)

  val update_pr: DK.Transaction.t -> PR.t -> unit result
  (** [update_pr t pr] applies the pull-request [pr] to the
      transaction [t]. *)

  (** {1 Git References} *)

  val update_ref: DK.Transaction.t -> Ref.state -> Ref.t -> unit result
  (** [update_ref t s r] applies the Git reference [r] to the
      transaction [t]. Depending on the state [s] it can either remove
      the directory or create an [head] file. *)

  (** {1 Events} *)

  val update_event: DK.Transaction.t -> Event.t -> unit result
  (** [update_event t e] applies the (webhook) event [e] to the
      transaction [t]. *)

  (** {1 Snapshots and diffs} *)

  val safe_diff: DK.Transaction.t -> DK.Commit.t -> Diff.Set.t Lwt.t
  (** [diff t c] computes the Github diff between the transaction [t]
      and the commit [c]. *)

  val snapshot: debug:string ->
    ?old:(DK.Commit.t * Snapshot.t) -> DK.Transaction.t -> Snapshot.t Lwt.t
  (** [snapshot dbg ?old t] is a snapshot of the transaction
      [t]. Note: this is expensive, so try to provide a previous
      (recent) snapshot [prev] if possible. *)

  val combine: Snapshot.t -> (DK.Transaction.t * Diff.Set.t) -> Snapshot.t Lwt.t
  (** [combine s d] is the snapshot obtained by applying [d] on top of
      [s]. [d] is a pair [t * diff] where [diff] contains the
      pull-requests, status and other objects to consider while [t] is
      the actual state of these objects stored in datakit. *)

  val apply: debug:string -> Snapshot.diff -> DK.Transaction.t -> unit result
  (** [apply d t] applies the snapshot diff [d] into the datakit
      transaction [t]. *)

end
