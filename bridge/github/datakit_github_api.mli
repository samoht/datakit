(** [Vgithub.API] implementation using [ocaml-github] bindings. *)

open Datakit_github_types

(** Signature for the GitHub API. *)
module type S = sig

  type token
  (** The type for API tokens. *)

  type 'a result = ('a, string) Result.result Lwt.t
  (** The type for results. *)

  val user_exists: token -> user:string -> bool result
  (** [exist_user t ~user] is true iff [user] exists. *)

  val repo_exists: token -> Repo.t -> bool result
  (** [exists_repo t r] is true iff the repository [r] exists. *)

  val repos: token -> user:string -> Repo.t list result
  (** [repos t ~user] is the list of repositories owned by user
      [user]. *)

  val status: token -> Commit.t -> Status.t list result
  (** [status t c] returns the list of status attached to the commit
      [c]. *)

  val set_status: token -> Status.t -> unit result
  (** [set_status t s] updates [Status.commit s]'s status with [s]. *)

  val set_ref: token -> Ref.t -> unit result
  (** [set_ref t r] updates the reference named [Ref.name r] with
      [r]. *)

  val remove_ref: token -> Repo.t -> string list -> unit result
  (** [remove_ref t n] removes the reference named [n]. *)

  val set_pr: token -> PR.t -> unit result
  (** [set_pr t pr] updates the PR number [PR.number pr] with [pr]. *)

  val prs: token -> Repo.t -> PR.t list result
  (** [prs t r] is the list of open pull-requests for the repo [r]. *)

  val refs: token -> Repo.t -> Ref.t list result
  (** [refs t r] is the list of references for the the repo [r]. *)

  val events: token -> Repo.t -> Event.t list result
  (** [event t r] is the list of events attached to the repository
      [r]. Note: can be slow/costly if multiple pages of events. *)

  module Webhook: sig

    type t
    (** The type for the webhook server state. *)

    val create: token -> Uri.t -> t
    (** [create tok uri] is the webhook server state configured to
        listen for incoming webhook events to the public address [uri]
        and using the token [tok] to perform GitHub API calls. The
        function [f] will be called everytime a new event is
        received. *)

    val run: t -> unit Lwt.t
    (** [run t] is a blocking lwt thread which runs the webook
        listener. *)

    val repos: t -> Repo.Set.t
    (** The list of watched repository. *)

    val watch: t -> Repo.t -> unit Lwt.t
    (** [watch t r] makes [t] watch the repo [r]. *)

    val events: t -> Event.t list
    (** [events t] is the list of events stored in [t]. *)

    val wait: t -> unit Lwt.t
    (** [wait t] waits for new events to be available. *)

    val clear: t -> unit
    (** [clear t] clears the list of events stored in [t]. *)

  end

end

include S with type token = Github.Token.t
