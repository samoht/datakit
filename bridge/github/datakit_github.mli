(** Virtual filesystem for the GitHub API. *)

module Capabilities: sig

  type t
  (** The type for API capabilities. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for capabilities. *)

  val of_string: string -> [ `Error of string | `Ok of t ]
  (** [of_string s] is [Some t] if there exists [t] such that [s] is
      the string representation of [t]; and [None] otherwise. *)

  type op = [`Read | `Write]
  (** The type for API operations. *)

  type resource = [`PR | `Status | `Ref | `Webhook]
  (** The type for API resources. *)

  val none: t
  (** [none] is the capability to do nothing. *)

  val all: t
  (** [all] is the capability to do everything. *)

  val allow: t -> op -> [`All | resource] -> t
  (** [allow t o r] is [t] with the capability to do API calls of type
      [o] to the kind of resource [r]. *)

  val disallow: t -> op -> [`All | resource] -> t
  (** [disallow t o r] is [t] without the capability to do API calls
      of type [o] to the kind of resource [r]. *)

  val check: t -> op -> resource -> bool
  (** [check t o r] is true if [t] is allowed to to [o] on the kind of
      resource [r]. *)

end

module Sync (API: Datakit_github_api.S) (DK: Datakit_S.CLIENT): sig

  type t
  (** The type for synchronizer state. *)

  val empty: t
  (** Create an empty sync state. *)

  val prune: DK.Branch.t -> unit Lwt.t
  (** [prune b] prunes the branch [b]. *)

  (** [sync t ~pub ~priv ~token] mirror GitHub changes in the DataKit
      public branch [pub]. It uses the private branch [priv] to store
      the received webhook event states. It connects to the GitHub API
      using the token [tok]. The default [policy] is [`Repeat]. By
      default [cap] is [Cap.all]. *)
  val sync:
    ?webhook:API.Webhook.t ->
    ?switch:Lwt_switch.t -> ?policy:[`Once|`Repeat] -> ?cap:Capabilities.t ->
    pub:DK.Branch.t -> priv:DK.Branch.t -> token:API.token ->
    t -> t Lwt.t

end
