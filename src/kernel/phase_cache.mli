(** An all-or-nothing cache mechanism that can be used for any phase *)

module type S = sig
  (** Phase input *)
  type t

  (** Phase output *)
  type output

  (** Phase computation *)
  val f : t -> output

  (** Phase title for logging *)
  val title : string

  module Fingerprint : sig
    type input

    (** Fingerprint used to determine whether the cache should be invalidated *)
    type t

    (** Creates a fingerprint from the phase input *)
    val make : input -> (t, string) result

    (** Determines whether two fingerprints are the same *)
    val equal : t -> t -> bool
  end
  with type input := t
end

module With_cache (Phase : S) : sig
  (** [Version] represents a naive, generic stamp of a cache entry and is recomputed each
      time the cache is refreshed. It is robust to [Fingerprint.make] errors.

      It is generic because it does not depend on [output], and is naive because a cache
      invalidation could result in the same [output], but the [version] would be the same. *)
  module Version : sig
    type t

    val equal : t -> t -> bool
  end

  type t = { output : Phase.output; cache_was_hit : bool; version : Version.t }

  (** [apply ~cache_disabling ~force_invalidation phase_input] runs the phase
      computation [Phase.f phase_input], if there's some [cache_disabling].
      Otherwise, the phase computation is run with a cache mechanism. Whether
      the cache is invalidated depends on the outcome of a [Phase.Fingerprint]
      comparison between the current fingerprint and the last one. Additionally,
      the invalidation of the cache can be forced by setting the
      force_invalidation parameter to true.*)
  val apply :
    ?cache_disabling:string option -> ?force_invalidation:bool -> Phase.t -> t
end
