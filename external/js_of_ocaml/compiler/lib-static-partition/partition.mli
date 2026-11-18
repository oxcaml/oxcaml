(** Assign compilation units to an eager partition and to deferred roots. *)

module Unit : sig
  type t =
    { name : string
    ; provides : string list
    ; requires : string list
    ; imports : (string * string option) list
    ; force_link : bool
    }
end

module Report : sig
  type t =
    { eager : string list
    ; lazy_ : string list
    ; shared : string list
    ; forced_eager : string list
    ; unused : string list
    ; main_exports : string list
    }

  val to_json : t -> Yojson.Safe.t
end

type t

val create_many :
     ?check_deferred_handles:(string * string) list
  -> Unit.t list
  -> main_unit:string
  -> lazy_units:string list
  -> require_deferred_units:string list
  -> t
(** Inputs are in initialization order. Compilation units, including all names they
    provide, are indivisible. Force-linked units are eager roots; ordinary [.cmo] inputs
    are candidates, not implicit roots. Imports check interface consistency, but only
    [requires] determines runtime reachability. Provider and interface consistency checks
    include unused candidates; dependency checks cover selected units.

    Independent roots share ordinary dependencies eagerly. Declared implementations
    ([lazy_units] and [require_deferred_units]) must belong to exactly one lazy
    closure. Runtime ownership ignores interface-only imports.

    Each [(handle, implementation)] in [check_deferred_handles] rejects runtime paths
    from [handle] to [implementation], independently of ordinary eager or shared demand
    for that implementation.

    Raises [Failure] if the units cannot be partitioned. *)

val create : Unit.t list -> main_unit:string -> lazy_unit:string -> t
(** [create_many] with a single root. *)

val lazy_roots : t -> (string * Unit.t list) list
(** The units only each root needs, in root order. *)

val eager : t -> Unit.t list

val lazy_units : t -> Unit.t list
(** The units of all roots, in initialization order. *)

val report : t -> Report.t
