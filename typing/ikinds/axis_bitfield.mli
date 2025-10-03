(** Low-level bitfield representation shared by axis-based lattices.

    Axis layout (index, axis name, levels from bottom (0) to top):
    {ul
    {- 0 — Areality (Regionality): Global → Regional → Local}
    {- 1 — Linearity: Many → Once}
    {- 2 — Uniqueness (monadic): Aliased → Unique}
    {- 3 — Portability: Portable → Nonportable}
    {- 4 — Contention (monadic): Contended → Shared → Uncontended}
    {- 5 — Yielding: Unyielding → Yielding}
    {- 6 — Statefulness: Stateless → Observing → Stateful}
    {- 7 — Visibility (monadic): Immutable → Read → Read_write}
    {- 8 — Externality: External → External64 → Internal}
    {- 9 — Nullability: Non_null → Maybe_null}
    {- 10 — Separability: Non_float → Separable → Maybe_separable}}

    Axes 0–7 are modal; 8–10 are non-modal, and 9–10 are the shallow axes
    sometimes excluded from masking operations.  Each 3-valued axis uses two
    bits, each 2-valued axis one bit, for a total of 17 bits stored inside an
    OCaml integer.  Callers build domain-specific abstractions (per-axis
    records, modality masks, etc.) on top of this module. *)

type t

val num_axes : int
(** Total number of encoded axes. *)

val axis_cardinality : axis:int -> int
(** Number of levels for the given axis (2 or 3). *)

val max_level : axis:int -> int
(** Highest valid level for the given axis. *)

val bot : t
val top : t

val join : t -> t -> t
val meet : t -> t -> t
val leq : t -> t -> bool
val co_sub : t -> t -> t
val equal : t -> t -> bool
val hash : t -> int

val mask : axis:int -> t
(** Bit mask covering all bits assigned to the given axis. *)

val mask_many : axes:int list -> t
(** Union of [mask] for a list of axis indices. *)

val get : t -> axis:int -> int
(** Extract the encoded level for an axis. *)

val set : t -> axis:int -> level:int -> t
(** Set the encoded level for an axis.  Callers must supply an in-range level. *)

val single_axis : axis:int -> level:int -> t
(** Bitfield with only the given axis raised to [level]. *)

(* Individual axis constants (single-axis bitfields). *)
val areality_global : t
val areality_regional : t
val areality_local : t

val linearity_many : t
val linearity_once : t

val uniqueness_aliased : t
val uniqueness_unique : t

val portability_portable : t
val portability_nonportable : t

val contention_contended : t
val contention_shared : t
val contention_uncontended : t

val yielding_unyielding : t
val yielding_yielding : t

val statefulness_stateless : t
val statefulness_observing : t
val statefulness_stateful : t

val visibility_immutable : t
val visibility_read : t
val visibility_read_write : t

val externality_external : t
val externality_external64 : t
val externality_internal : t

val nullability_non_null : t
val nullability_maybe_null : t

val separability_non_float : t
val separability_separable : t
val separability_maybe_separable : t

val encode : levels:int array -> t
(** Pack per-axis levels (one entry per axis index) into a bitfield. *)

val decode : t -> int array
(** Unpack all axis levels. *)

val non_bot_axes : t -> int list
(** Indices of axes that are not at their bottom level. *)

val to_string : t -> string
(** Debug string exposing all axis levels. *)

val mask_shallow : t
(** Mask covering the nullability and separability axes (9 and 10). *)
