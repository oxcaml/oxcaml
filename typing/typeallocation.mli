(* Registration and settling of the modes of allocations. *)

open Mode

type allocation =
  (* A locality mode *variable* with constraints, to be zapped as local as
      possible, as late as possible. *)
  | Known of Locality.r
  (* Pattern-matching an array that might be a float array. *)
  | Maybe_float_array of Typedtree.pattern

val reset_allocations : unit -> unit

(* Register a locality mode variable to be zapped as local as possible, as late
   as possible. *)
val register_mode_for_optimisation :
  (* A diagnostic hint pinpoint for the location of this allocation. *)
  Hint.pinpoint ->
  (* All closures enclosing this allocation, innermost-first, along with their
      allocation modes. *)
  ?closures:(Hint.pinpoint * Allocation.r) list ->
  allocation ->
  unit

val register_allocation_mode :
  env:Env.t -> loc:Location.t -> ('l * allowed) Locality.t -> unit

val register_allocation_value_mode :
  env:Env.t ->
  loc:Location.t ->
  ?desc:Hint.allocation_desc ->
  With_regionality.r ->
  Locality.r * With_regionality.r

val register_closure_allocation :
  env:Env.t ->
  With_regionality.r ->
  loc:Location.t ->
  Locality.lr * With_locality.lr * With_regionality.r

val register_mod_allocation :
  env:Env.t -> loc:Location.t -> desc:Hint.pinpoint_desc -> unit

val register_zero_alloc_application_allocation :
  env:Env.t ->
  pos:Typedtree.apply_position ->
  Typedtree.expression ->
  (Typedtree.arg_label * Typedtree.apply_arg * 'a) list ->
  With_regionality.l ->
  unit

val relax_alloc :
  Types.value_description ->
  is_applied:bool ->
  With_regionality.l ->
  With_regionality.l

(** For every allocation that has to be on heap ([global]), constrain
    the enclosing closures to be [alloc].
    Must only be called before zapping the allocation axis of
    closure modes. *)
val constrain_closures : unit -> unit

(** For every closure that has to be [noalloc_strict]/[noalloc],
    constrain allocations it encloses to be [local] and on the stack.
    Must only be called before zapping the locality axis of allocation
    modes. *)
val constrain_allocations : unit -> unit

val optimise_allocations : unit -> unit
