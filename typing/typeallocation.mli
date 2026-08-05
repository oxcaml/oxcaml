(* Registration and settling of the modes of allocations. *)

open Mode

val reset_allocations : unit -> unit

val register_mode_for_optimisation :
  Hint.pinpoint ->
  ?closures:(Hint.pinpoint * Allocation.r) list ->
  ('l * allowed) Alloc.t ->
  unit

val register_allocation_mode :
  env:Env.t -> loc:Location.t -> ('l * allowed) Alloc.t -> unit

val register_allocation_value_mode :
  env:Env.t ->
  loc:Location.t ->
  ?desc:Hint.allocation_desc ->
  Value.r ->
  Alloc.r * Value.r

val register_closure_allocation :
  env:Env.t -> Value.r -> loc:Location.t -> Alloc.lr * Value.r

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
