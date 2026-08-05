(** Registration of the allocations performed by the expressions of the
    structure being type-checked, and the passes that settle their modes.

    An allocation is registered together with the allocation modes of the
    closures enclosing it, so that the passes below can propagate information
    in both directions: an allocation known to be on the heap forces those
    closures to be [alloc], while a closure required not to allocate forces the
    allocations it encloses onto the stack. *)

open Mode

(** Forget every allocation registered so far. Called before type-checking a
    structure. *)
val reset_allocations : unit -> unit

(** [register_mode_for_optimisation pp ?closures alloc_mode] registers an
    allocation of mode [alloc_mode] pinpointed by [pp] and enclosed by
    [closures] (from the innermost closure to the outermost one, each paired
    with its own pinpoint).

    This only makes the allocation visible to the passes below; it does not by
    itself constrain [alloc_mode]. It is therefore sound to omit the call, at
    the cost of worse optimisation. *)
val register_mode_for_optimisation :
  Hint.pinpoint ->
  ?closures:(Hint.pinpoint * Allocation.r) list ->
  ('l * allowed) Alloc.t ->
  unit

(** Similar to [register_mode_for_optimisation], but the enclosing closures are
    looked up in [env] instead of being given explicitly. *)
val register_allocation_mode :
  env:Env.t -> loc:Location.t -> ('l * allowed) Alloc.t -> unit

(** [register_allocation_value_mode ~env ~loc ?desc mode] registers an
    allocation at [loc] whose result is used at [mode]. [desc] describes the
    allocation, for error messages. Returns the mode of the allocation, and the
    mode of its potential subcomponents. *)
val register_allocation_value_mode :
  env:Env.t ->
  loc:Location.t ->
  ?desc:Hint.allocation_desc ->
  Value.r ->
  Alloc.r * Value.r

(** Similar to [register_allocation_value_mode], but for closures. Unlike most
    allocations, which can be the highest mode allowed by the expected mode,
    functions have more constraints. For example, a two parameter function
    needs to be made global if its partial application to one argument must be
    global. As a result, a function gets an [Alloc.lr] allocation mode that can
    be further constrained. *)
val register_closure_allocation :
  env:Env.t -> Value.r -> loc:Location.t -> Alloc.lr * Value.r

(** For every allocation registered while type-checking the current structure
    that is already known to be on the heap, constrain the enclosing closures
    to be [alloc], and forget the allocation. Allocations that may still be
    stack-allocated are left to [optimise_allocations].

    Must be called before anything that zaps the mode of a structure item (in
    particular the modality zapping done while building the inferred
    signature), since the allocation axis of those modes is only raised
    here. *)
val constrain_closures : unit -> unit

(** For every allocation registered while type-checking the current structure
    that is enclosed by a closure required not to allocate, demand that it is
    stack-allocated, and forget it.

    Must be called before anything that defaults the areality of arrow types:
    once the enclosing function's return mode has been defaulted to [global],
    the demand can no longer be satisfied. *)
val constrain_allocations : unit -> unit

(** Settle the areality of every allocation registered while type-checking the
    current structure, pushing each to the highest (i.e. most stack-like) mode
    it is allowed, and reject the ones that end up on the heap inside a
    [noalloc] closure.

    Must be called after everything that can constrain the areality of an
    allocation, in particular the inclusion check against the [.mli] and the
    defaulting of type-level mode variables; otherwise allocations that belong
    on the heap would be made [local]. *)
val optimise_allocations : unit -> unit
