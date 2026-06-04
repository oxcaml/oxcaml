(* TEST
    flags += "-extension mode_alpha";
    expect;
*)

(* The semantics of [val_modes] (modes annotated on a value description in a
   signature). On each annotated axis, the mode overrides whatever is derived
   from the surrounding structure's mode and [val_modalities]. *)

(* Basic: [@ portable] on val *)
module type S = sig
  val foo @ portable : 'a -> 'a
end
[%%expect{|
module type S = sig val foo @ portable : 'a -> 'a end
|}]

(* [val_modes] combines with surrounding signature default modalities. *)
module type S = sig @@ contended
  val foo @ portable : 'a -> 'a
end
[%%expect{|
module type S = sig val foo @ portable : 'a -> 'a end
|}]

(* [val_modes] overrides on the specified axis: outer is portable, val
   annotates portable on its own axis (idempotent). *)
module type S = sig @@ portable
  val foo @ portable : 'a -> 'a
end
[%%expect{|
module type S = sig val foo @ portable : 'a -> 'a end
|}]

(* It is an error to specify both [val_modes] and [val_modalities] on a
   value description. *)
module type S = sig
  val foo @ portable : 'a -> 'a @@ contended
end
[%%expect{|
Line 2, characters 2-44:
2 |   val foo @ portable : 'a -> 'a @@ contended
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A value description cannot have both mode annotations ( ...) and
       modality annotations (@ ...).
|}]

(* external supports [val_modes] too *)
module type S = sig
  external foo @ portable : 'a -> 'a = "%identity"
end
[%%expect{|
module type S = sig external foo @ portable : 'a -> 'a = "%identity" end
|}]

(* Inclusion check: a value annotated [@ portable] is more permissive than the
   default (nonportable), so the structure must actually provide a portable
   value to satisfy the signature. *)
module type S = sig
  val foo @ portable : 'a -> 'a
end

module M : S = struct
  let foo x = x
end
[%%expect{|
module type S = sig val foo @ portable : 'a -> 'a end
module M : S
|}]

(* Inclusion check: signature without [val_modes] (default legacy) cannot
   include a more restrictive signature with [@ portable]. *)
module type S_legacy = sig
  val foo : 'a -> 'a
end

module type S_portable = sig
  val foo @ portable : 'a -> 'a
end

module M : S_legacy = struct
  let (foo @ nonportable) x = x
end

module M' : S_portable = (M : S_legacy)
[%%expect{|
module type S_legacy = sig val foo : 'a -> 'a end
module type S_portable = sig val foo @ portable : 'a -> 'a end
module M : S_legacy
Line 13, characters 25-39:
13 | module M' : S_portable = (M : S_legacy)
                              ^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         S_legacy @ nonportable
       is not included in
         S_portable @ nonportable
       Values do not match:
         val foo : 'a -> 'a (* in a structure at nonportable *)
       is not included in
         val foo @ portable : 'a -> 'a (* in a structure at nonportable *)
       The first is "nonportable"
       but the second is "portable".
|}]

(* Tests for the structure-memaddr-mode check at module inclusion.

   When no coercion happens ([Tcoerce_none]), the structure memaddr mode of the
   implementation must submode the expected one.

   When a coercion happens, a new memory block is constructed, so the RHS
   memaddr mode is not constrained by the LHS memaddr mode. *)

(* Tcoerce_none branch: signature matches the implementation exactly, no
   coercion. The memory-block mode submode check applies. *)
module type S_empty = sig end

module M1 : S_empty = struct end
[%%expect{|
module type S_empty = sig end
module M1 : S_empty
|}]

(* Tcoerce_none branch where the submode check fails: a functor that takes a
   local module and returns it as global. The body is the (local) parameter
   itself, so no coercion is generated, and the memaddr submode check fires
   on local vs. global. *)
module F (M : sig end @ local) : sig end @ global = M
[%%expect{|
Line 1, characters 52-53:
1 | module F (M : sig end @ local) : sig end @ global = M
                                                        ^
Error: Signature mismatch:
       Got "local"
       but expected "global".
|}]

(* Tcoerce_structure branch: when a coercion happens, a new memory block is
   constructed, so the LHS memaddr mode does NOT need to submode the RHS.
   Here the functor argument is at [local] but the result type is [global];
   because the argument has an extra item (y), a coercion is generated, and
   the local-vs-global submode check is skipped. *)
module F (M : sig val x : int val y : int end @ local)
  : sig val x : int end @ global = M
[%%expect{|
module F :
  functor (M : sig val x : int val y : int end @ local) ->
    sig val x : int end
  @@ stateless
|}]

(* Incompleteness mentioned in the CR-someday comment in [includemod.ml]:
   the surface-mode submode check rejects this functor even though the
   program is morally sound. Both sides have [x @ local], so all items are
   already local; the module's "real" memaddr mode is local on both sides
   and the program is sound. But the current check looks only at the surface
   mode (local vs global) and rejects it. *)
module F (M : sig val x @ local : int -> int end @ local)
  : sig val x @ local : int -> int end @ global = M
[%%expect{|
Line 2, characters 50-51:
2 |   : sig val x @ local : int -> int end @ global = M
                                                      ^
Error: Signature mismatch:
       Got "local"
       but expected "global".
|}]

(* [val_modes] on multiple axes: each axis is overridden independently. *)
module type S = sig
  val foo @ portable contended : 'a -> 'a
end
[%%expect{|
module type S = sig val foo @ portable contended : 'a -> 'a end
|}]

(* [val_modes] overrides only the specified axes; other axes still come from
   the surrounding signature's default. Here, [contended] is from the sig
   default, [portable] is from [val_modes]. *)
module type S = sig @@ contended
  val foo @ portable : 'a -> 'a
end

module M : S = struct
  let (foo @ portable) x = x
end
[%%expect{|
module type S = sig val foo @ portable : 'a -> 'a end
module M : S
|}]

(* Inclusion between two signatures, both using [val_modes]: matching
   annotations pass. *)
module type S1 = sig
  val foo @ portable : 'a -> 'a
end

module type S2 = sig
  val foo @ portable : 'a -> 'a
end

module F (M : S1) : S2 = M
[%%expect{|
module type S1 = sig val foo @ portable : 'a -> 'a end
module type S2 = sig val foo @ portable : 'a -> 'a end
module F : functor (M : S1) -> S2 @@ stateless
|}]

(* Module type equivalence check uses the [All] branch in
   [child_modes_with_val_modes]. When both sides have [val_modes] (Overriding)
   but annotate different axes, the per-axis mismatch is detected. *)
module type S1 = sig
  module type T = sig val foo @ portable : 'a -> 'a end
end

module type S2 = sig
  module type T = sig val foo @ contended : 'a -> 'a end
end

module F (M : S1) : S2 = M
[%%expect{|
module type S1 =
  sig module type T = sig val foo @ portable : 'a -> 'a end end
module type S2 =
  sig module type T = sig val foo @ contended : 'a -> 'a end end
Line 9, characters 25-26:
9 | module F (M : S1) : S2 = M
                             ^
Error: Signature mismatch:
       Modules do not match:
         sig module type T = sig val foo @ portable : 'a -> 'a end end
       is not included in
         S2
       Module type declarations do not match:
         module type T = sig val foo @ portable : 'a -> 'a end
       does not match
         module type T = sig val foo @ contended : 'a -> 'a end
       At position "module type T = <here>"
       Module types do not match:
         sig val foo @ portable : 'a -> 'a end
       is not equal to
         sig val foo @ contended : 'a -> 'a end
       At position "module type T = <here>"
       Mode annotations on foo do not match:
       Mode annotations on axis portability differ: one is annotated,
       the other is not.
|}]

(* Module type equivalence: both sides annotate the same axis but with values
   that do not submode each other. Triggers the [Val_modes_le_failure]
   error via the [All] branch. *)
module type S1 = sig
  module type T = sig val foo @ contended : 'a -> 'a end
end

module type S2 = sig
  module type T = sig val foo @ uncontended : 'a -> 'a end
end

module F (M : S1) : S2 = M
[%%expect{|
module type S1 =
  sig module type T = sig val foo @ contended : 'a -> 'a end end
module type S2 =
  sig module type T = sig val foo @ uncontended : 'a -> 'a end end
Line 9, characters 25-26:
9 | module F (M : S1) : S2 = M
                             ^
Error: Signature mismatch:
       Modules do not match:
         sig module type T = sig val foo @ contended : 'a -> 'a end end
       is not included in
         S2
       Module type declarations do not match:
         module type T = sig val foo @ contended : 'a -> 'a end
       does not match
         module type T = sig val foo @ uncontended : 'a -> 'a end
       The first module type is not included in the second
       At position "module type T = <here>"
       Module types do not match:
         sig val foo @ contended : 'a -> 'a end
       is not equal to
         sig val foo @ uncontended : 'a -> 'a end
       At position "module type T = <here>"
       Mode annotations on foo do not match:
       Mode annotations differ:
       the first is contended,
       the second is uncontended.
|}]
