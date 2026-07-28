(* TEST
 flags += "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

module M = struct
  let id x = x
  let const a b = a
  let compose f g x = f (g x)
  let curried a b c d = (a, b, c, d)
end
[%%expect{|
module M :
  sig
    val id : 'a @ [< 'm] -> 'a @ [> 'm]
    val const :
      'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)]
    val compose :
      ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< 'mm0 @@ past & 'o @@ past & global] ->
      (('c @ [> 'p] -> 'a @ [< 'n & global]) @ [< 'q @@ past & global] ->
       ('c @ [< 'p] -> 'b @ [> 'm | dynamic]) @ [> 'q | 'mm0]) @ [> 'o]
    val curried :
      'a @ [< 'm & global] ->
      ('b @ [< 'n & global] ->
       ('c @ [< 'o & global] ->
        ('d @ [< 'p & global] -> 'a * 'b * 'c * 'd @ [> 'p | 'o | 'n | 'm]) @ [> close('m) | close('n) | close('o)]) @ [> close('m) | close('n)]) @ [> close('m)]
  end
|}]

module M_self : module type of M = M
[%%expect{|
module M_self :
  sig
    val id : 'a @ [< 'm] -> 'a @ [> 'm] @@ stateless
    val const :
      'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] @@
      stateless
    val compose :
      ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< 'mm0 @@ past & 'o @@ past & global] ->
      (('c @ [> 'p] -> 'a @ [< 'n & global]) @ [< 'q @@ past & global] ->
       ('c @ [< 'p] -> 'b @ [> 'm | dynamic]) @ [> 'q | 'mm0]) @ [> 'o]
      @@ stateless
    val curried :
      'a @ [< 'm & global] ->
      ('b @ [< 'n & global] ->
       ('c @ [< 'o & global] ->
        ('d @ [< 'p & global] -> 'a * 'b * 'c * 'd @ [> 'p | 'o | 'n | 'm]) @ [> close('m) | close('n) | close('o)]) @ [> close('m) | close('n)]) @ [> close('m)]
      @@ stateless
  end
|}]

module M_restruct : module type of M = struct
  let id x = x
  let const a b = a
  let compose f g x = f (g x)
  let curried a b c d = (a, b, c, d)
end
[%%expect{|
module M_restruct :
  sig
    val id : 'a @ [< 'm] -> 'a @ [> 'm] @@ stateless
    val const :
      'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] @@
      stateless
    val compose :
      ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< 'mm0 @@ past & 'o @@ past & global] ->
      (('c @ [> 'p] -> 'a @ [< 'n & global]) @ [< 'q @@ past & global] ->
       ('c @ [< 'p] -> 'b @ [> 'm | dynamic]) @ [> 'q | 'mm0]) @ [> 'o]
      @@ stateless
    val curried :
      'a @ [< 'm & global] ->
      ('b @ [< 'n & global] ->
       ('c @ [< 'o & global] ->
        ('d @ [< 'p & global] -> 'a * 'b * 'c * 'd @ [> 'p | 'o | 'n | 'm]) @ [> close('m) | close('n) | close('o)]) @ [> close('m) | close('n)]) @ [> close('m)]
      @@ stateless
  end
|}]

module type S = module type of M

module M_via_sig : S = M
[%%expect{|
module type S =
  sig
    val id : 'a @ [< 'm] -> 'a @ [> 'm] @@ stateless
    val const :
      'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] @@
      stateless
    val compose :
      ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< 'mm0 @@ past & 'o @@ past & global] ->
      (('c @ [> 'p] -> 'a @ [< 'n & global]) @ [< 'q @@ past & global] ->
       ('c @ [< 'p] -> 'b @ [> 'm | dynamic]) @ [> 'q | 'mm0]) @ [> 'o]
      @@ stateless
    val curried :
      'a @ [< 'm & global] ->
      ('b @ [< 'n & global] ->
       ('c @ [< 'o & global] ->
        ('d @ [< 'p & global] -> 'a * 'b * 'c * 'd @ [> 'p | 'o | 'n | 'm]) @ [> close('m) | close('n) | close('o)]) @ [> close('m) | close('n)]) @ [> close('m)]
      @@ stateless
  end
module M_via_sig : S
|}]

let use_portable (x @ portable) = x

let use_local (x @ local) = ()
[%%expect{|
val use_portable : 'a @ [< 'm & portable] -> 'a @ [> 'm] = <fun>
val use_local : 'a @ [> local unforkable yielding] -> unit @ 'm = <fun>
|}]

module Bounded = struct
  let annotated_arg (x @ portable) = x
  let constrained_by_use x = use_portable x
  let local_arg (x @ local) = ()
  let two_axes (x @ global) (y @ unique) = x
  let dup x = (x, x)

  let tick =
    let r = ref 0 in
    fun () ->
      incr r;
      !r
end
[%%expect{|
module Bounded :
  sig
    val annotated_arg : 'a @ [< 'm & portable] -> 'a @ [> 'm]
    val constrained_by_use :
      'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic]
    val local_arg : 'a @ [> local unforkable yielding] -> unit @ 'm
    val two_axes :
      'a @ [< 'm & global forkable unyielding] ->
      ('b @ [< unique] -> 'a @ [> 'm]) @ [> close('m)]
    val dup : 'a @ [< 'm & global many] -> 'a * 'a @ [> 'm | aliased]
    val tick : unit -> int @ [> aliased nonportable stateful dynamic]
  end
|}]

module Bounded_self : module type of Bounded = Bounded
[%%expect{|
module Bounded_self :
  sig
    val annotated_arg : 'a @ [< 'm & portable] -> 'a @ [> 'm] @@ stateless
    val constrained_by_use :
      'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic]
    val local_arg : 'a @ [> local unforkable yielding] -> unit @ 'm @@
      stateless
    val two_axes :
      'a @ [< 'm & global forkable unyielding] ->
      ('b @ [< unique] -> 'a @ [> 'm]) @ [> close('m)] @@ stateless
    val dup : 'a @ [< 'm & global many] -> 'a * 'a @ [> 'm | aliased] @@
      stateless
    val tick : unit -> int @ [> aliased nonportable stateful dynamic]
  end
|}]

module Bounded_restruct : module type of Bounded = struct
  let annotated_arg (x @ portable) = x
  let constrained_by_use x = use_portable x
  let local_arg (x @ local) = ()
  let two_axes (x @ global) (y @ unique) = x
  let dup x = (x, x)

  let tick =
    let r = ref 0 in
    fun () ->
      incr r;
      !r
end
[%%expect{|
module Bounded_restruct :
  sig
    val annotated_arg : 'a @ [< 'm & portable] -> 'a @ [> 'm] @@ stateless
    val constrained_by_use :
      'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic]
    val local_arg : 'a @ [> local unforkable yielding] -> unit @ 'm @@
      stateless
    val two_axes :
      'a @ [< 'm & global forkable unyielding] ->
      ('b @ [< unique] -> 'a @ [> 'm]) @ [> close('m)] @@ stateless
    val dup : 'a @ [< 'm & global many] -> 'a * 'a @ [> 'm | aliased] @@
      stateless
    val tick : unit -> int @ [> aliased nonportable stateful dynamic]
  end
|}]

module Pass_stronger_sig : sig
  val f : 'a @ portable -> 'a
end = struct
  let f x = x
end
[%%expect{|
module Pass_stronger_sig : sig val f : 'a @ portable -> 'a end
|}]

module Portable_arg = struct
  let f (x @ portable) = x
end
[%%expect{|
module Portable_arg : sig val f : 'a @ [< 'm & portable] -> 'a @ [> 'm] end
|}]

module More_general_than_portable : module type of Portable_arg = struct
  let f x = x
end
[%%expect{|
module More_general_than_portable :
  sig val f : 'a @ [< 'm & portable] -> 'a @ [> 'm] @@ stateless end
|}]

module Local_arg = struct
  let f (x @ local) = ()
end
[%%expect{|
module Local_arg :
  sig val f : 'a @ [> local unforkable yielding] -> unit @ 'm end
|}]

module More_general_than_local : module type of Local_arg = struct
  let f x = ()
end
[%%expect{|
module More_general_than_local :
  sig
    val f : 'a @ [> local unforkable yielding] -> unit @ 'm @@ stateless
  end
|}]

module Use_constrained = struct
  let f x = use_portable x
end
[%%expect{|
module Use_constrained :
  sig val f : 'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic] end
|}]

module More_general_than_use : module type of Use_constrained = struct
  let f x = x
end
[%%expect{|
module More_general_than_use :
  sig val f : 'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic] end
|}]

module Base = struct
  let f x = x
end
[%%expect{|
module Base : sig val f : 'a @ [< 'm] -> 'a @ [> 'm] end
|}]

module Fail_less_polymorphic_local : module type of Base = struct
  let f (x @ local) = x
end
[%%expect{|
module Fail_less_polymorphic_local :
  sig val f : 'a @ [< 'm] -> 'a @ [> 'm] @@ stateless end
|}]

module Fail_less_polymorphic_unique : module type of Base = struct
  let f (x @ unique) = x
end
[%%expect{|
module Fail_less_polymorphic_unique :
  sig val f : 'a @ [< 'm] -> 'a @ [> 'm] @@ stateless end
|}]

module Fail_less_polymorphic_global : module type of Base = struct
  let f (x @ global) = x
end
[%%expect{|
module Fail_less_polymorphic_global :
  sig val f : 'a @ [< 'm] -> 'a @ [> 'm] @@ stateless end
|}]

module Fail_less_polymorphic_portable : module type of Base = struct
  let f (x @ portable) = x
end
[%%expect{|
module Fail_less_polymorphic_portable :
  sig val f : 'a @ [< 'm] -> 'a @ [> 'm] @@ stateless end
|}]

module Producer = struct
  let f x y = y
end
[%%expect{|
module Producer :
  sig
    val f :
      'a @ [< 'm @@ past & global] -> ('b @ [< 'n] -> 'b @ [> 'n]) @ [> 'm]
  end
|}]

module Good_client : module type of Producer = struct
  let f x y = y
end

let keep = Good_client.f 1
[%%expect{|
module Good_client :
  sig
    val f :
      'a @ [< 'm @@ past & global] -> ('b @ [< 'n] -> 'b @ [> 'n]) @ [> 'm]
      @@ stateless
  end
val keep : '_weak1 -> '_weak1 @ [> aliased nonportable stateful dynamic] =
  <fun>
|}]

(* Without subsumption, the following inclusion is wrongly accepted and the
   partial application of the coerced [f] is miscompiled. curry_mode_subsumption.ml is the
   executable version demonstrating what goes wrong. *)
module Bad_client : module type of Producer = struct
  let f (x @ local) y = y
end
[%%expect{|
module Bad_client :
  sig
    val f :
      'a @ [< 'm @@ past & global] -> ('b @ [< 'n] -> 'b @ [> 'n]) @ [> 'm]
      @@ stateless
  end
|}]

module Fail_local_escapes : sig
  val f : 'a @ local -> 'a
end = struct
  let f x = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a @ [< 'm] -> 'a @ [> 'm] end
       is not included in
         sig val f : 'a @ local -> 'a end
       Values do not match:
         val f : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val f : 'a @ local -> 'a
       The type
         "'a @ [< 'm > local aliased nonportable unforkable yielding stateful dynamic] ->
         'a @ [> 'm | local aliased nonportable unforkable yielding stateful dynamic]"
       is not compatible with the type "'a @ local -> 'a"
|}]

module Fail_arg_needs_portable : sig
  val f : 'a -> 'a
end = struct
  let f x = use_portable x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = use_portable x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val f : 'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic]
         end
       is not included in
         sig val f : 'a -> 'a end
       Values do not match:
         val f : 'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic]
       is not included in
         val f : 'a -> 'a
       The type "'a @ [< 'm & global portable] -> 'a @ [> 'm | dynamic]"
       is not compatible with the type "'a -> 'a"
|}]

module Fail_nonportable_modality : sig
  val f : unit -> int @@ portable
end = struct
  let r = ref 0

  let f () =
    incr r;
    !r
end
[%%expect{|
Lines 3-9, characters 6-3:
3 | ......struct
4 |   let r = ref 0
5 |
6 |   let f () =
7 |     incr r;
8 |     !r
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val r : int ref
           val f :
             unit @ 'm -> int @ [> aliased nonportable stateful dynamic]
         end @ nonportable
       is not included in
         sig val f : unit -> int @@ portable end @ nonportable
       Values do not match:
         val f : unit @ 'm -> int @ [> aliased nonportable stateful dynamic] (* in a structure at nonportable *)
       is not included in
         val f : unit -> int @@ portable (* in a structure at nonportable *)
       The first is "nonportable"
         because it contains a usage (of the value "r" at line 7, characters 9-10)
         which is expected to be "uncontended".
       However, the second is "portable".
|}]
