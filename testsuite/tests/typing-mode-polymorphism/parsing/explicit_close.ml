(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(*
 * This file tests writing the hidden curry mode of a polymorphic function
 * type explicitly with [close('m)], and when the printer hides it again
*)

let use_portable (_ @ portable) = ()
[%%expect{|
val use_portable : 'a @ [< portable] -> unit @ 'm = <fun>
|}]

module type Explicit_close = sig
  val k : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local once]
end
[%%expect{|
module type Explicit_close =
  sig
    val k :
      'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local once]
  end
|}]

module M_explicit_close : Explicit_close = struct
  let k x y = x
end
[%%expect{|
module M_explicit_close : Explicit_close
|}]

let test_explicit_close () =
  use_portable (M_explicit_close.k 42); ()
[%%expect{|
val test_explicit_close : unit @ 'n -> unit @ 'm = <fun>
|}]

module type Explicit_close_not_hide = sig
  val k : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
end
[%%expect{|
module type Explicit_close_not_hide =
  sig
    val k : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local]
  end
|}]

module M_explicit_close_not_hide : Explicit_close_not_hide = struct
  let k x y = x
end
[%%expect{|
module M_explicit_close_not_hide : Explicit_close_not_hide
|}]

let test_explicit_close_not_hide () =
  use_portable (M_explicit_close_not_hide.k 42); ()
[%%expect{|
val test_explicit_close_not_hide : unit @ 'n -> unit @ 'm = <fun>
|}]

module type Explicit_close_hide = sig
  val k : 'a @ [< 'm > local]
          -> ('b @ 'n -> 'a @ [> 'm])
             @ [> close('m) | local stateful]
end
[%%expect{|
module type Explicit_close_hide =
  sig val k : 'a @ [< 'm > local] -> 'b @ 'n -> 'a @ [> 'm | local] end
|}]

module M_explicit_close_hide : Explicit_close_hide = struct
  let k x y = x
end
[%%expect{|
module M_explicit_close_hide : Explicit_close_hide
|}]

(* [stateful] implies [nonportable]: the lower bound of the curry mode in
   [Explicit_close_hide] is thus [nonportable] *)

let test_explicit_close_hide () =
  use_portable (M_explicit_close_hide.k 42); ()
[%%expect{|
Line 2, characters 15-43:
2 |   use_portable (M_explicit_close_hide.k 42); ()
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

module type Explicit_close_global = sig
  val k : 'a @ [< 'm & global]
          -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | once]
end
[%%expect{|
module type Explicit_close_global =
  sig
    val k :
      'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | once]
  end
|}]

module M_explicit_close_global : Explicit_close_global = struct
  let k x y = x
end
[%%expect{|
module M_explicit_close_global : Explicit_close_global
|}]

let test_explicit_close_global () =
  use_portable (M_explicit_close_global.k 42); ()
[%%expect{|
val test_explicit_close_global : unit @ 'n -> unit @ 'm = <fun>
|}]

module type Explicit_close_global_no_modality = sig
  val k : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)]
end
[%%expect{|
module type Explicit_close_global_no_modality =
  sig
    val k : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)]
  end
|}]

module M_explicit_close_global_no_modality : Explicit_close_global_no_modality =
struct
  let k x y = x
end
[%%expect{|
module M_explicit_close_global_no_modality :
  Explicit_close_global_no_modality
|}]

let test_explicit_close_global_no_modality () =
  use_portable (M_explicit_close_global_no_modality.k 42); ()
[%%expect{|
val test_explicit_close_global_no_modality : unit @ 'n -> unit @ 'm = <fun>
|}]

module type Explicit_close_global_modality = sig
  val k : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] @@ stateless
end
[%%expect{|
module type Explicit_close_global_modality =
  sig val k : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] @@ stateless end
|}]

module M_explicit_close_global_modality : Explicit_close_global_modality =
struct
  let k x y = x
end
[%%expect{|
module M_explicit_close_global_modality : Explicit_close_global_modality
|}]

let test_explicit_close_global_modality () =
  use_portable (M_explicit_close_global_modality.k 42); ()
[%%expect{|
val test_explicit_close_global_modality : unit @ 'n -> unit @ 'm = <fun>
|}]

module type Implicit_close_global_modality = sig
  val k : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] @@ portable
end
[%%expect{|
module type Implicit_close_global_modality =
  sig val k : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] @@ portable end
|}]

module M_implicit_close_global_modality : Implicit_close_global_modality =
struct
  let k x y = x
end
[%%expect{|
module M_implicit_close_global_modality : Implicit_close_global_modality
|}]

let test_implicit_close_global_modality () =
  use_portable (M_implicit_close_global_modality.k 42); ()
[%%expect{|
val test_implicit_close_global_modality : unit @ 'n -> unit @ 'm = <fun>
|}]

module type Implicit_close_global_hide = sig
  val k : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm]
end
[%%expect{|
module type Implicit_close_global_hide =
  sig val k : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] end
|}]

module M_implicit_close_global_hide : Implicit_close_global_hide = struct
  let k x y = x
end
[%%expect{|
module M_implicit_close_global_hide : Implicit_close_global_hide
|}]

(* By default, the implicit curry mode is joined with [legacy], which means
   the partial application is [nonportable] *)

let test_implicit_close_global_hide () =
  use_portable (M_implicit_close_global_hide.k 42); ()
[%%expect{|
Line 2, characters 15-50:
2 |   use_portable (M_implicit_close_global_hide.k 42); ()
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

module type Explicit_close_unique_hide = sig
  val k : 'a @ [< 'm & unique global]
          -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | once]
end
[%%expect{|
module type Explicit_close_unique_hide =
  sig
    val k :
      'a @ [< 'm & global unique] ->
      ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | once]
  end
|}]

module M_explicit_close_unique_hide : Explicit_close_unique_hide = struct
  let k x y = x
end
[%%expect{|
module M_explicit_close_unique_hide : Explicit_close_unique_hide
|}]

let test_explicit_close_unique_hide () =
  use_portable (M_explicit_close_unique_hide.k 42); ()
[%%expect{|
val test_explicit_close_unique_hide : unit @ 'n -> unit @ 'm = <fun>
|}]

module type Explicit_close_unique = sig
  val k : 'a @ [< 'm & unique global]
          -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) mod many]
end
[%%expect{|
module type Explicit_close_unique =
  sig
    val k :
      'a @ [< 'm & global unique] ->
      ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) mod many]
  end
|}]

(* closing over a [unique] [x] makes the partial application [once]; [mod many]
   drops that constraint, which no implementation returning [x] can honour *)

module M_explicit_close_unique : Explicit_close_unique = struct
  let k x y = x
end
[%%expect{|
Lines 1-3, characters 57-3:
1 | .........................................................struct
2 |   let k x y = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val k : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)]
         end
       is not included in
         Explicit_close_unique
       Values do not match:
         val k : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)]
       is not included in
         val k :
           'a @ [< 'm & global unique] ->
           ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) mod many]
       The type
         "'a @ [< 'm > past('o)] ->
         ('b @ [> past('n)] -> 'a @ [> 'm]) @ [> close('m)]"
       is not compatible with the type
         "'a @ [< 'p & past('o) & global unique] ->
         ('b @ [< past('n)] -> 'a @ [> 'p]) @ [> close('p) mod many]"
|}]

let k x y = x
[%%expect{|
val k : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

let test_k () = use_portable (k 42); ()
[%%expect{|
val test_k : unit @ 'n -> unit @ 'm = <fun>
|}]
