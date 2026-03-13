(* TEST
   flags = "-w +198";
   expect.opt;
*)

(** This tests the typing behavior of `[@zero_alloc]` attributes on function arguments.

    These tests are just about what is allowed and not allowed by the
    type checker.  The implementation of the actual `[@zero_alloc]` backend checks
    (including how the annotations in signatures affect those checks) are tested
    in the `tests/backend/checkmach` directory at the root of the project.
*)

(** Presence of zero_alloc on function arguments in module type definitions. *)

type t = int -> int;;
module type S_with_t_in_arg = sig
  val f : (t [@zero_alloc arity 1]) -> int [@@zero_alloc]
end;;
module type S_with_t_not_arg = sig
  val f : t [@zero_alloc arity 1]
end;;
module type S_with_t_not_arg_proper = sig
  val f : t [@@zero_alloc arity 1]
end;;
module type S_not_t = sig
  val f : ((int -> int) [@zero_alloc arity 1]) -> int
end;;
[%%expect {|
type t = int -> int
module type S_with_t_in_arg =
  sig val f : (t [@zero_alloc arity 1]) -> int [@@zero_alloc] end
module type S_with_t_not_arg = sig val f : t end
module type S_with_t_not_arg_proper =
  sig val f : t [@@zero_alloc arity 1] end
module type S_not_t =
  sig val f : ((int -> int) [@zero_alloc arity 1]) -> int end
|}];;

(** Module typing: zero_alloc information on a parameter should be printed only
    when the attribute is present. *)

module M1 = struct
  let f (g[@zero_alloc arity 1]) x = g (x + 1)
end;;
[%%expect {|
module M1 : sig val f : ((int -> 'a) [@zero_alloc arity 1]) -> int -> 'a end
|}];;

module M2 = struct
  let f g x = g (x + 1)
end;;
[%%expect {|
module M2 : sig val f : (int -> 'a) -> int -> 'a end
|}];;

module M3 = struct
  let f (g[@zero_alloc arity 2]) x = g (x + 1) (x - 1)
end;;
[%%expect {|
module M3 :
  sig val f : ((int -> int -> 'a) [@zero_alloc arity 2]) -> int -> 'a end
|}];;

module M4 = struct
  let f (g[@zero_alloc arity 1]) x = g (x + 1) (x - 1)
end;;
[%%expect {|
module M4 :
  sig val f : ((int -> int -> 'a) [@zero_alloc arity 1]) -> int -> 'a end
|}];;

(** Examples of zero_alloc in function arguments, with further frontend and
    backend checks. *)

let[@zero_alloc] g (f [@zero_alloc arity 1]) = f 42;; (* should succeed *)
[%%expect {|
val g : ((int -> 'a) [@zero_alloc arity 1]) -> 'a [@@zero_alloc] = <fun>
|}];;

let[@zero_alloc] f x = x;;
let _ = g f;; (* should succeed *)
[%%expect {|
val f : 'a -> 'a [@@zero_alloc] = <fun>
- : int = 42
|}];;

let _ =
  let[@zero_alloc] f x = x in
  g f;; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

let _ =
  let f x = x in
  g f;; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc]) -> int =
  fun (g [@zero_alloc arity 1]) -> g 42;; (* should succeed *)
[%%expect {|
val f : ((int -> int) [@zero_alloc arity 1]) -> int [@@zero_alloc] = <fun>
|}];;

let[@zero_alloc] f : ((int -> int -> int) [@zero_alloc]) -> int =
  fun (g [@zero_alloc arity 1]) -> g 42 123;; (* should fail *)
[%%expect {|
Line 2, characters 6-31:
2 |   fun (g [@zero_alloc arity 1]) -> g 42 123;; (* should fail *)
          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "zero_alloc" attribute on this function parameter conflicts
       with the one on its type. zero_alloc arity mismatch:
       When using "zero_alloc" in a signature or function parameter, the
       syntactic arity of the implementation must match the function type in
       the interface. Here the former is 1 and the latter is 2.
|}];;

(* checks with strict *)

let[@zero_alloc] f : ((int -> int) [@zero_alloc strict]) -> int =
  fun (g [@zero_alloc strict arity 1]) -> g 42;; (* should succeed *)
[%%expect {|
val f : ((int -> int) [@zero_alloc strict arity 1]) -> int [@@zero_alloc] =
  <fun>
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc strict]) -> int =
  fun (g [@zero_alloc arity 1]) -> g 42;; (* should fail *)
[%%expect {|
Line 2, characters 6-31:
2 |   fun (g [@zero_alloc arity 1]) -> g 42;; (* should fail *)
          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "zero_alloc" attribute on this function parameter conflicts
       with the one on its type.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc]) -> int =
  fun (g [@zero_alloc strict arity 1]) -> g 42;; (* should fail *)
[%%expect {|
Line 2, characters 6-38:
2 |   fun (g [@zero_alloc strict arity 1]) -> g 42;; (* should fail *)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "zero_alloc" attribute on this function parameter conflicts
       with the one on its type.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

let[@zero_alloc] test_f : ((int -> int) [@zero_alloc strict]) -> int =
  fun (g [@zero_alloc strict arity 1]) -> g 123;; (* should succeed *)
[%%expect {|
val test_f : ((int -> int) [@zero_alloc strict arity 1]) -> int
  [@@zero_alloc] = <fun>
|}];;

let _ =
  let[@zero_alloc] f x = x + 555 in
  test_f f;;
[%%expect {|
Line 3, characters 9-10:
3 |   test_f f;;
             ^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

(* checks with opt *)

let[@zero_alloc] f : ((int -> int) [@zero_alloc opt]) -> int =
  fun (g [@zero_alloc opt arity 1]) -> g 42;; (* should succeed *)
[%%expect {|
val f : ((int -> int) [@zero_alloc opt arity 1]) -> int [@@zero_alloc] =
  <fun>
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc opt]) -> int =
  fun (g [@zero_alloc arity 1]) -> g 42;; (* should fail *)
[%%expect {|
Line 2, characters 6-31:
2 |   fun (g [@zero_alloc arity 1]) -> g 42;; (* should fail *)
          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "zero_alloc" attribute on this function parameter conflicts
       with the one on its type.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc]) -> int =
  fun (g [@zero_alloc opt arity 1]) -> g 42;; (* should fail *)
[%%expect {|
Line 2, characters 6-35:
2 |   fun (g [@zero_alloc opt arity 1]) -> g 42;; (* should fail *)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "zero_alloc" attribute on this function parameter conflicts
       with the one on its type.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

(* assume not supported on function parameters *)
let[@zero_alloc] f : ((int -> int) [@zero_alloc assume]) -> int =
  fun (g [@zero_alloc assume arity 1]) -> g 42;; (* should fail *)
[%%expect {|
Line 1, characters 21-63:
1 | let[@zero_alloc] f : ((int -> int) [@zero_alloc assume]) -> int =
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid zero-alloc payload for a higher-order function argument.
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc assume]) -> int =
  fun (g [@zero_alloc arity 1]) -> g 42;; (* should fail *)
[%%expect {|
Line 1, characters 21-63:
1 | let[@zero_alloc] f : ((int -> int) [@zero_alloc assume]) -> int =
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid zero-alloc payload for a higher-order function argument.
|}];;

(* partial application *)
let _ =
  let[@zero_alloc] f x y = x + y in
  let f' = f 123 in
  g f';; (* should fail; f' zero_alloc information is not available *)
[%%expect {|
Line 4, characters 4-6:
4 |   g f';; (* should fail; f' zero_alloc information is not available *)
        ^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

(* partial application with specified zero_alloc *)
let _ =
  let[@zero_alloc] f x y = x + y in
  let[@zero_alloc] f' = f 123 in
  g f';; (* should succeed *)
[%%expect {|
Line 4, characters 4-6:
4 |   g f';; (* should succeed *)
        ^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

let _ = g (fun[@zero_alloc] x -> 42);; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

let _ = g (fun[@zero_alloc] x -> (x, 123));; (* should fail in the backend *)
[%%expect {|
Line 1, characters 16-26:
1 | let _ = g (fun[@zero_alloc] x -> (x, 123));; (* should fail in the backend *)
                    ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP34._$.(fun) (camlTOP34__fn[:1,10--42]_26_27_code).
Line 1, characters 33-41:
1 | let _ = g (fun[@zero_alloc] x -> (x, 123));; (* should fail in the backend *)
                                     ^^^^^^^^
Error: allocation of 24 bytes
|}];;

(* Function abstraction argument that is not marked zero_alloc. *)
let _ = g (fun x -> 1);; (* should succeed, inferred *)
[%%expect {|
- : int = 1
|}];;

let _ = g (fun x -> x + 111);; (* should succeed *)
[%%expect {|
- : int = 153
|}]

let _ = g (fun x -> x);; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

let _ = g (fun x -> [x]);; (* should fail in the backend *)
[%%expect {|
Line 1, characters 10-24:
1 | let _ = g (fun x -> [x]);; (* should fail in the backend *)
              ^^^^^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP38._$.(fun) (camlTOP38__fn[:1,10--24]_34_35_code).
Line 1, characters 20-23:
1 | let _ = g (fun x -> [x]);; (* should fail in the backend *)
                        ^^^
Error: allocation of 24 bytes
|}];;

let _ =
  let[@zero_alloc] id x = x in
  let[@zero_alloc] id' = id id in
  g id';; (* should fail; id' is not "syntactically" a function in the let-binding *)
[%%expect {|
Line 4, characters 4-7:
4 |   g id';; (* should fail; id' is not "syntactically" a function in the let-binding *)
        ^^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

let _ =
  let[@zero_alloc] id x = x in
  let id' = id id in
  g id';; (* should fail, as above *)
[%%expect {|
Line 4, characters 4-7:
4 |   g id';; (* should fail, as above *)
        ^^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

(* eta expansion helps *)
let _ =
  let[@zero_alloc] id x = x in
  let[@zero_alloc] id' x = id id x in
  g id';; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

let _ =
  let[@zero_alloc] id x = x in
  let id' x = id id x in
  g id';; (* should succeed; zero-allocness inferred *)
[%%expect {|
- : int = 42
|}];;

let _ =
  let id x = x in
  let id' = id id in
  g id';; (* should succeed *)
[%%expect {|
Line 4, characters 4-7:
4 |   g id';; (* should succeed *)
        ^^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

(** Frontend analysis fails when the arguments is neither an identifier nor a
    function abstraction. *)

let _ =
  let[@zero_alloc] f x y = x + y in
  g (f 1);; (* should fail in the frontend *)
[%%expect {|
Line 6, characters 4-9:
6 |   g (f 1);; (* should fail in the frontend *)
        ^^^^^
Error: This function application expects an argument that does not allocate.
       Argument must be an identifier or a function binding.
|}];;

let _ = g ((fun x -> x) (fun x -> x));; (* should fail in the frontend *)
[%%expect {|
Line 1, characters 10-37:
1 | let _ = g ((fun x -> x) (fun x -> x));; (* should fail in the frontend *)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function application expects an argument that does not allocate.
       Argument must be an identifier or a function binding.
|}];;

type rcd = {x : int -> int};;
let _ = fun t -> g t.x;; (* should fail in the frontend *)
[%%expect {|
type rcd = { x : int -> int; }
Line 2, characters 19-22:
2 | let _ = fun t -> g t.x;; (* should fail in the frontend *)
                       ^^^
Error: This function application expects an argument that does not allocate.
       Argument must be an identifier or a function binding.
|}];;

(** Modules and module types.
    These tests check that modules and module types have expected behaviour. *)

module Needs_arity_on_argument = struct
  let[@zero_alloc] f' (g' [@zero_alloc]) = g' 123 456 (* should fail *)
end;;
[%%expect {|
Line 5, characters 22-40:
5 |   let[@zero_alloc] f' (g' [@zero_alloc]) = g' 123 456 (* should fail *)
                          ^^^^^^^^^^^^^^^^^^
Error: Zero-alloc annotations on function arguments must specify arity.
|}];;

module Needs_arity_on_argument' : sig
  val f': ((int -> 'b) [@zero_alloc]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' (g' [@zero_alloc]) = g' 123 (* should fail *)
end;;
[%%expect {|
Line 4, characters 22-40:
4 |   let[@zero_alloc] f' (g' [@zero_alloc]) = g' 123 (* should fail *)
                          ^^^^^^^^^^^^^^^^^^
Error: Zero-alloc annotations on function arguments must specify arity.
|}];;

module Zero_alloc_on_arg : sig
  val f': ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' (g' [@zero_alloc arity 2]) = g' 123 456 (* should succeed *)
end;;
[%%expect {|
module Zero_alloc_on_arg :
  sig
    val f' : ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
  end
|}];;

module Inferred_on_value_description : sig
  val f': ((int -> int -> 'b) [@zero_alloc]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' (g' [@zero_alloc arity 2]) = g' 123 456 (* should succeed *)
end;;
[%%expect {|
module Inferred_on_value_description :
  sig
    val f' : ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
  end
|}];;

module Mismatched_zero_alloc_arities : sig
  val f': ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' (g' [@zero_alloc arity 1]) = g' 123 456 (* should fail *)
end;;
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let[@zero_alloc] f' (g' [@zero_alloc arity 1]) = g' 123 456 (* should fail *)
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           val f' : ((int -> int -> 'a) [@zero_alloc arity 1]) -> 'a
             [@@zero_alloc]
         end
       is not included in
         sig
           val f' : ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b
             [@@zero_alloc]
         end
       Values do not match:
         val f' : ((int -> int -> 'a) [@zero_alloc arity 1]) -> 'a
           [@@zero_alloc]
       is not included in
         val f' : ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b
           [@@zero_alloc]
       The type "((int -> int -> 'a) [@zero_alloc arity 1]) -> 'a"
       is not compatible with the type
         "((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b"
       Zero-alloc attributes should match.
|}];;

module Zero_alloc_arity_is_2 : sig
  val f': ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
end = struct
  (* CR aivaskovic: at some point in the future, we should be able to infer
  zero-allocness on function arguments *)
  let[@zero_alloc] f' g' = g' 123 456 (* fails now, will succeed later *)
end;;
[%%expect {|
Lines 3-7, characters 6-3:
3 | ......struct
4 |   (* CR aivaskovic: at some point in the future, we should be able to infer
5 |   zero-allocness on function arguments *)
6 |   let[@zero_alloc] f' g' = g' 123 456 (* fails now, will succeed later *)
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f' : (int -> int -> 'a) -> 'a [@@zero_alloc] end
       is not included in
         sig
           val f' : ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b
             [@@zero_alloc]
         end
       Values do not match:
         val f' : (int -> int -> 'a) -> 'a [@@zero_alloc]
       is not included in
         val f' : ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b
           [@@zero_alloc]
       The type "(int -> int -> 'a) -> 'a" is not compatible with the type
         "((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b"
       Zero-alloc attributes should match.
|}];;

(* Interaction between the two meanings of Tpoly. *)
let w2 : ('a. (('a -> int) [@zero_alloc])) -> int =
  fun (f [@zero_alloc arity 1]) -> f 123;;
[%%expect {|
Line 2, characters 6-31:
2 |   fun (f [@zero_alloc arity 1]) -> f 123;;
          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "zero_alloc" attribute on this function parameter conflicts
       with the one on its type.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

(* Does zero-alloc information propagate across aliases?
   Strictly speaking, this has nothing to do with zero-alloc on
   function parameters, but it is related to where values in the
   environment get their zero-alloc information from. *)
module M : sig
  val[@zero_alloc] g : int -> int
end = struct
  let[@zero_alloc] f x = x + 1
  let g = f
end
[%%expect {|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let[@zero_alloc] f x = x + 1
5 |   let g = f
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int [@@zero_alloc] val g : int -> int end
       is not included in
         sig val g : int -> int [@@zero_alloc] end
       Values do not match:
         val g : int -> int
       is not included in
         val g : int -> int [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

(** First-class modules and reading zero_alloc from module types. *)

module type S1 = sig
  val baz : (int -> int) [@@zero_alloc strict]
end;;
module type S2 = sig
  val bam : int -> int [@@zero_alloc]
end;;
module type S3 = sig
  val bal : int -> int
end;;
[%%expect {|
module type S1 = sig val baz : int -> int [@@zero_alloc strict] end
module type S2 = sig val bam : int -> int [@@zero_alloc] end
module type S3 = sig val bal : int -> int end
|}];;

let[@zero_alloc] test_with_fc_module_1 (module M : S1) x =
  test_f M.baz;;
[%%expect {|
val test_with_fc_module_1 : (module S1) -> 'a -> int [@@zero_alloc] = <fun>
|}];;

let[@zero_alloc] test_with_fc_module_2 (module M : S2) x =
  test_f M.bam;;
[%%expect {|
Line 2, characters 9-14:
2 |   test_f M.bam;;
             ^^^^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

let[@zero_alloc] test_with_fc_module_3 (module M : S3) x =
  test_f M.bal;;
[%%expect {|
Line 2, characters 9-14:
2 |   test_f M.bal;;
             ^^^^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" assumptions.
|}];;

(** Dealing with inference: what happens when there are multiple competing
    zero_alloc constraints? *)

let g1 (f [@zero_alloc arity 1]) = f 500;;
let g2 (f [@zero_alloc strict arity 1]) = f 99;;
let g3 (f [@zero_alloc strict arity 2]) = f 111;;
[%%expect {|
val g1 : ((int -> 'a) [@zero_alloc arity 1]) -> 'a = <fun>
val g2 : ((int -> 'a) [@zero_alloc strict arity 1]) -> 'a = <fun>
val g3 : ((int -> 'a) [@zero_alloc strict arity 2]) -> 'a = <fun>
|}];;

let _ =
  let f x = x + 123 in
  let _ = g1 f in
  let _ = g2 f in
  f;;
[%%expect {|
- : int -> int = <fun>
|}];;

let _ =
  let f x = x + 123 in
  let _ = g1 f in
  let _ = g2 f in
  let _ = g3 f in
  f;;
[%%expect {|
- : int -> int = <fun>
|}];;

let _ =
  let f x = x + 123 in
  let _ = g2 f in
  let _ = g1 f in
  f;;
[%%expect {|
- : int -> int = <fun>
|}];;

let f_inferred x = x + 123;;
let _ = g1 f_inferred;;
let _ = g2 f_inferred;;
let _ = g3 f_inferred;;
[%%expect {|
val f_inferred : int -> int = <fun>
- : int = 623
- : int = 222
- : int = 234
|}];;
