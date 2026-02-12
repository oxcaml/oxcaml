(* TEST
   flags = "-w +198";
   expect.opt;
*)

(** This tests the typing behavior of `[@zero_alloc]` attributes on function arguments.

    These tests show what is permitted and what is prohibited by the type system.
    Some tests pass type checking, but end up erroring in the compiler backend;
    these are almost exclusively those where the zero_alloc attributes do not match
    the higher-order function semantics.
*)

(** Module typing: zero_alloc information on a parameter should be printed only
    when the attribute is present. *)

module M1 = struct
  let f (g [@zero_alloc arity 1]) x = g (x + 1)
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
  let f (g [@zero_alloc arity 2]) x = g (x + 1) (x - 1)
end;;
[%%expect {|
module M3 :
  sig val f : ((int -> int -> 'a) [@zero_alloc arity 2]) -> int -> 'a end
|}];;

module M4 = struct
  let f (g [@zero_alloc arity 1]) x = g (x + 1) (x - 1)
end;;
[%%expect {|
Line 2, characters 8-33:
2 |   let f (g [@zero_alloc arity 1]) x = g (x + 1) (x - 1)
            ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The arity in the "zero_alloc" attribute (1) does not match the
       number of parameters in the argument type (2).
|}];;

(** Examples of zero_alloc in function arguments, with further frontend and
    backend checks. *)

let[@zero_alloc] require_za_arity_1 (f [@zero_alloc arity 1]) =
  f 42;; (* should succeed *)
[%%expect {|
val require_za_arity_1 : ((int -> 'a) [@zero_alloc arity 1]) -> 'a
  [@@zero_alloc] = <fun>
|}];;

let _ =
  let[@zero_alloc] f x = x in
  require_za_arity_1 f;; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

let _ =
  let f x = x in
  require_za_arity_1 f;; (* should fail *)
[%%expect {|
Line 3, characters 21-22:
3 |   require_za_arity_1 f;; (* should fail *)
                         ^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
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
       with the one on its type.
       When using "zero_alloc" on function parameters, the arities in the
       type of the function and the parameter term mus match exactly.
       Here the arity in the actual parameter term is 1 and the arity in
       the type of the function is 2.
|}];;

let[@zero_alloc] f (g [@zero_alloc arity 2]) x = g x;; (* should fail *)
[%%expect {|
Line 1, characters 19-44:
1 | let[@zero_alloc] f (g [@zero_alloc arity 2]) x = g x;; (* should fail *)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The arity in the "zero_alloc" attribute (2) does not match the
       number of parameters in the argument type (1).
|}];;

let[@zero_alloc] f (g [@zero_alloc arity 2]) x = g x;; (* should fail *)
[%%expect {|
Line 1, characters 19-44:
1 | let[@zero_alloc] f (g [@zero_alloc arity 2]) x = g x;; (* should fail *)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The arity in the "zero_alloc" attribute (2) does not match the
       number of parameters in the argument type (1).
|}];;

let[@zero_alloc] f (g [@zero_alloc arity 2]) x y z = g x y z;; (* should fail *)
[%%expect {|
Line 1, characters 19-44:
1 | let[@zero_alloc] f (g [@zero_alloc arity 2]) x y z = g x y z;; (* should fail *)
                       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The arity in the "zero_alloc" attribute (2) does not match the
       number of parameters in the argument type (3).
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
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc]) -> int =
  fun (g [@zero_alloc strict arity 1]) -> g 42;; (* should succeed *)
[%%expect {|
val f : ((int -> int) [@zero_alloc arity 1]) -> int [@@zero_alloc] = <fun>
|}];;

let[@zero_alloc] test_f_strict : ((int -> int) [@zero_alloc strict]) -> int =
  fun (g [@zero_alloc strict arity 1]) -> g 123;; (* should succeed *)
[%%expect {|
val test_f_strict : ((int -> int) [@zero_alloc strict arity 1]) -> int
  [@@zero_alloc] = <fun>
|}];;

let _ =
  let[@zero_alloc] f x = x + 555 in
  test_f_strict f;;  (* should fail *)
[%%expect {|
Line 3, characters 16-17:
3 |   test_f_strict f;;  (* should fail *)
                    ^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}];;

(* checks with opt *)

let[@zero_alloc] f : ((int -> int) [@zero_alloc opt]) -> int =
  fun (g [@zero_alloc opt arity 1]) -> g 42;; (* should fail *)
[%%expect {|
Line 1, characters 5-15:
1 | let[@zero_alloc] f : ((int -> int) [@zero_alloc opt]) -> int =
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP21.f (camlTOP21__f_18_19_code).
Line 2, characters 39-43:
2 |   fun (g [@zero_alloc opt arity 1]) -> g 42;; (* should fail *)
                                           ^^^^
Error: called function may allocate (indirect tailcall)
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc opt]) -> int =
  fun (g [@zero_alloc arity 1]) -> g 42;; (* should succeed *)
[%%expect {|
val f : ((int -> int) [@zero_alloc opt arity 1]) -> int [@@zero_alloc] =
  <fun>
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc]) -> int =
  fun (g [@zero_alloc opt arity 1]) -> g 42;; (* should fail *)
[%%expect {|
Line 2, characters 6-35:
2 |   fun (g [@zero_alloc opt arity 1]) -> g 42;; (* should fail *)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "zero_alloc" attribute on this function parameter conflicts
       with the one on its type.
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}];;

(* assume not supported on function parameters *)
let[@zero_alloc] f : ((int -> int) [@zero_alloc assume]) -> int =
  fun (g [@zero_alloc assume arity 1]) -> g 42;; (* should fail *)
[%%expect {|
Line 1, characters 21-63:
1 | let[@zero_alloc] f : ((int -> int) [@zero_alloc assume]) -> int =
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "assume" is not permitted in "zero_alloc" attributes on function arguments.
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc]) -> int =
  fun (g [@zero_alloc arity 1]) -> g 42;; (* should succeed *)
[%%expect {|
val f : ((int -> int) [@zero_alloc arity 1]) -> int [@@zero_alloc] = <fun>
|}];;

(* partial application *)
let _ =
  let[@zero_alloc] f x y = x + y in
  let f' = f 123 in
  require_za_arity_1 f';; (* should fail; f' zero_alloc information is not available *)
[%%expect {|
Line 4, characters 21-23:
4 |   require_za_arity_1 f';; (* should fail; f' zero_alloc information is not available *)
                         ^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

(* partial application with specified zero_alloc *)
let _ =
  let[@zero_alloc] f x y = x + y in
  let[@zero_alloc] f' = f 123 in
  require_za_arity_1 f';; (* should fail *)
[%%expect {|
Line 4, characters 21-23:
4 |   require_za_arity_1 f';; (* should fail *)
                         ^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

let _ = require_za_arity_1 (fun[@zero_alloc] x -> 42);; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

let _ = require_za_arity_1 (fun[@zero_alloc] x -> (x, 123));; (* should fail in the backend *)
[%%expect {|
Line 1, characters 33-43:
1 | let _ = require_za_arity_1 (fun[@zero_alloc] x -> (x, 123));; (* should fail in the backend *)
                                     ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP29._$.(fun) (camlTOP29__fn[:1,27--59]_26_27_code).
Line 1, characters 50-58:
1 | let _ = require_za_arity_1 (fun[@zero_alloc] x -> (x, 123));; (* should fail in the backend *)
                                                      ^^^^^^^^
Error: allocation of 24 bytes
|}];;

(* Function abstraction argument that is not marked zero_alloc. *)
let _ = require_za_arity_1 (fun x -> 1);; (* should succeed, inferred *)
[%%expect {|
- : int = 1
|}];;

let _ = require_za_arity_1 (fun x -> x + 111);; (* should succeed *)
[%%expect {|
- : int = 153
|}]

let _ = require_za_arity_1 (fun x -> x);; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

let _ = require_za_arity_1 (fun x -> [x]);; (* should fail in the backend *)
[%%expect {|
Line 1, characters 27-41:
1 | let _ = require_za_arity_1 (fun x -> [x]);; (* should fail in the backend *)
                               ^^^^^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP33._$.(fun) (camlTOP33__fn[:1,27--41]_34_35_code).
Line 1, characters 37-40:
1 | let _ = require_za_arity_1 (fun x -> [x]);; (* should fail in the backend *)
                                         ^^^
Error: allocation of 24 bytes
|}];;

let _ =
  let[@zero_alloc] id x = x in
  let[@zero_alloc] id' = id id in
  require_za_arity_1 id';; (* should fail; id' is not "syntactically" a function in the let-binding; there is a warning emitted *)
[%%expect {|
Line 4, characters 21-24:
4 |   require_za_arity_1 id';; (* should fail; id' is not "syntactically" a function in the let-binding; there is a warning emitted *)
                         ^^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

let _ =
  let[@zero_alloc] id x = x in
  let id' = id id in
  require_za_arity_1 id';; (* should fail, as above *)
[%%expect {|
Line 4, characters 21-24:
4 |   require_za_arity_1 id';; (* should fail, as above *)
                         ^^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

(* eta expansion helps *)
let _ =
  let[@zero_alloc] id x = x in
  let[@zero_alloc] id' x = id id x in
  require_za_arity_1 id';; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

let _ =
  let[@zero_alloc] id x = x in
  let id' x = id id x in
  require_za_arity_1 id';; (* should fail; zero-allocness not inferred *)
[%%expect {|
Line 4, characters 21-24:
4 |   require_za_arity_1 id';; (* should fail; zero-allocness not inferred *)
                         ^^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

let _ =
  let id x = x in
  let id' = id id in
  require_za_arity_1 id';; (* should fail *)
[%%expect {|
Line 4, characters 21-24:
4 |   require_za_arity_1 id';; (* should fail *)
                         ^^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

let _ =
  let[@zero_alloc] require_za_arity (f [@zero_alloc arity 1]) x = f x in
  let id x = x in
  let[@zero_alloc] id' x = (id x, x) in
  require_za_arity id' 42;; (* should fail in the backend *)
[%%expect {|
Line 4, characters 7-17:
4 |   let[@zero_alloc] id' x = (id x, x) in
           ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP39._$.id' (camlTOP39__id'_41_43_code).
Line 4, characters 27-36:
4 |   let[@zero_alloc] id' x = (id x, x) in
                               ^^^^^^^^^
Error: allocation of 24 bytes
|}];;

(** Frontend analysis fails when the arguments is neither an identifier nor a
    function abstraction. *)

let _ =
  let[@zero_alloc] f x y = x + y in
  require_za_arity_1 (f 1);; (* should fail in the frontend *)
[%%expect {|
Line 6, characters 21-26:
6 |   require_za_arity_1 (f 1);; (* should fail in the frontend *)
                         ^^^^^
Error: This function application expects an argument that does not allocate.
       Only identifiers and anonymous functions may be passed as arguments
       to functions with "zero_alloc" parameters.
|}];;

let _ = require_za_arity_1 ((fun x -> x) (fun x -> x));; (* should fail in the frontend *)
[%%expect {|
Line 1, characters 27-54:
1 | let _ = require_za_arity_1 ((fun x -> x) (fun x -> x));; (* should fail in the frontend *)
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function application expects an argument that does not allocate.
       Only identifiers and anonymous functions may be passed as arguments
       to functions with "zero_alloc" parameters.
|}];;

(* CR-soon zero_alloc aivaskovic: add support for zero_alloc records *)
type rcd = {x : int -> int};;
let _ = fun t -> require_za_arity_1 t.x;; (* should fail in the frontend *)
[%%expect {|
type rcd = { x : int -> int; }
Line 2, characters 36-39:
2 | let _ = fun t -> require_za_arity_1 t.x;; (* should fail in the frontend *)
                                        ^^^
Error: This function application expects an argument that does not allocate.
       Only identifiers and anonymous functions may be passed as arguments
       to functions with "zero_alloc" parameters.
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
Line 4, characters 22-48:
4 |   let[@zero_alloc] f' (g' [@zero_alloc arity 1]) = g' 123 456 (* should fail *)
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The arity in the "zero_alloc" attribute (1) does not match the
       number of parameters in the argument type (2).
|}];;

module Nonsense_matching_arity : sig
  val f': ((int -> int -> 'b) [@zero_alloc arity 1]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' (g' [@zero_alloc arity 1]) = g' 123 456 (* should fail *)
end;;
[%%expect {|
Line 4, characters 22-48:
4 |   let[@zero_alloc] f' (g' [@zero_alloc arity 1]) = g' 123 456 (* should fail *)
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The arity in the "zero_alloc" attribute (1) does not match the
       number of parameters in the argument type (2).
|}];;

module Zero_alloc_arity_is_2 : sig
  val f': ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' g' = g' 123 456 (* fails now, will succeed later *)
end;;
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let[@zero_alloc] f' g' = g' 123 456 (* fails now, will succeed later *)
5 | end..
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
         "((int -> int -> 'a) [@zero_alloc arity 2]) -> 'a"
       The two types must agree on "zero_alloc": either both carry the annotation or neither does.
|}];;

(* Interaction between the two meanings of Tpoly. *)
let w2 : ('a. (('a -> int) [@zero_alloc])) -> int =
  fun (f [@zero_alloc arity 1]) -> f 123;;
[%%expect {|
val w2 : (('a. 'a -> int) [@zero_alloc arity 1]) -> int = <fun>
|}];;

(* zero_alloc on the Ptyp_poly node; arity inferred. *)
let w3 : ('a. 'a -> int) [@zero_alloc] -> int =
  fun (f [@zero_alloc arity 1]) -> f 123;;
[%%expect {|
val w3 : (('a. 'a -> int) [@zero_alloc arity 1]) -> int = <fun>
|}];;

(* zero_alloc on the Ptyp_poly node with explicit arity. *)
let w4 : ('a. 'a -> int) [@zero_alloc arity 1] -> int =
  fun (f [@zero_alloc arity 1]) -> f 123;;
[%%expect {|
val w4 : (('a. 'a -> int) [@zero_alloc arity 1]) -> int = <fun>
|}];;

(* Wrong explicit arity should fail. *)
let w5 : ('a. 'a -> int) [@zero_alloc arity 2] -> int =
  fun (f [@zero_alloc arity 1]) -> f 123;;
[%%expect {|
Line 1, characters 9-24:
1 | let w5 : ('a. 'a -> int) [@zero_alloc arity 2] -> int =
             ^^^^^^^^^^^^^^^
Error: The arity in the "zero_alloc" attribute (2) does not match the
       number of parameters in the argument type (1).
|}];;

(* Non-function inner type should fail. *)
let w6 : ('a. 'a) [@zero_alloc] -> int =
  fun _ -> 1;;
[%%expect {|
Line 1, characters 9-17:
1 | let w6 : ('a. 'a) [@zero_alloc] -> int =
             ^^^^^^^^
Error: "zero_alloc" attributes on function arguments require the argument
       to be a function type.
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
  test_f_strict M.baz;;
[%%expect {|
val test_with_fc_module_1 : (module S1) -> 'a -> int [@@zero_alloc] = <fun>
|}];;

let[@zero_alloc] test_with_fc_module_2 (module M : S2) x =
  test_f_strict M.bam;;
[%%expect {|
Line 2, characters 16-21:
2 |   test_f_strict M.bam;;
                    ^^^^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}];;

let[@zero_alloc] test_with_fc_module_3 (module M : S3) x =
  test_f_strict M.bal;;
[%%expect {|
Line 2, characters 16-21:
2 |   test_f_strict M.bal;;
                    ^^^^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

(** Dealing with inference: what happens when there are multiple competing
    zero_alloc constraints?
    The tests should succeed when arities can match exactly, but otherwise
    fail. *)

let g1 (f [@zero_alloc arity 1]) = f 500;;
let g2 (f [@zero_alloc strict arity 1]) = f 99;;
let g3 (f [@zero_alloc strict arity 2]) = f 111;;
[%%expect {|
val g1 : ((int -> 'a) [@zero_alloc arity 1]) -> 'a = <fun>
val g2 : ((int -> 'a) [@zero_alloc strict arity 1]) -> 'a = <fun>
Line 8, characters 7-39:
8 | let g3 (f [@zero_alloc strict arity 2]) = f 111;;
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The arity in the "zero_alloc" attribute (2) does not match the
       number of parameters in the argument type (1).
|}];;

let _ =
  let f x = x + 123 in
  let _ = g1 f in
  let _ = g2 f in
  f;;
[%%expect {|
Line 3, characters 13-14:
3 |   let _ = g1 f in
                 ^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

let _ =
  let f x = x + 123 in
  let _ = g1 f in
  let _ = g2 f in
  let _ = g3 f in
  f;;
[%%expect {|
Line 3, characters 13-14:
3 |   let _ = g1 f in
                 ^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

let _ =
  let f x = x + 123 in
  let _ = g2 f in
  let _ = g1 f in
  f;;
[%%expect {|
Line 3, characters 13-14:
3 |   let _ = g2 f in
                 ^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

let f_inferred x = x + 123;;
let _ = g1 f_inferred;;
let _ = g2 f_inferred;;
let _ = g3 f_inferred;;
[%%expect {|
val f_inferred : int -> int = <fun>
Line 2, characters 11-21:
2 | let _ = g1 f_inferred;;
               ^^^^^^^^^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

(* custom error messages can be placed on zero_alloc parameters *)

module type S = sig
  val f : (int -> int [@zero_alloc custom_error_message "my specific error"]) -> int
end;;

module F(X : S) = struct
  let g () = X.f (fun x -> Format.printf "foo"; x)
end;;
[%%expect {|
module type S = sig val f : ((int -> int) [@zero_alloc arity 1]) -> int end
Line 6, characters 17-50:
6 |   let g () = X.f (fun x -> Format.printf "foo"; x)
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP76.F.g.(fun) (camlTOP76__fn[:6,17--50]_64_67_code).
my specific error

File "format.ml", lines 1498-1500, characters 2-18:
Error: called function may allocate (direct call camlCamlinternalFormat__make_printf_120_401_code) (:6,27--46)
|}];;

(* zero_alloc requirements work on labeled parameters *)

module type S_labeled = sig
  val f : foo:((int -> int) [@zero_alloc]) -> int
end;;
[%%expect {|
module type S_labeled =
  sig val f : foo:((int -> int) [@zero_alloc arity 1]) -> int end
|}];;

let[@zero_alloc] f' x = x + 1;;

let[@zero_alloc] apply_labeled ~foo:(foo[@zero_alloc arity 1]) () = foo 0;;

let[@zero_alloc] use_labeled () = apply_labeled ~foo:f' ();;
[%%expect {|
val f' : int -> int [@@zero_alloc] = <fun>
val apply_labeled : foo:((int -> 'a) [@zero_alloc arity 1]) -> unit -> 'a
  [@@zero_alloc] = <fun>
val use_labeled : unit -> int [@@zero_alloc] = <fun>
|}];;

(* zero_alloc requirements cannot be placed on optional parameters *)

module type S = sig
  val f : ?foo:((int -> int) [@zero_alloc]) -> int
end;;

let[@zero_alloc] f' x = x + 1;;

module F(X : S) = struct
  let g () = X.f ~foo:f'
end;;
[%%expect {|
Line 2, characters 17-27:
2 |   val f : ?foo:((int -> int) [@zero_alloc]) -> int
                     ^^^^^^^^^^
Error: "zero_alloc" attributes are not supported on optional parameters.
|}];;

(* how zero_alloc propagates down a pattern *)

let needs_za_1 (f[@zero_alloc arity 1]) = f 42 in
let (f | f) = fun x -> (x, x) in
ignore (needs_za_1 f);;
[%%expect {|
Line 3, characters 19-20:
3 | ignore (needs_za_1 f);;
                       ^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

(** Aliases and zero_alloc parameters.
    Type aliases (type t = int -> int) work because the compiler expands them
    before checking.
    Value aliases (let g = f) are not yet supported. *)

(* Type alias for a single-arrow function used as the annotation on a
   zero_alloc parameter. *)
type arrow_alias = int -> int

let[@zero_alloc] use_arrow_alias (g [@zero_alloc arity 1] : arrow_alias) = g 42
[%%expect {|
type arrow_alias = int -> int
val use_arrow_alias : (arrow_alias [@zero_alloc arity 1]) -> int
  [@@zero_alloc] = <fun>
|}];;

(* Same, but for a two-arrow alias. *)
type arrow_alias_2 = int -> int -> int

let[@zero_alloc] use_arrow_alias_2
    (g [@zero_alloc arity 2] : arrow_alias_2) = g 42 43
[%%expect {|
type arrow_alias_2 = int -> int -> int
val use_arrow_alias_2 : (arrow_alias_2 [@zero_alloc arity 2]) -> int
  [@@zero_alloc] = <fun>
|}];;

(* A global value alias of a zero_alloc function does not carry zero_alloc
   information.
   CR-soon zero-alloc aivaskovic: add support for value aliases. *)
let[@zero_alloc] fn_for_alias x = x + 1
let fn_alias = fn_for_alias
let _ = require_za_arity_1 fn_alias
[%%expect {|
val fn_for_alias : int -> int [@@zero_alloc] = <fun>
val fn_alias : int -> int = <fun>
Line 3, characters 27-35:
3 | let _ = require_za_arity_1 fn_alias
                               ^^^^^^^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

(* A local alias of a zero_alloc parameter also loses the guarantee.
   CR-soon zero-alloc aivaskovic: add support for local aliases of zero_alloc
   parameters. *)
let[@zero_alloc] use_local_alias (f[@zero_alloc arity 1]) =
  let g = f in
  require_za_arity_1 g
[%%expect {|
Line 3, characters 21-22:
3 |   require_za_arity_1 g
                         ^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

(** Pattern aliases and zero_alloc parameters.
    Tests for `(pat as x)` pattern aliases in zero_alloc contexts.
    The zero_alloc annotation propagates to both the annotated binding and the
    alias, so both can be used in zero_alloc contexts. *)

(* Calling the alias of a zero_alloc-annotated pattern binding. *)
let[@zero_alloc] call_alias_of_za_param ((f [@zero_alloc arity 1]) as g) x = g x
[%%expect {|
val call_alias_of_za_param : (('a -> 'b) [@zero_alloc arity 1]) -> 'a -> 'b
  [@@zero_alloc] = <fun>
|}];;

(* The alias carries the zero_alloc annotation and can be passed to a
   function expecting a zero_alloc argument. *)
let[@zero_alloc] pass_alias_to_za_consumer ((f [@zero_alloc arity 1]) as g) x =
  let _ = require_za_arity_1 g in
  f x
[%%expect {|
val pass_alias_to_za_consumer :
  ((int -> 'a) [@zero_alloc arity 1]) -> int -> 'a [@@zero_alloc] = <fun>
|}];;

(* Only the annotated binding is used; alias is ignored. *)
let[@zero_alloc] use_annotated_ignore_alias ((f [@zero_alloc arity 1]) as _g) x = f x
[%%expect {|
val use_annotated_ignore_alias :
  (('a -> 'b) [@zero_alloc arity 1]) -> 'a -> 'b [@@zero_alloc] = <fun>
|}];;

(** `external` declarations and zero_alloc parameters.
    Zero_alloc attributes on parameters of `external` declarations behave
    the same as on `val` declarations in signatures. *)

(* external in a module type with a zero_alloc param *)
module type S_ext = sig
  external f : ((int -> int) [@zero_alloc arity 1]) -> int = "f"
end;;
[%%expect {|
module type S_ext =
  sig external f : ((int -> int) [@zero_alloc arity 1]) -> int = "f" end
|}];;

(* strict variant *)
module type S_ext_strict = sig
  external f : ((int -> int) [@zero_alloc strict arity 1]) -> int = "f"
end;;
[%%expect {|
module type S_ext_strict =
  sig
    external f : ((int -> int) [@zero_alloc strict arity 1]) -> int = "f"
  end
|}];;

(* error: zero_alloc on a non-function param *)
module type S_ext_bad_nonfun = sig
  external f : (int [@zero_alloc]) -> int = "f"
end;;
[%%expect {|
Line 2, characters 15-41:
2 |   external f : (int [@zero_alloc]) -> int = "f"
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "zero_alloc" attributes on function arguments require the argument
       to be a function type.
|}];;
