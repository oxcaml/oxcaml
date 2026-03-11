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
  val f : (t [@zero_alloc arity 1]) -> int
end;;
module type S_with_t_not_arg = sig
  val f : t [@zero_alloc arity 1]
end;;
[%%expect {|
type t = int -> int
module type S_with_t_in_arg = sig val f : t -> int end
module type S_with_t_not_arg = sig val f : t end
|}];;

(** Examples of zero_alloc in function arguments, with further frontend and
    backend checks. *)

let[@zero_alloc] g (f [@zero_alloc arity 1]) = f 42;; (* should succeed *)
[%%expect {|
val g : (int -> 'a) [@zero_alloc arity 1] -> 'a [@@zero_alloc] = <fun>
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
  g f;; (* should fail in the frontend *)
[%%expect {|
- : int = 42
|}];;

let _ =
  let[@zero_alloc] f x y = x + y in
  let f' = f 123 in
  g f';; (* should fail; f' zero_alloc information is not available *)
[%%expect {|
Line 4, characters 4-6:
4 |   g f';; (* should fail; f' zero_alloc information is not available *)
        ^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" guarantees.
|}];;

(* CR aivaskovic: needs fixing *)
let _ =
  let[@zero_alloc] f x y = x + y in
  let[@zero_alloc] f' = f 123 in
  g f';; (* should succeed *)
[%%expect {|
Line 4, characters 4-6:
4 |   g f';; (* should succeed *)
        ^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" guarantees.
|}];;

let _ = g (fun[@zero_alloc] x -> 42);; (* should succeed *)
[%%expect {|
- : int = 42
|}];;

(* incompatible arities *)
let _ = g (fun[@zero_alloc arity 5] x -> 42);; (* should fail in the frontend *)
[%%expect {|
Line 1, characters 16-26:
1 | let _ = g (fun[@zero_alloc arity 5] x -> 42);; (* should fail in the frontend *)
                    ^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'zero_alloc'.
The "arity" field is only supported on "zero_alloc" in signatures or on function arguments

- : int = 42
|}];;

let _ = g (fun[@zero_alloc] x -> (x, 123));; (* should fail in the backend *)
[%%expect {|
Line 1, characters 16-26:
1 | let _ = g (fun[@zero_alloc] x -> (x, 123));; (* should fail in the backend *)
                    ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP16._$.(fun) (camlTOP16__fn[:1,10--42]_12_13_code).
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
Error: Annotation check for zero_alloc failed on function TOP20._$.(fun) (camlTOP20__fn[:1,10--24]_20_21_code).
Line 1, characters 20-23:
1 | let _ = g (fun x -> [x]);; (* should fail in the backend *)
                        ^^^
Error: allocation of 24 bytes
|}];;

let _ =
  let[@zero_alloc] id x = x in
  let[@zero_alloc] id' = id id in
  g id';; (* should succeed *)
[%%expect {|
Line 4, characters 4-7:
4 |   g id';; (* should succeed *)
        ^^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" guarantees.
|}];;

let _ =
  let[@zero_alloc] id x = x in
  let id' = id id in
  g id';; (* should succeed *)
[%%expect {|
Line 4, characters 4-7:
4 |   g id';; (* should succeed *)
        ^^^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" guarantees.
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
       There is a mismatch between the two "zero_alloc" guarantees.
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
    These tests check that modules and module types have expected behaviour  *)

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
    val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b [@@zero_alloc]
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
    val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b [@@zero_alloc]
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
           val f' : (int -> int -> 'a) [@zero_alloc arity 1] -> 'a
             [@@zero_alloc]
         end
       is not included in
         sig
           val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b
             [@@zero_alloc]
         end
       Values do not match:
         val f' : (int -> int -> 'a) [@zero_alloc arity 1] -> 'a
           [@@zero_alloc]
       is not included in
         val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b
           [@@zero_alloc]
       The type "(int -> int -> 'a) [@zero_alloc arity 1] -> 'a"
       is not compatible with the type
         "(int -> int -> 'b) [@zero_alloc arity 2] -> 'b"
       Zero-alloc attributes should match.
|}];;

module Zero_alloc_arity_is_2 : sig
  val f': ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
end = struct
  (* CR aivaskovic: needs fixing *)
  let[@zero_alloc] f' g' = g' 123 456 (* should succeed *)
end;;
[%%expect {|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   (* CR aivaskovic: needs fixing *)
5 |   let[@zero_alloc] f' g' = g' 123 456 (* should succeed *)
6 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f' : (int -> int -> 'a) -> 'a [@@zero_alloc] end
       is not included in
         sig
           val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b
             [@@zero_alloc]
         end
       Values do not match:
         val f' : (int -> int -> 'a) -> 'a [@@zero_alloc]
       is not included in
         val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b
           [@@zero_alloc]
       The type "(int -> int -> 'a) -> 'a" is not compatible with the type
         "(int -> int -> 'b) [@zero_alloc arity 2] -> 'b"
       Zero-alloc attributes should match.
|}];;

(* CR aivaskovic: add tests with strict and other various details *)
