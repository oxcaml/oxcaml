(* TEST
   flags = "-w +198 -zero-alloc-check opt";
   expect.opt;
*)

(** Tests for `[@zero_alloc opt]` on function arguments, run under
    `-zero-alloc-check opt`. Under this mode, `opt` annotations are active;
    compare with the analogous tests in zero_alloc_param.ml
    which run under the default mode where `opt` is inactive. *)

let[@zero_alloc] require_za_arity_1 (f [@zero_alloc arity 1]) =
  f 42;;
[%%expect {|
val require_za_arity_1 : ((int -> 'a) [@zero_alloc arity 1]) -> 'a
  [@@zero_alloc] = <fun>
|}];;

let f_requires_opt (g [@zero_alloc opt arity 1]) = g 0;;
[%%expect {|
val f_requires_opt : ((int -> 'a) [@zero_alloc opt arity 1]) -> 'a = <fun>
|}];;

let[@zero_alloc] za_fn x = x + 1;;
let non_za_fn x = x + 1;;
[%%expect {|
val za_fn : int -> int [@@zero_alloc] = <fun>
val non_za_fn : int -> int = <fun>
|}];;

(* Passing a [@zero_alloc] function to an opt parameter: succeeds. *)
let _ = f_requires_opt za_fn;;
[%%expect {|
- : int = 1
|}];;

(* Passing a function with no zero_alloc to an opt parameter: fails under opt
   mode. Under the default mode this would succeed because opt is inactive. *)
let _ = f_requires_opt non_za_fn;;
[%%expect {|
Line 1, characters 23-32:
1 | let _ = f_requires_opt non_za_fn;;
                           ^^^^^^^^^
Error: Function argument zero alloc assumption violated.
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}];;

(* Under opt mode, [@zero_alloc opt] on a function definition is checked. *)
let[@zero_alloc opt] allocating_fn x = (x, x);; (* should fail in the backend *)
[%%expect {|
Line 1, characters 5-15:
1 | let[@zero_alloc opt] allocating_fn x = (x, x);; (* should fail in the backend *)
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP8.allocating_fn (camlTOP8__allocating_fn_8_9_code).
Line 1, characters 39-45:
1 | let[@zero_alloc opt] allocating_fn x = (x, x);; (* should fail in the backend *)
                                           ^^^^^^
Error: allocation of 24 bytes
|}];;

let[@zero_alloc opt] non_allocating_fn x = x + 1;;
[%%expect {|
val non_allocating_fn : int -> int [@@zero_alloc opt] = <fun>
|}];;

(** The same opt vs. non-opt conflict tests as in zero_alloc_param.ml.
    Under opt mode, [@zero_alloc opt] is a real guarantee, so calling an opt
    function in a [@zero_alloc] body succeeds (unlike under default mode). *)

let[@zero_alloc] f : ((int -> int) [@zero_alloc opt]) -> int =
  fun (g [@zero_alloc opt arity 1]) -> g 42;;  (* succeeds under opt mode *)
[%%expect {|
val f : ((int -> int) [@zero_alloc opt arity 1]) -> int [@@zero_alloc] =
  <fun>
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc opt]) -> int =
  fun (g [@zero_alloc arity 1]) -> g 42;;
[%%expect {|
val f : ((int -> int) [@zero_alloc opt arity 1]) -> int [@@zero_alloc] =
  <fun>
|}];;

let[@zero_alloc] f : ((int -> int) [@zero_alloc]) -> int =
  fun (g [@zero_alloc opt arity 1]) -> g 42;;  (* should fail in the frontend *)
[%%expect {|
Line 2, characters 6-35:
2 |   fun (g [@zero_alloc opt arity 1]) -> g 42;;  (* should fail in the frontend *)
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The "zero_alloc" attribute on this function parameter conflicts
       with the one on its type.
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}];;
