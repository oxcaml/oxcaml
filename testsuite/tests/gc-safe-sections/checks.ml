(* TEST
   expect;
*)

(* Static checking of GC-safe sections: [%critical]/[%blocking] expressions,
   [@@critical]/[@@blocking] attributes on externals and let-bound functions,
   and the corresponding signature-inclusion rules. Sections are still
   compiled transparently; only the static checks are tested here. *)

(*********************************************************)
(* Structural rules inside [%critical] *)

let ok_arith (x : int) (y : int) = [%critical (x * y) lxor 3]
[%%expect{|
val ok_arith : int -> int -> int = <fun>
|}]

let bad_alloc (x : int) = [%critical (x, x)]
[%%expect{|
Line 1, characters 37-43:
1 | let bad_alloc (x : int) = [%critical (x, x)]
                                         ^^^^^^
Error: This allocation is not allowed inside a [%critical] section.
|}]

let bad_raise () = [%critical raise Exit]
[%%expect{|
Line 1, characters 30-40:
1 | let bad_raise () = [%critical raise Exit]
                                  ^^^^^^^^^^
Error: This raise is not allowed inside a [%critical] section.
|}]

let bad_try (x : int) = [%critical (try x with _ -> 0)]
[%%expect{|
Line 1, characters 35-54:
1 | let bad_try (x : int) = [%critical (try x with _ -> 0)]
                                       ^^^^^^^^^^^^^^^^^^^
Error: This exception handler is not allowed inside a [%critical] section.
|}]

let bad_checked (s : string) = [%critical s.[0]]
[%%expect{|
Line 1, characters 42-47:
1 | let bad_checked (s : string) = [%critical s.[0]]
                                              ^^^^^
Error: This operation that may raise is not allowed inside a [%critical] section.
|}]

let bad_fundef (x : int) = [%critical (fun y -> y) x]
[%%expect{|
Line 1, characters 38-52:
1 | let bad_fundef (x : int) = [%critical (fun y -> y) x]
                                          ^^^^^^^^^^^^^^
Error: This function call is not allowed inside a [%critical] section.
|}]

let bad_nested (x : int) = [%critical [%critical x]]
[%%expect{|
Line 1, characters 49-50:
1 | let bad_nested (x : int) = [%critical [%critical x]]
                                                     ^
Error: This nested section is not allowed inside a [%critical] section.
|}]

let bad_nested_deep (x : int) = [%critical 1 + [%blocking x]]
[%%expect{|
Line 1, characters 58-59:
1 | let bad_nested_deep (x : int) = [%critical 1 + [%blocking x]]
                                                              ^
Error: This nested section is not allowed inside a [%critical] section.
|}]

(* Malformed payloads fall through to the generic extension error. *)
let bad_payload = [%critical]
[%%expect{|
Line 1, characters 20-28:
1 | let bad_payload = [%critical]
                        ^^^^^^^^
Error: Uninterpreted extension 'critical'.
|}]

(*********************************************************)
(* Structural rules inside [%blocking] *)

let ok_blocking_arith (x : int) = [%blocking x land 7]
[%%expect{|
val ok_blocking_arith : int -> int = <fun>
|}]

let bad_blocking_alloc (x : int) = [%blocking (let p = (x, x) in fst p)]
[%%expect{|
Line 1, characters 55-61:
1 | let bad_blocking_alloc (x : int) = [%blocking (let p = (x, x) in fst p)]
                                                           ^^^^^^
Error: This allocation is not allowed inside a [%blocking] section.
|}]

(*********************************************************)
(* Externals: [@@critical]/[@@blocking] attributes *)

external crit_safe : int -> int -> int = "caml_int_compare"
  [@@noalloc] [@@critical safe]
external crit_only : int -> int -> int = "caml_int_compare"
  [@@noalloc] [@@critical]
external block_only : int -> int -> int = "caml_int_compare"
  [@@noalloc] [@@blocking]
external unannotated : int -> int -> int = "caml_int_compare" [@@noalloc]
[%%expect{|
external crit_safe : int -> int -> int = "caml_int_compare" [@@critical safe]
  [@@noalloc]
external crit_only : int -> int -> int = "caml_int_compare" [@@critical]
  [@@noalloc]
external block_only : int -> int -> int = "caml_int_compare" [@@blocking]
  [@@noalloc]
external unannotated : int -> int -> int = "caml_int_compare" [@@noalloc]
|}]

let ok_ext_crit (x : int) = [%critical crit_safe x 1]
let ok_ext_only (x : int) = [%critical crit_only x 1]
let ok_ext_block (x : int) = [%blocking block_only x 1]
[%%expect{|
val ok_ext_crit : int -> int = <fun>
val ok_ext_only : int -> int = <fun>
val ok_ext_block : int -> int = <fun>
|}]

let bad_ext_unannot (x : int) = [%critical unannotated x 1]
[%%expect{|
Line 1, characters 43-58:
1 | let bad_ext_unannot (x : int) = [%critical unannotated x 1]
                                               ^^^^^^^^^^^^^^^
Error: This unannotated external call is not allowed inside a [%critical] section.
|}]

let bad_ext_kind (x : int) = [%critical block_only x 1]
[%%expect{|
Line 1, characters 40-54:
1 | let bad_ext_kind (x : int) = [%critical block_only x 1]
                                            ^^^^^^^^^^^^^^
Error: This unannotated external call is not allowed inside a [%critical] section.
|}]

external crit_unsafe : int -> int -> int = "caml_int_compare"
  [@@noalloc] [@@critical unsafe]
let bad_ext_unsafe (x : int) = [%critical crit_unsafe x 1]
[%%expect{|
external crit_unsafe : int -> int -> int = "caml_int_compare" [@@noalloc]
Line 3, characters 42-57:
3 | let bad_ext_unsafe (x : int) = [%critical crit_unsafe x 1]
                                              ^^^^^^^^^^^^^^^
Error: This unannotated external call is not allowed inside a [%critical] section.
|}]

(* Section attributes on externals require [@@noalloc]. *)
external bad_ext_noalloc : int -> int -> int = "caml_int_compare"
  [@@critical safe]
[%%expect{|
Lines 1-2, characters 0-19:
1 | external bad_ext_noalloc : int -> int -> int = "caml_int_compare"
2 |   [@@critical safe]
Error: "[@@critical]" and "[@@blocking]" require "[@@noalloc]".
|}]

(* Bad attribute payloads warn and are ignored. *)
external bad_ext_payload : int -> int -> int = "caml_int_compare"
  [@@noalloc] [@@critical sometimes]
[%%expect{|
Line 2, characters 14-36:
2 |   [@@noalloc] [@@critical sometimes]
                  ^^^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute "critical".
  It must be either 'safe', 'unsafe', 'only' or empty

external bad_ext_payload : int -> int -> int = "caml_int_compare" [@@noalloc]
|}]

(* Externals may not be used first-class inside a section. *)
let bad_ext_firstclass (x : int) =
  [%critical (let f = crit_safe in f x 1)]
[%%expect{|
Line 2, characters 13-41:
2 |   [%critical (let f = crit_safe in f x 1)]
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function definition is not allowed inside a [%critical] section.
|}]

(*********************************************************)
(* Let-bound functions: [@@critical safe] *)

let triple (x : int) = x * 3 [@@critical safe]
let ok_fn_call (x : int) = [%critical triple (triple x)]
[%%expect{|
val triple : int -> int = <fun>
val ok_fn_call : int -> int = <fun>
|}]

let rec ok_fn_rec (n : int) (acc : int) =
  if n = 0 then acc else ok_fn_rec (n - 1) (acc + n)
  [@@critical safe]
let ok_fn_rec_use (n : int) = [%critical ok_fn_rec n 0]
[%%expect{|
val ok_fn_rec : int -> int -> int = <fun>
val ok_fn_rec_use : int -> int = <fun>
|}]

(* Critical-safe functions may call each other. *)
let chain (x : int) = triple (triple x) [@@critical safe]
let ok_fn_chain (x : int) = [%critical chain x]
[%%expect{|
val chain : int -> int = <fun>
val ok_fn_chain : int -> int = <fun>
|}]

(* Verified functions remain ordinary functions outside sections. *)
let ok_outside (x : int) = triple x + 1
[%%expect{|
val ok_outside : int -> int = <fun>
|}]

let unverified (x : int) = x + 1
let bad_fn_unannot (x : int) = [%critical unverified x]
[%%expect{|
val unverified : int -> int = <fun>
Line 2, characters 42-54:
2 | let bad_fn_unannot (x : int) = [%critical unverified x]
                                              ^^^^^^^^^^^^
Error: This function call is not allowed inside a [%critical] section.
|}]

(* Calls must be fully applied... *)
let add (x : int) (y : int) = x + y [@@critical safe]
let bad_fn_partial (x : int) = [%critical add x]
[%%expect{|
val add : int -> int -> int = <fun>
Line 2, characters 42-47:
2 | let bad_fn_partial (x : int) = [%critical add x]
                                              ^^^^^
Error: This function call is not allowed inside a [%critical] section.
|}]

(* ...and direct. *)
let bad_fn_alias_use (x : int) = [%critical (let g = triple in g x)]
[%%expect{|
Line 1, characters 44-67:
1 | let bad_fn_alias_use (x : int) = [%critical (let g = triple in g x)]
                                                ^^^^^^^^^^^^^^^^^^^^^^^
Error: This function call is not allowed inside a [%critical] section.
|}]

(* The annotation requires a syntactic function... *)
let bad_fn_alias_def = triple [@@critical safe]
[%%expect{|
Line 1, characters 4-20:
1 | let bad_fn_alias_def = triple [@@critical safe]
        ^^^^^^^^^^^^^^^^
Error: [@@critical safe] must be applied to a syntactic function.
|}]

(* ...taking all of its arguments. *)
let bad_fn_arity (x : int) : int -> int = ( + ) x [@@critical safe]
[%%expect{|
Line 1, characters 4-16:
1 | let bad_fn_arity (x : int) : int -> int = ( + ) x [@@critical safe]
        ^^^^^^^^^^^^
Error: A [@@critical safe] function must take all of its arguments syntactically.
|}]

(* The body is verified. *)
let bad_fn_body (x : int) = (x, x) [@@critical safe]
[%%expect{|
Line 1, characters 28-34:
1 | let bad_fn_body (x : int) = (x, x) [@@critical safe]
                                ^^^^^^
Error: This allocation is not allowed inside a [%critical] section.
|}]

let bad_fn_calls_param (f : int -> int) (x : int) = f x [@@critical safe]
[%%expect{|
Line 1, characters 4-22:
1 | let bad_fn_calls_param (f : int -> int) (x : int) = f x [@@critical safe]
        ^^^^^^^^^^^^^^^^^^
Error: This function call is not allowed inside a [%critical] section.
|}]

(* Unsupported function annotations. *)
let bad_fn_blocking (x : int) = x [@@blocking]
[%%expect{|
Line 1, characters 4-19:
1 | let bad_fn_blocking (x : int) = x [@@blocking]
        ^^^^^^^^^^^^^^^
Error: [@@blocking] is only supported on external declarations.
|}]

let bad_fn_only (x : int) = x [@@critical]
[%%expect{|
Line 1, characters 4-15:
1 | let bad_fn_only (x : int) = x [@@critical]
        ^^^^^^^^^^^
Error: [@@critical] sections-only functions are not yet supported.
|}]

(*********************************************************)
(* Signature inclusion *)

module Ok_match : sig
  val f : int -> int [@@critical safe]
end = struct
  let f (x : int) = x * 2 [@@critical safe]
end
let ok_sig_use (x : int) = [%critical Ok_match.f x]
[%%expect{|
module Ok_match : sig val f : int -> int end
val ok_sig_use : int -> int = <fun>
|}]

(* The interface may drop the annotation (weakening)... *)
module Ok_weaken : sig
  val f : int -> int
end = struct
  let f (x : int) = x * 2 [@@critical safe]
end
[%%expect{|
module Ok_weaken : sig val f : int -> int end
|}]

(* ...after which sections may not call the function. *)
let bad_weakened_use (x : int) = [%critical Ok_weaken.f x]
[%%expect{|
Line 1, characters 44-57:
1 | let bad_weakened_use (x : int) = [%critical Ok_weaken.f x]
                                                ^^^^^^^^^^^^^
Error: This function call is not allowed inside a [%critical] section.
|}]

(* The interface may not claim an annotation the implementation lacks. *)
module Bad_claim : sig
  val f : int -> int [@@critical safe]
end = struct
  let f (x : int) = x * 2
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f (x : int) = x * 2
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int end
       is not included in
         sig val f : int -> int end
       Values do not match:
         val f : int -> int
       is not included in
         val f : int -> int
       The two declarations have different "[@@critical]" or "[@@blocking]" annotations
|}]
