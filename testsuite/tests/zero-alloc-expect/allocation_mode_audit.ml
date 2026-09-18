(* TEST
 flat-float-array;
 stack-allocation;
 flags = "-extension mode_alpha -extension mode_polymorphism_alpha";
 expect.opt;
*)

let (second @ noalloc_strict) : float iarray @ local -> float @ local = function
  | [: _; x; _ :] -> x
  | _ -> assert false
[%%expect{|
Line 2, characters 4-17:
2 |   | [: _; x; _ :] -> x
        ^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at lines 1-3, characters 72-21,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Primitive_result = struct
  let (f @ noalloc_strict) x = exp x
end
[%%expect{|
Line 2, characters 31-34:
2 |   let (f @ noalloc_strict) x = exp x
                                   ^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 27-36,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Curried_pattern = struct
  type t = { x : float }
  let f { x } () = x
  let g @ noalloc_strict = f { x = 1.0 }
end
[%%expect{|
Line 3, characters 8-13:
3 |   let f { x } () = x
            ^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 3, characters 14-20,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Let_pattern = struct
  type t = { x : float }
  let (f @ noalloc_strict) r =
    let { x } = r in x
end
[%%expect{|
Line 4, characters 8-13:
4 |     let { x } = r in x
            ^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at lines 3-4, characters 27-22,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Allocating_foreign_body = struct
  external allocate : int -> int -> int array @ local = "caml_make_vect"
  let (f @ noalloc_strict) n = exclave_ allocate n 0
end
[%%expect{|
Line 3, characters 40-48:
3 |   let (f @ noalloc_strict) n = exclave_ allocate n 0
                                            ^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 3, characters 27-52,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Nonallocating_patterns = struct
  let (ints @ noalloc_strict) xs =
    match xs with [| x |] -> x + 1 | _ -> 0
  let (wildcards @ noalloc_strict) (xs : float iarray) =
    match xs with [: _; _ :] -> true | _ -> false
  let (mutable_wildcard @ noalloc_strict) (xs : float array) =
    match xs with [| _ |] -> true | _ -> false
end
[%%expect{|
module Nonallocating_patterns :
  sig
    val ints : int array -> int
    val wildcards : float iarray -> bool @@ stateless nonportable
    val mutable_wildcard : float array -> bool @@ stateless nonportable
  end @@ portable noalloc_strict
|}]

module Primitive_representations = struct
  external exp_local : float @ local -> float @ local =
    "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
  let (local_result @ noalloc_strict) x = exclave_ exp_local x
  external exp_unboxed : float# -> float# =
    "caml_exp_float" "exp" [@@noalloc]
  let (unboxed_result @ noalloc_strict) x = exp_unboxed x
end
[%%expect{|
module Primitive_representations :
  sig
    external exp_local : float @ local -> float @ local = "caml_exp_float"
      "exp" [@@unboxed] [@@noalloc]
    val local_result : float -> float @ local @@ noalloc_strict
    external exp_unboxed : float# -> float# = "caml_exp_float" "exp"
      [@@noalloc]
    val unboxed_result : float# -> float# @@ noalloc_strict
  end
|}]

module Layout_inference = struct
  let unboxed () =
    let get = function [| x |] -> x | _ -> assert false in
    get [| #1L |]
end
[%%expect{|
module Layout_inference : sig val unboxed : unit -> int64_u end @@ stateless
|}]

module Unboxable = struct
  let[@unboxable] (constant @ noalloc_strict) () = 2.0
  let (identity @ noalloc_strict) : float @ global -> float @ global =
    fun (x[@unboxable]) -> x
end
[%%expect{|
module Unboxable :
  sig val constant : unit -> float val identity : float -> float end @@
  stateless noalloc_strict
|}]

let (apply @ noalloc_strict) f = f ()
[%%expect{|
Line 1, characters 5-37:
1 | let (apply @ noalloc_strict) f = f ()
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP10.apply (camlTOP10__apply_8_17_code).
Backend verification of inferred noalloc_strict mode failed.

Line 1, characters 33-37:
1 | let (apply @ noalloc_strict) f = f ()
                                     ^^^^
Error: called function may allocate (indirect tailcall)
|}]
