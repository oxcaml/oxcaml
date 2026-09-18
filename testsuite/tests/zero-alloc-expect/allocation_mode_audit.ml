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

module Layout_inference = struct
  let unboxed () =
    let get = function [| x |] -> x | _ -> assert false in
    get [| #1L |]
end
[%%expect{|
module Layout_inference : sig val unboxed : unit -> int64_u end @@ stateless
|}]
