(* TEST
 flags += " -extension mode_alpha";
 flags += " -zero-alloc-check default";
 flags += " -zero-alloc-checker-details-cutoff 20";
 expect.opt;
*)

(* To prevent some default constraints on top-level functions, every test
  is wrapped in module Test. *)

module Test = struct

  end
[%%expect{|
module Test : sig end @@ stateless noalloc_strict
|}]

(* ==================================================================== *)
(* Part 1: the [zero_alloc] backend check (background)                  *)
(* ==================================================================== *)

(* [@zero_alloc strict] is a *backend* check.  After the middle end is done
   optimising, the checker walks the CFG of the function and proves that it
   contains no allocation instruction at all -- on any path, including the
   ones that raise.  Because it runs after optimisation, it sees the code
   that will actually execute, not the code as written. *)

(* 1a. A function that performs no allocation passes the check. *)

module Test = struct
  let[@zero_alloc strict] span (a : int) (b : int) =
    if a < b then b - a else a - b
end
[%%expect{|
module Test : sig val span : int -> int -> int [@@zero_alloc strict] end @@
  portable
|}]

(* 1b. Change it to hand back both endpoints as a tuple and the check fails.
   The tuple is a real heap block, and the witness points straight at it. *)

module Test = struct
  let[@zero_alloc strict] span_pair (a : int) (b : int) =
    if a < b then (a, b) else (b, a)
end
[%%expect{|
Line 2, characters 7-17:
2 |   let[@zero_alloc strict] span_pair (a : int) (b : int) =
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP3.Test.span_pair (camlTOP3__span_pair_4_5_code).
Line 3, characters 18-24:
3 |     if a < b then (a, b) else (b, a)
                      ^^^^^^
Error: allocation of 24 bytes
Line 3, characters 30-36:
3 |     if a < b then (a, b) else (b, a)
                                  ^^^^^^
Error: allocation of 24 bytes
|}]

(* 1c. The very same tuple, but now consumed inside the function instead of
   returned.  Flambda2 unboxes it, so no allocation survives to the CFG and
   the check passes again.  This is the flip side of checking the backend:
   the verdict depends on what the optimiser managed to do, not on the
   source text. *)

module Test = struct
  let[@zero_alloc strict] span_pair_used (a : int) (b : int) =
    let (lo, hi) = if a < b then (a, b) else (b, a) in
    hi - lo
end
[%%expect{|
module Test :
  sig val span_pair_used : int -> int -> int [@@zero_alloc strict] end @@
  portable
|}]

(* ==================================================================== *)
(* Part 2: [zero_alloc] is too restrictive for higher-order arguments   *)
(* ==================================================================== *)

(* Here is [List.iter], written out.  It allocates nothing itself: it
   destructures a list and calls [f].  Whether the whole thing allocates
   depends entirely on which [f] the caller supplies.

   The backend checker cannot know that.  All it sees is an indirect call
   through an unknown function pointer, which it must assume may allocate,
   so the annotation is rejected.  There is no way to write "iter does not
   allocate provided f does not allocate" -- the property [zero_alloc] can
   express is not conditional on the argument. *)

module Test = struct
  let[@zero_alloc strict] rec iter f l =
    match l with
    | [] -> ()
    | x :: xs -> f x; iter f xs
end
[%%expect{|
Line 2, characters 7-17:
2 |   let[@zero_alloc strict] rec iter f l =
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP5.Test.iter (camlTOP5__iter_8_9_code).
Line 5, characters 17-20:
5 |     | x :: xs -> f x; iter f xs
                     ^^^
Error: called function may allocate (indirect call)
|}]

(* ==================================================================== *)
(* Part 3: the Allocation mode axis                                     *)
(* ==================================================================== *)

(* Comonadic axis: noalloc_strict < noalloc < alloc
   For now we do not distinguish [noalloc_strict] and [noalloc]. *)

module Test = struct
  let rec (iter @ noalloc_strict) f l =
    match l with
    | [] -> ()
    | x :: xs -> f x; iter f xs
end
[%%expect{|
module Test : sig val iter : ('a -> 'b) -> ('a list -> unit) @ local end @@
  stateless noalloc_strict
|}]

(* Call iter with noalloc arguments *)
module Test = struct
  let rec (iter @ noalloc_strict) f l =
    match l with
    | [] -> ()
    | x :: xs -> f x; iter f xs

  let (span @ noalloc_strict) a b = if a < b then b - a else a - b

  let (test @ noalloc_strict) l =
    let _ = iter span l in
    ()
end
[%%expect{|
module Test :
  sig
    val iter : ('a -> 'b @ local) -> ('a list -> unit) @ local @@ stateless
      nonportable
    val span : int -> (int -> int) @ local
    val test : int list -> unit
  end @@ portable noalloc_strict
|}]

(* Call iter with alloc arguments *)
module Test = struct
  let rec (iter @ noalloc_strict) f l =
    match l with
    | [] -> ()
    | x :: xs -> f x; iter f xs

  let span_pair a b = if a < b then (a, b) else (b, a)

  let (test @ noalloc_strict) l =
    let _ = iter span_pair l in
    ()
end
[%%expect{|
Line 10, characters 17-26:
10 |     let _ = iter span_pair l in
                      ^^^^^^^^^
Error: The value "span_pair" is "alloc"
         because it closes over the allocation at line 7, characters 18-54
         which is "alloc".
       However, the value "span_pair" highlighted is expected to be "noalloc_strict"
         because it is used inside the function at lines 9-11, characters 30-6
         which is expected to be "noalloc_strict".
|}]

(* ==================================================================== *)
(* Part 4: what the Allocation mode axis actually guarantees            *)
(* ==================================================================== *)

(* The property is: a noalloc function does not allocate when it is called
   with noalloc arguments (including both full and partial application) or
   simply referenced without any arguments. *)

(* 4a. A closure that closes over an [alloc] function is [alloc]. *)
module Test = struct
  let g x = (x, x)

  let (capture_alloc @ noalloc_strict) () = g 1
end
[%%expect{|
Line 4, characters 44-45:
4 |   let (capture_alloc @ noalloc_strict) () = g 1
                                                ^
Error: The value "g" is "alloc"
         because it closes over the allocation at line 2, characters 12-18
         which is "alloc".
       However, the value "g" highlighted is expected to be "noalloc_strict"
         because it is used inside the function at line 4, characters 39-47
         which is expected to be "noalloc_strict".
|}]

(* 4b. Primitives are [alloc] by default, but one that provably does not
   allocate when fully applied is exempt from that. *)
module Test = struct
  let (add @ noalloc_strict) (x : int) (y : int) = x + y
  let (add_partial @ alloc) (x : int) = (+) x

  (* Referencing the primitive without applying it allocates a closure. *)
  let (add_ref @ noalloc_strict) () = (+)

  (* Float [+.] boxes its result, so it allocates even when fully applied.*)
  let (add_float @ alloc) (x : float) (y : float) = x +. y
end
[%%expect{|
Line 6, characters 38-41:
6 |   let (add_ref @ noalloc_strict) () = (+)
                                          ^^^
Error: The value "(+)" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 6, characters 33-41
         which is expected to be "noalloc_strict".
|}]

(* 4c. A [noalloc_strict] function forces every allocation in its body to be
   local. *)
module Test = struct
  let (make_pair @ noalloc_strict) (x : int) = exclave_ (x, x)
end
[%%expect{|
module Test : sig val make_pair : int -> int * int @ local end @@ stateless
  noalloc_strict
|}]

(* 4d. Closure allocation is just another allocation, so in a multi-argument
   [noalloc_strict] function the intermediate closures are local too. *)
module Test = struct
  (*= let (add3 @ noalloc_strict) (x : int) (y : int) (z : int) = z *)
  let (add3 @ noalloc_strict) (x: int) (y: int) = let (foo @ global) = fun x -> x in foo
  let escaping_partial () = add3 1 2
end
[%%expect{|
Line 3, characters 71-81:
3 |   let (add3 @ noalloc_strict) (x: int) (y: int) = let (foo @ global) = fun x -> x in foo
                                                                           ^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 3, characters 30-88,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* ==================================================================== *)
(* Part 5: Utilizing [zero_alloc] backend check                         *)
(* ==================================================================== *)

module Test = struct
  module M_strict : sig
    val span_pair_used : int -> int -> int [@@zero_alloc strict]
  end = struct
    let[@zero_alloc strict] span_pair_used (a : int) (b : int) =
      let (lo, hi) = if a < b then (a, b) else (b, a) in
      hi - lo
  end

  let (fully_applied @ noalloc_strict) () =
    M_strict.span_pair_used 1 0

  let (partially_applied @ alloc) () =
    let _ = M_strict.span_pair_used 1 in ()

  let (ref_partially_applied @ alloc) () =
    let f = M_strict.span_pair_used in
    f 1

  let (ref_only @ alloc) () =
    M_strict.span_pair_used
end
[%%expect{|
Line 14, characters 12-37:
14 |     let _ = M_strict.span_pair_used 1 in ()
                 ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

module Test :
  sig
    module M_strict :
      sig val span_pair_used : int -> int -> int [@@zero_alloc strict] end
    val fully_applied : unit -> int @@ noalloc_strict
    val partially_applied : unit -> unit
    val ref_partially_applied : unit -> int -> int
    val ref_only : unit -> int -> int -> int
  end @@ portable
|}]

(* Limitation: Cannot utlize [zero_alloc] annotation on let-bindings. *)
module Test = struct
  let[@zero_alloc strict] span_pair_used (a : int) (b : int) =
    let (lo, hi) = if a < b then (a, b) else (b, a) in
    hi - lo

  let (fully_applied @ noalloc_strict) () =
    span_pair_used 1 0
end
[%%expect{|
Line 7, characters 4-18:
7 |     span_pair_used 1 0
        ^^^^^^^^^^^^^^
Error: The value "span_pair_used" is "alloc"
         because it closes over the allocation at lines 2-4, characters 51-11
         which is "alloc".
       However, the value "span_pair_used" highlighted is expected to be "noalloc_strict"
         because it is used inside the function at lines 6-7, characters 39-22
         which is expected to be "noalloc_strict".
|}]


(* ==================================================================== *)
(* Part 6: More cases: mult-arg functions, escape from zero_alloc...    *)
(* ==================================================================== *)

module Test = struct
  module M_strict : sig
    val span_pair_used : int -> int -> int [@@zero_alloc strict]
  end = struct
    let[@zero_alloc strict] span_pair_used (a : int) (b : int) =
      let (lo, hi) = if a < b then (a, b) else (b, a) in
      hi - lo
  end

  module Wrapper : sig
    type t
    val wrapper : unit -> t [@@zero_alloc strict arity 1]
    (* val wrapper : unit -> int *)
  end = struct
    type t = int -> int -> int * int
    let [@zero_alloc strict] wrapper () =
      let g = fun a b -> (a, b) in
      g

    (* type t = int -> int -> int
    let [@zero_alloc strict] wrapper ()  = M_strict.span_pair_used *)
    (* let (wrapper @ noalloc_strict) ()  = M_strict.span_pair_used 1 0 *)
  end

  let (whitewashed @ noalloc_strict) () = Wrapper.wrapper ()
end
[%%expect{|
Line 25, characters 42-60:
25 |   let (whitewashed @ noalloc_strict) () = Wrapper.wrapper ()
                                               ^^^^^^^^^^^^^^^^^^
Error: The return value of a zero_alloc function is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 25, characters 37-60
         which is expected to be "noalloc_strict".
|}]

(* Example: applied_arity > func arity *)
module Test = struct

  let f x = x

  let g x y = (x, y)

  let test () =
    f g 1 2

end
[%%expect{|
module Test : sig val f : 'a -> 'a end @@ stateless noalloc_strict
|}]


(* ==================================================================== *)
(* Appendix: assorted [zero_alloc] backend-check behaviours             *)
(* ==================================================================== *)

(* Backend check: a function that really is zero-alloc. *)

let[@zero_alloc] add x y = x + y
[%%expect{|
val add : int -> int -> int [@@zero_alloc] = <fun>
|}]

(* Backend check: an allocation is reported, with a witness pointing at it. *)

let[@zero_alloc] pair x = (x, x)
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc] pair x = (x, x)
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP17.pair (camlTOP17__pair_36_35_code).
Line 1, characters 26-32:
1 | let[@zero_alloc] pair x = (x, x)
                              ^^^^^^
Error: allocation of 24 bytes
|}]

(* Backend check: [strict] also rules out allocation on the raising path, and
   says so in the error message. *)

exception E of int
let[@zero_alloc strict] may_raise x = if x > 0 then x else raise (E x)
[%%expect{|
exception E of int
Line 2, characters 5-15:
2 | let[@zero_alloc strict] may_raise x = if x > 0 then x else raise (E x)
         ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP19.may_raise (camlTOP19__may_raise_38_37_code).
Line 2, characters 65-70:
2 | let[@zero_alloc strict] may_raise x = if x > 0 then x else raise (E x)
                                                                     ^^^^^
Error: allocation of 24 bytes
|}]

(* Backend check, interprocedural: [assume] lets a caller pass even though the
   callee allocates.  Both must be in the same phrase. *)

module Assume = struct
  let[@zero_alloc assume] allocates x = (x, x)
  let[@zero_alloc] caller x = let (a, _) = allocates x in a
end
[%%expect{|
module Assume :
  sig
    val allocates : 'a -> 'a * 'a [@@zero_alloc]
    val caller : 'a -> 'a [@@zero_alloc]
  end @@ stateless
|}]

(* Without [assume], the same shape is rejected. *)

module No_assume = struct
  let[@inline never] allocates x = (x, x)
  let[@zero_alloc] caller x = let (a, _) = allocates x in a
end
[%%expect{|
Line 3, characters 7-17:
3 |   let[@zero_alloc] caller x = let (a, _) = allocates x in a
           ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP21.No_assume.caller (camlTOP21__caller_45_45_code).
Line 3, characters 43-54:
3 |   let[@zero_alloc] caller x = let (a, _) = allocates x in a
                                               ^^^^^^^^^^^
Error: called function may allocate (direct call camlTOP21__allocates_44_44_code)
|}]

(* Frontend allocation axis: the [noalloc] mode.  This is a typing-time check,
   so it reports before the backend ever runs. *)

let call_noalloc (g : (int -> int) @ noalloc) = g 1
[%%expect{|
val call_noalloc : (int -> int) @ noalloc -> int = <fun>
|}]

let ok = call_noalloc (fun x -> x)
[%%expect{|
val ok : int = 1
|}]

let bad = call_noalloc (fun x -> ignore (Sys.opaque_identity [x]); x)
[%%expect{|
Line 1, characters 41-60:
1 | let bad = call_noalloc (fun x -> ignore (Sys.opaque_identity [x]); x)
                                             ^^^^^^^^^^^^^^^^^^^
Error: The value "Sys.opaque_identity" is "alloc"
       but is expected to be "noalloc"
         because it is used inside the function at line 1, characters 23-69
         which is expected to be "noalloc".
|}]

(* CR shsong: The following example demostrates an issue: when a recursive
  function is define at the top level, Allocation mode inference does not
  work very well. *)
let rec (iter @ noalloc_strict) f l =
  match l with
  | [] -> ()
  | x :: xs -> f x; iter f xs
[%%expect{|
Line 4, characters 20-24:
4 |   | x :: xs -> f x; iter f xs
                        ^^^^
Error: The value "iter" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 1-4, characters 32-29
         which is expected to be "noalloc_strict".
|}]
