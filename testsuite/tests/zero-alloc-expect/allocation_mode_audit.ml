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

let (apply @ noalloc_strict) (f @ noalloc_strict) = f ()
[%%expect{|
val apply : (unit -> 'a) @ noalloc_strict -> 'a = <fun>
|}]

module Call_contracts = struct
  let (captured @ noalloc_strict) (f @ noalloc_strict) =
    let (inner @ noalloc_strict) () = f () in
    let result = inner () in
    result

  type callback = { run : (unit -> int) @@ noalloc_strict }

  let (projected @ noalloc_strict) callback = callback.run ()

  let (returned @ noalloc_strict) (get @ noalloc_strict) =
    let f @ noalloc_strict = get () in
    f ()

  let (relaxed @ noalloc) (f @ noalloc) = f ()
end
[%%expect{|
module Call_contracts :
  sig
    val captured : (unit -> 'a) @ noalloc_strict -> 'a @@ noalloc_strict
    type callback = { run : unit -> int @@ noalloc_strict; }
    val projected : callback -> int @@ noalloc_strict
    val returned :
      (unit -> (unit -> 'a) @ noalloc_strict) @ noalloc_strict -> 'a @@
      noalloc_strict
    val relaxed : (unit -> 'a) @ noalloc -> 'a
  end @@ stateless noalloc
|}]

let[@zero_alloc strict] unknown f = f ()
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc strict] unknown f = f ()
         ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP12.unknown (camlTOP12__unknown_13_27_code).
Line 1, characters 36-40:
1 | let[@zero_alloc strict] unknown f = f ()
                                        ^^^^
Error: called function may allocate (indirect tailcall)
|}]

let[@zero_alloc strict] relaxed_callee (f @ noalloc) = f ()
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc strict] relaxed_callee (f @ noalloc) = f ()
         ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP13.relaxed_callee (camlTOP13__relaxed_callee_14_29_code).
Line 1, characters 55-59:
1 | let[@zero_alloc strict] relaxed_callee (f @ noalloc) = f ()
                                                           ^^^^
Error: called function may allocate on a path to exceptional return (indirect tailcall)
|}]

let[@zero_alloc strict] allocating_after_call (f @ noalloc_strict) =
  [f ()]
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc strict] allocating_after_call (f @ noalloc_strict) =
         ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP14.allocating_after_call (camlTOP14__allocating_after_call_15_31_code).
Line 2, characters 2-8:
2 |   [f ()]
      ^^^^^^
Error: allocation of 24 bytes
|}]

module Parameter_contracts = struct
  let inferred f = f ()
  let (outer_only @ noalloc_strict) f = f ()
  let (both @ noalloc_strict) (f @ noalloc_strict) = f ()
end
[%%expect{|
module Parameter_contracts :
  sig
    val inferred : (unit -> 'a) -> 'a
    val outer_only : (unit -> 'a) -> 'a
    val both : (unit -> 'a) @ noalloc_strict -> 'a
  end @@ stateless noalloc_strict
|}]

module Untrusted_summary = struct
  let[@inline never] (apply @ noalloc_strict) f = f ()
  let[@inline never] allocate () = ref 0
  let[@zero_alloc strict] unknown f = apply f
  let[@zero_alloc strict] allocating () = apply allocate
end
[%%expect{|
Line 4, characters 7-17:
4 |   let[@zero_alloc strict] unknown f = apply f
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP16.Untrusted_summary.unknown (camlTOP16__unknown_21_44_code).
Line 4, characters 38-45:
4 |   let[@zero_alloc strict] unknown f = apply f
                                          ^^^^^^^
Error: called function may allocate (direct tailcall camlTOP16__apply_19_42_code)
Line 5, characters 7-17:
5 |   let[@zero_alloc strict] allocating () = apply allocate
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP16.Untrusted_summary.allocating (camlTOP16__allocating_22_45_code).
Line 5, characters 42-56:
5 |   let[@zero_alloc strict] allocating () = apply allocate
                                              ^^^^^^^^^^^^^^
Error: called function may allocate (direct tailcall camlTOP16__apply_19_42_code)
|}]

module Untrusted_inlining = struct
  let[@inline always] (apply @ noalloc_strict) f = f ()
  let[@inline never] allocate () = ref 0
  let[@zero_alloc strict] allocating () = apply allocate
end
[%%expect{|
Line 4, characters 7-17:
4 |   let[@zero_alloc strict] allocating () = apply allocate
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP17.Untrusted_inlining.allocating (camlTOP17__allocating_25_51_code).
Line 2, characters 51-55:
2 |   let[@inline always] (apply @ noalloc_strict) f = f ()
                                                       ^^^^
Error: called function may allocate (direct tailcall camlTOP17__allocate_24_50_code) (:4,42--56)
|}]

module Trusted_wrappers = struct
  let[@inline never] (apply @ noalloc_strict) f = f ()
  let[@inline always] (apply_inline @ noalloc_strict) f = f ()
  let[@zero_alloc strict] through_summary (f @ noalloc_strict) = apply f
  let[@zero_alloc strict] through_inlining (f @ noalloc_strict) =
    apply_inline f
end
[%%expect{|
module Trusted_wrappers :
  sig
    val apply : (unit -> 'a) -> 'a
    val apply_inline : (unit -> 'a) -> 'a
    val through_summary : (unit -> 'a) @ noalloc_strict -> 'a
      [@@zero_alloc strict]
    val through_inlining : (unit -> 'a) @ noalloc_strict -> 'a
      [@@zero_alloc strict]
  end @@ stateless noalloc_strict
|}]

let[@zero_alloc] (explicit_relaxed @ noalloc_strict) f = f ()
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc] (explicit_relaxed @ noalloc_strict) f = f ()
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP19.explicit_relaxed (camlTOP19__explicit_relaxed_30_61_code).
Line 1, characters 57-61:
1 | let[@zero_alloc] (explicit_relaxed @ noalloc_strict) f = f ()
                                                             ^^^^
Error: called function may allocate (indirect tailcall)
|}]

let (nested @ noalloc_strict) f =
  let inner () = f () in
  let result = inner () in result
[%%expect{|
val nested : (unit -> 'a) -> 'a = <fun>
|}]

let (generic_bigarray_set @ noalloc_strict) a i x =
  Bigarray.Array1.unsafe_set a i x
[%%expect{|
Line 2, characters 2-28:
2 |   Bigarray.Array1.unsafe_set a i x
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at lines 1-2, characters 44-34,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

let (specialized_bigarray_set @ noalloc_strict)
    (a : (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t) i x =
  Bigarray.Array1.unsafe_set a i x
[%%expect{|
val specialized_bigarray_set :
  (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (int -> int -> unit) @ local = <fun>
|}]

let (generic_bigarray_get @ noalloc_strict)
    (a : (int, Bigarray.int_elt, 'layout) Bigarray.Array1.t) i =
  Bigarray.Array1.unsafe_get a i
[%%expect{|
Line 3, characters 2-28:
3 |   Bigarray.Array1.unsafe_get a i
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at lines 2-3, characters 4-32,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Application_operators = struct
  let (apply @ noalloc_strict) f = f @@ ()
  let (reverse_apply @ noalloc_strict) f = () |> f
end
[%%expect{|
module Application_operators :
  sig
    val apply : (unit -> 'a) -> 'a
    val reverse_apply : (unit -> 'a) -> 'a
  end @@ portable noalloc_strict
|}]

module Inlined_definition = struct
  let[@inline always] make f = fun () -> f ()
  let specialize f = make f
end
[%%expect{|
module Inlined_definition :
  sig
    val make : (unit -> 'a) -> unit -> 'a
    val specialize : (unit -> 'a) -> unit -> 'a
  end @@ stateless
|}]

module Allocating_specialization = struct
  let[@inline always] make f = fun () -> f ()
  let[@inline always] allocate () = ref 0
  let specialized = make allocate
end
[%%expect{|
module Allocating_specialization :
  sig
    val make : (unit -> 'a) -> unit -> 'a @@ stateless nonportable
    val allocate : unit -> int ref
    val specialized : unit -> int ref
  end @@ portable
|}]

module Untrusted_allocating_specialization = struct
  let[@inline always] make f = fun () -> f ()
  let[@inline always] allocate () = ref 0
  let[@zero_alloc strict] bad () =
    let specialized = make allocate in
    specialized ()
end
[%%expect{|
Line 4, characters 7-17:
4 |   let[@zero_alloc strict] bad () =
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP27.Untrusted_allocating_specialization.bad (camlTOP27__bad_44_91_code).
Line 3, characters 36-41:
3 |   let[@inline always] allocate () = ref 0
                                        ^^^^^
Error: allocation of 16 bytes (:2,41--45;:6,4--18)
|}]

module Untrusted_returned_function = struct
  let[@inline never] allocate x = ref x
  let[@inline always][@zero_alloc strict] make () =
    let f x = allocate x in
    f
  let[@zero_alloc strict] bad x =
    let f = make () in
    f x
end
[%%expect{|
Line 6, characters 7-17:
6 |   let[@zero_alloc strict] bad x =
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP28.Untrusted_returned_function.bad (camlTOP28__bad_48_100_code).
Line 4, characters 14-24:
4 |     let f x = allocate x in
                  ^^^^^^^^^^
Error: called function may allocate (direct tailcall camlTOP28__allocate_45_97_code) (:8,4--7)
|}]
