(* TEST
 flags += " -extension mode_alpha";
 flags += " -zero-alloc-check default";
 flags += " -zero-alloc-checker-details-cutoff 20";
 expect.opt;
*)

(* Cross-check the front end's primitive-allocation classification against the
   back end's.

   Every function below calls one fully-applied primitive, and carries two
   independent annotations:

   - [@ noalloc_strict], the Allocation mode axis. Typing decides this from
     [Translprim.fully_applied_may_allocate], which looks the primitive up and
     specializes it exactly as [Translprim] will.
   - [[@zero_alloc strict]], the back-end check. It walks the CFG after
     optimisation and proves no allocation instruction survives.

   The back-end check is the ground truth. If a function is accepted at
   [noalloc_strict] but rejected by [zero_alloc strict], the front-end
   classification is unsound and this test says so.

   Tests are wrapped in [module Test] to avoid the default constraints on
   top-level functions, and take a single parameter so that no intermediate
   closure shows up. *)

(* ==================================================================== *)
(* Part 1: primitives that map to a non-allocating [Lambda.primitive]   *)
(* ==================================================================== *)

(* Tagged-integer arithmetic and comparison: [Pscalar]. *)
module Test = struct
  let[@zero_alloc strict] (add @ noalloc_strict) (x : int) = x + 1
  let[@zero_alloc strict] (sub @ noalloc_strict) (x : int) = x - 1
  let[@zero_alloc strict] (mul @ noalloc_strict) (x : int) = x * 2
  let[@zero_alloc strict] (div @ noalloc_strict) (x : int) = x / 2
  let[@zero_alloc strict] (neg @ noalloc_strict) (x : int) = - x
  let[@zero_alloc strict] (logand @ noalloc_strict) (x : int) = x land 3
  let[@zero_alloc strict] (shift @ noalloc_strict) (x : int) = x lsl 1
  let[@zero_alloc strict] (lt @ noalloc_strict) (x : int) = x < 0
end
[%%expect{|
module Test :
  sig
    val add : int -> int [@@zero_alloc strict]
    val sub : int -> int [@@zero_alloc strict]
    val mul : int -> int [@@zero_alloc strict]
    val div : int -> int [@@zero_alloc strict]
    val neg : int -> int [@@zero_alloc strict]
    val logand : int -> int [@@zero_alloc strict]
    val shift : int -> int [@@zero_alloc strict]
    val lt : int -> bool [@@zero_alloc strict]
  end @@ portable noalloc_strict
|}]

(* Booleans: [Pnot], [Psequand], [Psequor]. *)
module Test = struct
  let[@zero_alloc strict] (bnot @ noalloc_strict) (x : bool) = not x
  let[@zero_alloc strict] (band @ noalloc_strict) (x : bool) = x && false
  let[@zero_alloc strict] (bor @ noalloc_strict) (x : bool) = x || true
end
[%%expect{|
module Test :
  sig
    val bnot : bool -> bool [@@zero_alloc strict]
    val band : bool -> bool [@@zero_alloc strict]
    val bor : bool -> bool [@@zero_alloc strict]
  end @@ portable noalloc_strict
|}]

(* Block access: [Pfield] and [Psetfield]. *)
module Test = struct
  let[@zero_alloc strict] (deref @ noalloc_strict) (r : int ref) = !r
  let[@zero_alloc strict] (assign @ noalloc_strict) (r : int ref) = r := 0
  let[@zero_alloc strict] (incr_ref @ noalloc_strict) (r : int ref) = incr r
end
[%%expect{|
module Test :
  sig
    val deref : int ref -> int [@@zero_alloc strict]
    val assign : int ref -> unit [@@zero_alloc strict]
    val incr_ref : int ref -> unit [@@zero_alloc strict]
  end @@ portable noalloc_strict
|}]

(* Lengths and representation tests: [Pstringlength], [Pbyteslength],
   [Parraylength], [Pisint]. *)
module Test = struct
  let[@zero_alloc strict] (slen @ noalloc_strict) (s : string) =
    String.length s
  let[@zero_alloc strict] (blen @ noalloc_strict) (b : bytes) = Bytes.length b
  let[@zero_alloc strict] (alen @ noalloc_strict) (a : int array) =
    Array.length a
  let[@zero_alloc strict] (isint @ noalloc_strict) (x : Obj.t) = Obj.is_int x
end
[%%expect{|
module Test :
  sig
    val slen : string -> int [@@zero_alloc strict]
    val blen : bytes -> int [@@zero_alloc strict]
    val alen : int array -> int @@ stateless nonportable
      [@@zero_alloc strict]
    val isint : Obj.t -> bool [@@zero_alloc strict]
  end @@ portable noalloc_strict
|}]

(* [%identity] and [%opaque] compile to the argument itself and to [Popaque],
   neither of which allocates. *)
module Test = struct
  external my_id : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  let[@zero_alloc strict] (ident @ noalloc_strict) (x : int) = my_id x
  let[@zero_alloc strict] (opaque @ noalloc_strict) (x : int) =
    Sys.opaque_identity x
end
[%%expect{|
module Test :
  sig
    external my_id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
    val ident : int -> int @@ noalloc_strict [@@zero_alloc strict]
    val opaque : int -> int @@ portable noalloc_strict [@@zero_alloc strict]
  end
|}]

(* Closing over an allocation is not the same as allocating when called. The
   ref below is allocated once, where it is defined; calling the closure only
   performs a setfield. Both checks agree that the closure is allocation-free.
   Compare [functor.ml], where [let f' = let r = ref 42 in fun () -> r := 24]
   raises exactly this question. *)
module Test = struct
  let[@zero_alloc strict] (bump_param @ noalloc_strict) (r : int ref) =
    r := 24
end
[%%expect{|
module Test : sig val bump_param : int ref -> unit [@@zero_alloc strict] end
  @@ portable noalloc_strict
|}]
module Test = struct
  let r = ref 42
  let[@zero_alloc strict] (bump_captured @ noalloc_strict) () = r := 24
end
[%%expect{|
module Test :
  sig
    val r : int ref
    val bump_captured : unit -> unit @@ noalloc_strict [@@zero_alloc strict]
  end
|}]

(* Whereas allocating on each call is rejected, as it should be. *)
module Test = struct
  let (copy @ noalloc_strict) (r : int ref) = ref !r
end
[%%expect{|
Line 2, characters 46-52:
2 |   let (copy @ noalloc_strict) (r : int ref) = ref !r
                                                  ^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* -------------------------------------------------------------------- *)
(* Known conservatism: inference does not give these [noalloc_strict]     *)
(* -------------------------------------------------------------------- *)

(* These are the shapes from [typing-modes/functor.ml] and
   [typing-modes/module.ml]. Calling any of them performs only a setfield --
   the ref was allocated once, where it is bound, not on each call -- so they
   are genuinely allocation-free, and the back-end check at the end of this
   section confirms it.

   The current implementation is conservative about them: the modality is not
   *inferred*, though it is accepted when written out. So the checking is
   precise and only the inference falls short. *)

(* [functor.ml]: inferred, and no [noalloc_strict] appears. *)
module Test = struct
  let f = let r = ref 42 in fun () -> r := 24; ()
end
[%%expect{|
module Test : sig val f : unit -> unit end @@ noalloc_strict
|}]

(* The same definition with the modality written out is accepted. *)
module Test = struct
  let (f @ noalloc_strict) = let r = ref 42 in fun () -> r := 24; ()
end
[%%expect{|
module Test : sig val f : unit -> unit end @@ noalloc_strict
|}]

(* [module.ml]: same conservatism for a ref bound in the enclosing structure.
   Note [x] itself is inferred [noalloc_strict] while [f] is not. *)
module Test = struct
  let x : int ref = ref 42
  let f () = x := 24
end
[%%expect{|
module Test : sig val x : int ref @@ stateless val f : unit -> unit end @@
  noalloc_strict
|}]

(* Written out, it is accepted -- and the back end agrees that calling it
   allocates nothing. *)
module Test = struct
  let x : int ref = ref 42
  let[@zero_alloc strict] (f @ noalloc_strict) () = x := 24
end
[%%expect{|
module Test :
  sig
    val x : int ref @@ stateless
    val f : unit -> unit [@@zero_alloc strict]
  end @@ noalloc_strict
|}]

(* ==================================================================== *)
(* Part 2: primitives whose allocation depends on the types involved    *)
(* ==================================================================== *)

(* These are the cases a name-only classification cannot express: the same
   primitive allocates at one type and not at another. *)

(* [%compare] at [int] becomes a [Pscalar] three-way compare; at [string] it
   becomes [caml_string_compare], a C call declared [[@@noalloc]]. *)
module Test = struct
  let[@zero_alloc strict] (cmp_int @ noalloc_strict) (x : int) = compare x 0
  let[@zero_alloc strict] (cmp_string @ noalloc_strict) (x : string) =
    compare x "a"
  let[@zero_alloc strict] (eq_string @ noalloc_strict) (x : string) = x = "a"
  let[@zero_alloc strict] (cmp_char @ noalloc_strict) (x : char) = compare x 'a'
end
[%%expect{|
module Test :
  sig
    val cmp_int : int -> int [@@zero_alloc strict]
    val cmp_string : string -> int [@@zero_alloc strict]
    val eq_string : string -> bool [@@zero_alloc strict]
    val cmp_char : char -> int [@@zero_alloc strict]
  end @@ portable noalloc_strict
|}]

(* A constant constructor argument specializes generic equality to an integer
   comparison even though the type alone would not allow it. *)
module Test = struct
  let[@zero_alloc strict] (eq_none @ noalloc_strict) (x : int option) =
    x = None
end
[%%expect{|
module Test : sig val eq_none : int option -> bool [@@zero_alloc strict] end
  @@ portable noalloc_strict
|}]

(* Reading an [int array] does not box the element. *)
module Test = struct
  let[@zero_alloc strict] (get_int @ noalloc_strict) (a : int array) =
    Array.unsafe_get a 0
  let[@zero_alloc strict] (get_int_safe @ noalloc_strict) (a : int array) =
    a.(0)
  let[@zero_alloc strict] (set_int @ noalloc_strict) (a : int array) =
    Array.unsafe_set a 0 1
end
[%%expect{|
module Test :
  sig
    val get_int : int array -> int [@@zero_alloc strict]
    val get_int_safe : int array -> int [@@zero_alloc strict]
    val set_int : int array -> unit [@@zero_alloc strict]
  end @@ portable noalloc_strict
|}]

(* Reading a [float array] unboxes into a fresh box, so it does allocate and
   the front end must reject it. *)
module Test = struct
  let (get_float @ noalloc_strict) (a : float array) = Array.unsafe_get a 0
end
[%%expect{|
Line 2, characters 55-71:
2 |   let (get_float @ noalloc_strict) (a : float array) = Array.unsafe_get a 0
                                                           ^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 35-75,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* At an unknown type [%compare] becomes [caml_compare], which allocates. *)
module Test = struct
  let (cmp_poly @ noalloc_strict) x = compare x x
end
[%%expect{|
Line 2, characters 38-45:
2 |   let (cmp_poly @ noalloc_strict) x = compare x x
                                          ^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 34-49,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* ==================================================================== *)
(* Part 3: C externals                                                  *)
(* ==================================================================== *)

(* [[@@noalloc]] on an external is [prim_alloc = false], which the classifier
   reads directly. *)
module Test = struct
  external eq_nofail : string -> string -> bool = "caml_string_equal"
    [@@noalloc]
  let[@zero_alloc strict] (c_noalloc @ noalloc_strict) (x : string) =
    eq_nofail x "a"
end
[%%expect{|
module Test :
  sig
    external eq_nofail : string -> string -> bool = "caml_string_equal"
      [@@noalloc]
    val c_noalloc : string -> bool @@ noalloc_strict [@@zero_alloc strict]
  end
|}]

(* The same C function without the attribute is assumed to allocate. *)
module Test = struct
  external eq_may_alloc : string -> string -> bool = "caml_string_equal"
  let (c_alloc @ noalloc_strict) (x : string) = eq_may_alloc x "a"
end
[%%expect{|
Line 3, characters 48-60:
3 |   let (c_alloc @ noalloc_strict) (x : string) = eq_may_alloc x "a"
                                                    ^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 3, characters 33-66,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* ==================================================================== *)
(* Part 4: the application operators                                    *)
(* ==================================================================== *)

(* [%revapply] and [%apply] compile to a direct application of their function
   argument. The operator allocates nothing; whatever the callee allocates is
   accounted for through the callee's own mode. *)
module Test = struct
  let[@zero_alloc strict] (succ_ @ noalloc_strict) (x : int) = x + 1
  let[@zero_alloc strict] (revapply @ noalloc_strict) (x : int) = x |> succ_
  let[@zero_alloc strict] (apply @ noalloc_strict) (x : int) = succ_ @@ x
end
[%%expect{|
module Test :
  sig
    val succ_ : int -> int [@@zero_alloc strict]
    val revapply : int -> int [@@zero_alloc strict]
    val apply : int -> int [@@zero_alloc strict]
  end @@ portable noalloc_strict
|}]

(* ==================================================================== *)
(* Part 5: where a partial application's closure is accounted for       *)
(* ==================================================================== *)

(* [collect_apply_args] stops when it runs out of source arguments, so a
   trailing positional partial application produces no [Omitted] node and never
   reaches [type_omitted_parameters_and_build_result_type]. The closure it
   builds is therefore *not* registered by that function's
   [register_allocation_mode]; it is accounted for by the mode of the primitive
   identifier itself, which is [alloc]. Only a parameter skipped mid-list --
   a labelled one commuted past -- becomes [Omitted]. *)

(* A [Prim_local] result is the sharpest case: [register_prim_allocation]
   registers *nothing* for it, because the primitive's own result goes on the
   stack. A partial application still builds a heap closure, and the two have
   genuinely different modes -- the eta-expansion wrapper is
   [{nlocal = 1} ... : localfloat] inside a [{nlocal = 0}] closure. What
   rejects this is the identifier's own [alloc] mode, not allocation
   registration. *)
module Test = struct
  external getl : float array -> int -> local_ float = "%array_unsafe_get"
  let (partial_local_result @ noalloc_strict) (a : float array) =
    ignore (getl a)
end
[%%expect{|
Line 4, characters 12-16:
4 |     ignore (getl a)
                ^^^^
Error: The allocation is "local"
         because it is allocated inside the function at lines 3-4, characters 46-19,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* Same for a bare reference and for a partial application of a primitive with
   a [Prim_global] result. *)
module Test = struct
  let (bare_ref @ noalloc_strict) () = ( + )
end
[%%expect{|
Line 2, characters 39-44:
2 |   let (bare_ref @ noalloc_strict) () = ( + )
                                           ^^^^^
Error: The value "(+)" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 2, characters 34-44
         which is expected to be "noalloc_strict".
|}]
module Test = struct
  let (partial @ noalloc_strict) (a : int) = ( + ) a
end
[%%expect{|
Line 2, characters 45-50:
2 |   let (partial @ noalloc_strict) (a : int) = ( + ) a
                                                 ^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 33-52,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* A [Prim_poly] primitive, referenced without arguments. *)
module Test = struct
  external mk_poly : 'a -> ('a ref[@local_opt]) = "%makemutable"
  let (bare_ref_poly @ noalloc_strict) () = mk_poly
end
[%%expect{|
Line 3, characters 44-51:
3 |   let (bare_ref_poly @ noalloc_strict) () = mk_poly
                                                ^^^^^^^
Error: The value "mk_poly" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at line 3, characters 39-51
         which is expected to be "noalloc_strict".
|}]

(* A [Prim_poly] primitive partially applied, guarded with [exclave_] so that
   the result is allowed to be local. That does not rescue it: [exclave_] moves
   the *result* into the caller's region, but the partial-application closure
   itself is still built on the heap, so the identifier's own [alloc] mode
   still rejects the occurrence. *)
module Test = struct
  external addf :
    (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt])
    = "%addfloat"
  let (partial_poly @ noalloc_strict) (x : float) = exclave_ addf x
end
[%%expect{|
Line 5, characters 61-65:
5 |   let (partial_poly @ noalloc_strict) (x : float) = exclave_ addf x
                                                                 ^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 5, characters 38-67,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* The back-end check on the same code, with the mode annotation dropped, and
   it agrees: the closure really is a heap allocation. *)
module Test = struct
  external addf :
    (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt])
    = "%addfloat"
  let[@zero_alloc strict] partial_poly (x : float) = exclave_ addf x
end
[%%expect{|
Line 5, characters 7-17:
5 |   let[@zero_alloc strict] partial_poly (x : float) = exclave_ addf x
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP26.Test.partial_poly (camlTOP26__partial_poly_76_79_code).
Line 5, characters 62-68:
5 |   let[@zero_alloc strict] partial_poly (x : float) = exclave_ addf x
                                                                  ^^^^^^
Error: allocation of 40 bytes for closure
|}]

(* This is not specific to primitives: an ordinary function partially applied
   under [exclave_] allocates its closure on the heap too. *)
module Test = struct
  let add3 (x : int) (y : int) = x + y
  let[@zero_alloc strict] partial_ordinary (x : int) = exclave_ add3 x
end
[%%expect{|
Line 3, characters 7-17:
3 |   let[@zero_alloc strict] partial_ordinary (x : int) = exclave_ add3 x
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP27.Test.partial_ordinary (camlTOP27__partial_ordinary_82_84_code).
Line 3, characters 64-70:
3 |   let[@zero_alloc strict] partial_ordinary (x : int) = exclave_ add3 x
                                                                    ^^^^^^
Error: allocation of 40 bytes for closure
|}]

(* And the back-end check is not simply counting every allocation: [exclave_]
   does keep an ordinary block off the heap, and that passes. So the two
   failures above are really about the closure. *)
module Test = struct
  let[@zero_alloc strict] make_pair (x : int) = exclave_ (x, x)
end
[%%expect{|
module Test :
  sig val make_pair : int -> int * int @ local [@@zero_alloc strict] end @@
  stateless
|}]

(* The mid-list case, which does go through
   [type_omitted_parameters_and_build_result_type]: [~b] is supplied but [~a]
   is not, so [~a] becomes an [Omitted] node and its closure is registered
   there. *)
module Test = struct
  let (omitted_mid_list @ noalloc_strict) () =
    (fun ~a ~b -> a + b) ~b:2
end
[%%expect{|
Line 3, characters 4-24:
3 |     (fun ~a ~b -> a + b) ~b:2
        ^^^^^^^^^^^^^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at lines 2-3, characters 42-29,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* ==================================================================== *)
(* Part 6: primitives that really do allocate                           *)
(* ==================================================================== *)

(* Negative controls: the front end must reject these, and the back end agrees
   -- see the [zero_alloc strict] failures for the same shapes. *)

(* [%makemutable] builds a block. *)
module Test = struct
  let (mk_ref @ noalloc_strict) (x : int) = ref x
end
[%%expect{|
Line 2, characters 44-49:
2 |   let (mk_ref @ noalloc_strict) (x : int) = ref x
                                                ^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* [%addfloat] boxes its result. *)
module Test = struct
  let (add_float @ noalloc_strict) (x : float) = x +. 1.0
end
[%%expect{|
Line 2, characters 49-57:
2 |   let (add_float @ noalloc_strict) (x : float) = x +. 1.0
                                                     ^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* The back end confirms the same two, independently of the mode axis. *)
module Test = struct
  let[@zero_alloc strict] mk_ref (x : int) = ref x
end
[%%expect{|
Line 2, characters 7-17:
2 |   let[@zero_alloc strict] mk_ref (x : int) = ref x
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP32.Test.mk_ref (camlTOP32__mk_ref_88_88_code).
Line 2, characters 45-50:
2 |   let[@zero_alloc strict] mk_ref (x : int) = ref x
                                                 ^^^^^
Error: allocation of 16 bytes
|}]

module Test = struct
  let[@zero_alloc strict] add_float (x : float) = x +. 1.0
end
[%%expect{|
Line 2, characters 7-17:
2 |   let[@zero_alloc strict] add_float (x : float) = x +. 1.0
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP33.Test.add_float (camlTOP33__add_float_90_90_code).
Line 2, characters 50-58:
2 |   let[@zero_alloc strict] add_float (x : float) = x +. 1.0
                                                      ^^^^^^^^
Error: allocation of 16 bytes for float
|}]

(* ==================================================================== *)
(* Part 7: over-application and [Translcore.can_apply_primitive]        *)
(* ==================================================================== *)

(* An occurrence of a primitive is compiled to a direct primitive application
   only when [Translprim.application_kind] answers [Direct]; otherwise
   [Translcore] eta-expands the primitive, and that wrapper is a heap closure
   ([Translprim.transl_primitive] passes [~mode:alloc_heap]). Both phases ask
   the same function, so they cannot disagree about which happens.

   Over-application in tail position with a local result mode is the subtle
   case. [-dlambda] shows the wrapper:

     over_local_tail = (function x : int
       (apply (function {nlocal = 1} prim stub : local prim) succ_ x))

   The [zero_alloc strict] check does not catch this by itself -- Flambda2
   deletes the stub -- so the mode axis is the only thing that rejects it. *)
module Test = struct
  external myid_l : (int -> int) -> local_ (int -> int) = "%identity"
  let[@zero_alloc strict] succ_ (x : int) = x + 1
  let[@zero_alloc strict] (over_local_tail @ noalloc_strict) (x : int) =
    myid_l succ_ x
end
[%%expect{|
Line 5, characters 4-10:
5 |     myid_l succ_ x
        ^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at lines 4-5, characters 61-18,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* The two neighbouring cases really are direct primitive applications --
   [-dlambda] shows a plain [(apply succ_ x)] for both -- so they guard against
   over-correcting into rejecting valid direct applications. Still in tail
   position, but with a [Prim_global] result, [application_kind] answers
   [Direct] without consulting any mode variable... *)
module Test = struct
  external myid_g : (int -> int) -> (int -> int) = "%identity"
  let[@zero_alloc strict] succ_ (x : int) = x + 1
  let[@zero_alloc strict] (over_global_tail @ noalloc_strict) (x : int) =
    myid_g succ_ x
end
[%%expect{|
module Test :
  sig
    external myid_g : (int -> int) -> int -> int = "%identity"
    val succ_ : int -> int @@ portable noalloc_strict [@@zero_alloc strict]
    val over_global_tail : int -> int @@ noalloc_strict [@@zero_alloc strict]
  end
|}]

(* ... and so does the same local result out of tail position. *)
module Test = struct
  external myid_l : (int -> int) -> local_ (int -> int) = "%identity"
  let[@zero_alloc strict] succ_ (x : int) = x + 1
  let[@zero_alloc strict] (over_local_nontail @ noalloc_strict) (x : int) =
    (myid_l succ_ x) + 0
end
[%%expect{|
module Test :
  sig
    external myid_l : (int -> int) -> (int -> int) @ local = "%identity"
    val succ_ : int -> int @@ portable noalloc_strict [@@zero_alloc strict]
    val over_local_nontail : int -> int @@ noalloc_strict
      [@@zero_alloc strict]
  end
|}]

(* ==================================================================== *)
(* Part 8: arity-0 primitives (see CR below)                            *)
(* ==================================================================== *)

(* A primitive of arity 0 is a value, not a function, so [Translcore] never
   eta-expands it -- [transl_primitive] emits it inline when it has no
   parameters. [%loc_LINE] and [%loc_FILE] compile to constants and are
   accepted, which is correct. *)
module Test = struct
  let[@zero_alloc strict] (line @ noalloc_strict) () = __LINE__
  let[@zero_alloc strict] (file @ noalloc_strict) () = __FILE__
end
[%%expect{|
module Test :
  sig
    val line : unit -> int [@@zero_alloc strict]
    val file : unit -> string [@@zero_alloc strict]
  end @@ stateless noalloc_strict
|}]

(* CR shsong: unsound -- [noalloc_strict] accepts a function that the back end
   proves allocates. This is the one direction this test exists to catch: see
   the header, "if a function is accepted at [noalloc_strict] but rejected by
   [zero_alloc strict], the front-end classification is unsound".

   [Sys.argv] is [external argv : string array = "%sys_argv"], arity 0, and it
   compiles to a C call declared [~alloc:true]. Nothing catches it. The
   identifier's own value mode does not, the way it does for a bare reference
   to a function-typed primitive: [string array] is not a function type, so
   the [alloc] mode crosses away. And the bare-reference site in
   [Typecore.type_ident] registers nothing for a [Prim_global] result.

   Fix by classifying an arity-0 bare reference through
   [Translprim.fully_applied_may_allocate] with no arguments -- an arity-0
   primitive is a value, never eta-expanded, so a bare reference to it is
   already a full application. That separates [%loc_LINE] and [%loc_FILE],
   which are constants, from [%sys_argv], which is not. Promote both blocks
   below when it lands: the first should become a mode error. *)
module Test = struct
  let (argv @ noalloc_strict) () = Sys.argv
end
[%%expect{|
module Test : sig val argv : unit -> string array end @@ noalloc_strict
|}]

module Test = struct
  let[@zero_alloc strict] argv () = Sys.argv
end
[%%expect{|
Line 2, characters 7-17:
2 |   let[@zero_alloc strict] argv () = Sys.argv
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP39.Test.argv (camlTOP39__argv_106_106_code).
Line 2, characters 36-44:
2 |   let[@zero_alloc strict] argv () = Sys.argv
                                        ^^^^^^^^
Error: called function may allocate (external call to caml_sys_argv)
|}]

(* ==================================================================== *)
(* Part 9: a bare primitive reference costs nothing (known limitation)  *)
(* ==================================================================== *)

(* KNOWN LIMITATION -- this block documents current behaviour, not intended
   behaviour.

   Referencing a primitive without applying it makes [Translcore] eta-expand
   it, and [-dlambda] does show a closure:

     (let (add = (function {nlocal = 2} prim[L] prim[L] stub : localint32
                    (%int32_add[L] prim prim)))
       (ignore (opaque add)))

   That closure's own mode is not the [@local_opt] variable -- it is the
   constant [alloc_heap] that [Translprim.transl_primitive] passes -- so it is
   never stack-allocated. It does not have to be: the eta-expansion captures
   nothing, so it is a *closed* closure and gets lifted to a static symbol.
   Nothing is allocated at run time. The back-end check below agrees, even
   when the closure escapes into a mutable global, which rules out its being
   deleted as dead.

   The front end disagrees: [relax_alloc] only relaxes a primitive's [alloc]
   mode when [~is_applied], so a bare reference keeps it and cannot appear in
   a [noalloc_strict] function. This is a false positive, not a soundness
   problem. Note the rejection comes from the identifier's own value mode, not
   from allocation registration -- registering the [@local_opt] variable at
   this site is load-bearing for a different reason, see the comment in
   [Typecore.type_ident] and [partprim2] in
   [testsuite/tests/typing-local/alloc.ml]. *)

(* The back end proves it: allocation-free even through [Sys.opaque_identity]
   and even when stored into a mutable global. *)
module Test = struct
  let[@zero_alloc strict] opaque_ref () =
    let add = Int32.add in
    let _ : int32 -> int32 -> int32 = Sys.opaque_identity add in
    ()
  let sink : (int32 -> int32 -> int32) ref = ref Int32.add
  let[@zero_alloc strict] escaping_ref () = sink := Int32.add
end
[%%expect{|
module Test :
  sig
    val opaque_ref : unit -> unit @@ portable [@@zero_alloc strict]
    val sink : (int32 -> int32 -> int32) ref
    val escaping_ref : unit -> unit [@@zero_alloc strict]
  end
|}]

(* The front end rejects the same code. Promote when the two agree. *)
module Test = struct
  let (bare_ref_int32 @ noalloc_strict) () =
    let add = Int32.add in
    let _ : int32 -> int32 -> int32 = Sys.opaque_identity add in
    ()
end
[%%expect{|
Line 3, characters 14-23:
3 |     let add = Int32.add in
                  ^^^^^^^^^
Error: The value "Int32.add" is "alloc"
       but is expected to be "noalloc_strict"
         because it is used inside the function at lines 2-5, characters 40-6
         which is expected to be "noalloc_strict".
|}]
