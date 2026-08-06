(* TEST
 flags += " -extension mode_alpha";
 flags += " -zero-alloc-check default";
 flags += " -zero-alloc-checker-details-cutoff 20";
 expect.opt;
*)

(* Cross-check the front end's primitive-allocation classification against the
   back end's. The back end is the ground truth. *)

(* ==================================================================== *)
(* Part 1: primitives that map to a non-allocating [Lambda.primitive]   *)
(* ==================================================================== *)

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

module Test = struct
  let (copy @ noalloc_strict) (r : int ref) = ref !r
end
[%%expect{|
Line 2, characters 46-49:
2 |   let (copy @ noalloc_strict) (r : int ref) = ref !r
                                                  ^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 30-52,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* CR shsong: conservatism on mode inference - will be fixed later *)
module Test = struct
  let f = let r = ref 42 in fun () -> r := 24; ()
end
[%%expect{|
module Test : sig val f : unit -> unit end @@ noalloc_strict
|}]

module Test = struct
  let (f @ noalloc_strict) = let r = ref 42 in fun () -> r := 24; ()
end
[%%expect{|
module Test : sig val f : unit -> unit end @@ noalloc_strict
|}]

module Test = struct
  let x : int ref = ref 42
  let f () = x := 24
end
[%%expect{|
module Test : sig val x : int ref @@ stateless val f : unit -> unit end @@
  noalloc_strict
|}]

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

(* [%compare] at [int] is a [Pscalar] compare; at [string] it is a
   [[@@noalloc]] C call. *)
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

(* A constant constructor argument specializes generic equality. *)
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

(* Reading a [float array] boxes, so it is rejected. *)
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

(* At an unknown type [%compare] is [caml_compare], which allocates. *)
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

(* Without the attribute, assumed to allocate. *)
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

(* A trailing positional partial application produces no [Omitted] node, so its
   closure is not registered by [register_prim_allocation]; what rejects it is
   the mode of the primitive identifier itself, which is [alloc]. That covers a
   [Prim_local] result, a bare reference, and a [Prim_global] result. *)
module Test = struct
  external getl : float array -> int -> local_ float = "%array_unsafe_get"
  let (partial_local_result @ noalloc_strict) (a : float array) =
    ignore (getl a)
end
[%%expect{|
Line 4, characters 11-19:
4 |     ignore (getl a)
               ^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

Line 4, characters 12-16:
4 |     ignore (getl a)
                ^^^^
Error: The allocation is "local"
         because it is allocated inside the function at lines 3-4, characters 46-19,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

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

(* [exclave_] moves the result into the caller's region, but the
   partial-application closure is still built on the heap. *)
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

(* The back end agrees. *)
module Test = struct
  external addf :
    (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt])
    = "%addfloat"
  let[@zero_alloc strict] partial_poly (x : float) = exclave_ (addf x)
end
[%%expect{|
Line 5, characters 7-17:
5 |   let[@zero_alloc strict] partial_poly (x : float) = exclave_ (addf x)
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP26.Test.partial_poly (camlTOP26__partial_poly_76_79_code).
Line 5, characters 62-70:
5 |   let[@zero_alloc strict] partial_poly (x : float) = exclave_ (addf x)
                                                                  ^^^^^^^^
Error: allocation of 40 bytes for closure
|}]

(* Not specific to primitives. *)
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

(* [exclave_] on an ordinary block does pass, so the two failures above really
   are about the closure. *)
module Test = struct
  let[@zero_alloc strict] make_pair (x : int) = exclave_ (x, x)
end
[%%expect{|
module Test :
  sig val make_pair : int -> int * int @ local [@@zero_alloc strict] end @@
  stateless noalloc_strict
|}]

(* The mid-list case, which does produce an [Omitted] node. *)
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
       However, the allocation highlighted is expected to be "global"
         because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is the function in a tail call.
|}]

(* ==================================================================== *)
(* Part 6: primitives that really do allocate                           *)
(* ==================================================================== *)

module Test = struct
  let (mk_ref @ noalloc_strict) (x : int) = ref x
end
[%%expect{|
Line 2, characters 44-47:
2 |   let (mk_ref @ noalloc_strict) (x : int) = ref x
                                                ^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 32-49,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Test = struct
  let (add_float @ noalloc_strict) (x : float) = x +. 1.0
end
[%%expect{|
Line 2, characters 51-53:
2 |   let (add_float @ noalloc_strict) (x : float) = x +. 1.0
                                                       ^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 35-57,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

(* The back end confirms the same two. *)
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

(* Over-application in tail position with a local result mode is eta-expanded
   into a heap closure. *)
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

(* The two neighbouring cases really are direct primitive applications, which
   guards against over-correcting: a [Prim_global] result in tail position... *)
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

(* ... and the same local result out of tail position. *)
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
(* Part 8: arity-0 primitives                                           *)
(* ==================================================================== *)

(* An arity-0 primitive is a value, never eta-expanded. Seven of the eight
   compile to constants and are correctly accepted. [%loc_POS] is the
   interesting one: its type is a tuple, but it compiles to a static constant
   block rather than a run-time allocation. *)
module Test = struct
  external fp : bool = "%frame_pointers"
  let[@zero_alloc strict] (line @ noalloc_strict) () = __LINE__
  let[@zero_alloc strict] (file @ noalloc_strict) () = __FILE__
  let[@zero_alloc strict] (loc @ noalloc_strict) () = __LOC__
  let[@zero_alloc strict] (pos @ noalloc_strict) () = __POS__
  let[@zero_alloc strict] (module_ @ noalloc_strict) () = __MODULE__
  let[@zero_alloc strict] (function_ @ noalloc_strict) () = __FUNCTION__
  let[@zero_alloc strict] (frame_pointers @ noalloc_strict) () = fp
end
[%%expect{|
module Test :
  sig
    external fp : bool = "%frame_pointers"
    val line : unit -> int @@ stateless noalloc_strict [@@zero_alloc strict]
    val file : unit -> string @@ stateless noalloc_strict
      [@@zero_alloc strict]
    val loc : unit -> string @@ stateless noalloc_strict
      [@@zero_alloc strict]
    val pos : unit -> string * int * int * int @@ stateless noalloc_strict
      [@@zero_alloc strict]
    val module_ : unit -> string @@ stateless noalloc_strict
      [@@zero_alloc strict]
    val function_ : unit -> string @@ stateless noalloc_strict
      [@@zero_alloc strict]
    val frame_pointers : unit -> bool @@ stateless noalloc_strict
      [@@zero_alloc strict]
  end
|}]

(* [Sys.argv] is [external argv : string array = "%sys_argv"], arity 0.
   It is not a function type, but compiles to a C call declared
   [~alloc:true]. *)
module Test = struct
  let (argv @ noalloc_strict) () = Sys.argv
end
[%%expect{|
Line 2, characters 35-43:
2 |   let (argv @ noalloc_strict) () = Sys.argv
                                       ^^^^^^^^
Error: The allocation is "local"
         because it is allocated inside the function at line 2, characters 30-43,
         which is "noalloc_strict" and thus cannot allocate on the heap.
       However, the allocation highlighted is expected to be "global".
|}]

module Test = struct
  let[@zero_alloc strict] argv () = Sys.argv
end
[%%expect{|
Line 2, characters 7-17:
2 |   let[@zero_alloc strict] argv () = Sys.argv
           ^^^^^^^^^^
Error: Annotation check for zero_alloc strict failed on function TOP39.Test.argv (camlTOP39__argv_114_114_code).
Line 2, characters 36-44:
2 |   let[@zero_alloc strict] argv () = Sys.argv
                                        ^^^^^^^^
Error: called function may allocate (external call to caml_sys_argv)
|}]

(* And the set is closed at those eight: [Typedecl.transl_value_decl] allows
   arity 0 only for a [%]-primitive, so a C external -- which is assumed to
   allocate unless it carries [[@@noalloc]] -- can never have a non-arrow type
   and can never reach this hole. *)
module Test = struct
  external c0 : string array = "caml_sys_argv"
end
[%%expect{|
Line 2, characters 16-28:
2 |   external c0 : string array = "caml_sys_argv"
                    ^^^^^^^^^^^^
Error: External identifiers must be functions
|}]

(* ==================================================================== *)
(* Part 9: a bare primitive reference                                   *)
(* ==================================================================== *)

(* Primitive reference is handled conservatively. *)
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
