(* TEST *)

(* Block indices: reads, writes and index deepening over the block shapes the
   runtime has to walk (plain blocks, nested unboxed records).  These go
   through [caml_get_idx_bytecode], [caml_set_idx_bytecode],
   [caml_deepen_idx_bytecode] and [caml_idx_array_advance_bytecode] in the
   JavaScript and Wasm runtimes. *)

open Stdlib_upstream_compatible

(* The accessors are declared here rather than taken from [Stdlib_stable]
   because OxCaml 5.4 renamed them: the layout-polymorphic primitives went
   from [%unsafe_get_idx]/[%unsafe_set_idx] ([Idx_mut.unsafe_get]/
   [unsafe_set]) to [%get_idx]/[%set_idx] ([Idx_mut.get]/[set]). *)

external idx_get : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b
  = "%unsafe_get_idx"
[@@layout_poly] [@@if ocaml_version < (5, 4, 0)]

external idx_get : ('a : value_or_null) ('b : any). 'a -> ('a, 'b) idx_mut -> 'b
  = "%get_idx"
[@@layout_poly] [@@if ocaml_version >= (5, 4, 0)]

external idx_set : 'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit
  = "%unsafe_set_idx"
[@@layout_poly] [@@if ocaml_version < (5, 4, 0)]

external idx_set
  : ('a : value_or_null) ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit
  = "%set_idx"
[@@layout_poly] [@@if ocaml_version >= (5, 4, 0)]

external idx_imm_get : 'a ('b : any). 'a -> ('a, 'b) idx_imm -> 'b
  = "%unsafe_get_idx_imm"
[@@layout_poly] [@@if ocaml_version < (5, 4, 0)]

external idx_imm_get
  : ('a : value_or_null) ('b : any). 'a -> ('a, 'b) idx_imm -> 'b
  = "%get_idx_imm"
[@@layout_poly] [@@if ocaml_version >= (5, 4, 0)]

external unsafe_array_idx : ('a : any mod non_float). int -> ('a array, 'a) idx_mut
  = "%unsafe_array_idx"
[@@layout_poly]

external idx_advance
  : ('a : value_or_null) ('b : any mod separable).
  ('a, 'b) idx_mut -> int -> ('a, 'b) idx_mut
  = "%unsafe_idx_array_advance"
[@@layout_poly]

external idx_advance_by_int32
  : ('a : value_or_null) ('b : any mod separable).
  ('a, 'b) idx_mut -> int32_u -> ('a, 'b) idx_mut
  = "%unsafe_idx_array_advance_indexed_by_int32#"
[@@layout_poly]

external idx_advance_by_int64
  : ('a : value_or_null) ('b : any mod separable).
  ('a, 'b) idx_mut -> int64_u -> ('a, 'b) idx_mut
  = "%unsafe_idx_array_advance_indexed_by_int64#"
[@@layout_poly]

type boxed_record =
  { s : string
  ; mutable f : float
  }

let test_boxed_record () =
  let r = { s = "foo"; f = 1.0 } in
  assert (idx_imm_get r (.s) = "foo");
  assert (idx_get r (.f) = 1.0);
  idx_set r (.f) 2.0;
  assert (r.f = 2.0);
  assert (idx_get r (.f) = 2.0)

type mixed_record =
  { i : int
  ; mutable u : float#
  ; str : string
  }

let test_mixed_record () =
  let r = { i = -100; u = #1.0; str = "foo" } in
  assert (Float_u.to_float (idx_get r (.u)) = 1.0);
  idx_set r (.u) #2.0;
  assert (Float_u.to_float r.u = 2.0);
  assert (r.i = -100);
  assert (r.str = "foo")

(* Nested unboxed record: the index has depth 2, so the runtime must follow
   two field positions. *)
type nested_record =
  { g : float#
  ; mutable inner : boxed_record#
  }

let test_nested_record () =
  let r = { g = -#100.0; inner = #{ s = "foo"; f = 1.0 } } in
  assert (idx_get r (.inner.#f) = 1.0);
  assert (idx_get r (.inner.#s) = "foo");
  idx_set r (.inner.#f) 2.0;
  assert (r.inner.#f = 2.0);
  assert (r.inner.#s = "foo")

type pt =
  { x : int
  ; y : int
  }

type line =
  { mutable p : pt#
  ; mutable q : pt#
  }

let test_deepening () =
  let l = { p = #{ x = 1; y = 2 }; q = #{ x = 3; y = 4 } } in
  let q : (line, pt#) idx_mut = (.q) in
  (* Deepening an existing index goes through [caml_deepen_idx_bytecode]. *)
  let qy : (line, int) idx_mut = (.idx_mut(q).#y) in
  assert (idx_get l qy = 4);
  idx_set l qy 40;
  assert (l.q.#y = 40);
  assert (l.q.#x = 3);
  assert (l.p.#y = 2)

(* Deepening twice: the runtime concatenates three positions. *)
type inner =
  { a : pt#
  ; b : int
  }

type outer = { mutable o : inner# }

let test_deepening_twice () =
  let v = { o = #{ a = #{ x = 1; y = 2 }; b = 3 } } in
  let o : (outer, inner#) idx_mut = (.o) in
  let oa : (outer, pt#) idx_mut = (.idx_mut(o).#a) in
  let oax : (outer, int) idx_mut = (.idx_mut(oa).#x) in
  assert (idx_get v oax = 1);
  idx_set v oax 10;
  assert (v.o.#a.#x = 10);
  assert (v.o.#a.#y = 2);
  assert (v.o.#b = 3)

(* A record whose only field is an unboxed product stores the product's fields
   directly in its block, so an index to the whole product has an empty path:
   writing through it must overwrite every field of the block. *)
type single = { mutable sp : pt# }

let test_singleton_product () =
  let r = { sp = #{ x = 1; y = 2 } } in
  let sp : (single, pt#) idx_mut = (.sp) in
  let #{ x; y } = idx_get r sp in
  assert (x = 1 && y = 2);
  idx_set r sp #{ x = 3; y = 4 };
  assert (r.sp.#x = 3 && r.sp.#y = 4);
  idx_set r (.idx_mut(sp).#y) 40;
  assert (r.sp.#x = 3 && r.sp.#y = 40)

let test_advance () =
  let arr = [| 0; 1; 2; 3; 4 |] in
  let idx0 : (int array, int) idx_mut = unsafe_array_idx 0 in
  let idx3 = idx_advance idx0 3 in
  assert (idx_get arr idx3 = 3);
  assert (idx_get arr (idx_advance idx3 (-2)) = 1);
  assert (idx_get arr (idx_advance_by_int32 idx0 #4l) = 4);
  assert (idx_get arr (idx_advance_by_int64 idx3 #1L) = 4);
  idx_set arr (idx_advance idx0 2) 20;
  assert (arr.(2) = 20 && arr.(1) = 1 && arr.(3) = 3);
  let floats : float# array = [| #0.5; #1.5; #2.5 |] in
  let fidx : (float# array, float#) idx_mut = unsafe_array_idx 0 in
  assert (Float_u.to_float (idx_get floats (idx_advance fidx 2)) = 2.5);
  let pairs : #(int * string) array = [| #(0, "a"); #(1, "b"); #(2, "c") |] in
  let pidx : (#(int * string) array, #(int * string)) idx_mut = unsafe_array_idx 0 in
  let #(i, s) = idx_get pairs (idx_advance pidx 2) in
  assert (i = 2 && s = "c")

let () =
  test_boxed_record ();
  test_mixed_record ();
  test_nested_record ();
  test_deepening ();
  test_deepening_twice ();
  test_singleton_product ();
  test_advance ();
  print_endline "OK"
