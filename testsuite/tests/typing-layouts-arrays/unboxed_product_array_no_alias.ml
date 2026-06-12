(* TEST
 include stdlib_stable;
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Regression test for a bytecode bug where unboxed products stored in arrays
   (represented as boxed blocks in bytecode) were not deep-copied at array
   reads, writes, allocations, and duplications, causing mutations via
   [Idx_mut.set] to be visible through previously-read values or to affect
   the source of a write/blit. *)

open Stdlib_stable

external[@layout_poly] array_get
  : ('a : any mod separable). 'a array -> int -> 'a
  = "%array_safe_get"

external[@layout_poly] array_set
  : ('a : any mod separable). 'a array -> int -> 'a -> unit
  = "%array_safe_set"

external[@layout_poly] array_unsafe_get
  : ('a : any mod separable). 'a array -> int -> 'a
  = "%array_unsafe_get"

external[@layout_poly] array_unsafe_set
  : ('a : any mod separable). 'a array -> int -> 'a -> unit
  = "%array_unsafe_set"

external[@layout_poly] makearray_dynamic
  : ('a : any mod separable). int -> 'a -> 'a array
  = "%makearray_dynamic"

external[@layout_poly] array_blit
  : ('a : any mod separable).
    'a array -> int -> 'a array -> int -> int -> unit
  = "%arrayblit"

(* ===== Scannable product (all-value fields) ===== *)

type u = #{ x : int; y : int }

let test_makearray () =
  let u = #{ x = 1; y = 1 } in
  let a = [| u |] in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert (u.#y = 1);
  assert ((array_get a 0).#y = 10)

let test_makearray_multi () =
  let u = #{ x = 1; y = 1 } in
  let a = [| u; u |] in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert (u.#y = 1);
  assert ((array_get a 0).#y = 10);
  assert ((array_get a 1).#y = 1)

let test_arrayset () =
  let u = #{ x = 1; y = 1 } in
  let a = [| #{ x = 0; y = 0 } |] in
  array_set a 0 u;
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert (u.#y = 1);
  assert ((array_get a 0).#y = 10)

let test_arrayget () =
  let a = [| #{ x = 1; y = 1 } |] in
  let u = array_get a 0 in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert (u.#y = 1);
  assert ((array_get a 0).#y = 10)

(* A whole-element [idx_mut] into a product array reads/writes the entire
   product block.  Without deep-copying through [Pget_idx]/[Pset_idx] the
   returned/stored value would alias the array slot. *)

let test_idx_get_whole_element () =
  let a = [| #{ x = 1; y = 1 } |] in
  let whole : (u array, u) idx_mut = Idx_mut.unsafe_create_into_array 0 in
  let u = Idx_mut.get a whole in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert (u.#y = 1);
  assert ((array_get a 0).#y = 10)

let test_idx_set_whole_element () =
  let u = #{ x = 1; y = 1 } in
  let a = [| #{ x = 0; y = 0 } |] in
  let whole : (u array, u) idx_mut = Idx_mut.unsafe_create_into_array 0 in
  Idx_mut.set a whole u;
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert (u.#y = 1);
  assert ((array_get a 0).#y = 10)

let test_unsafe_arrayset () =
  let u = #{ x = 1; y = 1 } in
  let a = [| #{ x = 0; y = 0 } |] in
  array_unsafe_set a 0 u;
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert (u.#y = 1);
  assert ((array_get a 0).#y = 10)

let test_unsafe_arrayget () =
  let a = [| #{ x = 1; y = 1 } |] in
  let u = array_unsafe_get a 0 in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert (u.#y = 1);
  assert ((array_get a 0).#y = 10)

let test_makearray_dynamic () =
  let u = #{ x = 1; y = 1 } in
  let a = makearray_dynamic 3 u in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert (u.#y = 1);
  assert ((array_get a 0).#y = 10);
  assert ((array_get a 1).#y = 1);
  assert ((array_get a 2).#y = 1)

let test_arrayblit () =
  let src = [| #{ x = 1; y = 1 }; #{ x = 2; y = 2 } |] in
  let dst = [| #{ x = 0; y = 0 }; #{ x = 0; y = 0 } |] in
  array_blit src 0 dst 0 2;
  Idx_mut.set dst
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#y)
      : (u array, _) idx_mut)
    10;
  assert ((array_get src 0).#y = 1);
  assert ((array_get dst 0).#y = 10);
  assert ((array_get dst 1).#y = 2)

(* ===== Nested scannable product ===== *)

type inner = #{ a : int; b : int }
type outer = #{ inner : inner; c : int }

let test_nested_makearray () =
  let o = #{ inner = #{ a = 1; b = 2 }; c = 3 } in
  let a = [| o |] in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#inner.#a)
      : (outer array, _) idx_mut)
    99;
  assert (o.#inner.#a = 1);
  assert ((array_get a 0).#inner.#a = 99)

let test_nested_arrayget () =
  let a = [| #{ inner = #{ a = 1; b = 2 }; c = 3 } |] in
  let o = array_get a 0 in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#inner.#a)
      : (outer array, _) idx_mut)
    99;
  assert (o.#inner.#a = 1);
  assert ((array_get a 0).#inner.#a = 99)

(* ===== Ignorable product (contains non-value fields) ===== *)

type ig = #{ i : int; f : float# }

let test_ignorable_makearray () =
  let v = #{ i = 1; f = #1.0 } in
  let a = [| v |] in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#i)
      : (ig array, _) idx_mut)
    10;
  assert (v.#i = 1);
  assert ((array_get a 0).#i = 10)

let test_ignorable_arrayset () =
  let v = #{ i = 1; f = #1.0 } in
  let a = [| #{ i = 0; f = #0.0 } |] in
  array_set a 0 v;
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#i)
      : (ig array, _) idx_mut)
    10;
  assert (v.#i = 1);
  assert ((array_get a 0).#i = 10)

let test_ignorable_arrayget () =
  let a = [| #{ i = 1; f = #1.0 } |] in
  let v = array_get a 0 in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#i)
      : (ig array, _) idx_mut)
    10;
  assert (v.#i = 1);
  assert ((array_get a 0).#i = 10)

let test_ignorable_idx_get_whole_element () =
  let a = [| #{ i = 1; f = #1.0 } |] in
  let whole : (ig array, ig) idx_mut = Idx_mut.unsafe_create_into_array 0 in
  let v = Idx_mut.get a whole in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#i)
      : (ig array, _) idx_mut)
    10;
  assert (v.#i = 1);
  assert ((array_get a 0).#i = 10)

let test_ignorable_idx_set_whole_element () =
  let v = #{ i = 1; f = #1.0 } in
  let a = [| #{ i = 0; f = #0.0 } |] in
  let whole : (ig array, ig) idx_mut = Idx_mut.unsafe_create_into_array 0 in
  Idx_mut.set a whole v;
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#i)
      : (ig array, _) idx_mut)
    10;
  assert (v.#i = 1);
  assert ((array_get a 0).#i = 10)

let test_ignorable_makearray_dynamic () =
  let v = #{ i = 1; f = #1.0 } in
  let a = makearray_dynamic 3 v in
  Idx_mut.set a
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#i)
      : (ig array, _) idx_mut)
    10;
  assert (v.#i = 1);
  assert ((array_get a 0).#i = 10);
  assert ((array_get a 1).#i = 1);
  assert ((array_get a 2).#i = 1)

let test_ignorable_arrayblit () =
  let src = [| #{ i = 1; f = #1.0 }; #{ i = 2; f = #2.0 } |] in
  let dst = [| #{ i = 0; f = #0.0 }; #{ i = 0; f = #0.0 } |] in
  array_blit src 0 dst 0 2;
  Idx_mut.set dst
    ((.idx_mut(Idx_mut.unsafe_create_into_array 0).#i)
      : (ig array, _) idx_mut)
    10;
  assert ((array_get src 0).#i = 1);
  assert ((array_get dst 0).#i = 10);
  assert ((array_get dst 1).#i = 2)

(* Regression tests: the evaluation order of arguments for [makearray_dynamic]
   and [array_blit] is consistent unboxed products and non-products. *)

let collect_eval_order f =
  let log = ref [] in
  let record s () = log := s :: !log in
  f record;
  List.rev !log

let test_eval_order_makearray_dynamic () =
  let order_product =
    collect_eval_order (fun record ->
        let _ : u array =
          makearray_dynamic
            (record "n" (); 1)
            (record "init" (); #{ x = 0; y = 0 })
        in
        ())
  in
  let order_int =
    collect_eval_order (fun record ->
        let _ : int array =
          makearray_dynamic (record "n" (); 1) (record "init" (); 0)
        in
        ())
  in
  assert (order_product = order_int)

let test_eval_order_arrayblit () =
  let order_product =
    collect_eval_order (fun record ->
        let src = [| #{ x = 1; y = 1 } |] in
        let dst = [| #{ x = 0; y = 0 } |] in
        array_blit
          (record "src" (); src)
          (record "srcofs" (); 0)
          (record "dst" (); dst)
          (record "dstofs" (); 0)
          (record "len" (); 1))
  in
  let order_int =
    collect_eval_order (fun record ->
        let src = [| 1 |] in
        let dst = [| 0 |] in
        array_blit
          (record "src" (); src)
          (record "srcofs" (); 0)
          (record "dst" (); dst)
          (record "dstofs" (); 0)
          (record "len" (); 1))
  in
  assert (order_product = order_int)

let () =
  test_makearray ();
  test_makearray_multi ();
  test_arrayset ();
  test_arrayget ();
  test_idx_get_whole_element ();
  test_idx_set_whole_element ();
  test_unsafe_arrayset ();
  test_unsafe_arrayget ();
  test_makearray_dynamic ();
  test_arrayblit ();
  test_nested_makearray ();
  test_nested_arrayget ();
  test_ignorable_makearray ();
  test_ignorable_arrayset ();
  test_ignorable_arrayget ();
  test_ignorable_idx_get_whole_element ();
  test_ignorable_idx_set_whole_element ();
  test_ignorable_makearray_dynamic ();
  test_ignorable_arrayblit ();
  test_eval_order_makearray_dynamic ();
  test_eval_order_arrayblit ();
  print_endline "All tests passed"
