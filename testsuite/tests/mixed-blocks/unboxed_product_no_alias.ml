(* TEST
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Test that reading or writing an unboxed product from/to a mutable field does
   not alias the original. This is a regression test for a bytecode bug where
   unboxed products (represented as boxed blocks in bytecode) were not
   deep-copied when read or written, causing mutations via set_idx to be visible
   through previously-read values or to affect the source of a write. *)

external set_idx : 'a -> ('a, 'b) idx_mut -> 'b -> unit = "%unsafe_set_idx"

(* Simple case: unboxed record with two fields *)
type r = #{ i : int; j : unit }
type t = { mutable r : r }

let test_simple () =
  let t = { r = #{ i = 1; j = () } } in
  let r = t.r in
  set_idx t (.r.#i) 2;
  (* r should still have i = 1, not 2 *)
  assert (r.#i = 1);
  (* t.r should have i = 2 *)
  assert (t.r.#i = 2)

(* Simple case with multiple fields in outer record *)
type t_multi = { prefix : unit; mutable r : r }

let test_simple_multi () =
  let t = { prefix = (); r = #{ i = 1; j = () } } in
  let r = t.r in
  set_idx t (.r.#i) 2;
  assert (r.#i = 1);
  assert (t.r.#i = 2)

(* Nested case: unboxed record containing another unboxed record *)
type inner = #{ a : int; b : int }
type outer = #{ inner : inner; c : int }
type t_nested = { mutable outer : outer }

let test_nested () =
  let t = { outer = #{ inner = #{ a = 10; b = 20 }; c = 30 } } in
  let outer = t.outer in
  set_idx t (.outer.#inner.#a) 100;
  (* outer should still have inner.a = 10 *)
  assert (outer.#inner.#a = 10);
  (* t.outer should have inner.a = 100 *)
  assert (t.outer.#inner.#a = 100)

(* Nested case with multiple fields in outer record *)
type t_nested_multi = { prefix : unit; mutable outer : outer }

let test_nested_multi () =
  let t = { prefix = (); outer = #{ inner = #{ a = 10; b = 20 }; c = 30 } } in
  let outer = t.outer in
  set_idx t (.outer.#inner.#a) 100;
  assert (outer.#inner.#a = 10);
  assert (t.outer.#inner.#a = 100)

(* Deeply nested case *)
type level3 = #{ x : int; y : int }
type level2 = #{ l3 : level3; z : int }
type level1 = #{ l2 : level2; w : int }
type t_deep = { mutable l1 : level1 }

let test_deep () =
  let t = { l1 = #{ l2 = #{ l3 = #{ x = 1; y = 2 }; z = 3 }; w = 4 } } in
  let l1 = t.l1 in
  set_idx t (.l1.#l2.#l3.#x) 999;
  (* l1 should still have l2.l3.x = 1 *)
  assert (l1.#l2.#l3.#x = 1);
  (* t.l1 should have l2.l3.x = 999 *)
  assert (t.l1.#l2.#l3.#x = 999)

(* Deeply nested case with multiple fields in outer record *)
type t_deep_multi = { prefix : unit; mutable l1 : level1 }

let test_deep_multi () =
  let t =
    { prefix = (); l1 = #{ l2 = #{ l3 = #{ x = 1; y = 2 }; z = 3 }; w = 4 } }
  in
  let l1 = t.l1 in
  set_idx t (.l1.#l2.#l3.#x) 999;
  assert (l1.#l2.#l3.#x = 1);
  assert (t.l1.#l2.#l3.#x = 999)

(* Mixed block with unboxed product containing non-value types *)
type mixed_inner = #{ f : float#; i : int }
type mixed_outer = #{ inner : mixed_inner; s : string }
type t_mixed = { mutable mixed : mixed_outer }

let test_mixed () =
  let t = { mixed = #{ inner = #{ f = #3.14; i = 42 }; s = "hello" } } in
  let mixed = t.mixed in
  set_idx t (.mixed.#inner.#i) 100;
  (* mixed should still have inner.i = 42 *)
  assert (mixed.#inner.#i = 42);
  (* t.mixed should have inner.i = 100 *)
  assert (t.mixed.#inner.#i = 100)

(* Mixed block with multiple fields in outer record *)
type t_mixed_multi = { prefix : unit; mutable mixed : mixed_outer }

let test_mixed_multi () =
  let t =
    { prefix = (); mixed = #{ inner = #{ f = #3.14; i = 42 }; s = "hello" } }
  in
  let mixed = t.mixed in
  set_idx t (.mixed.#inner.#i) 100;
  assert (mixed.#inner.#i = 42);
  assert (t.mixed.#inner.#i = 100)

(* ===== Tests for set aliasing ===== *)
(* Test that writing an unboxed product to a mutable field does not alias
   the source value. *)

let test_set_simple () =
  let r = #{ i = 1; j = () } in
  let t = { r = #{ i = 0; j = () } } in
  t.r <- r;
  set_idx t (.r.#i) 2;
  (* r should still have i = 1, not 2 *)
  assert (r.#i = 1);
  (* t.r should have i = 2 *)
  assert (t.r.#i = 2)

let test_set_simple_multi () =
  let r = #{ i = 1; j = () } in
  let t = { prefix = (); r = #{ i = 0; j = () } } in
  t.r <- r;
  set_idx t (.r.#i) 2;
  assert (r.#i = 1);
  assert (t.r.#i = 2)

let test_set_nested () =
  let outer = #{ inner = #{ a = 10; b = 20 }; c = 30 } in
  let t = { outer = #{ inner = #{ a = 0; b = 0 }; c = 0 } } in
  t.outer <- outer;
  set_idx t (.outer.#inner.#a) 100;
  (* outer should still have inner.a = 10 *)
  assert (outer.#inner.#a = 10);
  (* t.outer should have inner.a = 100 *)
  assert (t.outer.#inner.#a = 100)

let test_set_nested_multi () =
  let outer = #{ inner = #{ a = 10; b = 20 }; c = 30 } in
  let t = { prefix = (); outer = #{ inner = #{ a = 0; b = 0 }; c = 0 } } in
  t.outer <- outer;
  set_idx t (.outer.#inner.#a) 100;
  assert (outer.#inner.#a = 10);
  assert (t.outer.#inner.#a = 100)

let test_set_deep () =
  let l1 = #{ l2 = #{ l3 = #{ x = 1; y = 2 }; z = 3 }; w = 4 } in
  let t = { l1 = #{ l2 = #{ l3 = #{ x = 0; y = 0 }; z = 0 }; w = 0 } } in
  t.l1 <- l1;
  set_idx t (.l1.#l2.#l3.#x) 999;
  (* l1 should still have l2.l3.x = 1 *)
  assert (l1.#l2.#l3.#x = 1);
  (* t.l1 should have l2.l3.x = 999 *)
  assert (t.l1.#l2.#l3.#x = 999)

let test_set_deep_multi () =
  let l1 = #{ l2 = #{ l3 = #{ x = 1; y = 2 }; z = 3 }; w = 4 } in
  let t =
    { prefix = (); l1 = #{ l2 = #{ l3 = #{ x = 0; y = 0 }; z = 0 }; w = 0 } }
  in
  t.l1 <- l1;
  set_idx t (.l1.#l2.#l3.#x) 999;
  assert (l1.#l2.#l3.#x = 1);
  assert (t.l1.#l2.#l3.#x = 999)

let test_set_mixed () =
  let mixed = #{ inner = #{ f = #3.14; i = 42 }; s = "hello" } in
  let t = { mixed = #{ inner = #{ f = #0.0; i = 0 }; s = "" } } in
  t.mixed <- mixed;
  set_idx t (.mixed.#inner.#i) 100;
  (* mixed should still have inner.i = 42 *)
  assert (mixed.#inner.#i = 42);
  (* t.mixed should have inner.i = 100 *)
  assert (t.mixed.#inner.#i = 100)

let test_set_mixed_multi () =
  let mixed = #{ inner = #{ f = #3.14; i = 42 }; s = "hello" } in
  let t = { prefix = (); mixed = #{ inner = #{ f = #0.0; i = 0 }; s = "" } } in
  t.mixed <- mixed;
  set_idx t (.mixed.#inner.#i) 100;
  assert (mixed.#inner.#i = 42);
  assert (t.mixed.#inner.#i = 100)

let () =
  (* Read aliasing tests *)
  test_simple ();
  test_simple_multi ();
  test_nested ();
  test_nested_multi ();
  test_deep ();
  test_deep_multi ();
  test_mixed ();
  test_mixed_multi ();
  (* Set aliasing tests *)
  test_set_simple ();
  test_set_simple_multi ();
  test_set_nested ();
  test_set_nested_multi ();
  test_set_deep ();
  test_set_deep_multi ();
  test_set_mixed ();
  test_set_mixed_multi ();
  print_endline "All tests passed"
