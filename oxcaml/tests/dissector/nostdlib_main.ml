(* Main module: No stdlib - tests OCaml<->OCaml cross-partition calls *)
(* Uses exit code to indicate success/failure instead of printing *)

external ( + ) : int -> int -> int = "%addint"

external ( - ) : int -> int -> int = "%subint"

external ( = ) : 'a -> 'a -> bool = "%equal"

external ( <> ) : 'a -> 'a -> bool = "%notequal"

(* Use sys_exit to report result *)
external sys_exit : int -> 'a = "caml_sys_exit"

let check_eq expected actual = if expected <> actual then sys_exit 1

let () =
  (* Test 1: Direct calls to module A *)
  let a1 = Nostdlib_a.increment () in
  check_eq 1 a1;
  let a2 = Nostdlib_a.increment () in
  check_eq 2 a2;
  let magic = Nostdlib_a.get_magic () in
  check_eq 42 magic;
  let elem = Nostdlib_a.array_get Nostdlib_a.arr1 5 in
  check_eq 5 elem;
  (* Test 2: Calls to module B (which calls A) *)
  let b1 = Nostdlib_b.increment () in
  check_eq 1 b1;
  let a_from_b = Nostdlib_b.call_a_increment () in
  check_eq 3 a_from_b;
  let b_elem = Nostdlib_a.array_get Nostdlib_b.arr1 10 in
  check_eq 110 b_elem;
  (* Test 3: Calls to module C (which calls A and B) *)
  let c1 = Nostdlib_c.increment () in
  check_eq 1 c1;
  let a_from_c = Nostdlib_c.call_a_increment () in
  check_eq 4 a_from_c;
  let b_from_c = Nostdlib_c.call_b_increment () in
  check_eq 2 b_from_c;
  let c_elem = Nostdlib_a.array_get Nostdlib_c.arr1 15 in
  check_eq 215 c_elem;
  (* Test 4: Calls to module D (which calls A, B, C) *)
  let d1 = Nostdlib_d.increment () in
  check_eq 1 d1;
  let ia, ib, ic, id = Nostdlib_d.increment_all () in
  check_eq 5 ia;
  check_eq 3 ib;
  check_eq 2 ic;
  check_eq 2 id;
  (* Test 5: call_all functions - just verify they return something *)
  let a_all = Nostdlib_a.call_all 1 in
  let _ = a_all in
  let b_all = Nostdlib_b.call_all 1 in
  let _ = b_all in
  let c_all = Nostdlib_c.call_all 1 in
  let _ = c_all in
  let d_all = Nostdlib_d.call_all 1 in
  let _ = d_all in
  (* Success - exit with 0 *)
  sys_exit 0
