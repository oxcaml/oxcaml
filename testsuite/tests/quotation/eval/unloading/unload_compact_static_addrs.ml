(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Compaction safety for JIT-emitted static data blocks.

   The compactor moves heap-pool blocks and rewrites every heap
   pointer to its new location. JIT static blocks live in the
   JIT-allocated buffer, not in any pool, so they are never moved;
   but [compact_update_value] (runtime/shared_heap.c) treats any
   block whose status equals [caml_global_heap_state.MARKED] as
   having been evacuated and reads [Field(v, 0)] as a forwarding
   pointer. If a JIT static block ever appeared MARKED at
   compaction time the compactor would silently corrupt every heap
   reference to it.

   The runtime flips registered unloadable static blocks to
   NOT_MARKABLE before compaction and restores them to UNMARKED
   after, so [compact_update_value] takes the NOT_MARKABLE
   early-out path. This test holds heap references to several JIT
   static blocks (both directly returned and captured in closure
   env) across multiple [Gc.compact ()] calls and asserts:
     (a) physical identity is preserved (no forwarding corruption);
     (b) value content is intact (no in-place mangling).  *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

(* Closures that directly return a lifted-constant block on every
   call. The constant lives in the unit's [.data] section; the
   closure body's compiled code returns its address via a static
   relocation, no env capture. *)
let direct_str = ref (fun () -> "")
let direct_tup = ref (fun () -> (0, "", 0))
let direct_opt = ref (fun () -> None)
let direct_lst = ref (fun () -> [])

(* Closures that capture a lifted-constant block in their env. The
   closure is heap-allocated; its env field holds the static-block
   pointer. The compactor walks the closure's scannable fields and
   processes the env field via [compact_update_value]. *)
let captured_str = ref (fun () -> "")
let captured_lst = ref (fun () -> [])

let[@inline never] populate () =
  direct_str := Eval.eval <[ fun () -> "direct-static-string" ]>;
  direct_tup := Eval.eval <[ fun () -> (7, "tag", 99) ]>;
  direct_opt := Eval.eval <[ fun () -> Some 42 ]>;
  direct_lst := Eval.eval <[ fun () -> [101; 202; 303] ]>;
  captured_str := Eval.eval <[
    let s = "captured-static-string" in
    fun () -> s
  ]>;
  captured_lst := Eval.eval <[
    let l = [11; 22; 33; 44; 55] in
    fun () -> l
  ]>

let phys_eq a b = Obj.repr a == Obj.repr b

(* Allocate ~16 KiB of short-lived heap data between compactions so
   the compactor finds evacuation candidates. *)
let[@inline never] make_garbage () =
  let _ : int array = Array.init 512 (fun i -> i) in
  let _ : bytes = Bytes.create 1024 in
  let _ : string list = List.init 32 (fun i -> string_of_int i) in
  ()

let[@inline never] hold_and_compact () =
  let s_d = !direct_str () in
  let t_d = !direct_tup () in
  let o_d = !direct_opt () in
  let l_d = !direct_lst () in
  let s_c = !captured_str () in
  let l_c = !captured_lst () in
  let bad = ref 0 in
  for k = 1 to 4 do
    for _ = 1 to 8 do make_garbage () done;
    Gc.compact ();
    let s_d' = !direct_str () in
    let t_d' = !direct_tup () in
    let o_d' = !direct_opt () in
    let l_d' = !direct_lst () in
    let s_c' = !captured_str () in
    let l_c' = !captured_lst () in
    if not (phys_eq s_d' s_d) then
      (incr bad; Printf.printf "FAIL direct_str identity at %d\n" k);
    if not (phys_eq t_d' t_d) then
      (incr bad; Printf.printf "FAIL direct_tup identity at %d\n" k);
    if not (phys_eq o_d' o_d) then
      (incr bad; Printf.printf "FAIL direct_opt identity at %d\n" k);
    if not (phys_eq l_d' l_d) then
      (incr bad; Printf.printf "FAIL direct_lst identity at %d\n" k);
    if not (phys_eq s_c' s_c) then
      (incr bad; Printf.printf "FAIL captured_str identity at %d\n" k);
    if not (phys_eq l_c' l_c) then
      (incr bad; Printf.printf "FAIL captured_lst identity at %d\n" k);
    if s_d' <> "direct-static-string" then
      (incr bad; Printf.printf "FAIL direct_str value at %d\n" k);
    (let (a, b, c) = t_d' in
     if not (a = 7 && b = "tag" && c = 99) then
       (incr bad; Printf.printf "FAIL direct_tup value at %d\n" k));
    (match o_d' with
     | Some 42 -> ()
     | _ ->
       (incr bad; Printf.printf "FAIL direct_opt value at %d\n" k));
    if l_d' <> [101; 202; 303] then
      (incr bad; Printf.printf "FAIL direct_lst value at %d\n" k);
    if s_c' <> "captured-static-string" then
      (incr bad; Printf.printf "FAIL captured_str value at %d\n" k);
    if l_c' <> [11; 22; 33; 44; 55] then
      (incr bad; Printf.printf "FAIL captured_lst value at %d\n" k)
  done;
  if !bad = 0 then
    Printf.printf "identity and value preserved across 4 stress compacts\n"

let () =
  report "start";
  populate ();
  report "after populate";
  hold_and_compact ();
  report "after 4 stress compacts (closures held)";
  direct_str := (fun () -> "");
  direct_tup := (fun () -> (0, "", 0));
  direct_opt := (fun () -> None);
  direct_lst := (fun () -> []);
  captured_str := (fun () -> "");
  captured_lst := (fun () -> []);
  Gc.full_major ();
  Gc.compact ();
  report "after release + full_major + compact"
