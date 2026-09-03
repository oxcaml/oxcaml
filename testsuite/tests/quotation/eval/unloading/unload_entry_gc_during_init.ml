(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Sanity test for the design choice that the entry function's
   [Code_block] has zero dependency fields (see
   [to_cmm_code_blocks.ml:emit_entry_code_block]).

   If a major GC fires *during* eval'd module initialisation and walks
   the running entry's stack frame, F.2 darkens the entry's Code_block
   via the back-pointer — but, with zero dep fields, that darken does
   not recursively mark any of the unit's other Code_blocks or static
   data. The reasoning in the design comment is that every same-CU
   function the entry transitively calls is itself on the stack
   (F.2 keeps its Code_block alive), and any other static data the
   entry references is reachable via the gc_roots scan.

   This test stresses that path by:
     - allocating heavily inside the eval'd initialiser to force
       multiple minor and major GCs mid-init
     - recursing deeply inside the initialiser via a same-CU helper
       so the runtime stack has multiple live frames into the unit's
       text when GCs fire
     - constructing a closure whose value-slots reference both
       freshly-allocated heap data and same-CU lifted constants

   If anything is wrong with the zero-dep-entry strategy, expect
   either a crash on the next mutator instruction after GC or wrong
   results when reading the captured value-slots. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n%!"
    label r u (r - u)

let f = ref (fun () -> (0, 0, 0))

let[@inline never] populate () =
  f := Eval.eval <[
    (* Same-CU helper, recursive: every level of recursion puts a new
       frame into this unit's text. *)
    let rec build_list n acc =
      if n <= 0 then acc
      else
        (* Each iteration allocates a fresh pair on the heap. Many
           iterations push the minor heap over its threshold and
           trigger several major slices mid-initialisation. *)
        build_list (n - 1) ((n, n * n) :: acc)
    in
    (* 10_000 cons cells + 10_000 pairs is more than enough to force
       major GC during the initialiser. The recursion stays inside
       build_list so build_list's frame, and the entry's frame above
       it, both have return addresses into this unit's text at every
       GC point. *)
    let xs = build_list 10_000 [] in
    let sum_of_squares =
      List.fold_left (fun acc (_, sq) -> acc + sq) 0 xs
    in
    let len = List.length xs in
    (* Capture both the list and a derived scalar in the returned
       closure's value-slots. If the list's cons cells were freed
       during init by an under-mark, traversing it below would
       segfault. *)
    fun () ->
      let s = List.fold_left (fun a (n, _) -> a + n) 0 xs in
      (s, sum_of_squares, len)
  ]>

let () =
  report "start";
  populate ();
  let (s, sq, len) = !f () in
  (* Expected: sum 1..10000 = 50_005_000, sum_of_squares 1..10000 =
     333_383_335_000, len = 10000. *)
  Printf.printf "sum=%d sum_of_squares=%d len=%d\n" s sq len;
  assert (s = 50_005_000);
  assert (sq = 333_383_335_000);
  assert (len = 10_000);
  report "after populate";

  (* Force several full GCs while the closure is held. Captured data
     must still produce the same result on every call. *)
  for _ = 1 to 3 do Gc.compact () done;
  let (s', sq', len') = !f () in
  assert (s = s' && sq = sq' && len = len');
  report "after 3x compact (held)";

  (* Drop and unload. *)
  f := (fun () -> (0, 0, 0));
  Gc.compact ();
  report "after release + compact"
