(* TEST
 flags += " -O3 -use-ssa -ssa-delete-empty-loops";
 only-default-codegen;
 no-tsan;
 expect.opt;
*)

(* Exercises empty-loop deletion. A terminating loop that does no observable
   work may be removed, but a loop whose body has *any* side effect -- even one
   that does not flow through a header parameter -- must be preserved, as must a
   loop whose termination cannot be established. See delete_empty_loops_exits.ml
   for loops with a second way out and delete_empty_loops_shapes.ml for other
   shapes and the analysis's known limitations.

   Each function is followed by the SSA that leaves the SSA pipeline
   ([%%expect_ssa]), headed by the number of loops Delete_empty_loops removed
   from it. In a deleted loop the header's terminator has become a jump to the
   exit block and the body has been pruned, so no block has a back edge to the
   header; in a kept loop the header still has a predecessor from its body. The
   comment on each function says which to expect, and the [%%expect] blocks
   check what the functions compute. The unit test oxcaml/tests/backend/ssa/
   loop_analysis.ml asserts exact deletion counts on hand-built graphs; this
   checks that the graphs the compiler builds for OCaml loops are among the
   shapes it recognises. The SSA shown is that of the x86-64 backend, so the
   [%%expect_ssa] blocks are tagged X86_64 and checked on that architecture
   only. *)

(* Genuinely empty: spins a dead counter, no observable effect. Deleted. *)
let[@inline never] spin (n : int) =
  for _i = 0 to Sys.opaque_identity n - 1 do
    ()
  done
[%%expect_ssa X86_64{|
spin:
Delete_empty_loops: 1 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  Popaque/v0 = opaque n/B0.0
  for_stop/v1 = Popaque/v0 + -2
  v2 = for_stop/v1 >=s 1
  switch(v2) | 0 -> B5 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  goto B2(omitted_since_unused)

B2(for_counter_naked/B2.0 : int) [dominator=B1 depth=2] <- B1
  goto B4()

B4() [dominator=B2 depth=3] <- B2
  v7 = 1
  return(v7)

B5() [dominator=B0 depth=1] <- B0
  v8 = 1
  return(v8)
|}]

let () = spin 1000
[%%expect {|
|}]

(* Both bounds variable: the termination proof must discharge the no-overflow
   obligation from the untagging shape of the bound (and the [a <= b] entry
   guard is among the facts). Deleted. *)
let[@inline never] spin_range (a : int) (b : int) =
  for _i = Sys.opaque_identity a to Sys.opaque_identity b do
    ()
  done
[%%expect_ssa X86_64{|
spin_range:
Delete_empty_loops: 1 deleted
B0(a/B0.0 : int, b/B0.1 : int) [dominator=B0 depth=0]
  for_start/v0 = opaque a/B0.0
  for_stop/v1 = opaque b/B0.1
  v2 = for_start/v0 <=s for_stop/v1
  switch(v2) | 0 -> B5 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  goto B2(omitted_since_unused)

B2(for_counter_naked/B2.0 : int) [dominator=B1 depth=2] <- B1
  goto B4()

B4() [dominator=B2 depth=3] <- B2
  v7 = 1
  return(v7)

B5() [dominator=B0 depth=1] <- B0
  v8 = 1
  return(v8)
|}]

let () = spin_range 1 1000
[%%expect {|
|}]

(* Counting down with a variable bound. Kept. Known limitation: the
   termination check needs the bound register to be at least [- max_int], but
   the untagging shift bounds it below only by [min_int], and that fact cannot
   even be written with the analysis's [int] coefficients (see
   [LoopTermination.bound_in_range]). The symmetric [to] loop above is deleted
   because the upper bound of the shift, [max_int], is representable. *)
let[@inline never] spin_downto (a : int) (b : int) =
  for _i = Sys.opaque_identity a downto Sys.opaque_identity b do
    ()
  done
[%%expect_ssa X86_64{|
spin_downto:
Delete_empty_loops: 0 deleted
B0(a/B0.0 : int, b/B0.1 : int) [dominator=B0 depth=0]
  for_start/v0 = opaque a/B0.0
  for_stop/v1 = opaque b/B0.1
  v2 = for_start/v0 >=s for_stop/v1
  switch(v2) | 0 -> B5 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  for_start_naked/v3 = for_start/v0 >>s 1
  for_stop_naked/v4 = for_stop/v1 >>s 1
  goto B2(for_start_naked/v3)

B2(for_counter_naked/B2.0 : int) [dominator=B1 depth=2] <- B3, B1
  for_next_naked/v5 = for_counter_naked/B2.0 + -1
  v6 = for_next_naked/v5 >=s for_stop_naked/v4
  switch(v6) | 0 -> B4 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  goto B2(for_next_naked/v5)

B4() [dominator=B2 depth=3] <- B2
  v7 = 1
  return(v7)

B5() [dominator=B0 depth=1] <- B0
  v8 = 1
  return(v8)
|}]

let () = spin_downto 1000 1
[%%expect {|
|}]

(* Side effect (print) not flowing through a header parameter: must NOT be
   deleted, so that all [n] characters are printed. *)
let[@inline never] noisy (n : int) =
  for _i = 0 to n - 1 do
    print_char 'x'
  done
[%%expect_ssa X86_64{|
noisy:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  for_stop/v0 = n/B0.0 + -2
  v1 = for_stop/v0 >=s 1
  switch(v1) | 0 -> B6 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  for_stop_naked/v2 = for_stop/v0 >>s 1
  v3 = 0
  goto B2(v3)

B2(for_counter_naked/B2.0 : int) [dominator=B1 depth=2] <- B4, B1
  v4 = 241
  v5 = "camlStdlib__print_char_42_93"
  v6 = val[v5 + 16]
  call_prim caml_ml_output_char(v6, v4) may_raise -> B3

B3(param/B3.0 : val) [dominator=B2 depth=3] <- B2
  for_next_naked/v7 = for_counter_naked/B2.0 + 1
  v8 = for_next_naked/v7 <=s for_stop_naked/v2
  switch(v8) | 0 -> B5 | 1 -> B4

B4() [dominator=B3 depth=4] <- B3
  goto B2(for_next_naked/v7)

B5() [dominator=B3 depth=4] <- B3
  v9 = 1
  return(v9)

B6() [dominator=B0 depth=1] <- B0
  v10 = 1
  return(v10)
|}]

(* Side effect (store) not flowing through a header parameter: must NOT be
   deleted, so that the store happens [n] times. *)
let[@inline never] store_loop (a : int array) (n : int) =
  for _i = 0 to n - 1 do
    a.(0) <- a.(0) + 1
  done
[%%expect_ssa X86_64{|
store_loop:
Delete_empty_loops: 0 deleted
B0(a/B0.0 : val, n/B0.1 : int) [dominator=B0 depth=0]
  for_stop/v0 = n/B0.1 + -2
  v1 = for_stop/v0 >=s 1
  switch(v1) | 0 -> B7 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  for_stop_naked/v2 = for_stop/v0 >>s 1
  v3 = 0
  goto B2(v3)

B2(for_counter_naked/B2.0 : int) [dominator=B1 depth=2] <- B4, B1
  v4 = int mut[a/B0.0 + -8]
  v5 = v4 << 8
  v6 = v5 >>u 17
  v7 = v6 >u 1
  switch(v7) | 0 -> B6 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  [a/B0.0] +:= 2
  for_next_naked/v10 = for_counter_naked/B2.0 + 1
  v11 = for_next_naked/v10 <=s for_stop_naked/v2
  switch(v11) | 0 -> B5 | 1 -> B4

B4() [dominator=B3 depth=4] <- B3
  goto B2(for_next_naked/v10)

B5() [dominator=B3 depth=4] <- B3
  v12 = 1
  return(v12)

B6() [dominator=B2 depth=3] <- B2
  v13 = "camlTOP__block"
  raise(v13)

B7() [dominator=B0 depth=1] <- B0
  v14 = 1
  return(v14)
|}]

let _ =
  let a = [| 0 |] in
  store_loop a 100;
  a.(0)
[%%expect {|
- : int = 100
|}]

(* Two induction variables compared against each other: the comparison operand
   is loop-variant, so termination is genuinely unknown and the loop (which here
   does terminate because [j] outruns [i]) must run to completion rather than be
   deleted. [i] goes 0, 2, 4, ... and [j] goes n, n + 1, ..., so the gap of [n]
   closes at one per step: [n] steps. *)
let[@inline never] two_counter (n : int) =
  let i = ref 0 and j = ref (Sys.opaque_identity n) and hits = ref 0 in
  while !i < !j do
    incr hits;
    i := !i + 2;
    j := !j + 1
  done;
  !hits
[%%expect_ssa X86_64{|
two_counter:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  Popaque/v0 = opaque n/B0.0
  v1 = 1
  v2 = 1
  goto B1(v2, Popaque/v0, v1)

B1(i_unboxed0/B1.0 : int, j_unboxed0/B1.1 : int, hits_unboxed0/B1.2 : int) [dominator=B0 depth=1] <- B2, B0
  v3 = i_unboxed0/B1.0 <s j_unboxed0/B1.1
  switch(v3) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = hits_unboxed0/B1.2 + 2
  v5 = j_unboxed0/B1.1 + 2
  v6 = i_unboxed0/B1.0 + 4
  goto B1(v6, v5, v4)

B3() [dominator=B1 depth=2] <- B1
  goto B4(hits_unboxed0/B1.2)

B4(hits_unboxed0/B4.0 : val) [dominator=B3 depth=3] <- B3
  return(hits_unboxed0/B4.0)
|}]

let _ = two_counter 10
[%%expect {|
- : int = 10
|}]

(* The exit comparison is also the loop's result. The loop must NOT be
   deleted: the header would then compute the comparison with the initial
   counter rather than with the one that ended the loop, and [0 < m] would be
   returned in place of [false]. The bound [n lsr 3] is what lets the
   termination analysis accept the loop in the first place (the logical shift
   bounds the register, so the tagged counter provably never wraps). *)
let[@inline never] exit_cond_returned (n : int) =
  let m = n lsr 3 in
  let rec loop i =
    let b = i < m in
    if b then loop (i + 1) else b
  in
  loop 0
[%%expect_ssa X86_64{|
exit_cond_returned:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B2, B0
  prim/v3 = i/B1.0 <s m/v1
  switch(prim/v3) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = i/B1.0 + 2
  goto B1(v4)

B3() [dominator=B1 depth=2] <- B1
  v5 = prim/v3  * 2 + 1
  return(v5)
|}]

let _ = exit_cond_returned 40
[%%expect {|
- : bool = false
|}]

(* The same through a mutable variable. *)
let[@inline never] exit_cond_via_ref (n : int) =
  let m = n lsr 3 in
  let i = ref 0 in
  let b = ref true in
  while
    b := !i < m;
    !b
  do
    incr i
  done;
  !b
[%%expect_ssa X86_64{|
exit_cond_via_ref:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i_unboxed0/B1.0 : int) [dominator=B0 depth=1] <- B2, B0
  prim/v3 = i_unboxed0/B1.0 <s m/v1
  switch(prim/v3) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = i_unboxed0/B1.0 + 2
  goto B1(v4)

B3() [dominator=B1 depth=2] <- B1
  v5 = prim/v3  * 2 + 1
  goto B4(v5)

B4(int_lessthan/B4.0 : val) [dominator=B3 depth=3] <- B3
  return(int_lessthan/B4.0)
|}]

let _ = exit_cond_via_ref 40
[%%expect {|
- : bool = false
|}]

(* The same with the comparison consumed by an instruction after the loop
   rather than returned directly. *)
let[@inline never] exit_cond_as_int (n : int) =
  let m = n lsr 3 in
  let rec loop i =
    let b = i < m in
    if b then loop (i + 1) else Sys.opaque_identity (Bool.to_int b)
  in
  loop 0
[%%expect_ssa X86_64{|
exit_cond_as_int:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B2, B0
  prim/v3 = i/B1.0 <s m/v1
  switch(prim/v3) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = i/B1.0 + 2
  goto B1(v4)

B3() [dominator=B1 depth=2] <- B1
  v5 = prim/v3  * 2 + 1
  v6 = opaque v5
  return(v6)
|}]

let _ = exit_cond_as_int 40
[%%expect {|
- : int = 0
|}]

(* A loop inside a [try]: the handler is set up by a trap marker before the
   loop and popped after it, and the call may raise into it. The loop itself
   is as empty as [spin], and is deleted; the trap markers and the exception
   edge are outside it. *)
let[@inline never] spin_in_try (f : int -> int) (n : int) =
  try
    let k = f n in
    for _i = 0 to Sys.opaque_identity k - 1 do
      ()
    done;
    k
  with Exit -> -1
[%%expect_ssa X86_64{|
spin_in_try:
Delete_empty_loops: 1 deleted
B0(f/B0.0 : val, n/B0.1 : int) [dominator=B0 depth=0]
  try_region/v0 = beginregion
  push_trap B8
  goto B1()

B1() [dominator=B0 depth=1] <- B0
  v1 = int mut[f/B0.0]
  call_indirect(v1, n/B0.1, f/B0.0) may_raise -> B2 exn_successor=B8

B2(k/B2.0 : int) [dominator=B1 depth=2] <- B1
  Popaque/v2 = opaque k/B2.0
  for_stop/v3 = Popaque/v2 + -2
  v4 = for_stop/v3 >=s 1
  switch(v4) | 0 -> B7 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  goto B4(omitted_since_unused)

B4(for_counter_naked/B4.0 : int) [dominator=B3 depth=4] <- B3
  goto B6()

B6() [dominator=B4 depth=5] <- B4
  pop_trap B8
  return(k/B2.0)

B7() [dominator=B2 depth=3] <- B2
  pop_trap B8
  return(k/B2.0)

B8(exn/B8.0 : val) [dominator=B1 depth=2] <- B1
  endregion try_region/v0
  v10 = "camlStdlib__Exit1460_42"
  v11 = exn/B8.0 == v10
  switch(v11) | 0 -> B10 | 1 -> B9

B9() [dominator=B8 depth=3] <- B8
  v12 = -1
  return(v12)

B10() [dominator=B8 depth=3] <- B8
  raise(exn/B8.0)
|}]

let _ = spin_in_try (fun k -> k) 1000, spin_in_try (fun _ -> raise Exit) 1000
[%%expect {|
- : int * int = (1000, -1)
|}]

(* The header computes a value from outside the loop and the exit returns it.
   The value is the same on every visit of the header, so the loop is deleted
   and the value the retained header computes is the one returned. *)
let[@inline never] header_value (n : int) (x : int) =
  let m = n lsr 3 in
  let rec loop i =
    let t = x + 1 in
    if i < m then loop (i + 1) else t
  in
  loop 0
[%%expect_ssa X86_64{|
header_value:
Delete_empty_loops: 1 deleted
B0(n/B0.0 : int, x/B0.1 : int) [dominator=B0 depth=0]
  goto B1(omitted_since_unused)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B0
  goto B3()

B3() [dominator=B1 depth=2] <- B1
  v5 = x/B0.1 + 2
  return(v5)
|}]

let _ = header_value 800 5, header_value 0 5
[%%expect {|
- : int * int = (6, 6)
|}]

(* Continuing while the counter equals the bound: any non-zero step leaves
   the bound. Deleted. *)
let[@inline never] spin_while_equal (n : int) =
  let m = n lsr 3 in
  let rec loop i = if i = m then loop (i + 1) else () in
  loop 0
[%%expect_ssa X86_64{|
spin_while_equal:
Delete_empty_loops: 1 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  goto B1(omitted_since_unused)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B0
  goto B3()

B3() [dominator=B1 depth=2] <- B1
  v5 = 1
  return(v5)
|}]

let () = spin_while_equal 8
[%%expect {|
|}]
