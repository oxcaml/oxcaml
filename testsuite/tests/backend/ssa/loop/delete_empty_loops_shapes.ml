(* TEST
 flags += " -O3 -use-ssa -ssa-delete-empty-loops";
 only-default-codegen;
 no-tsan;
 expect.opt;
*)

(* Loop shapes around empty-loop deletion: nested loops, several back edges, a
   self tail call as the back edge, a variable step, counters that outlive the
   loop, a non-integer counter. Deleting a loop never changes what a program
   does, so a loop that is deleted leaves nothing to observe; each function
   below checks whatever its loops do determine, and the rest exercises the
   analysis on shapes it must at least not mishandle.

   Each comment says whether the pass deletes the loop and, when it does not,
   why: the loop is not empty, or its shape is one the analysis does not
   recognise, marked as a known limitation. The [%%expect_ssa] block after each
   function says how many loops were deleted and shows the result; see
   delete_empty_loops.ml for how to read it. *)

(* Inner and outer loop both empty. Only the inner loop is deleted. Known
   limitation: the analysis takes the terminator of the loop's header for its
   exit branch, so it only recognises loops tested at the header: [while] loops,
   and [for] loops whose body is a single block. The outer loop's header is the
   block holding the inner loop's entry guard, and its exit test is in the block
   after the inner loop; deleting the inner loop leaves that shape as it is. *)
let[@inline never] nested_spin (n : int) (m : int) =
  for _i = 0 to n - 1 do
    for _j = 0 to m - 1 do
      ()
    done
  done
[%%expect_ssa X86_64{|
nested_spin:
Delete_empty_loops: 1 deleted
B0(n/B0.0 : int, m/B0.1 : int) [dominator=B0 depth=0]
  for_stop/v0 = n/B0.0 + -2
  v1 = for_stop/v0 >=s 1
  switch(v1) | 0 -> B11 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  for_stop_naked/v2 = for_stop/v0 >>s 1
  v3 = 0
  goto B2(v3)

B2(for_counter_naked/B2.0 : int) [dominator=B1 depth=2] <- B9, B1
  for_stop/v4 = m/B0.1 + -2
  v5 = for_stop/v4 >=s 1
  switch(v5) | 0 -> B7 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  goto B4(omitted_since_unused)

B4(for_counter_naked/B4.0 : int) [dominator=B3 depth=4] <- B3
  goto B6()

B6() [dominator=B4 depth=5] <- B4
  goto B8()

B7() [dominator=B2 depth=3] <- B2
  goto B8()

B8() [dominator=B2 depth=3] <- B7, B6
  for_next_naked/v10 = for_counter_naked/B2.0 + 1
  v11 = for_next_naked/v10 <=s for_stop_naked/v2
  switch(v11) | 0 -> B10 | 1 -> B9

B9() [dominator=B8 depth=4] <- B8
  goto B2(for_next_naked/v10)

B10() [dominator=B8 depth=4] <- B8
  v12 = 1
  return(v12)

B11() [dominator=B0 depth=1] <- B0
  v13 = 1
  return(v13)
|}]

let () = nested_spin 10 10
[%%expect {|
|}]

(* The inner loop is empty and is deleted; the outer counts its rounds, which
   are returned, so its counter is not dead and it is kept: [n]. *)
let[@inline never] nested_outer_effect (n : int) (m : int) =
  let count = ref 0 in
  for _i = 0 to n - 1 do
    for _j = 0 to m - 1 do
      ()
    done;
    incr count
  done;
  !count
[%%expect_ssa X86_64{|
nested_outer_effect:
Delete_empty_loops: 1 deleted
B0(n/B0.0 : int, m/B0.1 : int) [dominator=B0 depth=0]
  for_stop/v0 = n/B0.0 + -2
  v1 = for_stop/v0 >=s 1
  switch(v1) | 0 -> B11 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  for_stop_naked/v2 = for_stop/v0 >>s 1
  v3 = 1
  v4 = 0
  goto B2(v4, v3)

B2(for_counter_naked/B2.0 : int, count_unboxed0/B2.1 : int) [dominator=B1 depth=2] <- B9, B1
  for_stop/v5 = m/B0.1 + -2
  v6 = for_stop/v5 >=s 1
  switch(v6) | 0 -> B7 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  goto B4(omitted_since_unused)

B4(for_counter_naked/B4.0 : int) [dominator=B3 depth=4] <- B3
  goto B6()

B6() [dominator=B4 depth=5] <- B4
  goto B8()

B7() [dominator=B2 depth=3] <- B2
  goto B8()

B8() [dominator=B2 depth=3] <- B7, B6
  int_add/v11 = count_unboxed0/B2.1 + 2
  for_next_naked/v12 = for_counter_naked/B2.0 + 1
  v13 = for_next_naked/v12 <=s for_stop_naked/v2
  switch(v13) | 0 -> B10 | 1 -> B9

B9() [dominator=B8 depth=4] <- B8
  goto B2(for_next_naked/v12, int_add/v11)

B10() [dominator=B8 depth=4] <- B8
  return(int_add/v11)

B11() [dominator=B0 depth=1] <- B0
  v14 = 1
  return(v14)
|}]

let _ = nested_outer_effect 10 7
[%%expect {|
- : int = 10
|}]

(* The inner loop counts: [n * m]. Neither loop is deleted: both carry the
   count, which is returned. *)
let[@inline never] nested_inner_effect (n : int) (m : int) =
  let count = ref 0 in
  for _i = 0 to n - 1 do
    for _j = 0 to m - 1 do
      incr count
    done
  done;
  !count
[%%expect_ssa X86_64{|
nested_inner_effect:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int, m/B0.1 : int) [dominator=B0 depth=0]
  for_stop/v0 = n/B0.0 + -2
  v1 = for_stop/v0 >=s 1
  switch(v1) | 0 -> B11 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  for_stop_naked/v2 = for_stop/v0 >>s 1
  v3 = 1
  v4 = 0
  goto B2(v4, v3)

B2(for_counter_naked/B2.0 : int, count_unboxed0/B2.1 : int) [dominator=B1 depth=2] <- B9, B1
  for_stop/v5 = m/B0.1 + -2
  v6 = for_stop/v5 >=s 1
  switch(v6) | 0 -> B7 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  for_stop_naked/v7 = for_stop/v5 >>s 1
  v8 = 0
  goto B4(v8, count_unboxed0/B2.1)

B4(for_counter_naked/B4.0 : int, count_unboxed0/B4.1 : int) [dominator=B3 depth=4] <- B5, B3
  int_add/v9 = count_unboxed0/B4.1 + 2
  for_next_naked/v10 = for_counter_naked/B4.0 + 1
  v11 = for_next_naked/v10 <=s for_stop_naked/v7
  switch(v11) | 0 -> B6 | 1 -> B5

B5() [dominator=B4 depth=5] <- B4
  goto B4(for_next_naked/v10, int_add/v9)

B6() [dominator=B4 depth=5] <- B4
  goto B8(int_add/v9)

B7() [dominator=B2 depth=3] <- B2
  goto B8(count_unboxed0/B2.1)

B8(count_unboxed0/B8.0 : int) [dominator=B2 depth=3] <- B7, B6
  for_next_naked/v12 = for_counter_naked/B2.0 + 1
  v13 = for_next_naked/v12 <=s for_stop_naked/v2
  switch(v13) | 0 -> B10 | 1 -> B9

B9() [dominator=B8 depth=4] <- B8
  goto B2(for_next_naked/v12, count_unboxed0/B8.0)

B10() [dominator=B8 depth=4] <- B8
  return(count_unboxed0/B8.0)

B11() [dominator=B0 depth=1] <- B0
  v14 = 1
  return(v14)
|}]

let _ = nested_inner_effect 10 7
[%%expect {|
- : int = 70
|}]

(* Two back edges updating the counter by the same step: one induction
   variable with two update expressions. Deleted. *)
let[@inline never] two_back_edges_same (n : int) (p : bool) =
  let m = n lsr 3 in
  let rec loop i =
    if i < m then if p then loop (i + 1) else loop (1 + i) else ()
  in
  loop 0
[%%expect_ssa X86_64{|
two_back_edges_same:
Delete_empty_loops: 1 deleted
B0(n/B0.0 : int, p/B0.1 : int) [dominator=B0 depth=0]
  goto B1(omitted_since_unused)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B0
  goto B5()

B5() [dominator=B1 depth=2] <- B1
  v7 = 1
  return(v7)
|}]

let () = two_back_edges_same 40 true; two_back_edges_same 40 false
[%%expect {|
|}]

(* Two back edges with different steps: not a single induction variable, so the
   loop is kept. The counter is also returned: [m] when stepping by one, the
   first even number not below [m] when stepping by two. *)
let[@inline never] two_back_edges_diff (n : int) (p : bool) =
  let m = n lsr 3 in
  let rec loop i =
    if i < m then if p then loop (i + 1) else loop (i + 2) else i
  in
  loop 0
[%%expect_ssa X86_64{|
two_back_edges_diff:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int, p/B0.1 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B4, B3, B0
  v3 = i/B1.0 <s m/v1
  switch(v3) | 0 -> B5 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = p/B0.1 == 1
  switch(v4) | 0 -> B4 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  v5 = i/B1.0 + 4
  goto B1(v5)

B4() [dominator=B2 depth=3] <- B2
  v6 = i/B1.0 + 2
  goto B1(v6)

B5() [dominator=B1 depth=2] <- B1
  return(i/B1.0)
|}]

let _ = two_back_edges_diff 40 true, two_back_edges_diff 40 false
[%%expect {|
- : int * int = (5, 6)
|}]

(* The back edge is a self tail call ([@loop never] keeps Flambda 2 from
   turning it into a local loop), so the loop's header is the function's entry
   block, which thereby has a predecessor. With a constant bound the loop is
   empty, and is deleted. *)
let[@inline never] [@loop never] rec self_spin (i : int) =
  if i < 1000 then self_spin (i + 1) else 42
[%%expect_ssa X86_64{|
self_spin:
Delete_empty_loops: 1 deleted
B0(i/B0.0 : int) [dominator=B0 depth=0]
  goto B2()

B2() [dominator=B0 depth=1] <- B0
  v2 = 85
  return(v2)
|}]

let _ = self_spin 0
[%%expect {|
- : int = 42
|}]

(* The same with the bound passed along unchanged. Kept. Known limitation:
   every parameter of the loop header must be an induction variable, and a
   parameter passed back unchanged (a step of zero) is not recognised as
   one. *)
let[@inline never] [@loop never] rec self_spin_bound (i : int) (n : int) =
  if i < n then self_spin_bound (i + 1) n else ()
[%%expect_ssa X86_64{|
self_spin_bound:
Delete_empty_loops: 0 deleted
B0(i/B0.0 : int, n/B0.1 : int) [dominator=B0 depth=0] <- B1
  v0 = i/B0.0 <s n/B0.1
  switch(v0) | 0 -> B2 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  v1 = i/B0.0 + 2
  goto B0(v1, n/B0.1)

B2() [dominator=B0 depth=1] <- B0
  v2 = 1
  return(v2)
|}]

let () = self_spin_bound 0 12
[%%expect {|
|}]

(* A variable step. Kept: the step is not a constant, so no termination
   argument is attempted (it would need the sign of [s]); the counter is also
   returned: 0, 3, 6 for [m = 5]. *)
let[@inline never] var_step (n : int) (s : int) =
  let m = n lsr 3 in
  let rec loop i = if i < m then loop (i + s) else i in
  loop 0
[%%expect_ssa X86_64{|
var_step:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int, s/B0.1 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B2, B0
  v3 = i/B1.0 <s m/v1
  switch(v3) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = i/B1.0 + s/B0.1 + -1
  goto B1(v4)

B3() [dominator=B1 depth=2] <- B1
  return(i/B1.0)
|}]

let _ = var_step 40 3
[%%expect {|
- : int = 6
|}]

(* The counter outlives the loop, so it is not dead and the loop is kept:
   [m]. *)
let[@inline never] counter_after (n : int) =
  let m = n lsr 3 in
  let i = ref 0 in
  while !i < m do
    incr i
  done;
  !i
[%%expect_ssa X86_64{|
counter_after:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i_unboxed0/B1.0 : int) [dominator=B0 depth=1] <- B2, B0
  v3 = i_unboxed0/B1.0 <s m/v1
  switch(v3) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = i_unboxed0/B1.0 + 2
  goto B1(v4)

B3() [dominator=B1 depth=2] <- B1
  goto B4(i_unboxed0/B1.0)

B4(i_unboxed0/B4.0 : val) [dominator=B3 depth=3] <- B3
  return(i_unboxed0/B4.0)
|}]

let _ = counter_after 40
[%%expect {|
- : int = 5
|}]

(* An accumulator alongside the counter: the sum of [0 .. m - 1]. Kept: the
   accumulator is returned, and its step, the counter, is not loop-invariant,
   so it is not an induction variable either. *)
let[@inline never] sum_to (n : int) =
  let m = n lsr 3 in
  let rec loop i acc = if i < m then loop (i + 1) (acc + i) else acc in
  loop 0 0
[%%expect_ssa X86_64{|
sum_to:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  v3 = 1
  goto B1(v3, v2)

B1(i/B1.0 : int, acc/B1.1 : int) [dominator=B0 depth=1] <- B2, B0
  v4 = i/B1.0 <s m/v1
  switch(v4) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v5 = acc/B1.1 + i/B1.0 + -1
  v6 = i/B1.0 + 2
  goto B1(v6, v5)

B3() [dominator=B1 depth=2] <- B1
  return(acc/B1.1)
|}]

let _ = sum_to 40
[%%expect {|
- : int = 10
|}]

(* Counting down to a constant. Deleted. *)
let[@inline never] countdown (n : int) =
  let rec loop i = if i > 0 then loop (i - 1) else () in
  loop (n lsr 3)
[%%expect_ssa X86_64{|
countdown:
Delete_empty_loops: 1 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  goto B1(omitted_since_unused)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B0
  goto B3()

B3() [dominator=B1 depth=2] <- B1
  v4 = 1
  return(v4)
|}]

let () = countdown 40
[%%expect {|
|}]

(* An inequality as the exit test; the counter meets the bound exactly. Kept.
   Known limitation: continuing while the counter differs from the bound is
   never shown to stop, as that needs the counter not to step over the bound
   (here: a step of one from a start not above it). *)
let[@inline never] ne_exit (n : int) =
  let m = n lsr 3 in
  let rec loop i = if i <> m then loop (i + 1) else () in
  loop 0
[%%expect_ssa X86_64{|
ne_exit:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B2, B0
  v3 = i/B1.0 != m/v1
  switch(v3) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = i/B1.0 + 2
  goto B1(v4)

B3() [dominator=B1 depth=2] <- B1
  v5 = 1
  return(v5)
|}]

let () = ne_exit 40
[%%expect {|
|}]

(* A floating-point counter. Kept: only integer induction variables are
   recognised. *)
let[@inline never] float_spin (n : int) =
  let f = ref 0.0 in
  let m = float_of_int (n lsr 3) in
  while !f < m do
    f := !f +. 1.0
  done
[%%expect_ssa X86_64{|
float_spin:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 4
  prim/v1 = int->float v0
  v2 = 0.
  goto B1(v2)

B1(unboxed_float/B1.0 : float) [dominator=B0 depth=1] <- B2, B0
  v3 = unboxed_float/B1.0 < prim/v1
  switch(v3) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = 1.
  v5 = unboxed_float/B1.0 +. v4
  goto B1(v5)

B3() [dominator=B1 depth=2] <- B1
  v6 = 1
  return(v6)
|}]

let () = float_spin 40
[%%expect {|
|}]
