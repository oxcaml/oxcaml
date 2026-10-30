(* TEST
 flags += " -O3 -use-ssa -ssa-delete-empty-loops";
 only-default-codegen;
 no-tsan;
 expect.opt;
*)

(* Loops with more than one way out. Empty-loop deletion replaces the header's
   exit branch by a jump to its target, so a loop may only be deleted when that
   branch is the only edge leaving the loop body: a body that can return another
   value, raise, print or fail a bounds check must be kept, however little else
   it does.

   No loop in this file is deleted: every [%%expect_ssa] block says
   "0 deleted" and shows the header with its back edge. Each comment says which
   check of the analysis keeps the loop. See delete_empty_loops.ml for how to
   read the blocks. *)

(* [p] is loop-invariant, but when it holds the loop returns [1] from inside
   the body rather than [0] from the header's exit. The body is effect-free, the
   counter is dead, the bound is in range and the exit target takes no
   arguments, so everything else about the loop would allow deletion; only the
   extra exit edge, from the body's test of [p] to the block returning [1],
   keeps it. Without that check [early_exit _ true] would return [0]. *)
let[@inline never] early_exit (n : int) (p : bool) =
  let m = n lsr 3 in
  let rec loop i = if i < m then if p then 1 else loop (i + 1) else 0 in
  loop 0
[%%expect_ssa X86_64{|
early_exit:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int, p/B0.1 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B3, B0
  v3 = i/B1.0 <s m/v1
  switch(v3) | 0 -> B5 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = p/B0.1 == 1
  switch(v4) | 0 -> B4 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  v5 = i/B1.0 + 2
  goto B1(v5)

B4() [dominator=B2 depth=3] <- B2
  v6 = 3
  return(v6)

B5() [dominator=B1 depth=2] <- B1
  v7 = 1
  return(v7)
|}]

let _ = early_exit 40 true, early_exit 40 false
[%%expect {|
- : int * int = (1, 0)
|}]

(* The same, leaving the body with an exception: the extra exit edge leads to
   the raise. *)
exception Early

let[@inline never] raise_exit (n : int) (p : bool) =
  let m = n lsr 3 in
  let rec loop i = if i < m then if p then raise Early else loop (i + 1) in
  loop 0
[%%expect_ssa X86_64{|
raise_exit:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int, p/B0.1 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B3, B0
  v3 = i/B1.0 <s m/v1
  switch(v3) | 0 -> B5 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = p/B0.1 == 1
  switch(v4) | 0 -> B4 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  v5 = i/B1.0 + 2
  goto B1(v5)

B4() [dominator=B2 depth=3] <- B2
  v6 = "camlTOP__raise_exit"
  v7 = val[v6 + 24]
  raise(v7)

B5() [dominator=B1 depth=2] <- B1
  v8 = 1
  return(v8)
|}]

let _ =
  (match raise_exit 40 true with
   | () -> "returned"
   | exception Early -> "Early"),
  raise_exit 40 false
[%%expect {|
- : string * unit = ("Early", ())
|}]

(* The same, leaving the body for a side effect that the header's exit does not
   perform: the extra exit edge leads to the call. The body itself is
   effect-free, so only that edge keeps the loop. *)
let[@inline never] early_exit_effect (n : int) (p : bool) =
  let m = n lsr 3 in
  let rec loop i =
    if i < m then if p then print_string "early" else loop (i + 1)
  in
  loop 0
[%%expect_ssa X86_64{|
early_exit_effect:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int, p/B0.1 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B3, B0
  v3 = i/B1.0 <s m/v1
  switch(v3) | 0 -> B6 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = p/B0.1 == 1
  switch(v4) | 0 -> B4 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  v5 = i/B1.0 + 2
  goto B1(v5)

B4() [dominator=B2 depth=3] <- B2
  v6 = 11
  v7 = 1
  v8 = "camlTOP__immstring"
  v9 = "camlStdlib__print_string_43_94"
  v10 = val[v9 + 16]
  call_prim caml_ml_output(v10, v8, v7, v6) may_raise -> B5

B5(B5.0 : val) [dominator=B4 depth=4] <- B4
  return(B5.0)

B6() [dominator=B1 depth=2] <- B1
  v11 = 1
  return(v11)
|}]

(* A bounds check in a [for] loop. The loop body has two blocks: the header
   holds the check, and the block after it holds the counter's exit test. The
   analysis takes the header's branch for the loop's exit branch, so it sees the
   branch to the raise, whose condition does not involve the counter, and
   establishes no termination; the real exit test, in the second block, is a
   further edge out of the loop. The loop is kept so that the exception is
   raised. *)
let[@inline never] bounds_check (a : int array) (n : int) (k : int) =
  for _i = 0 to n - 1 do
    ignore a.(k)
  done
[%%expect_ssa X86_64{|
bounds_check:
Delete_empty_loops: 0 deleted
B0(a/B0.0 : val, n/B0.1 : int, k/B0.2 : int) [dominator=B0 depth=0]
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
  v7 = k/B0.2 <u v6
  switch(v7) | 0 -> B6 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  for_next_naked/v8 = for_counter_naked/B2.0 + 1
  v9 = for_next_naked/v8 <=s for_stop_naked/v2
  switch(v9) | 0 -> B5 | 1 -> B4

B4() [dominator=B3 depth=4] <- B3
  goto B2(for_next_naked/v8)

B5() [dominator=B3 depth=4] <- B3
  v10 = 1
  return(v10)

B6() [dominator=B2 depth=3] <- B2
  v11 = "camlTOP__block"
  raise(v11)

B7() [dominator=B0 depth=1] <- B0
  v12 = 1
  return(v12)
|}]

let _ =
  (match bounds_check [| 1 |] 3 5 with
   | () -> "returned"
   | exception Invalid_argument _ -> "Invalid_argument"),
  bounds_check [| 1 |] 3 0
[%%expect {|
- : string * unit = ("Invalid_argument", ())
|}]

(* A second exit that returns the same value as the header's. The two returns
   are distinct blocks in the SSA graph, so this is a second exit like the
   others and the loop is kept. Known limitation: a second exit is rejected even
   when it leads to the same outcome as the header's exit; only edges to the
   header's exit target itself are allowed. *)
let[@inline never] same_exit (n : int) (p : bool) =
  let m = n lsr 3 in
  let rec loop i = if i < m then if p then 0 else loop (i + 1) else 0 in
  loop 0
[%%expect_ssa X86_64{|
same_exit:
Delete_empty_loops: 0 deleted
B0(n/B0.0 : int, p/B0.1 : int) [dominator=B0 depth=0]
  v0 = n/B0.0 >>u 3
  m/v1 = v0 | 1
  v2 = 1
  goto B1(v2)

B1(i/B1.0 : int) [dominator=B0 depth=1] <- B3, B0
  v3 = i/B1.0 <s m/v1
  switch(v3) | 0 -> B5 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v4 = p/B0.1 == 1
  switch(v4) | 0 -> B4 | 1 -> B3

B3() [dominator=B2 depth=3] <- B2
  v5 = i/B1.0 + 2
  goto B1(v5)

B4() [dominator=B2 depth=3] <- B2
  v6 = 1
  return(v6)

B5() [dominator=B1 depth=2] <- B1
  v7 = 1
  return(v7)
|}]

let _ = same_exit 40 true, same_exit 40 false
[%%expect {|
- : int * int = (0, 0)
|}]
