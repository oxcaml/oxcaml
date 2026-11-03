(* TEST
 flags += " -O3 -use-ssa -ssa-bounds-check-elim";
 only-default-codegen;
 no-tsan;
 expect.opt;
*)

(* Exercises bounds-check elimination. An array access whose index is known,
   from the branches dominating it and from the certified bounds of loop
   counters, to lie within the array may lose its check; a check these facts
   do not entail must be kept, so that the exception is raised.

   Each function is followed by the SSA that leaves the SSA pipeline
   ([%%expect_ssa]), headed by the number of checks Bounds_check_elim removed
   from it. Where a check was removed its block ends in a jump to the block
   that accesses the array, and the block that raised Invalid_argument has
   been pruned; a kept check still ends in a two-way branch on the unsigned
   comparison. The comment on each function says which to expect, and the
   [%%expect] blocks check what the functions compute, including that a kept
   check still raises. The unit test oxcaml/tests/backend/ssa/
   bounds_check_analysis.ml asserts exact elimination counts on hand-built
   graphs; this checks that the graphs the compiler builds for OCaml array
   accesses are among the shapes it recognises. The SSA shown is that of the
   x86-64 backend, so the [%%expect_ssa] blocks are tagged X86_64 and checked
   on that architecture only. *)

(* The tests the programmer wrote, [0 <= i] and [i < len], dominate the check
   and entail it. Eliminated. *)
let[@inline never] guarded (a : int array) (i : int) =
  let len = Array.length a in
  if i >= 0 && i < len then a.(i) else -1
[%%expect_ssa X86_64{|
guarded:
Bounds_check_elim: 1 eliminated
B0(a/B0.0 : val, i/B0.1 : int) [dominator=B0 depth=0]
  v0 = int mut[a/B0.0 + -8]
  v1 = v0 << 8
  v2 = v1 >>u 17
  len/v3 = v2 | 1
  v4 = i/B0.1 >=s 1
  switch(v4) | 0 -> B6 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  v5 = i/B0.1 <s len/v3
  switch(v5) | 0 -> B5 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  goto B3()

B3() [dominator=B2 depth=3] <- B2
  v7 = int mut[a/B0.0 + i/B0.1 * 4 + -4]
  return(v7)

B5() [dominator=B1 depth=2] <- B1
  v9 = -1
  return(v9)

B6() [dominator=B0 depth=1] <- B0
  v10 = -1
  return(v10)
|}]

let _ =
  guarded [| 1; 2; 3 |] 1, guarded [| 1; 2; 3 |] 3, guarded [| 1; 2; 3 |] (-1)
[%%expect {|
- : int * int * int = (2, -1, -1)
|}]

(* Only the upper test: a negative [i] passes it and must still fail the
   check. Kept. *)
let[@inline never] half_guarded (a : int array) (i : int) =
  if i < Array.length a then a.(i) else -1
[%%expect_ssa X86_64{|
half_guarded:
Bounds_check_elim: 0 eliminated
B0(a/B0.0 : val, i/B0.1 : int) [dominator=B0 depth=0]
  v0 = int mut[a/B0.0 + -8]
  v1 = v0 << 8
  v2 = v1 >>u 17
  Parraylength/v3 = v2 | 1
  v4 = i/B0.1 <s Parraylength/v3
  switch(v4) | 0 -> B4 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  v5 = i/B0.1 <u Parraylength/v3
  switch(v5) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  v6 = int mut[a/B0.0 + i/B0.1 * 4 + -4]
  return(v6)

B3() [dominator=B1 depth=2] <- B1
  v7 = "camlTOP__block"
  raise(v7)

B4() [dominator=B0 depth=1] <- B0
  v8 = -1
  return(v8)
|}]

let _ =
  ( half_guarded [| 1; 2; 3 |] 2,
    match half_guarded [| 1; 2; 3 |] (-1) with
    | n -> string_of_int n
    | exception Invalid_argument _ -> "Invalid_argument" )
[%%expect {|
- : int * string = (3, "Invalid_argument")
|}]

(* A recursive loop over the array. Its counter starts at the literal 0 and
   grows by one, and the header's exit test, which bounds it by the length,
   shows that the update never wraps; so the counter's certified lower bound
   and the exit test together entail the check. Eliminated. *)
let[@inline never] sum_rec (a : int array) =
  let n = Array.length a in
  let rec loop i acc = if i >= n then acc else loop (i + 1) (acc + a.(i)) in
  loop 0 0
[%%expect_ssa X86_64{|
sum_rec:
Bounds_check_elim: 1 eliminated
B0(a/B0.0 : val) [dominator=B0 depth=0]
  v0 = int mut[a/B0.0 + -8]
  v1 = v0 << 8
  v2 = v1 >>u 17
  n/v3 = v2 | 1
  v4 = 1
  v5 = 1
  goto B1(v5, v4)

B1(i/B1.0 : int, acc/B1.1 : int) [dominator=B0 depth=1] <- B4, B0
  v6 = i/B1.0 >=s n/v3
  switch(v6) | 0 -> B3 | 1 -> B2

B2() [dominator=B1 depth=2] <- B1
  return(acc/B1.1)

B3() [dominator=B1 depth=2] <- B1
  goto B4()

B4() [dominator=B3 depth=3] <- B3
  v8 = int mut[a/B0.0 + i/B1.0 * 4 + -4]
  v9 = acc/B1.1 + v8 + -1
  v10 = i/B1.0 + 2
  goto B1(v10, v9)
|}]

let _ = sum_rec [| 1; 2; 3; 4 |], sum_rec [||]
[%%expect {|
- : int * int = (10, 0)
|}]

(* A [for] loop. The index the check tests is the loop counter re-tagged by a
   scaled addition, an instruction specific to the target, which the model
   reads through the target's linear reading of it. The exit test is on the
   counter's next value, so the stop value it bounds that by is certified as
   the counter's upper bound; that bound shows the update not to wrap, and
   with the lower bound entails the check. Eliminated. *)
let[@inline never] fill (a : int array) (v : int) =
  for i = 0 to Array.length a - 1 do
    a.(i) <- v
  done
[%%expect_ssa X86_64{|
fill:
Bounds_check_elim: 1 eliminated
B0(a/B0.0 : val, v/B0.1 : int) [dominator=B0 depth=0]
  v0 = int mut[a/B0.0 + -8]
  v1 = v0 << 8
  v2 = v1 >>u 17
  Parraylength/v3 = v2 | 1
  for_stop/v4 = Parraylength/v3 + -2
  v5 = for_stop/v4 >=s 1
  switch(v5) | 0 -> B7 | 1 -> B1

B1() [dominator=B0 depth=1] <- B0
  for_stop_naked/v6 = for_stop/v4 >>s 1
  v7 = 0
  goto B2(v7)

B2(for_counter_naked/B2.0 : int) [dominator=B1 depth=2] <- B4, B1
  i/v8 = for_counter_naked/B2.0  * 2 + 1
  goto B3()

B3() [dominator=B2 depth=3] <- B2
  int[a/B0.0 + i/v8 * 4 + -4] := v/B0.1 (assign)
  for_next_naked/v12 = for_counter_naked/B2.0 + 1
  v13 = for_next_naked/v12 <=s for_stop_naked/v6
  switch(v13) | 0 -> B5 | 1 -> B4

B4() [dominator=B3 depth=4] <- B3
  goto B2(for_next_naked/v12)

B5() [dominator=B3 depth=4] <- B3
  v14 = 1
  return(v14)

B7() [dominator=B0 depth=1] <- B0
  v16 = 1
  return(v16)
|}]

let _ =
  let a = Array.make 3 0 in
  fill a 7;
  a
[%%expect {|
- : int array = [|7; 7; 7|]
|}]
