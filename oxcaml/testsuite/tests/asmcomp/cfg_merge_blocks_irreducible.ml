(* TEST
 flags += " -cfg-merge-blocks -dcfg-invariants";
 native;
*)

(* Regression test for [Cfg_merge_blocks] making the CFG irreducible: after
   register allocation, the return continuations of the two [g x] calls below
   (one in the [if], one in the loop body) become identical blocks and get
   merged, redirecting the [if] path into the middle of the [while] loop while
   the [else] path still enters at the loop header. The resulting CFG has no
   dominating loop header, i.e. is irreducible, so [Cfg_merge_blocks] must run
   after the last pass relying on loop infos ([Cfg_stack_checks]). When the
   pass ran before stack checks, compiling this file used to fail with "Cfg
   invariant failed: CFG is not reducible". *)

let[@inline never] g r = decr r

let[@inline never] f a b =
  let x = ref a in
  let z = ref 0 in
  g x;
  g z;
  (if b then g x else incr z);
  while !x > 0 do
    g x
  done;
  !x + !z

let () =
  assert (f 5 true = -1);
  assert (f 5 false = 0);
  assert (f (-5) true = -8);
  assert (f (-5) false = -6)
