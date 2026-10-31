(* [LoopComparisons.facts] must never state a fact that the comparison does not
   imply. Forming the inequality for [la >= lb] subtracts the two forms'
   constants, which can leave the OCaml [int] range even though each form fits
   it; a silently wrapped constant once turned the always-true guard below into
   the fact [x - y >= 2]. Combined with a satisfiable second guard this made the
   system infeasible, from which [FourierMotzkin.entails] "proved" any goal at
   all. The overflowing comparison must contribute no fact. (This is now proved,
   as [facts_sound] in backend/ssa/loop/rocq/LoopComparisons.v; the test checks
   the extracted code on the original example.) *)

module A = FourierMotzkin.Affine

let () =
  (* Guard 1: [x + max_int >= y - max_int]. Always true, and the difference of
     the constants, [2 * max_int], does not fit an [int]. *)
  let la = { A.const = max_int; terms = [0, 1] } in
  let lb = { A.const = -max_int; terms = [1, 1] } in
  let facts1 = LoopComparisons.facts false Cmm.Cge la lb in
  assert (List.length facts1 = 0);
  (* Guard 2: [y >= x - 1]. Satisfiable, e.g. by [x = y = 0]. *)
  let la2 = { A.const = 0; terms = [1, 1] } in
  let lb2 = { A.const = -1; terms = [0, 1] } in
  let facts2 = LoopComparisons.facts false Cmm.Cge la2 lb2 in
  assert (List.length facts2 = 1);
  let facts = facts1 @ facts2 in
  assert (FourierMotzkin.feasible facts);
  (* [x <= 0], i.e. [0 - x >= 0], is not implied by the guards. *)
  let goal = { A.const = 0; terms = [0, -1] } in
  assert (not (FourierMotzkin.entails facts goal));
  (* The same guards with [Cle] in the other orientation, and the negated forms,
     must likewise contribute nothing rather than a wrapped fact. *)
  assert (List.length (LoopComparisons.facts false Cmm.Cle lb la) = 0);
  assert (List.length (LoopComparisons.facts true Cmm.Clt la lb) = 0);
  assert (List.length (LoopComparisons.facts false Cmm.Ceq la lb) = 0);
  print_endline "OK"
