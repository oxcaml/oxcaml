(* An outer floating clause composes onto module declarations, and composition only
   strengthens: a nested member cannot weaken it from inside (a written [nonportable] is
   absorbed), so the exemption lands on [M]'s declaration, after which [M]'s portable
   member re-claims individually. *)
let counter = ref 0
let double x = x * 2
let triple x = x * 3

module M = struct
  let bump x = x + !counter
  let ok x = x * 4
end
