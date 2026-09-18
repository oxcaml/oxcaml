(* [ok] already writes [@@ portable]; with two more provably portable values that makes
   three of four, so the claim hoists. The written atom must survive the hoist: a
   strengthened signature never deletes user-written modalities. *)
let counter = ref 0
let ok x = x * 1
let double x = x * 2
let triple x = x * 3
let bump x = x + !counter
