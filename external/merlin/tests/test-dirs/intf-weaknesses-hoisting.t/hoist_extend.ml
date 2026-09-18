(* The signature already floats [@@ many]; the portable majority extends that clause
   rather than adding a second one, and never touches the axis it already writes. *)
let counter = ref 0
let double x = x * 2
let triple x = x * 3
let quad x = x * 4
let bump x = x + !counter
