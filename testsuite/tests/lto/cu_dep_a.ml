(* Candidates for calling-convention changes: [sum_pair]'s parameter and
   [make_pair]'s result could be unboxed if the analysis were free to change
   their calling conventions. Whatever it decides, the decision must agree
   between this unit's rebuild (which rewrites the definitions) and the main
   module's rebuild (which rewrites the call sites). *)

let[@inline never] sum_pair p = fst p + snd p

let[@inline never] make_pair x = x, x * 3
