(* Examples that Pprintast currently prints as unparseable output: exception
   and effect patterns are printed without parentheses even in delimited
   positions. The resulting failures are recorded in test.reference.

   CR-someday zqian: fix Pprintast and empty out the corresponding failures
   in test.reference. *)

let f1 (exception e) = ()
let f2 (effect E, k) = ()
let f3 = function lazy (exception e) -> () | _ -> ()
