(* TEST *)

(* Mantis PR7447 *)

let rec r =
  let rec x = `A r and y () = x in
  y

let (`A x) = r ()

let _ = x ()
