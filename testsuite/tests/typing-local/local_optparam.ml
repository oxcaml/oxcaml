(* TEST *)

let foo (f @ local) l = l
let go a b g ?(x = foo (fun y -> a, y) b) () =
  let x = foo (fun y -> a, y) x in
  g x