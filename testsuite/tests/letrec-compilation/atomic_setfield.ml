(* TEST *)

type t = { mutable x : int [@atomic] }

let t = { x = 1 }

let rec r = (t.x <- 2)

let () = assert (t.x = 2)

type m =
  { mutable y : int [@atomic];
    z : float#
  }

let m = { y = 1; z = #3.14 }

let rec s = (m.y <- 2)

let () = assert (m.y = 2)

let () = print_endline "ok"
