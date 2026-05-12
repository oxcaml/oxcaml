(* Cursor is inside a variant (some of which don't allocate) *)

let f g x y =
  let z = x + y in
  `Some (g z)
      (* ^ *)
;;

let f g x y =
  let z = x + y in
  exclave_ `Some (g z)
               (* ^ *)
;;

let f g x y =
  let z = `Some (g x) in
              (* ^ *)
  y
;;

let f g x y =
  let z = x + y in
  `None
  (* ^ *)
;;

let f g x y =
  let z = x + y in
  exclave_ `None
           (* ^ *)
;;
