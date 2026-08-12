(* Cursor is inside a variant constructor (some of which don't allocate) *)

let f g x y =
  let z = x + y in
  Some (g z)
     (* ^ *)
;;

let f g x y =
  let z = x + y in
  exclave_ Some (g z)
              (* ^ *)
;;

let f g x y =
  let z = Some (g x) in
             (* ^ *)
  y
;;

let f g x y =
  let z = x + y in
  None
 (* ^ *)
;;

let f g x y =
  let z = x + y in
  exclave_ None
          (* ^ *)
;;

let f (local_ _) = ()

let g x =
  f (Some x);
       (* ^ *)
  f (local_ Some x);
              (* ^ *)
  f (Some x)
       (* ^ *)

let g x = f (Some x) [@nontail]
               (* ^ *)

type t = Box of string [@@unboxed]

let f g x y =
  let z = x + y in
  Box (g z)
    (* ^ *)
;;
