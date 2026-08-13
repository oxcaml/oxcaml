(* Cursor is inside a record constructor (some of which don't allocate) *)

type t = { z : int ref }

let f g x y =
  let z = ref (x + y) in
  { z }
 (* ^ *)
;;

let f g x y =
  let z = ref (x + y) in
  exclave_ { z }
          (* ^ *)
;;

let f g x =
  let y = { z = x } in
             (* ^ *)
  y
;;

let f (local_ _) = ()

let g z =
  f { z };
   (* ^ *)
  f (local_ { z });
           (* ^ *)
  f { z }
   (* ^ *)

let g z = f { z } [@nontail]
           (* ^ *)

type t_ub = { z : int ref } [@@unboxed]

let f g x y =
  let z = ref (x + y) in
  { z }
 (* ^ *)
;;

let f g x y =
  let z = ref (x + y) in
  exclave_ { z }
          (* ^ *)
;;
