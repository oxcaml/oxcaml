(* Cursor on the constructor itself (we treat this case specially to improve LSP
   compatibility) *)

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

(* Pattern of a [let]-bound function (we treat this case specially to improve LSP
   compatibility) *)

let f g x y =
 (* ^ *)
  let z = x + y in
  exclave_ Some (g z)
and h g x y =
 (* ^ *)
  let z = x + y in
  exclave_ Some (g z)
;;

let ignore (local_ _) = ()

let () =
  let f g x y =
   (* ^ *)
    let z = x + y in
    exclave_ Some (g z)
  and h g x y =
   (* ^ *)
    let z = x + y in
    exclave_ Some (g z)
  in
  ignore f;
  ignore h

(* Ensure other [let]-bound patterns aren't treated this way *)

let x = Some 5
 (* ^ *)
