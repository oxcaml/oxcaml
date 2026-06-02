(* TEST
   include stdlib_upstream_compatible;
   expect;
*)

(* Typechecking for [box] is incomplete. *)

type (_, _ : any) boxes = Boxes : ('a : any). ('a box, 'a) boxes
[%%expect{|
type (_, _ : any) boxes = Boxes : ('a : any). ('a box, 'a) boxes
|}]

(* This function typechecks *)
let f (type a) (type (au : float64))
      (Boxes : (a, au) boxes) (Equal : (a, float) Type.eq)
      (au : au) : au =
  let z : a# = au in z
[%%expect{|
val f :
  'a ('au : float64). ('a, 'au) boxes -> ('a, float) Type.eq -> 'au -> 'au =
  <fun>
|}]

(* But not if you swap the order of the first two matches *)
let f (type a) (type (au : float64))
      (Equal : (a, float) Type.eq) (Boxes : (a, au) boxes)
      (au : au) : au =
  let z : a# = au in z
[%%expect{|
Line 4, characters 15-17:
4 |   let z : a# = au in z
                   ^^
Error: This expression has type "au" but an expression was expected of type
         "a#" = "float#"
|}]

(* Same example, using intermediate modules *)

(* This function typechecks *)
let f (type a) (type (au : float64))
      (eq : (a, float) Type.eq) (boxes : (a, au) boxes) (au : au) : au =
  let Boxes = boxes in
  let Equal = eq in
  let module M = struct
    type b = a
    let z : b# = au
  end in
  M.z
[%%expect{|
val f :
  'a ('au : float64). ('a, float) Type.eq -> ('a, 'au) boxes -> 'au -> 'au =
  <fun>
|}]

(* But not if you swap the order of the first two [let]s *)
let f (type a) (type (au : float64))
      (eq : (a, float) Type.eq) (boxes : (a, au) boxes) (au : au) : au =
  let Equal = eq in
  let Boxes = boxes in
  let module M = struct
    type b = a
    let z : b# = au
  end in
  M.z
[%%expect{|
Line 7, characters 17-19:
7 |     let z : b# = au
                     ^^
Error: This expression has type "au" but an expression was expected of type
         "b#" = "float#"
|}]
