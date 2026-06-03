(* TEST
   include stdlib_upstream_compatible;
   expect;
*)

(* Typechecking for [box] does not depend on the order in which GADT equations
   are introduced: learning [a = au box] and [a = float] (hence [au = float#])
   typechecks regardless of which equation comes first. *)

type (_, _ : any) boxes = Boxes : ('a : any). ('a box, 'a) boxes
[%%expect{|
type (_, _ : any) boxes = Boxes : ('a : any). ('a box, 'a) boxes
|}]

(* Matching [Boxes] before [Equal] *)
let f (type a) (type (au : float64))
      (Boxes : (a, au) boxes) (Equal : (a, float) Type.eq)
      (au : au) : au =
  let z : a# = au in z
[%%expect{|
val f :
  'a ('au : float64). ('a, 'au) boxes -> ('a, float) Type.eq -> 'au -> 'au =
  <fun>
|}]

(* Matching [Equal] before [Boxes] *)
let f (type a) (type (au : float64))
      (Equal : (a, float) Type.eq) (Boxes : (a, au) boxes)
      (au : au) : au =
  let z : a# = au in z
[%%expect{|
val f :
  'a ('au : float64). ('a, float) Type.eq -> ('a, 'au) boxes -> 'au -> 'au =
  <fun>
|}]

(* Same examples, using intermediate modules *)

(* Matching [Boxes] before [Equal] *)
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

(* Matching [Equal] before [Boxes] *)
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
val f :
  'a ('au : float64). ('a, float) Type.eq -> ('a, 'au) boxes -> 'au -> 'au =
  <fun>
|}]
