[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [affine_expr.mli] for the interface. *)

module Affine = Fourier_motzkin.Affine

type t =
  | Const of int
  | Atom of int
  | Add of t * t
  | Sub of t * t
  | Scale of int * t
  | Shr_atom of
      { atom : int;
        arg : t;
        bits : int
      }

let rec to_affine (e : t) : Affine.t * Affine.t list =
  match e with
  | Const c -> Affine.const c, []
  | Atom id -> Affine.var id, []
  | Add (a, b) ->
    let fa, sa = to_affine a in
    let fb, sb = to_affine b in
    Affine.add fa fb, sa @ sb
  | Sub (a, b) ->
    let fa, sa = to_affine a in
    let fb, sb = to_affine b in
    Affine.sub fa fb, sa @ sb
  | Scale (k, a) ->
    let fa, sa = to_affine a in
    Affine.scale k fa, sa
  | Shr_atom { atom; arg; bits } ->
    (* [atom] stands for [arg asr bits]; the exact value satisfies [2^bits *
       atom <= arg <= 2^bits * atom + 2^bits - 1]. *)
    let fa, sa = to_affine arg in
    let t = Affine.var atom in
    let pow = 1 lsl bits in
    let sides =
      Affine.sub fa (Affine.scale pow t)
      :: Affine.sub (Affine.add_const (Affine.scale pow t) (pow - 1)) fa
      :: sa
    in
    t, sides

let as_const (e : t) : int option =
  let form, _sides = to_affine e in
  if Affine.is_const form then Some form.Affine.const else None

let rec occurs a (e : t) : bool =
  match e with
  | Const _ -> false
  | Atom b -> Int.equal a b
  | Add (x, y) | Sub (x, y) -> occurs a x || occurs a y
  | Scale (_, x) -> occurs a x
  | Shr_atom { atom; arg; bits = _ } -> Int.equal a atom || occurs a arg

let rec coeff_of_atom a (e : t) : int option =
  let ( let* ) = Option.bind in
  match e with
  | Const _ -> Some 0
  | Atom b -> Some (if Int.equal a b then 1 else 0)
  | Add (x, y) ->
    let* cx = coeff_of_atom a x in
    let* cy = coeff_of_atom a y in
    Some (cx + cy)
  | Sub (x, y) ->
    let* cx = coeff_of_atom a x in
    let* cy = coeff_of_atom a y in
    Some (cx - cy)
  | Scale (k, x) ->
    let* cx = coeff_of_atom a x in
    Some (k * cx)
  | Shr_atom { atom; arg; bits = _ } ->
    (* The shift's value is the atom itself; [a] occurring inside [arg] would
       contribute non-affinely, so that is rejected. *)
    if Int.equal a atom then Some 1 else if occurs a arg then None else Some 0
