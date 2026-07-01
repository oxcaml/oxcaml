[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [fourier_motzkin.mli] for the interface. *)

(* === Affine forms over interned atoms ===

   [terms] maps an atom (an integer identifier standing for some opaque value we
   don't decompose) to a non-zero integer coefficient; [const] is the constant
   term. A value [v] represents the assertion [v >= 0]. *)
module Affine = struct
  type t =
    { const : int;
      terms : (int * int) list
    }

  let const c = { const = c; terms = [] }

  let var id = { const = 0; terms = [id, 1] }

  let is_const t = match t.terms with [] -> true | _ :: _ -> false

  let coeff id t =
    match List.assoc_opt id t.terms with Some c -> c | None -> 0

  let add_const t c = { t with const = t.const + c }

  let add a b =
    let ids =
      List.sort_uniq Int.compare (List.map fst a.terms @ List.map fst b.terms)
    in
    let terms =
      List.filter_map
        (fun id ->
          let c = coeff id a + coeff id b in
          if c = 0 then None else Some (id, c))
        ids
    in
    { const = a.const + b.const; terms }

  let scale k t =
    if k = 0
    then const 0
    else
      { const = t.const * k;
        terms = List.map (fun (id, c) -> id, c * k) t.terms
      }

  let neg t = scale (-1) t

  let sub a b = add a (neg b)
end

(* === Fourier-Motzkin === *)

(* Is the conjunction [{ f >= 0 | f in ineqs }] satisfiable over the rationals?
   Eliminate atoms one at a time: for each, combine every lower-bound (positive
   coeff) with every upper-bound (negative coeff) into an atom-free resolvent;
   atoms occurring with only one sign are unconstrained and dropped. The system
   is infeasible iff some constant-only inequality becomes negative. *)
let feasible (ineqs : Affine.t list) : bool =
  let atoms =
    List.sort_uniq Int.compare
      (List.concat_map (fun (f : Affine.t) -> List.map fst f.terms) ineqs)
  in
  let elim v ineqs =
    let cf f = Affine.coeff v f in
    let pos = List.filter (fun f -> cf f > 0) ineqs in
    let neg = List.filter (fun f -> cf f < 0) ineqs in
    let zero = List.filter (fun f -> cf f = 0) ineqs in
    let resolvents =
      List.concat_map
        (fun p ->
          List.map
            (fun n ->
              let cp = cf p and cn = -cf n in
              Affine.add (Affine.scale cn p) (Affine.scale cp n))
            neg)
        pos
    in
    List.rev_append zero resolvents
  in
  let reduced = List.fold_left (fun acc v -> elim v acc) ineqs atoms in
  List.for_all (fun (f : Affine.t) -> f.Affine.const >= 0) reduced

(* Does [{ f >= 0 | f in facts }] entail [goal >= 0]? Add the integer negation
   [goal <= -1] and test for infeasibility. *)
let entails (facts : Affine.t list) (goal : Affine.t) : bool =
  not (feasible (Affine.add_const (Affine.neg goal) (-1) :: facts))
