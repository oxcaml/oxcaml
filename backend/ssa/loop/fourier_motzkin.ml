[@@@ocaml.warning "+a-4-40-41-42-44"]

(* See [fourier_motzkin.mli] for the interface. *)

(* Raised internally when an integer operation would overflow the (63-bit) OCaml
   [int], so the coefficient it produced could no longer be trusted.
   Fourier-Motzkin cross-multiplication can blow coefficients up without bound,
   and a silently-wrapped coefficient could fabricate a spurious contradiction
   -> a false "infeasible" -> a false entailment -> an unsound rewrite. We
   detect the overflow and bail out conservatively instead (see [feasible] /
   [entails]). *)
exception Overflow

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

  (* Overflow-checked integer arithmetic, raising {!Overflow} rather than
     silently wrapping. Used only on the Fourier-Motzkin decision path. *)
  let add_ovf a b =
    let s = a + b in
    if a lxor s land (b lxor s) < 0 then raise Overflow else s

  let mul_ovf a b =
    if a = 0 || b = 0
    then 0
    else if (a = -1 && b = min_int) || (b = -1 && a = min_int)
    then raise Overflow
    else
      let p = a * b in
      if p / a <> b then raise Overflow else p

  let neg_ovf a = if a = min_int then raise Overflow else -a

  (* [scale] / [add] with overflow detection. *)
  let scale_checked k t =
    if k = 0
    then const 0
    else
      { const = mul_ovf t.const k;
        terms = List.map (fun (id, c) -> id, mul_ovf c k) t.terms
      }

  let add_checked a b =
    let ids =
      List.sort_uniq Int.compare (List.map fst a.terms @ List.map fst b.terms)
    in
    let terms =
      List.filter_map
        (fun id ->
          let c = add_ovf (coeff id a) (coeff id b) in
          if c = 0 then None else Some (id, c))
        ids
    in
    { const = add_ovf a.const b.const; terms }

  let add_const_checked t c = { t with const = add_ovf t.const c }
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
              let cp = cf p and cn = Affine.neg_ovf (cf n) in
              Affine.add_checked
                (Affine.scale_checked cn p)
                (Affine.scale_checked cp n))
            neg)
        pos
    in
    List.rev_append zero resolvents
  in
  (* If any coefficient overflows we cannot decide the system, so we
     conservatively report it as (possibly) feasible: [entails] then reports no
     entailment, which never licenses an unsound rewrite. *)
  try
    let reduced = List.fold_left (fun acc v -> elim v acc) ineqs atoms in
    List.for_all (fun (f : Affine.t) -> f.Affine.const >= 0) reduced
  with Overflow -> true

(* Does [{ f >= 0 | f in facts }] entail [goal >= 0]? Add the integer negation
   [goal <= -1] and test for infeasibility. An overflow while forming that
   negation likewise means we cannot prove entailment. *)
let entails (facts : Affine.t list) (goal : Affine.t) : bool =
  try
    not
      (feasible
         (Affine.add_const_checked (Affine.scale_checked (-1) goal) (-1)
         :: facts))
  with Overflow -> false
