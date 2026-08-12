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

  let equal a b =
    Int.equal a.const b.const
    && List.equal
         (fun (i1, c1) (i2, c2) -> Int.equal i1 i2 && Int.equal c1 c2)
         a.terms b.terms

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

(* === Resolvents ===

   The resolvent of a lower bound [p] (coefficient [cp > 0] of the atom being
   eliminated) and an upper bound [n] (coefficient [-cn < 0]) is [cn*p + cp*n].
   Computed naively over OCaml [int]s this overflows readily: range facts carry
   constants like [max_int] and untag relations carry coefficient 2, so even [2
   * max_int] escapes. Compute in checked [Int64] and normalize the result by
   the gcd [g] of its term coefficients, with the constant divided by [g]
   rounding toward negative infinity — for integers, [sum g*k_i*x_i + c >= 0]
   iff [sum k_i*x_i + floor(c/g) >= 0], so the normalized inequality is exactly
   equivalent (and integer-tightened). Anything that still does not fit an OCaml
   [int] raises {!Overflow}. *)

let add_ovf64 a b =
  let s = Int64.add a b in
  if Int64.logand (Int64.logxor a s) (Int64.logxor b s) < 0L
  then raise Overflow
  else s

let mul_ovf64 a b =
  if Int64.equal a 0L || Int64.equal b 0L
  then 0L
  else if
    (Int64.equal a (-1L) && Int64.equal b Int64.min_int)
    || (Int64.equal b (-1L) && Int64.equal a Int64.min_int)
  then raise Overflow
  else
    let p = Int64.mul a b in
    if Int64.equal (Int64.div p a) b then p else raise Overflow

let rec gcd64 a b = if Int64.equal b 0L then a else gcd64 b (Int64.rem a b)

(* Floor division for [g > 0]. *)
let fdiv64 c g =
  let q = Int64.div c g in
  if Int64.compare c 0L < 0 && not (Int64.equal (Int64.rem c g) 0L)
  then Int64.sub q 1L
  else q

let to_int_ovf (x : int64) : int =
  let i = Int64.to_int x in
  if Int64.equal (Int64.of_int i) x then i else raise Overflow

let resolvent ~cp ~cn (p : Affine.t) (n : Affine.t) : Affine.t =
  let cp64 = Int64.of_int cp and cn64 = Int64.of_int cn in
  let scaled k (t : Affine.t) =
    ( mul_ovf64 k (Int64.of_int t.Affine.const),
      List.map (fun (id, c) -> id, mul_ovf64 k (Int64.of_int c)) t.Affine.terms
    )
  in
  let pc, pterms = scaled cn64 p in
  let nc, nterms = scaled cp64 n in
  let ids =
    List.sort_uniq Int.compare (List.map fst pterms @ List.map fst nterms)
  in
  let coeff id terms =
    match List.assoc_opt id terms with Some c -> c | None -> 0L
  in
  let terms =
    List.filter_map
      (fun id ->
        let c = add_ovf64 (coeff id pterms) (coeff id nterms) in
        if Int64.equal c 0L then None else Some (id, c))
      ids
  in
  let const = add_ovf64 pc nc in
  match terms with
  | [] ->
    (* A constant-only inequality [c >= 0] is decided by [c]'s sign alone, so
       the magnitude need not be representable: saturate. *)
    { Affine.const = (if Int64.compare const 0L >= 0 then 0 else -1);
      terms = []
    }
  | _ :: _ ->
    let g = List.fold_left (fun g (_, c) -> gcd64 g (Int64.abs c)) 0L terms in
    let g = if Int64.compare g 1L > 0 then g else 1L in
    { Affine.const = to_int_ovf (fdiv64 const g);
      terms = List.map (fun (id, c) -> id, to_int_ovf (Int64.div c g)) terms
    }

(* Fourier-Motzkin elimination is worst-case doubly exponential in the number of
   atoms; a guard-heavy function could make a single query explode. Bail out
   (conservatively, via {!Overflow}) once the system grows past these bounds.
   Real queries from the loop passes involve a handful of atoms and tens of
   inequalities. *)
let max_atoms = 64

let max_ineqs = 10_000

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
    if List.compare_length_with ineqs max_ineqs > 0 then raise Overflow;
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
              resolvent ~cp ~cn p n)
            neg)
        pos
    in
    List.rev_append zero resolvents
  in
  if List.compare_length_with atoms max_atoms > 0
  then true
  else
    (* If any coefficient overflows we cannot decide the system, so we
       conservatively report it as (possibly) feasible: [entails] then reports
       no entailment, which never licenses an unsound rewrite. *)
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
