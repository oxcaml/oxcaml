(* ------------------------------------------- Lattice polynomials in DNF over
   variables V with coefficients in lattice C. Term = (S, c) representing (c /\
   (/\_{v in S} v)). Representation (canonical): Map<VarSet, coeff_hat>, where
   coeff_hat[S] = c[S] \ bigvee_{T ⊂ S} coeff_hat[T] using co_sub as "lattice
   subtraction". ------------------------------------------- *)

module type LATTICE = Lattice_intf.LATTICE

module type ORDERED = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make (C : LATTICE) (V : ORDERED) = struct
  let bump (name : string) = Global_counters.inc ("LP." ^ name)

  module VarSet = Set.Make (V)
  module VarMap = Map.Make (V)

  module VarSetKey = struct
    type t = VarSet.t

    let compare = VarSet.compare
  end

  module SetMap = Map.Make (VarSetKey)

  type coeff = C.t

  type vars = VarSet.t

  type term = vars * coeff

  (* Canonical polynomial: map from variable-set to canonical coefficient. *)
  type t = coeff SetMap.t

  (* Equality on coefficients via lattice order; avoids requiring physical
     equality. *)
  let eqc (a : coeff) (b : coeff) = C.equal a b

  let is_bot c = eqc c C.bot

  (* Merge helper: union with coefficient join. *)
  let merge_join (a : t) (b : t) : t =
    SetMap.union (fun _ x y -> Some (C.join x y)) a b

  (* Build a singleton canonical polynomial (S, c); drops if c = ⊥. *)
  let singleton (s : vars) (c : coeff) : t =
    if is_bot c then SetMap.empty else SetMap.add s c SetMap.empty

  (* Constant polynomial (empty set is the neutral element for /\ over
     variables). *)
  let const (c : coeff) : t =
    bump "const";
    singleton VarSet.empty c

  let bot : t = SetMap.empty

  let top : t = const C.top

  (* Variable as a polynomial: x = (⊤ /\ x) *)
  let var (v : V.t) : t =
    bump "var";
    singleton (VarSet.singleton v) C.top

  (* Turn arbitrary terms (possibly with duplicate sets) into a map where
     duplicates are joined. *)
  let of_terms (ts : term list) : t =
    bump "of_terms";
    List.fold_left
      (fun acc (s, c) ->
        if is_bot c
        then acc
        else
          let prev =
            match SetMap.find_opt s acc with Some x -> x | None -> C.bot
          in
          let c' = C.join prev c in
          if is_bot c' then SetMap.remove s acc else SetMap.add s c' acc)
      SetMap.empty ts

  let size_of (p : t) : int =
    bump "size_of";
    SetMap.fold (fun k _ acc -> acc + VarSet.cardinal k) p 0

  (* Canonicalization: Given a (non-canonical) map m, compute coeff_hat[S] =
     c[S] \ bigvee_{T ⊂ S} coeff_hat[T]. *)
  let canonicalize (m : t) : t =
    (* Gather non-bot entries and sort by increasing |S| then lex. *)
    let items =
      SetMap.bindings m
      |> List.filter (fun (_s, c) -> not (is_bot c))
      |> List.map (fun (s, c) -> VarSet.cardinal s, s, c)
    in
    let cmp (d1, s1, _) (d2, s2, _) =
      let c = Int.compare d1 d2 in
      if c <> 0 then c else VarSet.compare s1 s2
    in
    let sorted = List.sort cmp items in
    (* Build canonical map by increasing size. *)
    let res = ref SetMap.empty in
    List.iter
      (fun (_deg, s, c) ->
        (* Join of all canonical coefficients on strict subsets T ⊂ S *)
        let lower =
          SetMap.fold
            (fun t ct acc ->
              if t != s && VarSet.subset t s then C.join acc ct else acc)
            !res C.bot
        in
        let c_hat = C.co_sub c lower in
        if not (is_bot c_hat) then res := SetMap.add s c_hat !res)
      sorted;
    bump ("canonicalize.sizeof=" ^ string_of_int (size_of !res));
    !res

  (* of_list: Build from a list of terms and return canonical form *)
  let of_list ts =
    bump "of_list";
    canonicalize (of_terms ts)

  let to_list (p : t) : term list =
    bump "to_list";
    (* Sorted by (cardinality, lexicographic) *)
    let with_deg =
      SetMap.bindings p |> List.map (fun (s, c) -> VarSet.cardinal s, s, c)
    in
    let cmp (d1, s1, _) (d2, s2, _) =
      let c = Int.compare d1 d2 in
      if c <> 0 then c else VarSet.compare s1 s2
    in
    List.sort cmp with_deg |> List.map (fun (_, s, c) -> s, c)

  (* Lattice operations on polynomials *)
  let join (p : t) (q : t) : t =
    bump "join";
    canonicalize (merge_join p q)

  let meet (p : t) (q : t) : t =
    bump "meet";
    (* Distribute: (Σ_S c_S /\ ∧S) /\ (Σ_T d_T /\ ∧T) = Σ_{S,T} (c_S /\ d_T) /\
       ∧(S∪T), then canonicalize. *)
    let acc = ref SetMap.empty in
    SetMap.iter
      (fun s cs ->
        SetMap.iter
          (fun t dt ->
            let u = VarSet.union s t in
            let cd = C.meet cs dt in
            if not (is_bot cd)
            then
              let prev =
                match SetMap.find_opt u !acc with Some x -> x | None -> C.bot
              in
              let cd' = C.join prev cd in
              if is_bot cd'
              then acc := SetMap.remove u !acc
              else acc := SetMap.add u cd' !acc)
          q)
      p;
    canonicalize !acc

  (* Equality and order on canonical polynomials *)
  let equal (p : t) (q : t) : bool =
    bump "equal";
    (* Both assumed canonical; compare key sets and per-key coefficients via
       lattice-equality. *)
    let rec eq_bindings xs ys =
      match xs, ys with
      | [], [] -> true
      | (s1, c1) :: xs', (s2, c2) :: ys' ->
        VarSet.compare s1 s2 = 0 && eqc c1 c2 && eq_bindings xs' ys'
      | _ -> false
    in
    eq_bindings (SetMap.bindings p) (SetMap.bindings q)

  let hash (p : t) = Stdlib.Hashtbl.hash p

  let leq (p : t) (q : t) : bool =
    bump "leq";
    (* p ≤ q iff join p q = q (in canonical form) *)
    equal (join p q) q

  (* Support (variables appearing in any term) *)
  let support (p : t) : VarSet.t =
    bump "support";
    SetMap.fold (fun s _ acc -> VarSet.union s acc) p VarSet.empty

  (* Substitution: replace some variables with polynomials and normalize. For a
     term (S, c), split S into kept K and replaced R. Result = (K, c) /\ (/\_{v
     in R} subs[v]) = fold meet starting from singleton (K,c). *)
  let subst ~(subs : t VarMap.t) (p : t) : t =
    bump "subst";
    let add_poly acc poly = merge_join acc poly in
    let res = ref SetMap.empty in
    SetMap.iter
      (fun s c ->
        let kept =
          VarSet.fold
            (fun v acc -> if VarMap.mem v subs then acc else VarSet.add v acc)
            s VarSet.empty
        in
        let replaced =
          VarSet.fold
            (fun v acc -> if VarMap.mem v subs then v :: acc else acc)
            s []
        in
        (* Start with the residual term (kept, c) *)
        let base = singleton kept c in
        (* Sequentially meet with each substituted polynomial *)
        let expanded =
          List.fold_left
            (fun acc v -> meet acc (VarMap.find v subs))
            base replaced
        in
        res := add_poly !res expanded)
      p;
    canonicalize !res

  let subst1 ~(v : V.t) ~(by : t) (p : t) : t =
    bump "subst1";
    subst ~subs:(VarMap.add v by VarMap.empty) p

  (* q_down s = ⋁_{T ⊆ s} q̂[T] *)
  let q_down (q : t) (s : vars) : coeff =
    SetMap.fold
      (fun t ct acc -> if VarSet.subset t s then C.join acc ct else acc)
      q C.bot

  (* Approximate co-Heyting subtraction: r ≈ least x such that p ≤ q ∨ x. Result
     is canonical, but may be an over-approximation. *)
  let co_sub_approx (p : t) (q : t) : t =
    bump "co_sub";
    if SetMap.is_empty p
    then SetMap.empty
    else if SetMap.is_empty q
    then p (* p \ ⊥ = p *)
    else
      let raw =
        SetMap.fold
          (fun s cp acc ->
            let cover = q_down q s in
            let r = C.co_sub cp cover in
            if is_bot r then acc else SetMap.add s r acc)
          p SetMap.empty
      in
      canonicalize raw

  (* Evaluate a polynomial given a valuation for variables *)
  let eval (rho : V.t -> C.t) (p : t) : C.t =
    bump "eval";
    SetMap.fold
      (fun s c acc ->
        let var_val = VarSet.fold (fun v acc' -> C.meet acc' (rho v)) s C.top in
        C.join acc (C.meet c var_val))
      p C.bot

  (* Evaluate with all variables set to ⊤ (ceil) or ⊥ (floor). *)
  let ceil (p : t) : C.t =
    bump "ceil";
    eval (fun _ -> C.top) p

  let floor (p : t) : C.t =
    bump "floor";
    eval (fun _ -> C.bot) p

  (* Pretty printer with deterministic ordering. - Prints ⊥ for empty
     polynomial. - Prints ⊤ for constant-top. - Omits unnecessary meets with ⊤
     and joins with ⊥ (the latter never appear in canonical form). *)
  let pp (p : t) : string =
    bump "pp";
    let pp_coeff = C.to_string in
    let pp_var = V.to_string in
    if SetMap.is_empty p
    then "⊥"
    else
      let terms = to_list p in
      let n_terms = List.length terms in
      let term_body (s : vars) (c : coeff) : string * bool =
        (* returns (body, has_meet) *)
        let vs =
          VarSet.elements s |> List.map pp_var |> List.sort String.compare
        in
        let is_top = C.equal c C.top in
        match vs, is_top with
        | [], true -> "⊤", false
        | [], false -> pp_coeff c, false
        | _ :: _, true -> String.concat " ⊓ " vs, List.length vs > 1
        | _ :: _, false ->
          let body = pp_coeff c ^ " ⊓ " ^ String.concat " ⊓ " vs in
          body, true
      in
      let items =
        terms
        |> List.map (fun (s, c) ->
               let body, has_meet = term_body s c in
               body, has_meet)
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in
      items
      |> List.map (fun (body, has_meet) ->
             if n_terms > 1 && has_meet then "(" ^ body ^ ")" else body)
      |> String.concat " ⊔ "

  let to_string (p : t) : string =
    bump "to_string";
    pp p

  (* Backward-compat alias to emphasize approximation semantics. *)
  let co_sub = co_sub_approx

  let find_non_bot_axis (_p : t) : int option = None
end
