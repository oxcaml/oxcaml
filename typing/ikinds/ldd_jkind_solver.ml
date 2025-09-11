module Make
    (Lat : Lattice_intf.LATTICE) (Ty : sig
      type t

      val compare : t -> t -> int

      val to_string : t -> string
    end) (Constr : sig
      type t

      val compare : t -> t -> int

      val to_string : t -> string
    end) =
struct
  type ty = Ty.t

  type constr = Constr.t

  type lat = Lat.t

  type atom =
    { constr : constr;
      arg_index : int
    }

  module RigidName = struct
    type t =
      | Atom of atom
      | Ty of ty

    let compare (a : t) (b : t) : int =
      match a, b with
      | Atom a1, Atom a2 -> (
        match Constr.compare a1.constr a2.constr with
        | 0 -> Int.compare a1.arg_index a2.arg_index
        | c -> c)
      | Ty x, Ty y -> Ty.compare x y
      | Atom _, Ty _ -> -1
      | Ty _, Atom _ -> 1

    let to_string (a : t) : string =
      match a with
      | Atom a -> Printf.sprintf "%s.%d" (Constr.to_string a.constr) a.arg_index
      | Ty v -> Printf.sprintf "<%s>" (Ty.to_string v)

    let atomic constr arg_index = Atom { constr; arg_index }
  end

  module LSolver = Ldd.Make (Lat) (RigidName)

  type poly = LSolver.node

  type kind = LSolver.node

  type ops =
    { const : lat -> kind;
      join : kind list -> kind;
      meet : kind -> kind -> kind;
      modality : lat -> kind -> kind;
      constr : constr -> kind list -> kind;
      kind_of : ty -> kind;
      rigid : ty -> kind;
      pp_kind : kind -> string
    }

  type ckind = ops -> kind

  type constr_decl =
    | Ty of
        { args : ty list;
          kind : ckind;
          abstract : bool
        }
    | Poly of poly * poly list

  type env =
    { kind_of : ty -> ckind;
      lookup : constr -> constr_decl
    }

  type solver =
    { ops : ops;
      constr_kind_poly : constr -> poly * poly list
    }

  let make_solver (env : env) : solver =
    LSolver.clear_memos ();
    let enqueue_lfp_enabled =
      match Sys.getenv_opt "JKINDS_ENQUEUE_LFP" with
      | Some s ->
        let s = String.lowercase_ascii s in
        s = "1" || s = "true" || s = "yes"
      | None -> false
    in
    (* Define all the trivial ops *)
    let const l = LSolver.const l in
    let join ks = List.fold_left LSolver.join LSolver.bot ks in
    let modality l k = LSolver.meet (LSolver.const l) k in
    let meet a b = LSolver.meet a b in
    let rigid t = LSolver.var (LSolver.rigid (RigidName.Ty t)) in
    (* Create hash table mapping ty to kind for memoization *)
    let ty_to_kind = Hashtbl.create 0 in
    (* And hash table mapping constructor to coefficients *)
    let constr_to_coeffs = Hashtbl.create 0 in
    (* Define kind_of and constr ops *)
    let rec kind_of (t : ty) : kind =
      match Hashtbl.find_opt ty_to_kind t with
      | Some k -> k
      | None ->
        (* Pre-insert lattice solver var for this type *)
        let v = LSolver.new_var () in
        Hashtbl.add ty_to_kind t (LSolver.var v);
        let kind = env.kind_of t ops in
        (* Always solve LFPs here to avoid correctness issues *)
        LSolver.solve_lfp v kind;
        kind
    and constr_kind c =
      match Hashtbl.find_opt constr_to_coeffs c with
      | Some base_and_coeffs -> base_and_coeffs
      | None -> (
        match env.lookup c with
        | Poly (base, coeffs) -> base, coeffs
        | Ty { args; kind; abstract } ->
          let base = LSolver.new_var () in
          (* Allocate coefficient vars based on declared arity, not on ks
             length *)
          let coeffs =
            List.init (List.length args) (fun _ -> LSolver.new_var ())
          in
          Hashtbl.add constr_to_coeffs c
            (LSolver.var base, List.map LSolver.var coeffs);
          (* Recursively compute the kind of the body *)
          let rigid_vars =
            List.map (fun ty -> LSolver.rigid (RigidName.Ty ty)) args
          in
          List.iter2
            (fun ty var -> Hashtbl.add ty_to_kind ty (LSolver.var var))
            args rigid_vars;
          (* Compute body kind *)
          let kind' = kind ops in
          (* Extract coeffs' from kind' *)
          let base', coeffs' =
            LSolver.decompose_linear ~universe:rigid_vars kind'
          in
          if List.length coeffs <> List.length coeffs'
          then
            failwith
              (Printf.sprintf
                 "jkind_solver: coeffs mismatch for constr %s (length %d vs %d)"
                 (Constr.to_string c) (List.length coeffs) (List.length coeffs'));
          if abstract
          then (
            (* We need to assert that kind' is less than or equal to the base *)
            LSolver.enqueue_gfp base
              (LSolver.meet base'
                 (LSolver.var (LSolver.rigid (RigidName.atomic c 0))));
            List.iteri
              (fun i (coeff, coeff') ->
                let rhs = LSolver.join coeff' base' in
                let bound =
                  LSolver.meet rhs
                    (LSolver.var (LSolver.rigid (RigidName.atomic c (i + 1))))
                in
                LSolver.enqueue_gfp coeff bound)
              (List.combine coeffs coeffs'))
          else if (* We need to solve for the coeffs *)
                  enqueue_lfp_enabled
          then (
            LSolver.enqueue_lfp base base';
            List.iter2
              (fun coeff coeff' -> LSolver.enqueue_lfp coeff coeff')
              coeffs coeffs')
          else (
            LSolver.solve_lfp base base';
            List.iter2
              (fun coeff coeff' -> LSolver.solve_lfp coeff coeff')
              coeffs coeffs');
          LSolver.var base, List.map LSolver.var coeffs)
    and constr c ks =
      let base, coeffs = constr_kind c in
      (* Meet each arg with the corresponding coeff *)
      let ks' =
        (* Meet each provided argument with its coeff; missing args are ⊥. *)
        let nth_opt lst i = try Some (List.nth lst i) with _ -> None in
        List.mapi
          (fun i coeff ->
            let k =
              match nth_opt ks i with Some k -> k | None -> LSolver.bot
            in
            LSolver.meet k coeff)
          coeffs
      in
      (* Join all the ks'' plus the base *)
      let k' = List.fold_left LSolver.join base ks' in
      (* Return that kind *)
      k'
    and ops =
      let pp_kind (k : kind) =
        (* Print without solving pending fixpoints; pp() forces locally. *)
        LSolver.pp k
      in
      { const; join; meet; modality; constr; kind_of; rigid; pp_kind }
    in
    let constr_kind_poly c =
      let base, coeffs = constr_kind c in
      (* Ensure any pending fixpoints are installed before inspecting. *)
      LSolver.solve_pending ();
      let base_norm = base in
      let coeff_norms = List.map (fun coeff -> coeff) coeffs in
      let coeffs_minus_base =
        List.map (fun p -> LSolver.sub_subsets p base_norm) coeff_norms
      in
      base_norm, coeffs_minus_base
    in
    { ops; constr_kind_poly }

  let normalize (solver : solver) (k : ckind) : poly = k solver.ops

  let constr_kind_poly (solver : solver) (c : constr) : poly * poly list =
    solver.constr_kind_poly c

  let leq (solver : solver) (k1 : ckind) (k2 : ckind) : bool =
    let k1' = k1 solver.ops in
    let k2' = k2 solver.ops in
    LSolver.leq k1' k2'

  let leq_with_reason (solver : solver) (k1 : ckind) (k2 : ckind) : int option =
    let k1' = k1 solver.ops in
    let k2' = k2 solver.ops in
    LSolver.leq_with_reason k1' k2'

  let round_up (solver : solver) (k : ckind) : lat =
    let k' = k solver.ops in
    LSolver.round_up k'

  let clear_memos () : unit = LSolver.clear_memos ()

  let pp (p : poly) : string =
    (* Do not solve pending here; pp() forces locally. *)
    LSolver.pp p

  let pp_debug (p : poly) : string =
    LSolver.pp_debug p
end
