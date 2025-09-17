module Make
    (Lat : Lattice_intf.LATTICE) (Ty : sig
      type t

      val compare : t -> t -> int

      val unique_id : t -> int
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
      | Param of int

    let compare (a : t) (b : t) : int =
      match a, b with
      | Atom a1, Atom a2 -> (
        match Constr.compare a1.constr a2.constr with
        | 0 -> Int.compare a1.arg_index a2.arg_index
        | c -> c)
      | Param x, Param y -> Int.compare x y
      | Atom _, Param _ -> -1
      | Param _, Atom _ -> 1

    let to_string (a : t) : string =
      match a with
      | Atom a -> Printf.sprintf "%s.%d" (Constr.to_string a.constr) a.arg_index
      | Param i -> Printf.sprintf "param%d" i

    let atomic constr arg_index = Atom { constr; arg_index }
  end

  module LSolver = Ldd.Make (Lat) (RigidName)

  type poly = LSolver.node

  type kind = LSolver.node

  (* Hash tables avoiding polymorphic structural comparison on deep values. *)
  module TyTbl = Hashtbl.Make (struct
    type t = ty

    let equal a b = Ty.compare a b = 0

    let hash t = Hashtbl.hash (Ty.unique_id t)
  end)

  module ConstrTbl = Hashtbl.Make (struct
    type t = constr

    let equal a b = Constr.compare a b = 0

    let hash x = Hashtbl.hash (Constr.to_string x)
  end)

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

  type left_right =
    | Left
    | Right

  type solver =
    { ops : mode:left_right -> ops;
      constr_kind_poly : constr -> poly * poly list
    }

  let make_solver (env : env) : solver =
    LSolver.clear_memos ();
    (* Define all the trivial ops *)
    let const l = LSolver.const l in
    let join ks = List.fold_left LSolver.join LSolver.bot ks in
    let modality l k = LSolver.meet (LSolver.const l) k in
    let meet a b = LSolver.meet a b in
    (* Create hash table mapping ty to kind for memoization *)
    let ty_to_kind = TyTbl.create 0 in
    let rigid t =
      let param = Ty.unique_id t in
      LSolver.var (LSolver.rigid (RigidName.Param param))
    in
    (* And hash table mapping constructor to coefficients *)
    let constr_to_coeffs = ConstrTbl.create 0 in
    (* Define kind_of and constr ops *)
    let rec kind_of ~mode (t : ty) : kind =
      match TyTbl.find_opt ty_to_kind t with
      | Some k -> k
      | None ->
        (* Pre-insert lattice solver var for this type *)
        let v = LSolver.new_var () in
        TyTbl.add ty_to_kind t (LSolver.var v);
        let kind = env.kind_of t (ops ~mode) in
        (* Always solve LFPs here to avoid correctness issues *)
        LSolver.solve_lfp v kind;
        kind
    and constr_kind ~mode c =
      match ConstrTbl.find_opt constr_to_coeffs c with
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
          ConstrTbl.add constr_to_coeffs c
            (LSolver.var base, List.map LSolver.var coeffs);
          (* Recursively compute the kind of the body *)
        let rigid_vars =
          List.map
            (fun ty -> LSolver.rigid (RigidName.Param (Ty.unique_id ty))) args
        in
          List.iter2
            (fun ty var -> TyTbl.add ty_to_kind ty (LSolver.var var))
            args rigid_vars;
          (* Compute body kind *)
          (* CR jujacobs: we shouldn't need to compute the kind if
             we are in Right mode and abstract=true, but currently this is
             needed to correctly populate the cache. *)
          let kind' = kind (ops ~mode) in
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
            match mode with
            | Left ->
              LSolver.enqueue_gfp base base';
              List.iteri
                (fun _i (coeff, coeff') ->
                  let rhs = LSolver.join coeff' base' in
                  LSolver.enqueue_gfp coeff rhs)
                (List.combine coeffs coeffs')
            | Right ->
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
          else (
            LSolver.solve_lfp base base';
            List.iter2
              (fun coeff coeff' -> LSolver.solve_lfp coeff coeff')
              coeffs coeffs');
          LSolver.var base, List.map LSolver.var coeffs)
    and constr ~mode c ks =
      let base, coeffs = constr_kind ~mode c in
      (* Meet each arg with the corresponding coeff *)
      let ks' =
        let nth_opt lst i = try Some (List.nth lst i) with _ -> None in
        List.mapi
          (fun i coeff ->
            let k =
              match nth_opt ks i with
              | Some k -> k
              | None -> failwith "Missing arg"
            in
            LSolver.meet k coeff)
          coeffs
      in
      (* Join all the ks'' plus the base *)
      let k' = List.fold_left LSolver.join base ks' in
      (* Return that kind *)
      k'
    and ops ~mode =
      let pp_kind (k : kind) =
        (* Print without solving pending fixpoints; pp() forces locally. *)
        LSolver.pp k
      in
      { const;
        join;
        meet;
        modality;
        constr = constr ~mode;
        kind_of = kind_of ~mode;
        rigid;
        pp_kind
      }
    in
    let constr_kind_poly ~mode c =
      let base, coeffs = constr_kind ~mode c in
      (* Ensure any pending fixpoints are installed before inspecting. *)
      LSolver.solve_pending ();
      let base_norm = base in
      let coeff_norms = List.map (fun coeff -> coeff) coeffs in
      let coeffs_minus_base =
        List.map (fun p -> LSolver.sub_subsets p base_norm) coeff_norms
      in
      base_norm, coeffs_minus_base
    in
    { ops; constr_kind_poly = constr_kind_poly ~mode:Right }

  let normalize (solver : solver) (k : ckind) : poly =
    k (solver.ops ~mode:Right)

  let constr_kind_poly (solver : solver) (c : constr) : poly * poly list =
    solver.constr_kind_poly c

  let leq (solver : solver) (k1 : ckind) (k2 : ckind) : bool =
    let k2' = k2 (solver.ops ~mode:Right) in
    let k1' = k1 (solver.ops ~mode:Left) in
    LSolver.leq k1' k2'

  let leq_with_reason (solver : solver) (k1 : ckind) (k2 : ckind) :
      int list option =
    let k2' = k2 (solver.ops ~mode:Right) in
    let k1' = k1 (solver.ops ~mode:Left) in
    LSolver.leq_with_reason k1' k2'

  let round_up (solver : solver) (k : ckind) : lat =
    let k' = k (solver.ops ~mode:Left) in
    LSolver.round_up k'

  let clear_memos () : unit = LSolver.clear_memos ()

  let pp (p : poly) : string =
    (* Do not solve pending here; pp() forces locally. *)
    LSolver.pp p

  let pp_debug (p : poly) : string = LSolver.pp_debug p
end
