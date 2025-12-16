(* This forces ikinds globally on. *)
Clflags.ikinds := true;

(* Global feature toggles for the ikinds experiment.
   These are intended to be easy to flip while iterating on
   performance or correctness. *)
Types.ikind_debug := false
let enable_crossing = true
let enable_sub_jkind_l = true
let enable_sub_or_intersect = false
let enable_sub_or_error = false
let reset_constructor_ikind_on_substitution = false

module Ldd = Ikind.Ldd

module JK = struct
  (* A JKind/ikind solver specialized to [Ikind.Ldd] and [Types.type_expr].

     The solver computes LDD polynomials of the form
       base ⊔ Σ_i (arg_i ⊓ coeff_i)
     where [base] is the intrinsic kind of a constructor and each [coeff_i]
     describes the contribution coming from the i-th type argument.
     Least fixed points are used to interpret recursive types. *)

  module Ldd = Ikind.Ldd

  type ty = Types.type_expr

  type constr = Ldd.constr

  type lat = Ldd.lat

  type poly = Ldd.node

  type kind = Ldd.node

  (* Hash tables avoiding polymorphic structural comparison on deep values.
     We key types by their unique [Types.get_id] to keep lookups cheap. *)
  module TyTbl = Hashtbl.Make (struct
    type t = ty

    let equal a b = Int.equal (Types.get_id a) (Types.get_id b)

    let hash t = Hashtbl.hash (Types.get_id t)
  end)

  let constr_to_string (p : constr) : string = Format.asprintf "%a" Path.print p

  module ConstrTbl = Hashtbl.Make (struct
    type t = constr

    let equal a b = Path.compare a b = 0

    let hash x = Path.hash x
  end)

  type ckind = ctx -> kind

  and constr_decl =
    | Ty of
        { args : ty list;
          kind : ckind;
          abstract : bool
        }
    | Poly of poly * poly array

  and env =
    { kind_of : ctx -> ty -> kind;
      lookup : constr -> constr_decl
    }

  and ctx =
    { env : env;
      ty_to_kind : kind TyTbl.t;
      constr_to_coeffs : (poly * poly array) ConstrTbl.t
    }

  (* Start a new solver context.
     LDD memo tables are global; clearing them keeps solves independent. *)
  let create (env : env) : ctx =
    Ldd.clear_memos ();
    { env;
      ty_to_kind = TyTbl.create 64;
      constr_to_coeffs = ConstrTbl.create 64
    }

  (* A rigid variable corresponding to a type parameter [t]. *)
  let rigid (t : ty) : kind =
    let param = Types.get_id t in
    Ldd.var (Ldd.rigid (Ldd.Name.param param))

  (* Compute the kind for [t], memoizing results to preserve sharing.
     For recursive types we install a fresh placeholder var, then solve a
     least fixed point against the RHS produced by [env.kind_of]. *)
  let rec kind (ctx : ctx) (t : ty) : kind =
    match TyTbl.find_opt ctx.ty_to_kind t with
    | Some k -> k
    | None ->
        let v = Ldd.new_var () in
        let placeholder = Ldd.var v in
        TyTbl.add ctx.ty_to_kind t placeholder;
        let rhs = ctx.env.kind_of ctx t in
        Ldd.solve_lfp v rhs;
        rhs

  (* Fetch or compute the polynomial for constructor [c].  The returned
     nodes are placeholders stored in [constr_to_coeffs] so that mutually
     recursive constructors can refer to each other. *)
  and constr_kind (ctx : ctx) (c : constr) : poly * poly array =
    match ConstrTbl.find_opt ctx.constr_to_coeffs c with
    | Some base_and_coeffs -> base_and_coeffs
    | None -> (
        match ctx.env.lookup c with
        | Poly (base, coeffs) ->
            (* Install placeholder nodes before rehydrating cached
               polynomials.  This breaks recursion cycles between
               mutually-recursive types. *)
            let base_var = Ldd.new_var () in
            let coeff_vars =
              Array.init (Array.length coeffs) (fun _ -> Ldd.new_var ())
            in
            let base_node = Ldd.var base_var in
            let coeff_nodes = Array.map Ldd.var coeff_vars in
            ConstrTbl.add ctx.constr_to_coeffs c (base_node, coeff_nodes);
            (* Replace rigid atoms that refer to other constructors with the
               corresponding cached placeholders.  Atoms that refer back to
               [c] are kept rigid to avoid infinite expansion. *)
            let instantiate (name : Ldd.Name.t) : kind =
              match name with
              | Ldd.Name.Param _  | Ldd.Name.Unknown _ -> Ldd.var (Ldd.rigid name)
              | Ldd.Name.Atom { constr = constr'; arg_index } ->
                  if Path.compare constr' c = 0
                  then Ldd.var (Ldd.rigid name)
                  else
                    let base', coeffs' = constr_kind ctx constr' in
                    if arg_index = 0
                    then base'
                    else
                      if arg_index - 1 < Array.length coeffs'
                      then coeffs'.(arg_index - 1)
                      else Ldd.var (Ldd.rigid name)
            in
            let rehydrate node = Ldd.map_rigid instantiate node in
            let base_rhs = rehydrate base in
            let coeffs_rhs = Array.map rehydrate coeffs in
            Ldd.solve_lfp base_var base_rhs;
            Array.iter2 (fun v rhs -> Ldd.solve_lfp v rhs) coeff_vars coeffs_rhs;
            base_node, coeff_nodes
        | Ty { args; kind = body; abstract } ->
            let base_var = Ldd.new_var () in
            let coeff_vars =
              Array.init (List.length args) (fun _ -> Ldd.new_var ())
            in
            let base_node = Ldd.var base_var in
            let coeff_nodes = Array.map Ldd.var coeff_vars in
            ConstrTbl.add ctx.constr_to_coeffs c (base_node, coeff_nodes);
            let rigid_vars =
              List.map
                (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty)))
                args
            in
            (* Treat parameters as rigid vars while computing [kind'] so that
               the result is linear in those vars. *)
            List.iter2
              (fun ty var -> TyTbl.add ctx.ty_to_kind ty (Ldd.var var))
              args rigid_vars;
            (* Compute body kind *)
            (* CR jujacobs: still compute the kind in Right mode to keep
               the cache consistent. *)
            let kind' = body ctx in
            (* Decompose [kind'] into a base and one coefficient
               per parameter. *)
            let base', coeffs' =
              Ldd.decompose_linear ~universe:rigid_vars kind'
            in
            let coeffs' = Array.of_list coeffs' in
            if Array.length coeff_vars <> Array.length coeffs'
            then
              failwith
                (Printf.sprintf
                   "jkind_solver: coeffs mismatch for constr %s (length %d vs %d)"
                   (constr_to_string c) (Array.length coeff_vars)
                   (Array.length coeffs'));
            if abstract
            then (
              (* For abstract types we don't trust [kind'] as an exact formula.
                 Instead we relate the placeholders to it via GFP bounds, while
                 keeping the original rigid atoms around as conservative
                 unknowns. *)
              Ldd.enqueue_gfp base_var
                (Ldd.meet base' (Ldd.var (Ldd.rigid (Ldd.Name.atomic c 0))));
              let i = ref 0 in
              Array.iter2
                (fun coeff coeff' ->
                  let idx = !i in
                  let rhs = Ldd.join coeff' base' in
                  let bound =
                    Ldd.meet rhs
                      (Ldd.var (Ldd.rigid (Ldd.Name.atomic c (idx + 1))))
                  in
                  incr i;
                  Ldd.enqueue_gfp coeff bound)
                coeff_vars coeffs')
            else (
              Ldd.solve_lfp base_var base';
              Array.iter2
                (fun coeff coeff' -> Ldd.solve_lfp coeff coeff')
                coeff_vars coeffs');
            base_node, coeff_nodes)

  (* Apply a constructor polynomial to argument kinds. *)
  let constr (ctx : ctx) (c : constr) (args : kind list) : kind =
    let base, coeffs = constr_kind ctx c in
    let args = Array.of_list args in
    let ks =
      Array.mapi
        (fun i coeff ->
          if i < Array.length args
          then Ldd.meet args.(i) coeff
          else failwith "Missing arg")
        coeffs
    in
    Array.fold_left Ldd.join base ks

  (* Evaluate a ckind in [ctx] and flush pending GFP constraints. *)
  let normalize (k : kind) : poly =
    Ldd.solve_pending ();
    k

  (* Materialize a solved polynomial for storing in
     [Types.constructor_ikind]. *)
  let constr_kind_poly (ctx : ctx) (c : constr) : poly * poly array =
    let base, coeffs = constr_kind ctx c in
    Ldd.solve_pending ();
    base, coeffs

  let leq_with_reason (k1 : kind) (k2 : kind) :
      int list option =
    Ldd.solve_pending ();
    Ldd.leq_with_reason k1 k2

  let round_up (k : kind) : lat =
    Ldd.round_up k
end
let ikind_reset : string -> Types.type_ikind = Types.ikind_reset

(* Converting surface jkinds to solver ckinds. *)
let ckind_of_jkind (ctx : JK.ctx) (j : ('l * 'r) Types.jkind) : JK.kind =
  (* Base is the modality bounds stored on this jkind. *)
  let base =
    Ldd.const (Axis_lattice_conv.of_mod_bounds j.jkind.mod_bounds)
  in
  (* For each with-bound (ty, axes), contribute
     modality(axes_mask, kind_of ty). *)
  Jkind.With_bounds.to_seq j.jkind.with_bounds
  |> Seq.fold_left (fun acc (ty, info) ->
         let axes = Jkind.With_bounds.type_info_relevant_axes info in
         let mask = Axis_lattice.of_axis_set axes in
         let kty = JK.kind ctx ty in
         Ldd.join acc (Ldd.meet (Ldd.const mask) kty))
       base

let ckind_of_jkind_l (ctx : JK.ctx) (j : Types.jkind_l) : JK.kind = ckind_of_jkind ctx j

let ckind_of_jkind_r (j : Types.jkind_r) : JK.kind =
  (* For r-jkinds used in sub checks, with-bounds are not present
     on the right (see Jkind_desc.sub's precondition). So only the
     base mod-bounds matter. *)
  Ldd.const (Axis_lattice_conv.of_mod_bounds j.jkind.mod_bounds)

(* Guards against accidental infinite recursion when traversing types. *)
let kind_of_depth = ref 0

let kind_of_counter = ref 0

(* Compute the ikind polynomial for an arbitrary [type_expr].  This is the
   semantic counterpart of [Jkind.jkind_of_type], but expressed in LDD form. *)
let kind_of (ctx : JK.ctx) (ty : Types.type_expr) : JK.kind =
  incr kind_of_depth;
  if !kind_of_depth > 500 then failwith "kind_of_depth too deep" else ();
  incr kind_of_counter;
  if !kind_of_counter > 100000000 then failwith "kind_of_counter too big" else ();
  let res =
    (* [ty] is expected to be representative: no links/substs/fields/nil. *)
    match Types.get_desc ty with
    | Types.Tvar { name = _name; jkind } | Types.Tunivar { name = _name; jkind }
      -> 
      (* TODO: allow general jkinds here (including with-bounds) *)
      let jkind_l = Jkind.disallow_right jkind in
      (* Keep a rigid param, but cap it by its annotated jkind. *)
      Ldd.meet (JK.rigid ty) (ckind_of_jkind_l ctx jkind_l)
    | Types.Tconstr (p, args, _abbrev_memo) ->
      let arg_kinds = List.map (fun t -> JK.kind ctx t) args in
      JK.constr ctx p arg_kinds
    | Types.Ttuple elts ->
      (* Boxed tuples: immutable_data base + per-element contributions
         under id modality. *)
      let base = Ldd.const Axis_lattice.immutable_data in
      List.fold_left
        (fun acc (_lbl, t) ->
          let mask = Axis_lattice.mask_shallow in
          Ldd.join acc (Ldd.meet (Ldd.const mask) (JK.kind ctx t)))
        base elts
    | Types.Tunboxed_tuple elts ->
      (* Unboxed tuples: per-element contributions; shallow axes relevant
         only for arity = 1. *)
      let mask =
        match List.length elts with
        | 1 -> Axis_lattice.top  (* arity 1: include all axes *)
        | _ -> Axis_lattice.mask_shallow  (* arity > 1: exclude shallow axes *)
      in
      List.fold_left
        (fun acc (_lbl, t) ->
          Ldd.join acc (Ldd.meet (Ldd.const mask) (JK.kind ctx t)))
        (Ldd.const Axis_lattice.bot) elts
    | Types.Tarrow (_lbl, _t1, _t2, _commu) ->
      (* Arrows use the dedicated per-axis bounds (no with-bounds). *)
      Ldd.const Axis_lattice.arrow
    | Types.Tlink _ -> failwith "Tlink shouldn't appear in kind_of"
    | Types.Tsubst _ -> failwith "Tsubst shouldn't appear in kind_of"
    | Types.Tpoly (ty, _) ->
      JK.kind ctx ty
    | Types.Tof_kind jkind ->
      ckind_of_jkind ctx jkind
    | Types.Tobject _ ->
      Ldd.const Axis_lattice.object_legacy
    | Types.Tfield _ ->
      failwith "Tfield shouldn't appear in kind_of"
      (* Ldd.const Axis_lattice.value *)
    | Types.Tnil ->
      failwith "Tnil shouldn't appear in kind_of"
      (* Ldd.const Axis_lattice.value *)
    | Types.Tquote _ | Types.Tsplice _ ->
      (* Treat quoted/spliced types conservatively as boxed values. *)
      Ldd.const Axis_lattice.value
    | Types.Tvariant row ->
      if Btype.tvariant_not_immediate row
      then
        if Btype.static_row row
        then
          (* Closed, boxed polymorphic variant: immutable_data base plus
             per-constructor args. *)
          let base = Ldd.const Axis_lattice.immutable_data in
          let mask = Axis_lattice.mask_shallow in
          Btype.fold_row
            (fun acc ty ->
              let k_ty = JK.kind ctx ty in
              let k = Ldd.meet (Ldd.const mask) k_ty in
              Ldd.join acc k)
            base row
        else
          (* Open row: conservative non-float value (boxed) intersected with an
             unknown rigid so the solver treats it as an unknown element. *)
          let unknown =
            Ldd.var (Ldd.rigid (Ldd.Name.fresh_unknown ()))
          in
          Ldd.meet (Ldd.const Axis_lattice.nonfloat_value) unknown
      else
        (* All-constant (immediate) polymorphic variant. *)
        Ldd.const Axis_lattice.immediate
    | Types.Tpackage _ ->
      Ldd.const Axis_lattice.nonfloat_value
  in
  decr kind_of_depth;
  res

let has_mutable_label lbls =
  List.exists
    (fun (lbl : Types.label_declaration) ->
      match lbl.ld_mutable with Immutable -> false | Mutable _ -> true)
    lbls

let relevance_of_rep = function
  | `Record Types.Record_unboxed
  | `Record (Types.Record_inlined (_, _, Types.Variant_unboxed)) -> `Relevant
  | `Variant Types.Variant_unboxed -> `Relevant
  | (`Record _ | `Variant _) -> `Irrelevant

let constructor_ikind_polynomial
    (packed : Types.constructor_ikind) : JK.poly * JK.poly array =
  packed.base, packed.coeffs

(* Lookup function supplied to the solver.
   We prefer a stored ikind (when present) and otherwise recompute from the
   type declaration in [context]. *)
let lookup_of_context ~(context : Jkind.jkind_context) (p : Path.t) :
    JK.constr_decl =
  (* Note: this currently ignores any GADT-installed equations. *)
  match context.lookup_type p with
  | None ->
    Format.eprintf "ERROR: unknown constructor %a@." Path.print p;
    (* Fallback for unknown constructors: treat them as abstract,
       non-recursive values. *)
    let kind : JK.ckind = fun _ctx -> Ldd.const Axis_lattice.value in
    JK.Ty { args = []; kind; abstract = true }
  | Some decl ->
    (* Here we can switch to using the cached ikind or not. *)
    let fallback () =
      (* When we have no stored ikind, we go to this fallback and compute. *)
      match decl.type_manifest with
      | Some body_ty ->
        (* Concrete: compute kind of body. *)
        let args = decl.type_params in
        let kind : JK.ckind =
         fun ctx ->
          JK.kind ctx body_ty
        in
        JK.Ty { args; kind; abstract = false }
      | None ->
        begin
        (* No manifest: may still be "concrete" (record/variant/...). 
           Build ckind. *)
        let allow_any_crossing =
          match decl.type_kind with
          | Types.Type_record (_, _, umc_opt)
          | Types.Type_record_unboxed_product (_, _, umc_opt)
          | Types.Type_variant (_, _, umc_opt) ->
            Option.is_some umc_opt
          | Types.Type_abstract _ | Types.Type_open -> false
        in
        let use_decl_jkind () = 
          let kind : JK.ckind = fun ctx -> ckind_of_jkind_l ctx decl.type_jkind in
          JK.Ty { args = decl.type_params; kind; abstract = true } in
        (* If we cannot soundly derive a polynomial from components, fall back
           to the stored jkind and mark it abstract. *)
        match decl.type_kind with
        | _ when allow_any_crossing -> use_decl_jkind ()
        | Types.Type_abstract _ -> use_decl_jkind ()
        | Types.Type_record (lbls, rep, _umc_opt) ->
          (* Build from components: base (non-float value) + per-label
             contributions. *)
          let base_lat =
            if has_mutable_label lbls
            then Axis_lattice.mutable_data
            else
              match rep with
              | Types.Record_unboxed -> Axis_lattice.immediate
              | _ -> Axis_lattice.immutable_data
          in
          let relevant_for_shallow = relevance_of_rep (`Record rep) in
          let kind : JK.ckind =
           fun (ctx : JK.ctx) ->
            let base = Ldd.const base_lat in
            List.fold_left
              (fun acc (lbl : Types.label_declaration) ->
                let mask =
                  Axis_lattice.mask_of_modality
                    ~relevant_for_shallow
                    lbl.ld_modalities
                in
                Ldd.join acc
                  (Ldd.meet (Ldd.const mask) (JK.kind ctx lbl.ld_type)))
              base lbls
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_record_unboxed_product (lbls, _rep, _umc_opt) ->
          (* Unboxed products: non-float base; shallow axes relevant only
             for arity = 1. *)
          let base_lat =
            if has_mutable_label lbls
            then Axis_lattice.mutable_data
            else Axis_lattice.immediate
          in
          let kind : JK.ckind =
           fun (ctx : JK.ctx) ->
            let base = Ldd.const base_lat in
            let relevant_for_shallow =
              match List.length lbls with
              | 1 -> `Relevant
              | _ -> `Irrelevant
            in
            List.fold_left
              (fun acc (lbl : Types.label_declaration) ->
                let mask =
                  Axis_lattice.mask_of_modality
                    ~relevant_for_shallow lbl.ld_modalities
                in
                Ldd.join acc
                  (Ldd.meet (Ldd.const mask) (JK.kind ctx lbl.ld_type)))
              base lbls
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_variant (cstrs, rep, _umc_opt) ->
          (* Choose base: immediate for void-only variants; mutable if any
             record constructor has a mutable field; otherwise immutable. *)
          let all_args_void =
            List.for_all
              (fun (c : Types.constructor_declaration) ->
                match c.cd_args with
                | Types.Cstr_tuple args ->
                  List.for_all
                    (fun (arg : Types.constructor_argument) ->
                      Jkind_types.Sort.Const.all_void arg.ca_sort)
                    args
                | Types.Cstr_record lbls ->
                  List.for_all
                    (fun (lbl : Types.label_declaration) ->
                      Jkind_types.Sort.Const.all_void lbl.ld_sort)
                    lbls)
              cstrs
          in
          let has_mutable =
            List.exists
              (fun (c : Types.constructor_declaration) ->
                match c.cd_args with
                | Types.Cstr_tuple _ -> false
                | Types.Cstr_record lbls -> has_mutable_label lbls)
              cstrs
          in
          let base_lat =
            if all_args_void
            then Axis_lattice.immediate
            else if has_mutable
            then Axis_lattice.mutable_data
            else Axis_lattice.immutable_data
          in
          let relevant_for_shallow = relevance_of_rep (`Variant rep) in
          let kind : JK.ckind =
           fun (ctx : JK.ctx) ->
            let base = Ldd.const base_lat in
            List.fold_left
              (fun acc (c : Types.constructor_declaration) ->
                match c.cd_args with
                | Types.Cstr_tuple args ->
                  List.fold_left
                    (fun acc (arg : Types.constructor_argument) ->
                      let mask =
                        Axis_lattice.mask_of_modality
                          ~relevant_for_shallow
                          arg.ca_modalities
                      in
                      Ldd.join acc
                        (Ldd.meet (Ldd.const mask) (JK.kind ctx arg.ca_type)))
                    acc args
                | Types.Cstr_record lbls ->
                  List.fold_left
                    (fun acc (lbl : Types.label_declaration) ->
                      let mask =
                        Axis_lattice.mask_of_modality
                          ~relevant_for_shallow
                          lbl.ld_modalities
                      in
                      Ldd.join acc
                        (Ldd.meet (Ldd.const mask) (JK.kind ctx lbl.ld_type)))
                    acc lbls)
              base cstrs
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_open ->
          (* Use the stored jkind here in case it is `exn`,
             which is special. *)
          let kind : JK.ckind = fun ctx -> ckind_of_jkind_l ctx decl.type_jkind in
          JK.Ty { args = decl.type_params; kind; abstract = false }
          (* 
          (* This is the code we'd use otherwise *)
          let kind : JK.ckind =
           fun _ctx ->
            Ldd.const Axis_lattice.nonfloat_value
          in
          JK.Ty { args = decl.type_params; kind; abstract = false } 
          *)
        end
    in
    (* Prefer a stored constructor ikind if one is present and enabled. *)
    let ikind = match decl.type_ikind with
    | Types.Constructor_ikind constructor when !Clflags.ikinds ->
      let base, coeffs =
        constructor_ikind_polynomial constructor
      in
      JK.Poly (base, coeffs)
    | Types.No_constructor_ikind reason ->
      (* Print the reason *)
      (*= Format.eprintf "[ikind-miss] %s@." reason; *)
      ignore reason;
      fallback ()
    | Types.Constructor_ikind _ -> fallback () in 
    if !Types.ikind_debug
    then begin
      let ikind_msg =
        match ikind with
        | JK.Ty _ -> "Ty"
        | JK.Poly (base, coeffs) ->
            let coeffs =
              coeffs |> Array.map Ikind.Ldd.pp |> Array.to_list |> String.concat "; "
            in
            Format.asprintf "Poly(base=%s; coeffs=[%s])"
              (Ikind.Ldd.pp base) coeffs
      in
      Format.eprintf "[ikind] %a: %s@." Path.print p ikind_msg
    end;
    ikind

(* Package the above into a full evaluation context. *)
let make_ctx ~(context : Jkind.jkind_context) : JK.ctx =
  JK.create
    { kind_of = kind_of; lookup = lookup_of_context ~context }

let normalize ~(context : Jkind.jkind_context) (jkind : Types.jkind_l) :
    Ikind.Ldd.node =
  let ctx = make_ctx ~context in
  JK.normalize (ckind_of_jkind_l ctx jkind)

let type_declaration_ikind ~(context : Jkind.jkind_context)
    ~(path : Path.t) : Types.constructor_ikind =
  let ctx = make_ctx ~context in
  let base, coeffs = JK.constr_kind_poly ctx path in
  Types.constructor_ikind_create ~base ~coeffs

let type_declaration_ikind_gated ~(context : Jkind.jkind_context)
    ~(path : Path.t) : Types.type_ikind =
  if not !Clflags.ikinds
  then Types.ikind_reset "ikinds disabled"
  else
    let ikind = type_declaration_ikind ~context ~path in
    let payload = ikind in
    if !Types.ikind_debug
    then begin
      let stored_jkind =
        match context.lookup_type path with
        | None -> "?"
        | Some decl -> Format.asprintf "%a" Jkind.format decl.type_jkind
      in
      Format.eprintf "[ikind] %a: stored=%s, base=%s, coeffs=[%s]@."
        Path.print path
        stored_jkind
        (Ikind.Ldd.pp payload.base)
        (String.concat "; " (Array.to_list (Array.map Ikind.Ldd.pp payload.coeffs)))
    end;
    Types.Constructor_ikind ikind

let type_declaration_ikind_of_jkind ~(context : Jkind.jkind_context)
    ~(params : Types.type_expr list) (type_jkind : Types.jkind_l) :
    Types.type_ikind =
  if not !Clflags.ikinds
  then Types.ikind_reset "ikinds disabled"
  else
    let poly = normalize ~context type_jkind in
    let rigid_vars =
      List.map (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty))) params
    in
    let base, coeffs = Ldd.decompose_linear ~universe:rigid_vars poly in
    let coeffs = Array.of_list coeffs in
    let payload = Types.constructor_ikind_create ~base ~coeffs in
    if !Types.ikind_debug
    then begin
      Format.eprintf "[ikind] from jkind: base=%s; coeffs=[%s]@."
        (Ikind.Ldd.pp payload.base)
        (String.concat "; "
           (Array.to_list (Array.map Ikind.Ldd.pp payload.coeffs)))
    end;
    Types.Constructor_ikind payload

let sub_jkind_l ?allow_any_crossing ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) ~level (sub : Types.jkind_l)
    (super : Types.jkind_l) : (unit, Jkind.Violation.t) result =
  let open Misc.Stdlib.Monad.Result.Syntax in
  (* Check layouts first; if that fails, print both sides with full
     info and return the error. *)
  let* () =
    match Jkind.sub_jkind_l_layout ~context ~level sub super with
    | Ok () -> Ok ()
    | Error v -> Error v
  in
  if not (enable_sub_jkind_l && !Clflags.ikinds)
  then
    Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context ~level sub
      super
  else
    let allow_any =
      match allow_any_crossing with Some true -> true | _ -> false
    in
    let origin_suffix =
      match origin with
      | None -> ""
      | Some o -> " origin=" ^ o
    in
    if allow_any
    then (
      if !Types.ikind_debug
      then
        Format.eprintf
          "[ikind-subjkind] call%s allow_any=true@;sub=%a@;super=%a@."
          origin_suffix Jkind.format sub Jkind.format super;
      Ok ())
    else
      let ctx = make_ctx ~context in
      let sub_poly = ckind_of_jkind_l ctx sub in
      let super_poly = ckind_of_jkind_l ctx super in
      Ldd.solve_pending ();
      if !Types.ikind_debug
      then
        Format.eprintf
          "[ikind-subjkind] call%s allow_any=false@;sub=%a@;super=%a@;@;\
           sub_poly=%s@;super_poly=%s@."
          origin_suffix
          Jkind.format sub
          Jkind.format super
          (Ldd.pp sub_poly)
          (Ldd.pp super_poly);
      let ik_leq = Ldd.leq_with_reason sub_poly super_poly in
      match ik_leq with
      | None -> Ok ()
      | Some violating_axes ->
        let () =
          if !Types.ikind_debug
          then
            let axes =
              violating_axes
              |> List.map Axis_lattice.axis_number_to_axis_packed
              |> List.map
                   (fun (Jkind_axis.Axis.Pack ax) -> Jkind_axis.Axis.name ax)
              |> String.concat ", "
            in
            Format.eprintf
              "[ikind-subjkind] failure on axes: %s@;sub=%a@;super=%a@."
              axes Jkind.format sub Jkind.format super
        in
        (* Do not try to adjust allowances; Violation.Not_a_subjkind
           accepts an r-jkind. *)
        let axis_reasons =
          List.map
            (fun axis ->
              let axis_name =
                Axis_lattice.axis_number_to_axis_packed axis
              in
              Jkind.Sub_failure_reason.Axis_disagreement axis_name)
            violating_axes
        in
        Error
          (Jkind.Violation.of_ ~context
             (Jkind.Violation.Not_a_subjkind
                ( sub,
                  super,
                  axis_reasons )))

let sub ?origin ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) ~level (sub : Types.jkind_l)
    (super : Types.jkind_r) : bool =
  let _ = type_equal, origin, level in
  if not !Clflags.ikinds
  then Jkind.sub ~type_equal ~context ~level sub super
  else
    let ctx = make_ctx ~context in
    match
      JK.leq_with_reason (ckind_of_jkind_l ctx sub) (ckind_of_jkind_r super)
    with
    | None -> true
    | Some _ -> false

(* CR jujacobs: this is really slow when enabled. Fix performance. *)
let crossing_of_jkind ~(context : Jkind.jkind_context)
    (jkind : ('l * 'r) Types.jkind) : Mode.Crossing.t =
  if not (enable_crossing && !Clflags.ikinds) (* CR jujacobs: fix this *)
  then Jkind.get_mode_crossing ~context jkind
  else
    let ctx = make_ctx ~context in
    let lat = JK.round_up (ckind_of_jkind ctx jkind) in
  let mb = Axis_lattice_conv.to_mod_bounds lat in
    Jkind.Mod_bounds.to_mode_crossing mb

(* Intentionally no ikind versions of sub_or_intersect / sub_or_error.
   Keep Jkind as the single source for classification and error reporting. *)
(* CR jujacobs: fix this *)
type sub_or_intersect = Jkind.sub_or_intersect

(* CR jujacobs: performance issue here. *)
let sub_or_intersect ?origin:_origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) ~level
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) : sub_or_intersect =
  if not (enable_sub_or_intersect && !Clflags.ikinds) (* CR jujacobs: fix this *)
  then Jkind.sub_or_intersect ~type_equal ~context ~level t1 t2
  else
    (* CR jujacobs: enable this *)
    let _ik =
      sub ~type_equal ~context ~level (Jkind.disallow_right t1)
        (Jkind.disallow_left t2)
    in
    (* Preserve canonical Jkind classification for now. *)
    Jkind.sub_or_intersect ~type_equal ~context ~level t1 t2

let sub_or_error ?origin:_origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) ~level
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) :
    (unit, Jkind.Violation.t) result =
  if not (enable_sub_or_error && !Clflags.ikinds)
  then Jkind.sub_or_error ~type_equal ~context ~level t1 t2
  else if sub ~type_equal ~context ~level (Jkind.disallow_right t1)
            (Jkind.disallow_left t2)
  then Ok ()
  else
    (* Delegate to Jkind for detailed error reporting. *)
    Jkind.sub_or_error ~type_equal ~context ~level t1 t2

(** Substitute constructor ikinds according to [lookup] without requiring
    Env. *)

(* Collect, for each constructor path, the maximum arity at which it occurs in
   the given type/jkind.  This drives how many coefficients the identity
   environment must provide. *)
let rec max_arity_in_jkind (acc : int Path.Map.t) (jkind : _ Types.jkind) =
  Jkind.With_bounds.to_seq jkind.jkind.with_bounds
  |> Seq.fold_left (fun acc (ty, _) -> max_arity_in_type acc ty) acc

and max_arity_in_type (acc : int Path.Map.t) (ty : Types.type_expr) =
  let open Types in
  match get_desc ty with
  | Tvar { jkind; _ } | Tunivar { jkind; _ } ->
      max_arity_in_jkind acc jkind
  | Tconstr (p, args, _) ->
      let n = List.length args in
      let prev = match Path.Map.find_opt p acc with None -> 0 | Some m -> m in
      let acc = if n > prev then Path.Map.add p n acc else acc in
      List.fold_left max_arity_in_type acc args
  | Ttuple elts ->
      List.fold_left
        (fun a (_, t) -> max_arity_in_type a t) acc elts
  | Tunboxed_tuple elts ->
      List.fold_left
        (fun a (_, t) -> max_arity_in_type a t) acc elts
  | Tarrow (_, t1, t2, _) ->
      max_arity_in_type (max_arity_in_type acc t1) t2
  | Tpoly (t, ts) ->
      List.fold_left max_arity_in_type (max_arity_in_type acc t) ts
  | Tobject (t, name) ->
      let acc = max_arity_in_type acc t in
      (match !name with
       | None -> acc
       | Some (_, tl) -> List.fold_left max_arity_in_type acc tl)
  | Tfield (_, _, t1, t2) ->
      max_arity_in_type (max_arity_in_type acc t1) t2
  | Tvariant row ->
      let acc = Btype.fold_row max_arity_in_type acc row in
      max_arity_in_type acc (row_more row)
  | Tpackage (_, fields) ->
      List.fold_left
        (fun acc (_n, t) -> max_arity_in_type acc t) acc fields
  | Tquote t | Tsplice t -> max_arity_in_type acc t
  | Tlink t -> max_arity_in_type acc t
  | Tsubst (t, row_opt) ->
      let acc = max_arity_in_type acc t in
      (match row_opt with
       | None -> acc
       | Some row -> max_arity_in_type acc row)
  | Tof_kind jkind -> max_arity_in_jkind acc jkind
  | Tnil -> acc

let identity_lookup_from_arity_map (arity : int Path.Map.t) (p : Path.t)
    : JK.constr_decl =
  let open Ldd in
  let n = match Path.Map.find_opt p arity with None -> 0 | Some m -> m in
  (* Identity polynomial: base and coeffs are just fresh rigid atoms. *)
  let base = var (rigid (Name.atomic p 0)) in
  let coeffs =
    Array.init n (fun i -> var (rigid (Name.atomic p (i + 1))))
  in
  JK.Poly (base, coeffs)

let poly_of_type_function_in_identity_env ~(params : Types.type_expr list)
    ~(body : Types.type_expr) : JK.poly * JK.poly array =
  (* Approximate type-function substitution by evaluating in an identity
     environment, i.e. every constructor contributes an independent rigid
     atom. *)
  let arity = max_arity_in_type Path.Map.empty body in
  let lookup p = identity_lookup_from_arity_map arity p in
  let kind_of_identity = kind_of in
  let env : JK.env = { kind_of = kind_of_identity; lookup } in
  let ctx = JK.create env in
  let poly = JK.normalize (kind_of_identity ctx body)
  in
  let rigid_vars =
    List.map (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty))) params
  in
  let base, coeffs = Ldd.decompose_linear ~universe:rigid_vars poly in
  base, Array.of_list coeffs

let substitute_decl_ikind_with_lookup
    ~(lookup :
        Path.t ->
        [ `Path of Path.t
        | `Type_fun of Types.type_expr list * Types.type_expr ] option)
    (entry : Types.type_ikind) : Types.type_ikind =
  match entry with
  | Types.No_constructor_ikind _ -> entry
  | Types.Constructor_ikind _ when reset_constructor_ikind_on_substitution ->
      Types.ikind_reset "ikind substitution reset"
  | Types.Constructor_ikind packed ->
      let payload = packed in
      let memo :
        (Path.t, (JK.poly * JK.poly array)) Hashtbl.t =
        Hashtbl.create 17
      in
      (* Rewrite a polynomial by mapping each rigid atom through [lookup]. *)
      let rec map_poly (expanding : Path.Set.t) (poly : Ldd.node) : Ldd.node =
        Ldd.map_rigid (map_name expanding) poly
      and map_name (expanding : Path.Set.t) (name : Ldd.Name.t) : Ldd.node =
        match name with
        | Ldd.Name.Param _ -> Ldd.var (Ldd.rigid name)
        | Ldd.Name.Unknown _ -> Ldd.var (Ldd.rigid name)
        | Ldd.Name.Atom { constr = p; arg_index } -> (
            match lookup p with
            | None -> Ldd.var (Ldd.rigid name)
            | Some (`Path q) ->
                Ldd.var (Ldd.rigid (Ldd.Name.atomic q arg_index))
            | Some (`Type_fun _ as tf) ->
                (* Inline a type function by evaluating it in an identity
                   environment.  The [expanding] set prevents infinite
                   unfolding of recursive type functions. *)
                if Path.Set.mem p expanding
                then Ldd.var (Ldd.rigid name)
                else
                  let base_raw, coeffs_raw =
                    match tf with
                    | `Type_fun (params, body) ->
                        (* Memoized by [p] to avoid recomputation *)
                        (match Hashtbl.find_opt memo p with
                         | Some v -> v
                         | None ->
                             let v =
                               poly_of_type_function_in_identity_env
                                 ~params ~body
                             in
                             Hashtbl.add memo p v; v)
                  in
                  let expanding = Path.Set.add p expanding in
                  let base = map_poly expanding base_raw in
                  let coeffs = Array.map (map_poly expanding) coeffs_raw in
                  if arg_index = 0
                  then base
                  else
                    if arg_index - 1 < Array.length coeffs
                    then coeffs.(arg_index - 1)
                    else
                      (* Fallback: if coefficient missing, keep original
                         atom. *)
                      Ldd.var (Ldd.rigid name))
      in
      let base' = map_poly Path.Set.empty payload.base in
      let coeffs' = Array.map (map_poly Path.Set.empty) payload.coeffs in
      let payload =
        Types.constructor_ikind_create ~base:base' ~coeffs:coeffs'
      in
      Types.Constructor_ikind payload
