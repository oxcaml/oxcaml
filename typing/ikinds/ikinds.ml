(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This forces ikinds globally on. *)
Clflags.ikinds := true;
(* CR jujacobs: set this to false before merging. *)

(* Global feature toggles for the ikinds experiment.
   These are intended to be easy to flip while iterating on
   performance or correctness. *)
(* CR jujacobs: remove toggles in the final version. *)
Types.ikind_debug := false

let enable_crossing = true

let enable_sub_jkind_l = true

let enable_sub_or_intersect = false

let enable_sub_or_error = false

let reset_constructor_ikind_on_substitution = false

module Ldd = Ikind.Ldd

(** A kind solver specialized to [Ikind.Ldd] and [Types.type_expr].

      The solver computes LDD polynomials of the form
        base ⊔ Σ_i (arg_i ⊓ coeff_i)
      where [base] is the intrinsic kind of a constructor and each [coeff_i]
      describes the contribution coming from the i-th type argument. *)
module Solver = struct

  type mode =
    | Normal
    | Round_up

  (* Hash tables avoiding polymorphic structural comparison on deep values.
     [Btype.TypeHash] keys by the representative of a [type_expr], so 
     union-find aliases map to a single entry. This table is used to cache
     repeated kind_of computations, as well as to make circular types work. *)
  module TyTbl = Btype.TypeHash

  let constr_to_string (path : Path.t) : string =
    Format.asprintf "%a" Path.print path

  (* Hash table for caching constructor kinds. *)
  module ConstrTbl = Path.Tbl

  (** Kind function for constructors: computes a kind from a context.
      This is used because many kinds don't make sense outside of a 
      context, e.g., the kind of a type containing a constructor 
      depends on the context telling us what its kind is. *)
  type ckind = ctx -> Ldd.node

  (** Result of constructor lookup.
      [Ty] describes a constructor declaration with arguments and a kind
      function; [Poly] provides a cached polynomial form. *)
  and constr_decl =
    | Ty of
        { args : Types.type_expr list;
          kind : ckind;
          abstract : bool
        }
    | Poly of Ldd.node * Ldd.node array

  (* The environment supplies constructor lookup so callers can use
     alternative environments (e.g., identity environments when inlining
     type functions). *)
  and env = { lookup : Path.t -> constr_decl }

  and ctx =
    { env : env;
      mode : mode;
      ty_to_kind : Ldd.node TyTbl.t;
      constr_to_coeffs : (Ldd.node * Ldd.node array) ConstrTbl.t
    }

  (** Start a new solver context. *)
  let create_ctx ~(mode : mode) (env : env) : ctx =
    { env;
      mode;
      ty_to_kind = TyTbl.create 0;
      constr_to_coeffs = ConstrTbl.create 0
    }

  let reset_for_mode (ctx : ctx) ~(mode : mode) : ctx = { ctx with mode }

  let rigid_name (ctx : ctx) (name : Ldd.Name.t) : Ldd.node =
    match ctx.mode with
    | Normal -> Ldd.node_of_var (Ldd.rigid name)
    | Round_up -> Ldd.const Axis_lattice.top

  (** A rigid variable corresponding to a type parameter [t]. *)
  let rigid (ctx : ctx) (ty : Types.type_expr) : Ldd.node =
    let param_id = Types.get_id ty in
    rigid_name ctx (Ldd.Name.param param_id)

  let type_may_be_circular (ty : Types.type_expr) : bool =
    match Types.get_desc ty with
    | Types.Tvariant _ -> true
    | Types.Tconstr _ -> true
    | Types.Tobject _ -> true
    | _ -> false

  (** Fetch or compute the polynomial for constructor [c]. *)
  let rec constr_kind (ctx : ctx) (path : Path.t)
      : Ldd.node * Ldd.node array =
    (* Return placeholder nodes stored in [constr_to_coeffs] for recursion. *)
    match ConstrTbl.find_opt ctx.constr_to_coeffs path with
    | Some base_and_coeffs -> base_and_coeffs
    | None -> (
      match ctx.env.lookup path with
      | Poly (base, coeffs) ->
        (* Install placeholder nodes before rehydrating cached
           polynomials.  This breaks recursion cycles between
           mutually-recursive types. *)
        let base_var = Ldd.new_var () in
        let coeff_vars =
          Array.init (Array.length coeffs) (fun _ -> Ldd.new_var ())
        in
        let base_poly = Ldd.node_of_var base_var in
        let coeffs_poly = Array.map Ldd.node_of_var coeff_vars in
        ConstrTbl.add ctx.constr_to_coeffs path (base_poly, coeffs_poly);
        (* Replace rigid atoms that refer to other constructors with the
           corresponding cached placeholders.  Atoms that refer back to
           [c] are kept rigid to avoid infinite expansion. *)
        let instantiate (name : Ldd.Name.t) : Ldd.node =
          match name with
          | Ldd.Name.Param _ | Ldd.Name.Unknown _ -> rigid_name ctx name
          | Ldd.Name.Atom { constr = other_path; arg_index } ->
            if Path.same other_path path
            then rigid_name ctx name
            else
              let base_poly, coeffs_poly = constr_kind ctx other_path in
              if arg_index = 0
              then base_poly
              else if arg_index - 1 < Array.length coeffs_poly
              then coeffs_poly.(arg_index - 1)
              else rigid_name ctx name
        in
        let rehydrate poly = Ldd.map_rigid instantiate poly in
        let base_rhs = rehydrate base in
        let coeffs_rhs = Array.map rehydrate coeffs in
        Ldd.solve_lfp base_var base_rhs;
        Array.iter2 (fun v rhs -> Ldd.solve_lfp v rhs) coeff_vars coeffs_rhs;
        base_poly, coeffs_poly
      | Ty { args = params; kind = body; abstract } ->
        let base_var = Ldd.new_var () in
        let coeff_vars =
          Array.init (List.length params) (fun _ -> Ldd.new_var ())
        in
        let base_poly = Ldd.node_of_var base_var in
        let coeffs_poly = Array.map Ldd.node_of_var coeff_vars in
        ConstrTbl.add ctx.constr_to_coeffs path (base_poly, coeffs_poly);
        let rigid_vars =
          List.map
            (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty)))
            params
        in
        (* Treat parameters as rigid vars while computing [kind'] so that
           the result is linear in those vars. *)
        List.iter2
          (fun ty var -> TyTbl.add ctx.ty_to_kind ty (Ldd.node_of_var var))
          params rigid_vars;
        (* Compute body kind *)
        (* CR jujacobs: still compute the kind in Right mode to keep
           the cache consistent. *)
        let body_kind = body ctx in
        (* Decompose [body_kind] into a base and one coefficient
           per parameter. *)
        let base_rhs, coeffs_rhs_list =
          Ldd.decompose_into_linear_terms ~universe:rigid_vars body_kind
        in
        let coeffs_rhs = Array.of_list coeffs_rhs_list in
        if Array.length coeff_vars <> Array.length coeffs_rhs
        then
          failwith
            (Printf.sprintf
               "jkind_solver: coeffs mismatch for constr %s (length %d vs %d)"
               (constr_to_string path)
               (Array.length coeff_vars)
               (Array.length coeffs_rhs));
        if abstract
        then (
          (* For abstract types we don't trust [kind'] as an exact formula.
             Instead we relate the placeholders to it via GFP bounds, while
             keeping the original rigid atoms around as conservative
             unknowns. *)
          Ldd.enqueue_gfp base_var
            (Ldd.meet base_rhs (rigid_name ctx (Ldd.Name.atomic path 0)));
          Array.iteri
            (fun idx coeff_var ->
              let coeff_rhs = coeffs_rhs.(idx) in
              let rhs = Ldd.join coeff_rhs base_rhs in
              let bound =
                Ldd.meet rhs (rigid_name ctx (Ldd.Name.atomic path (idx + 1)))
              in
              Ldd.enqueue_gfp coeff_var bound)
            coeff_vars)
        else (
          Ldd.solve_lfp base_var base_rhs;
          Array.iter2
            (fun coeff_var coeff_rhs -> Ldd.solve_lfp coeff_var coeff_rhs)
            coeff_vars coeffs_rhs);
        base_poly, coeffs_poly)

  (* Apply a constructor polynomial to argument kinds. *)
  let constr (ctx : ctx) (path : Path.t) (arg_kinds : Ldd.node list)
      : Ldd.node =
    let base, coeffs = constr_kind ctx path in
    let rec loop acc remaining i =
      if i = Array.length coeffs
      then acc
      else
        match remaining with
        | arg_kind :: rest ->
          loop (Ldd.join acc (Ldd.meet arg_kind coeffs.(i))) rest (i + 1)
        | [] -> failwith "Missing arg"
    in
    loop base arg_kinds 0

  (* Converting surface jkinds to solver ckinds. *)
  let ckind_of_jkind_with_kind
      (kind_fn : ctx -> Types.type_expr -> Ldd.node) (ctx : ctx)
      (jkind : ('l * 'r) Types.jkind) : Ldd.node =
    (* Base is the modality bounds stored on this jkind. *)
    let base =
      Ldd.const (Axis_lattice_conv.of_mod_bounds jkind.jkind.mod_bounds)
    in
    (* For each with-bound (ty, axes), contribute
       modality(axes_mask, kind_of ty). *)
    Jkind.With_bounds.to_seq jkind.jkind.with_bounds
    |> Seq.fold_left
         (fun acc (ty, bound_info) ->
           let axes = bound_info.Types.With_bounds_type_info.relevant_axes in
           let mask = Axis_lattice.of_axis_set axes in
           let ty_kind = kind_fn ctx ty in
           Ldd.join acc (Ldd.meet (Ldd.const mask) ty_kind))
         base

  (* Guards against accidental infinite recursion when traversing types. *)
  let kind_of_depth = ref 0

  let kind_of_counter = ref 0

  (** Compute the kind for [t]. *)
  let rec kind (ctx : ctx) (ty : Types.type_expr) : Ldd.node =
    (* Memoize only potentially cyclic types; LFPs handle recursion. *)
    match TyTbl.find_opt ctx.ty_to_kind ty with
    | Some kind_poly -> kind_poly
    | None ->
      if type_may_be_circular ty
      then (
        let var = Ldd.new_var () in
        let placeholder = Ldd.node_of_var var in
        TyTbl.add ctx.ty_to_kind ty placeholder;
        let kind_rhs = kind_of ctx ty in
        Ldd.solve_lfp var kind_rhs;
        placeholder)
      else
        let kind_rhs = kind_of ctx ty in
        TyTbl.add ctx.ty_to_kind ty kind_rhs;
        kind_rhs

  (* Compute the ikind polynomial for an arbitrary [type_expr].  This is the
     semantic counterpart of [Jkind.jkind_of_type], but expressed in LDD
     form. *)
  and kind_of (ctx : ctx) (ty : Types.type_expr) : Ldd.node =
    incr kind_of_depth;
    if !kind_of_depth > 500 then failwith "kind_of_depth too deep" else ();
    incr kind_of_counter;
    if !kind_of_counter > 100000000
    then failwith "kind_of_counter too big"
    else ();
    let kind_poly =
      (* [ty] is expected to be representative: no links/substs/fields/nil. *)
      match Types.get_desc ty with
      | Types.Tvar { name = _name; jkind }
      | Types.Tunivar { name = _name; jkind } ->
        (* TODO: allow general jkinds here (including with-bounds) *)
        let jkind_l = Jkind.disallow_right jkind in
        (* Keep a rigid param, but cap it by its annotated jkind. *)
        Ldd.meet (rigid ctx ty)
          (ckind_of_jkind_with_kind kind ctx jkind_l)
      | Types.Tconstr (path, args, _abbrev_memo) ->
        let arg_kinds = List.map (fun t -> kind ctx t) args in
        constr ctx path arg_kinds
      | Types.Ttuple elts ->
        (* Boxed tuples: immutable_data base + per-element contributions
           under id modality. *)
        let base = Ldd.const Axis_lattice.immutable_data in
        let mask = Ldd.const Axis_lattice.mask_shallow in
        List.fold_left
          (fun acc (_lbl, t) -> Ldd.join acc (Ldd.meet mask (kind ctx t)))
          base elts
      | Types.Tunboxed_tuple elts ->
        (* Unboxed tuples: per-element contributions; shallow axes relevant
           only for arity = 1. *)
        let mask =
          match elts with
          | [_] -> Axis_lattice.top (* arity 1: include all axes *)
          | _ -> Axis_lattice.mask_shallow (* arity > 1: exclude shallow axes *)
        in
        let mask = Ldd.const mask in
        List.fold_left
          (fun acc (_lbl, t) -> Ldd.join acc (Ldd.meet mask (kind ctx t)))
          (Ldd.const Axis_lattice.bot)
          elts
      | Types.Tarrow (_lbl, _t1, _t2, _commu) ->
        (* Arrows use the dedicated per-axis bounds (no with-bounds). *)
        Ldd.const Axis_lattice.arrow
      | Types.Tlink _ -> failwith "Tlink shouldn't appear in kind_of"
      | Types.Tsubst _ -> failwith "Tsubst shouldn't appear in kind_of"
      | Types.Tpoly (ty, _) -> kind ctx ty
      | Types.Tof_kind jkind ->
        ckind_of_jkind_with_kind kind ctx jkind
      | Types.Tobject _ -> Ldd.const Axis_lattice.object_legacy
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
            let mask = Ldd.const Axis_lattice.mask_shallow in
            Btype.fold_row
              (fun acc ty ->
                let ty_kind = kind ctx ty in
                let contribution = Ldd.meet mask ty_kind in
                Ldd.join acc contribution)
              base row
          else
            (* Open row: conservative non-float value (boxed) intersected with
               an unknown rigid so the solver treats it as an unknown
               element. *)
            let unknown = rigid_name ctx (Ldd.Name.fresh_unknown ()) in
            Ldd.meet (Ldd.const Axis_lattice.nonfloat_value) unknown
        else
          (* All-constant (immediate) polymorphic variant. *)
          Ldd.const Axis_lattice.immediate
      | Types.Tpackage _ -> Ldd.const Axis_lattice.nonfloat_value
    in
    decr kind_of_depth;
    kind_poly

  let ckind_of_jkind (ctx : ctx) (jkind : ('l * 'r) Types.jkind)
      : Ldd.node =
    ckind_of_jkind_with_kind kind ctx jkind

  let ckind_of_jkind_l (ctx : ctx) (j : Types.jkind_l) : Ldd.node =
    ckind_of_jkind ctx j

  let ckind_of_jkind_r (jkind : Types.jkind_r) : Ldd.node =
    (* For r-jkinds used in sub checks, with-bounds are not present
       on the right (see Jkind_desc.sub's precondition). So only the
       base mod-bounds matter. *)
    Ldd.const (Axis_lattice_conv.of_mod_bounds jkind.jkind.mod_bounds)

  (* Evaluate a ckind in [ctx] and flush pending GFP constraints. *)
  let normalize (kind_poly : Ldd.node) : Ldd.node =
    Ldd.solve_pending ();
    kind_poly

  (* Materialize a solved polynomial for storing in
     [Types.constructor_ikind]. *)
  let constr_kind_poly (ctx : ctx) (c : Path.t)
      : Ldd.node * Ldd.node array =
    let base, coeffs = constr_kind ctx c in
    Ldd.solve_pending ();
    base, coeffs

  let leq_with_reason (left : Ldd.node) (right : Ldd.node) :
      Jkind_axis.Axis.packed list =
    Ldd.leq_with_reason left right

  let round_up (k : Ldd.node) : Axis_lattice.t = Ldd.round_up k
end

let constructor_ikind ~base ~coeffs : Types.constructor_ikind =
  (* Keep coefficients disjoint from the base (subtract-normal form). *)
  for i = 0 to Array.length coeffs - 1 do
    let coeff = coeffs.(i) in
    let coeff' = Ldd.sub_subsets coeff base in
    if coeff != coeff' then coeffs.(i) <- coeff'
  done;
  ({ Types.base = base; coeffs } : Types.constructor_ikind)

let ckind_of_jkind (ctx : Solver.ctx) (jkind : ('l * 'r) Types.jkind) :
    Ldd.node =
  Solver.ckind_of_jkind ctx jkind

let ckind_of_jkind_l (ctx : Solver.ctx) (j : Types.jkind_l) : Ldd.node =
  Solver.ckind_of_jkind_l ctx j

let ckind_of_jkind_r (jkind : Types.jkind_r) : Ldd.node =
  Solver.ckind_of_jkind_r jkind

let kind_of (ctx : Solver.ctx) (ty : Types.type_expr) : Ldd.node =
  Solver.kind_of ctx ty

let has_mutable_label lbls =
  List.exists
    (fun (lbl : Types.label_declaration) ->
      match lbl.ld_mutable with Immutable -> false | Mutable _ -> true)
    lbls

let relevance_of_rep = function
  | `Record Types.Record_unboxed
  | `Record (Types.Record_inlined (_, _, Types.Variant_unboxed)) ->
    `Relevant
  | `Variant Types.Variant_unboxed -> `Relevant
  | `Record _ | `Variant _ -> `Irrelevant

let constructor_ikind_polynomial (packed : Types.constructor_ikind) :
    Ldd.node * Ldd.node array =
  packed.base, packed.coeffs

(* Lookup function supplied to the solver.
   We prefer a stored ikind (when present) and otherwise recompute from the
   type declaration in [context]. *)
let lookup_of_context ~(context : Jkind.jkind_context) (path : Path.t) :
    Solver.constr_decl =
  (* Note: this currently ignores any GADT-installed equations. *)
  match context.lookup_type path with
  | None ->
    (* Format.eprintf "ERROR: unknown constructor %a@." Path.print path; *)
    (* WE CANNOT ACTUALLY GIVE AN ERROR HERE! *)
    (* Explanation: build systems sometimes heuristically do not
       include all cmis for performance reasons. Because of that,
       we could encounter types that appear not to exist. We must
       treat those as abstract unknowns. *)
    (* Fallback for unknown constructors: treat them as abstract,
       non-recursive values. *)
    let unknown = Ikind.Ldd.Name.fresh_unknown () in
    let kind : Solver.ckind = fun _ctx -> Ldd.node_of_var (Ldd.rigid unknown) in
    Solver.Ty { args = []; kind; abstract = true }
  | Some type_decl ->
    (* Here we can switch to using the cached ikind or not. *)
    let fallback () =
      (* When we have no stored ikind, we go to this fallback and compute. *)
      match type_decl.type_manifest with
      | Some body_ty ->
        (* Concrete: compute kind of body. *)
        let args = type_decl.type_params in
        let kind : Solver.ckind = fun ctx -> Solver.kind ctx body_ty in
        Solver.Ty { args; kind; abstract = false }
      | None -> (
        (* No manifest: may still be "concrete" (record/variant/...).
           Build ckind. *)
        let allow_any_crossing =
          match type_decl.type_kind with
          | Types.Type_record (_, _, umc_opt)
          | Types.Type_record_unboxed_product (_, _, umc_opt)
          | Types.Type_variant (_, _, umc_opt) ->
            Option.is_some umc_opt
          | Types.Type_abstract _ | Types.Type_open -> false
        in
        let use_decl_jkind () =
          let kind : Solver.ckind =
           fun ctx -> ckind_of_jkind_l ctx type_decl.type_jkind
          in
          Solver.Ty { args = type_decl.type_params; kind; abstract = true }
        in
        (* If we cannot soundly derive a polynomial from components, fall back
           to the stored jkind and mark it abstract. *)
        match type_decl.type_kind with
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
          let kind : Solver.ckind =
           fun (ctx : Solver.ctx) ->
            let base = Ldd.const base_lat in
            List.fold_left
              (fun acc (lbl : Types.label_declaration) ->
                let mask =
                  Axis_lattice.mask_of_modality ~relevant_for_shallow
                    lbl.ld_modalities
                in
                Ldd.join acc
                  (Ldd.meet (Ldd.const mask) (Solver.kind ctx lbl.ld_type)))
              base lbls
          in
          Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        | Types.Type_record_unboxed_product (lbls, _rep, _umc_opt) ->
          (* Unboxed products: non-float base; shallow axes relevant only
             for arity = 1. *)
          let base_lat =
            if has_mutable_label lbls
            then Axis_lattice.mutable_data
            else Axis_lattice.immediate
          in
          let kind : Solver.ckind =
           fun (ctx : Solver.ctx) ->
            let base = Ldd.const base_lat in
            let relevant_for_shallow =
              match List.length lbls with 1 -> `Relevant | _ -> `Irrelevant
            in
            List.fold_left
              (fun acc (lbl : Types.label_declaration) ->
                let mask =
                  Axis_lattice.mask_of_modality ~relevant_for_shallow
                    lbl.ld_modalities
                in
                Ldd.join acc
                  (Ldd.meet (Ldd.const mask) (Solver.kind ctx lbl.ld_type)))
              base lbls
          in
          Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        | Types.Type_variant (_cstrs, Types.Variant_with_null, _umc_opt) ->
          (* [Variant_with_null] (i.e. [or_null]) has semantics that are not
             captured by its constructors: nullability/separability and
             mode-crossing are baked into its representation. We defer to
             jkinds because ikinds cannot express this today. This deferral
             can be removed once separability and nullability become layout
             properties rather than modal axes. *)
          let kind : Solver.ckind =
           fun ctx -> ckind_of_jkind_l ctx type_decl.type_jkind
          in
          Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        | Types.Type_variant (cstrs, rep, _umc_opt) ->
          (* GADTs introduce existential type variables via [cd_res] and can
             install local equations. For ikinds, we conservatively round up
             when we see GADT constructors to avoid leaking existentials into
             stored polynomials. *)
          let has_gadt_constructor =
            List.exists
              (fun (c : Types.constructor_declaration) ->
                Option.is_some c.cd_res)
              cstrs
          in
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
          let kind : Solver.ckind =
           fun (ctx : Solver.ctx) ->
            let ctx =
              if has_gadt_constructor
              then Solver.reset_for_mode ctx ~mode:Solver.Round_up
              else ctx
            in
            let base = Ldd.const base_lat in
            List.fold_left
              (fun acc (c : Types.constructor_declaration) ->
                match c.cd_args with
                | Types.Cstr_tuple args ->
                  List.fold_left
                    (fun acc (arg : Types.constructor_argument) ->
                      let mask =
                        Axis_lattice.mask_of_modality ~relevant_for_shallow
                          arg.ca_modalities
                      in
                      Ldd.join acc
                        (Ldd.meet
                           (Ldd.const mask)
                           (Solver.kind ctx arg.ca_type)))
                    acc args
                | Types.Cstr_record lbls ->
                  List.fold_left
                    (fun acc (lbl : Types.label_declaration) ->
                      let mask =
                        Axis_lattice.mask_of_modality ~relevant_for_shallow
                          lbl.ld_modalities
                      in
                      Ldd.join acc
                        (Ldd.meet
                           (Ldd.const mask)
                           (Solver.kind ctx lbl.ld_type)))
                    acc lbls)
              base cstrs
          in
          Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        | Types.Type_open ->
          (* Use the stored jkind here in case it is `exn`,
             which is special. *)
          let kind : Solver.ckind =
           fun ctx -> ckind_of_jkind_l ctx type_decl.type_jkind
          in
          Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        (*
           (* This is the code we'd use otherwise *)
           let kind : Solver.ckind =
            fun _ctx ->
             Ldd.const Axis_lattice.nonfloat_value
           in
           Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        *)
        )
    in
    (* Prefer a stored constructor ikind if one is present and enabled. *)
    let ikind =
      match type_decl.type_ikind with
      | Types.Constructor_ikind constructor when !Clflags.ikinds ->
        let base, coeffs = constructor_ikind_polynomial constructor in
        Solver.Poly (base, coeffs)
      | Types.No_constructor_ikind reason ->
        (* Print the reason *)
        (*= Format.eprintf "[ikind-miss] %s@." reason; *)
        ignore reason;
        fallback ()
      | Types.Constructor_ikind _ -> fallback ()
    in
    (if !Types.ikind_debug
    then
      let ikind_msg =
        match ikind with
        | Solver.Ty _ -> "Ty"
        | Solver.Poly (base, coeffs) ->
          let coeffs =
            coeffs |> Array.map Ikind.Ldd.pp |> Array.to_list
            |> String.concat "; "
          in
          Format.asprintf "Poly(base=%s; coeffs=[%s])" (Ikind.Ldd.pp base)
            coeffs
      in
      Format.eprintf "[ikind] %a: %s@." Path.print path ikind_msg);
    ikind

(* Package the above into a full evaluation context. *)
let make_ctx_with_mode ~(mode : Solver.mode) ~(context : Jkind.jkind_context) :
    Solver.ctx =
  Solver.create_ctx ~mode { lookup = lookup_of_context ~context }

let make_ctx ~(context : Jkind.jkind_context) : Solver.ctx =
  make_ctx_with_mode ~mode:Solver.Normal ~context

let normalize ~(context : Jkind.jkind_context) (jkind : Types.jkind_l) :
    Ikind.Ldd.node =
  let ctx = make_ctx ~context in
  Solver.normalize (ckind_of_jkind_l ctx jkind)

let type_declaration_ikind ~(context : Jkind.jkind_context) ~(path : Path.t) :
    Types.constructor_ikind =
  let ctx = make_ctx ~context in
  let base, coeffs = Solver.constr_kind_poly ctx path in
  constructor_ikind ~base ~coeffs

let type_declaration_ikind_gated ~(context : Jkind.jkind_context)
    ~(path : Path.t) : Types.type_ikind =
  (* This function gets called separately for each 
    type definition of a mutually recursive group. This is
    safe but computationally wasteful. In the future we might
    want to give this function a list of paths and compute the
    ikind for all of them at once. Alternatively, keep the cache
    between calls to this function from the same mutually recursive
    group. *)
  if not !Clflags.ikinds
  then Types.ikinds_todo "ikinds disabled"
  else
    let ikind = type_declaration_ikind ~context ~path in
    let payload = ikind in
    (if !Types.ikind_debug
    then
      let stored_jkind =
        match context.lookup_type path with
        | None -> "?"
        | Some decl -> Format.asprintf "%a" Jkind.format decl.type_jkind
      in
      Format.eprintf "[ikind] %a: stored=%s, base=%s, coeffs=[%s]@." Path.print
        path stored_jkind
        (Ikind.Ldd.pp payload.base)
        (String.concat "; "
           (Array.to_list (Array.map Ikind.Ldd.pp payload.coeffs))));
    Types.Constructor_ikind ikind

let type_declaration_ikind_of_jkind ~(context : Jkind.jkind_context)
    ~(params : Types.type_expr list) (type_jkind : Types.jkind_l) :
    Types.type_ikind =
  if not !Clflags.ikinds
  then Types.ikinds_todo "ikinds disabled"
  else
    let poly = normalize ~context type_jkind in
    let rigid_vars =
      List.map (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty))) params
    in
    let base, coeffs =
      Ldd.decompose_into_linear_terms ~universe:rigid_vars poly
    in
    let coeffs = Array.of_list coeffs in
    let payload = constructor_ikind ~base ~coeffs in
    if !Types.ikind_debug
    then
      Format.eprintf "[ikind] from jkind: base=%s; coeffs=[%s]@."
        (Ikind.Ldd.pp payload.base)
        (String.concat "; "
           (Array.to_list (Array.map Ikind.Ldd.pp payload.coeffs)));
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
    Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context ~level sub super
  else
    let allow_any =
      match allow_any_crossing with Some true -> true | _ -> false
    in
    if allow_any
    then (
      (if !Types.ikind_debug
      then
        let origin_suffix =
          match origin with None -> "" | Some o -> " origin=" ^ o
        in
        Format.eprintf
          "[ikind-subjkind] call%s allow_any=true@;sub=%a@;super=%a@."
          origin_suffix Jkind.format sub Jkind.format super);
      Ok ())
    else
      let ctx = make_ctx ~context in
      let super_poly = ckind_of_jkind_l ctx super in
      let super_is_constant =
        Ldd.solve_pending ();
        Ldd.is_const super_poly
      in
      let sub_ctx =
        if super_is_constant
        then Solver.reset_for_mode ctx ~mode:Solver.Round_up
        else ctx
      in
      let sub_poly = ckind_of_jkind_l sub_ctx sub in
      let violating_axes = Ldd.leq_with_reason sub_poly super_poly in
      (if !Types.ikind_debug
      then
        let origin_suffix =
          match origin with None -> "" | Some o -> " origin=" ^ o
        in
        Format.eprintf
          "[ikind-subjkind] call%s allow_any=false@;\
           sub=%a@;\
           super=%a@;\
           @;\
           sub_poly=%s@;\
           super_poly=%s@."
          origin_suffix Jkind.format sub Jkind.format super (Ldd.pp sub_poly)
          (Ldd.pp super_poly));
      match violating_axes with
      | [] -> Ok ()
      | _ ->
        let () =
          if !Types.ikind_debug
          then
            let axes =
              violating_axes
              |> List.map (fun (Jkind_axis.Axis.Pack ax) ->
                     Jkind_axis.Axis.name ax)
              |> String.concat ", "
            in
            Format.eprintf
              "[ikind-subjkind] failure on axes: %s@;sub=%a@;super=%a@." axes
              Jkind.format sub Jkind.format super
        in
        (* Do not try to adjust allowances; Violation.Not_a_subjkind
           accepts an r-jkind. *)
        let axis_reasons =
          List.map
            (fun axis -> Jkind.Sub_failure_reason.Axis_disagreement axis)
            violating_axes
        in
        Error
          (Jkind.Violation.of_ ~context
             (Jkind.Violation.Not_a_subjkind (sub, super, axis_reasons)))

let sub ?origin ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) ~level (sub : Types.jkind_l)
    (super : Types.jkind_r) : bool =
  ignore origin;
  if not !Clflags.ikinds
  then Jkind.sub ~type_equal ~context ~level sub super
  else
    let ctx = make_ctx ~context in
    match
      Solver.leq_with_reason (ckind_of_jkind_l ctx sub) (ckind_of_jkind_r super)
    with
    | [] -> true
    | _ -> false

(* CR jujacobs: this is really slow when enabled. Fix performance. *)
let crossing_of_jkind ~(context : Jkind.jkind_context)
    (jkind : ('l * 'r) Types.jkind) : Mode.Crossing.t =
  if not (enable_crossing && !Clflags.ikinds) (* CR jujacobs: fix this *)
  then Jkind.get_mode_crossing ~context jkind
  else
    let ctx = make_ctx_with_mode ~mode:Solver.Round_up ~context in
    let lat = Solver.round_up (ckind_of_jkind ctx jkind) in
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
  (* CR jujacobs: fix this *)
  if not (enable_sub_or_intersect && !Clflags.ikinds)
  then Jkind.sub_or_intersect ~type_equal ~context ~level t1 t2
  else
    let layout_sub = Jkind.Layout.sub ~level t1.jkind.layout t2.jkind.layout in
    match layout_sub with
    | Jkind.Sub_result.Not_le reasons ->
      if Jkind.has_intersection ~level t1 t2
      then Jkind.Has_intersection reasons
      else Jkind.Disjoint reasons
    | Jkind.Sub_result.Equal | Jkind.Sub_result.Less -> (
      (* The RHS is a jkind_r (no with-bounds), so its ikind is constant. *)
      let ctx = make_ctx_with_mode ~mode:Solver.Round_up ~context in
      let sub_poly = ckind_of_jkind_l ctx (Jkind.disallow_right t1) in
      let super_poly = ckind_of_jkind_r (Jkind.disallow_left t2) in
      (* Layouts already checked above. Remaining failure reasons are
         per-axis disagreements. *)
      match Solver.leq_with_reason sub_poly super_poly with
      | [] -> Jkind.Sub
      | violating_axes ->
        let axis_reasons =
          List.map
            (fun axis -> Jkind.Sub_failure_reason.Axis_disagreement axis)
            violating_axes
        in
        let reasons : Jkind.Sub_failure_reason.t Misc.Nonempty_list.t =
          match axis_reasons with
          | [] -> Jkind.Sub_failure_reason.Layout_disagreement :: []
          | hd :: tl -> hd :: tl
        in
        Jkind.Has_intersection reasons)

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
  | Tvar { jkind; _ } | Tunivar { jkind; _ } -> max_arity_in_jkind acc jkind
  | Tconstr (path, args, _) ->
    let n = List.length args in
    let prev = match Path.Map.find_opt path acc with None -> 0 | Some m -> m in
    let acc = if n > prev then Path.Map.add path n acc else acc in
    List.fold_left max_arity_in_type acc args
  | Ttuple elts ->
    List.fold_left (fun a (_, t) -> max_arity_in_type a t) acc elts
  | Tunboxed_tuple elts ->
    List.fold_left (fun a (_, t) -> max_arity_in_type a t) acc elts
  | Tarrow (_, t1, t2, _) -> max_arity_in_type (max_arity_in_type acc t1) t2
  | Tpoly (t, ts) ->
    List.fold_left max_arity_in_type (max_arity_in_type acc t) ts
  | Tobject (t, name) -> (
    let acc = max_arity_in_type acc t in
    match !name with
    | None -> acc
    | Some (_, tl) -> List.fold_left max_arity_in_type acc tl)
  | Tfield (_, _, t1, t2) -> max_arity_in_type (max_arity_in_type acc t1) t2
  | Tvariant row ->
    let acc = Btype.fold_row max_arity_in_type acc row in
    max_arity_in_type acc (row_more row)
  | Tpackage (_, fields) ->
    List.fold_left (fun acc (_n, t) -> max_arity_in_type acc t) acc fields
  | Tquote t | Tsplice t -> max_arity_in_type acc t
  | Tlink t -> max_arity_in_type acc t
  | Tsubst (t, row_opt) -> (
    let acc = max_arity_in_type acc t in
    match row_opt with None -> acc | Some row -> max_arity_in_type acc row)
  | Tof_kind jkind -> max_arity_in_jkind acc jkind
  | Tnil -> acc

let identity_lookup_from_arity_map (arity : int Path.Map.t) (path : Path.t) :
    Solver.constr_decl =
  let open Ldd in
  let n = match Path.Map.find_opt path arity with None -> 0 | Some m -> m in
  (* Identity polynomial: base and coeffs are just fresh rigid atoms. *)
  let base = node_of_var (rigid (Name.atomic path 0)) in
  let coeffs =
    Array.init n (fun i -> node_of_var (rigid (Name.atomic path (i + 1))))
  in
  Solver.Poly (base, coeffs)

let poly_of_type_function_in_identity_env ~(params : Types.type_expr list)
    ~(body : Types.type_expr) : Ldd.node * Ldd.node array =
  (* Approximate type-function substitution by evaluating in an identity
     environment, i.e. every constructor contributes an independent rigid
     atom. *)
  let arity = max_arity_in_type Path.Map.empty body in
  let lookup path = identity_lookup_from_arity_map arity path in
  let ctx = Solver.create_ctx ~mode:Solver.Normal { lookup } in
  let poly = Solver.normalize (kind_of ctx body) in
  let rigid_vars =
    List.map (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty))) params
  in
  let base, coeffs =
    Ldd.decompose_into_linear_terms ~universe:rigid_vars poly
  in
  base, Array.of_list coeffs

type lookup_result =
  | Lookup_identity
  | Lookup_path of Path.t
  | Lookup_type_fun of Types.type_expr list * Types.type_expr

let substitute_decl_ikind_with_lookup
    ~(lookup : Path.t -> lookup_result)
    (ikind_entry : Types.type_ikind) : Types.type_ikind =
  (* Inline type functions in an identity environment (no Env). *)
  match ikind_entry with
  | Types.No_constructor_ikind _ -> ikind_entry
  | Types.Constructor_ikind _ when reset_constructor_ikind_on_substitution ->
    Types.ikinds_todo "ikind substitution reset"
  | Types.Constructor_ikind packed ->
    let payload = packed in
    let memo : (Path.t, Ldd.node * Ldd.node array) Hashtbl.t =
      Hashtbl.create 17
    in
    (* Rewrite a polynomial by mapping each rigid atom through [lookup]. *)
    let rec map_poly (expanding : Path.Set.t) (poly : Ldd.node) : Ldd.node =
      Ldd.map_rigid (map_name expanding) poly
    and map_name (expanding : Path.Set.t) (name : Ldd.Name.t) : Ldd.node =
      match name with
      | Param _ -> Ldd.node_of_var (Ldd.rigid name)
      | Unknown _ -> Ldd.node_of_var (Ldd.rigid name)
      | Atom { constr = path; arg_index } -> (
        match lookup path with
        | Lookup_identity -> Ldd.node_of_var (Ldd.rigid name)
        | Lookup_path alias_path ->
          Ldd.node_of_var (Ldd.rigid (Ldd.Name.atomic alias_path arg_index))
        | Lookup_type_fun (params, body) ->
          (* Inline a type function by evaluating it in an identity
             environment.  The [expanding] set prevents infinite
             unfolding of recursive type functions. *)
          if Path.Set.mem path expanding
          then Ldd.node_of_var (Ldd.rigid name)
          else
            let base_raw, coeffs_raw =
              (* Memoized by [path] to avoid recomputation *)
              match Hashtbl.find_opt memo path with
              | Some v -> v
              | None ->
                let v = poly_of_type_function_in_identity_env ~params ~body in
                Hashtbl.add memo path v;
                v
            in
            let expanding = Path.Set.add path expanding in
            let base = map_poly expanding base_raw in
            let coeffs = Array.map (map_poly expanding) coeffs_raw in
            if arg_index = 0
            then base
            else if arg_index - 1 < Array.length coeffs
            then coeffs.(arg_index - 1)
            else
              (* Fallback: if coefficient missing, keep original
                 atom. *)
              Ldd.node_of_var (Ldd.rigid name))
    in
    let base_poly = map_poly Path.Set.empty payload.base in
    let coeffs_poly = Array.map (map_poly Path.Set.empty) payload.coeffs in
    let payload = constructor_ikind ~base:base_poly ~coeffs:coeffs_poly in
    Types.Constructor_ikind payload
