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

(* Global feature toggles for ikinds. These are intended to be easy to flip
   while iterating on performance or correctness. *)
(* CR jujacobs: remove toggles in the final version. *)
let enable_crossing = true

let enable_sub_jkind_l = true

let enable_sub_or_intersect = true

let enable_sub_or_error = false

let reset_constructor_ikind_on_substitution = false

module Ldd = Types.Ldd

let instance_poly_for_jkind' =
  ref (fun _univars _ty -> Misc.fatal_error "instance_poly_for_jkind")

let fresh_unknown_uid () : Types.Uid.t =
  let current_unit =
    Some
      (Unit_info.make_dummy ~input_name:"<ikind>"
         (Compilation_unit.get_current_or_dummy ()))
  in
  Types.Uid.mk ~current_unit

module Provenance = struct
  let next_id = ref 0

  let names : Ldd.Name.t list ref = ref []

  let register_text ty : Ldd.Name.t =
    let id = !next_id in
    next_id := id + 1;
    let name = Ldd.Name.provenance ~id ~ty in
    names := name :: !names;
    name

  let register (ty : Types.type_expr) : Ldd.Name.t =
    Format_doc.asprintf "%a" Jkind.format_type_expr ty
    |> String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c)
    |> register_text

  let reset () = names := []

  let all_names () = List.rev !names
end

(** A kind solver specialized to [Types.Ldd] and [Types.type_expr].

    The solver computes LDD polynomials of the form base ⊔ Σ_i (arg_i ⊓ coeff_i)
    where [base] is the intrinsic kind of a constructor and each [coeff_i]
    describes the contribution coming from the i-th type argument. *)
module Solver = struct
  type mode =
    | Normal
    | Round_up

  (* Hash tables avoiding polymorphic structural comparison on deep values.
     [Btype.TypeHash] keys by the representative of a [type_expr], so
     union-find aliases map to a single entry. This table is used to cache
     repeated kind computations, as well as to make circular types work. *)
  module TyTbl = Btype.TypeHash

  let constr_to_string (path : Path.t) : string =
    Format_doc.asprintf "%a" Path.print path

  (* Hash table for caching constructor kinds. *)
  module ConstrTbl = Path.Tbl

  (** Kind function for constructors: computes a kind from a context. This is
      used because many kinds don't make sense outside of a context, e.g., the
      kind of a type containing a constructor depends on the context telling us
      what its kind is. *)
  type ckind = ctx -> Ldd.node

  (** Result of constructor lookup. [Ty] describes a constructor declaration
      with arguments and a kind function; [Poly] provides a cached polynomial
      form. *)
  and constr_decl =
    | Ty of
        { args : Types.type_expr list;
          kind : ckind;
          abstract : bool
        }
    | Poly of Ldd.node * Ldd.node array

  and ctx =
    { env : Env.t option;
      lookup_of_env : Env.t -> Path.t -> constr_decl;
      mode : mode;
      add_provenance : bool;
      ty_to_kind : Ldd.node TyTbl.t;
      constr_to_coeffs : (Ldd.node * Ldd.node array) ConstrTbl.t
    }

  let global_ty_to_kind : Ldd.node TyTbl.t = TyTbl.create 1

  let global_constr_to_coeffs : (Ldd.node * Ldd.node array) ConstrTbl.t =
    ConstrTbl.create 1

  let create_ctx ~(mode : mode) ~(env : Env.t option) ~add_provenance
      ~(lookup_of_env : Env.t -> Path.t -> constr_decl) =
    TyTbl.clear global_ty_to_kind;
    ConstrTbl.clear global_constr_to_coeffs;
    { env;
      lookup_of_env;
      mode;
      add_provenance;
      ty_to_kind = global_ty_to_kind;
      constr_to_coeffs = global_constr_to_coeffs
    }

  let reset_for_mode (ctx : ctx) ~(mode : mode) : ctx = { ctx with mode }

  let reset_for_provenance (ctx : ctx) ~(add_provenance : bool) : ctx =
    { ctx with add_provenance; ty_to_kind = TyTbl.create 1 }

  let rigid_name (ctx : ctx) (name : Ldd.Name.t) : Ldd.node =
    match ctx.mode with
    | Normal -> Ldd.node_of_var (Ldd.rigid name)
    | Round_up when match name with Provenance _ -> true | _ -> false ->
      Ldd.node_of_var (Ldd.rigid name)
    | Round_up -> Ldd.const Axis_lattice.top

  (** A rigid variable corresponding to a type parameter [t]. *)
  let rigid (ctx : ctx) (ty : Types.type_expr) : Ldd.node =
    let param_id = Types.get_id ty in
    rigid_name ctx (Ldd.Name.param param_id)

  let provenance (ctx : ctx) (ty : Types.type_expr) : Ldd.node =
    rigid_name ctx (Provenance.register ty)

  let provenance_text (ctx : ctx) (text : string) : Ldd.node =
    rigid_name ctx (Provenance.register_text text)

  let with_provenance_text (ctx : ctx) (text : unit -> string) (poly : Ldd.node)
      : Ldd.node =
    if ctx.add_provenance
    then Ldd.meet poly (provenance_text ctx (text ()))
    else poly

  let type_may_be_circular (ty : Types.type_expr) : bool =
    match Types.get_desc ty with
    | Types.Tvariant _ -> true
    | Types.Tconstr _ -> true
    | Types.Tobject _ -> true
    | _ -> !Clflags.recursive_types

  let is_principal_type (ty : Types.type_expr) : bool =
    (not !Clflags.principal) || Types.get_level ty = Btype.generic_level

  (* CR jujacobs: we could optimize the join with masks you see below
     using a combined [Ldd.join_with_mask left mask right] operation. *)

  let identity_constr_decl ~(arity : int) (path : Path.t) : constr_decl =
    let open Ldd in
    let base = node_of_var (rigid (Name.atomic path 0)) in
    let coeffs =
      Array.init arity (fun i -> node_of_var (rigid (Name.atomic path (i + 1))))
    in
    Poly (base, coeffs)

  let lookup_constr (ctx : ctx) ~(min_arity : int) (path : Path.t) : constr_decl
      =
    match ctx.env with
    | Some env -> ctx.lookup_of_env env path
    | None -> identity_constr_decl ~arity:min_arity path

  (** Fetch or compute the polynomial for constructor [c]. *)
  let rec constr_kind (ctx : ctx) ~(min_arity : int) (path : Path.t) :
      Ldd.node * Ldd.node array =
    (* Return placeholder nodes stored in [constr_to_coeffs] for recursion. *)
    match ConstrTbl.find_opt ctx.constr_to_coeffs path with
    | Some base_and_coeffs -> base_and_coeffs
    | None -> (
      match lookup_constr ctx ~min_arity path with
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
          | Param _ | Provenance _ | Unknown _ -> rigid_name ctx name
          | KAtom kpath -> (
            match ctx.env with
            | None -> rigid_name ctx name
            | Some env -> (
              match Env.find_jkind kpath env with
              | exception Not_found -> rigid_name ctx name
              | { jkind_manifest = None; _ } -> rigid_name ctx name
              | { jkind_manifest = Some jkind_const; _ } ->
                ckind_of_jkind_desc ctx jkind_const))
          | Atom { constr = other_path; arg_index } ->
            if Path.same other_path path
            then rigid_name ctx name
            else
              let base_poly, coeffs_poly =
                constr_kind ctx ~min_arity:arg_index other_path
              in
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
        let res =
          ( Ldd.inline_solved_vars base_poly,
            Array.map Ldd.inline_solved_vars coeffs_poly )
        in
        ConstrTbl.replace ctx.constr_to_coeffs path res;
        res
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
        (* We add the parameters to the TyTbl so that they will refer to
           rigid variables that represent them in the solver. *)
        List.iter2
          (fun ty var ->
            (* Parameters written as plain type variables may have explicit
               bounds, as in [('a : bound)]. Cap their rigid variables by those
               bounds (needed for recursive payload ikinds). Other parameter
               expressions do not have a written parameter bound to apply here,
               so keep their rigid atoms bare. *)
            let param_kind =
              match Types.get_desc ty with
              | Types.Tvar { jkind; _ } ->
                Ldd.meet (Ldd.node_of_var var) (ckind_of_jkind ctx jkind)
              | Types.Tunivar _ ->
                Misc.fatal_error
                  ("Ikind.type_declaration_ikind_of_jkind: "
                 ^ "unexpected Tunivar in parameter list")
              | _ -> Ldd.node_of_var var
            in
            TyTbl.add ctx.ty_to_kind ty param_kind)
          params rigid_vars;
        (* Compute body kind *)
        (* CR jujacobs: potential efficiency win:
           we could still compute the kind in Right mode to keep
           the cache consistent, but we don't need to. *)
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
               (constr_to_string path) (Array.length coeff_vars)
               (Array.length coeffs_rhs));
        if abstract
        then (
          (* For abstract types we solve the solver variables using
             greatest fixpoints. This ensures that abstract types'
             bounds are incorporated into all kind polynomials that
             mention the abstract type. This way, we can check kind
             subsumption without having to consider hypotheses for the
             bounds of abstract types. *)
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

  (* Apply a constructor polynomial to argument types. *)
  and constr ?self_ty (ctx : ctx) (path : Path.t) (args : Types.type_expr list)
      : Ldd.node =
    let constr_ctx =
      if ctx.add_provenance
      then reset_for_provenance ctx ~add_provenance:false
      else ctx
    in
    let base, coeffs =
      constr_kind constr_ctx ~min_arity:(List.length args) path
    in
    let base =
      match self_ty with
      | Some ty when ctx.add_provenance -> Ldd.meet base (provenance ctx ty)
      | Some _ | None -> base
    in
    let rec loop acc remaining i =
      if i = Array.length coeffs
      then acc
      else
        match remaining with
        | arg :: rest ->
          let arg_kind = kind ~use_tables:true ctx arg in
          loop (Ldd.join acc (Ldd.meet arg_kind coeffs.(i))) rest (i + 1)
        | [] -> failwith "Missing arg"
    in
    loop base args 0

  (* Converting surface jkinds to solver ckinds. *)
  and ckind_of_jkind_desc : type a l r.
      ctx -> (a, l * r) Types.base_and_axes -> Ldd.node =
   fun ctx jkind_desc ->
    let expand =
      match ctx.env with
      | None ->
        let expand : type b.
            (b, l * r) Types.base_and_axes ->
            Types.mod_bounds * (l * r) Types.with_bounds * Path.t option =
         fun jkind_desc ->
          let unresolved_base =
            match jkind_desc.base with
            | Types.Layout _ -> None
            | Types.Kconstr (path, _) -> Some path
          in
          jkind_desc.mod_bounds, jkind_desc.with_bounds, unresolved_base
        in
        expand
      | Some env ->
        let rec expand : type b.
            (b, l * r) Types.base_and_axes ->
            Types.mod_bounds * (l * r) Types.with_bounds * Path.t option =
         fun jkind_desc ->
          match Jkind.Const.expand_once env jkind_desc with
          | Some jkind_const -> expand jkind_const
          | None ->
            let unresolved_base =
              match jkind_desc.base with
              | Types.Layout _ -> None
              | Types.Kconstr (path, _) -> Some path
            in
            jkind_desc.mod_bounds, jkind_desc.with_bounds, unresolved_base
        in
        expand
    in
    let mod_bounds, with_bounds, unresolved_base = expand jkind_desc in
    let base_mod_bounds =
      Ldd.const (Jkind.Mod_bounds.to_axis_lattice mod_bounds)
    in
    let base =
      match unresolved_base with
      | None -> base_mod_bounds
      | Some path ->
        let atom = rigid_name ctx (Ldd.Name.katom path) in
        Ldd.meet base_mod_bounds atom
    in
    (* For each with-bound (ty, axes), contribute
       modality(axes_mask, kind ty). *)
    Jkind.With_bounds.to_seq with_bounds
    |> Seq.fold_left
         (fun acc (ty, bound_info) ->
           let axes = bound_info.Types.With_bounds_type_info.relevant_axes in
           let mask = Axis_lattice.of_axis_set axes in
           let ty_kind = kind ~use_tables:true ctx ty in
           Ldd.join acc (Ldd.meet (Ldd.const mask) ty_kind))
         base

  and ckind_of_jkind : type l r. ctx -> (l * r) Types.jkind -> Ldd.node =
   fun ctx jkind -> ckind_of_jkind_desc ctx jkind.jkind

  and mod_bounds_floor_of_jkind_desc : type a l r.
      ctx -> (a, l * r) Types.base_and_axes -> Ldd.node option =
   fun ctx jkind_desc ->
    let mod_bounds, unresolved_base =
      let rec expand : type b.
          (b, l * r) Types.base_and_axes -> Types.mod_bounds * Path.t option =
       fun jkind_desc ->
        match ctx.env with
        | None ->
          let unresolved_base =
            match jkind_desc.base with
            | Types.Layout _ -> None
            | Types.Kconstr (path, _) -> Some path
          in
          jkind_desc.mod_bounds, unresolved_base
        | Some env -> (
          match Jkind.Const.expand_once env jkind_desc with
          | Some jkind_const -> expand jkind_const
          | None ->
            let unresolved_base =
              match jkind_desc.base with
              | Types.Layout _ -> None
              | Types.Kconstr (path, _) -> Some path
            in
            jkind_desc.mod_bounds, unresolved_base)
      in
      expand jkind_desc
    in
    match unresolved_base with
    | Some _ -> None
    | None -> Some (Ldd.const (Jkind.Mod_bounds.to_axis_lattice mod_bounds))

  and mod_bounds_floor_of_jkind : type l r.
      ctx -> (l * r) Types.jkind -> Ldd.node option =
   fun ctx jkind -> mod_bounds_floor_of_jkind_desc ctx jkind.jkind

  (** Compute the kind for [t]. *)
  and kind ?(check_principality = true) ~use_tables (ctx : ctx)
      (ty : Types.type_expr) : Ldd.node =
    if check_principality && not (is_principal_type ty)
    then Ldd.const Axis_lattice.top
    else
      match TyTbl.find_opt ctx.ty_to_kind ty with
      | Some kind_poly -> kind_poly
      | None ->
        if not use_tables
        then kind_uncached ctx ty
        else if type_may_be_circular ty
        then (
          let var = Ldd.new_var () in
          let placeholder = Ldd.node_of_var var in
          TyTbl.add ctx.ty_to_kind ty placeholder;
          let kind_rhs = kind_uncached ctx ty in
          Ldd.solve_lfp var kind_rhs;
          let kind_inlined = Ldd.inline_solved_vars placeholder in
          TyTbl.replace ctx.ty_to_kind ty kind_inlined;
          kind_inlined)
        else
          let kind_rhs = kind_uncached ctx ty in
          TyTbl.add ctx.ty_to_kind ty kind_rhs;
          kind_rhs

  (* Worker for [kind]; does not memoize.
     Only call from [kind] so caching and LFP handling apply. *)
  and kind_uncached (ctx : ctx) (ty : Types.type_expr) : Ldd.node =
    (* Compute the ikind polynomial for an arbitrary [type_expr]. This is the
       semantic counterpart of [Jkind.jkind_of_type], but expressed in LDD
       form. *)
    let self_provenance poly =
      if ctx.add_provenance then Ldd.meet poly (provenance ctx ty) else poly
    in
    (* [ty] is expected to be representative: no links/substs/fields/nil.
       Provenance is attached only to the contribution introduced by this
       node, not to recursive child contributions. *)
    match Types.get_desc ty with
    | Types.Tvar { name = _name; jkind } | Types.Tunivar { name = _name; jkind }
      ->
      (* Keep a rigid param, but cap it by its annotated jkind. *)
      self_provenance (Ldd.meet (rigid ctx ty) (ckind_of_jkind ctx jkind))
    | Types.Tconstr (path, args, _abbrev_memo) ->
      constr ~self_ty:ty ctx path args
    | Types.Ttuple elts ->
      (* Boxed tuples: immutable_data base + per-element contributions
         under id modality. *)
      let base = self_provenance (Ldd.const Axis_lattice.immutable_data) in
      Ldd.sum elts ~base ~f:(fun (_lbl, t) -> kind ~use_tables:true ctx t)
    | Types.Tunboxed_tuple elts ->
      (* Unboxed tuples: per-element contributions; shallow axes relevant
         only for arity = 1. *)
      Ldd.sum elts ~base:Ldd.bot ~f:(fun (_lbl, t) ->
          kind ~use_tables:true ctx t)
    | Types.Tarrow (_lbl, _t1, _t2, _commu) ->
      (* Arrows use the dedicated per-axis bounds (no with-bounds). *)
      self_provenance (Ldd.const Axis_lattice.arrow)
    | Types.Tlink _ -> failwith "Tlink shouldn't appear in kind"
    | Types.Tsubst _ -> failwith "Tsubst shouldn't appear in kind"
    | Types.Trepr (ty, _sort_vars) -> kind ~use_tables:true ctx ty
    | Types.Tpoly (ty, univars) ->
      (* CR ikinds: this is sound but not fully precise.
        Internal ticket 5746. *)
      let ty = !instance_poly_for_jkind' univars ty in
      (* We intentionally skip the principality check here. Enforcing it
         breaks the stdlib build, and the old env-var escape hatch never had
         a viable setting in practice. Track removing this workaround as part
         of internal ticket 5746. *)
      kind ~check_principality:false ~use_tables:true ctx ty
    | Types.Tof_kind jkind -> self_provenance (ckind_of_jkind ctx jkind)
    | Types.Tobject _ -> self_provenance (Ldd.const Axis_lattice.object_legacy)
    | Types.Tbox t ->
      let base = self_provenance (Ldd.const Axis_lattice.mutable_data) in
      Ldd.join base (kind ~use_tables:true ctx t)
    | Types.Tfield _ -> failwith "Tfield shouldn't appear in kind"
    | Types.Tnil -> failwith "Tnil shouldn't appear in kind"
    | Types.Tquote _ | Types.Tsplice _ | Types.Tquote_eval _ ->
      (* Treat quoted/spliced/evaluated quoted types conservatively as
         boxed values. *)
      self_provenance (Ldd.const Axis_lattice.value)
    | Types.Tvariant row ->
      if Btype.tvariant_not_immediate row
      then
        if Btype.static_row row
        then
          (* Closed, boxed polymorphic variant: immutable_data base plus
             per-constructor args. *)
          let base = self_provenance (Ldd.const Axis_lattice.immutable_data) in
          Btype.fold_row
            (fun acc ty ->
              let ty_kind = kind ~use_tables:true ctx ty in
              Ldd.join acc ty_kind)
            base row
        else
          (* CR ikinds: open rows get conservative non-float value (boxed)
             intersected with an unknown rigid so the solver treats it as an
             unknown element. This can be improved. Internal ticket 6205. *)
          let unknown =
            rigid_name ctx (Ldd.Name.unknown (fresh_unknown_uid ()))
          in
          self_provenance
            (Ldd.meet (Ldd.const Axis_lattice.nonfloat_value) unknown)
      else
        (* All-constant (immediate) polymorphic variant. *)
        self_provenance (Ldd.const Axis_lattice.immediate)
    | Types.Tpackage _ ->
      (* Like open polymorphic variants, model first-class modules as boxed
         values intersected with an unknown so they behave as not-best. *)
      let unknown = rigid_name ctx (Ldd.Name.unknown (fresh_unknown_uid ())) in
      self_provenance (Ldd.meet (Ldd.const Axis_lattice.nonfloat_value) unknown)

  (* Evaluate a ckind in [ctx] and flush pending GFP constraints. *)
  let normalize (kind_poly : Ldd.node) : Ldd.node =
    Ldd.solve_pending ();
    kind_poly

  let node_of_name (ctx : ctx) (name : Ldd.Name.t) : Ldd.node =
    rigid_name ctx name

  (* Materialize a solved polynomial for storing in
     [Types.constructor_ikind]. *)
  let constr_kind_poly (ctx : ctx) (c : Path.t) : Ldd.node * Ldd.node array =
    let base, coeffs = constr_kind ctx ~min_arity:0 c in
    Ldd.solve_pending ();
    base, coeffs

  let round_up (k : Ldd.node) : Axis_lattice.t = Ldd.round_up k
end

let constructor_ikind ~base ~coeffs : Types.constructor_ikind =
  (* Keep coefficients disjoint from the base (subtract-normal form). *)
  for i = 0 to Array.length coeffs - 1 do
    let coeff = coeffs.(i) in
    let coeff' = Ldd.sub_subsets coeff base in
    if coeff != coeff' then coeffs.(i) <- coeff'
  done;
  ({ Types.base; coeffs } : Types.constructor_ikind)

let pp_coeffs (coeffs : Ldd.node array) : string =
  coeffs |> Array.map Ldd.pp |> Array.to_list |> String.concat "; "

let with_ikinds_enabled (f : unit -> Types.constructor_ikind) : Types.type_ikind
    =
  if not !Clflags.ikinds
  then Types.ikinds_todo "ikinds disabled"
  else Types.Constructor_ikind (f ())

let origin_suffix_of = function None -> "" | Some o -> " origin=" ^ o

let pp_axes (axes : Jkind_axis.Axis.packed list) : string =
  axes
  |> List.map (fun (Jkind_axis.Axis.Pack ax) -> Jkind_axis.Axis.name ax)
  |> String.concat ", "

let axis_name (Jkind_axis.Axis.Pack ax) = Jkind_axis.Axis.name ax

let same_axis a b = String.equal (axis_name a) (axis_name b)

let pp_axis_name ppf axis = Format_doc.fprintf ppf "%s" (axis_name axis)

let pp_axis_list_prose ppf (axes : Jkind_axis.Axis.packed list) =
  match axes with
  | [] -> Format_doc.fprintf ppf "<none>"
  | [axis] -> pp_axis_name ppf axis
  | [axis1; axis2] ->
    Format_doc.fprintf ppf "%a and %a" pp_axis_name axis1 pp_axis_name axis2
  | _ ->
    let rec split_last = function
      | [] -> assert false
      | [last] -> [], last
      | x :: xs ->
        let init, last = split_last xs in
        x :: init, last
    in
    let init, last = split_last axes in
    Format_doc.fprintf ppf "%a,@ and %a"
      (Format_doc.pp_print_list
         ~pp_sep:(fun ppf () -> Format_doc.fprintf ppf ",@ ")
         pp_axis_name)
      init pp_axis_name last

let collapse_whitespace s =
  let b = Buffer.create (String.length s) in
  let pending_space = ref false in
  String.iter
    (function
      | ' ' | '\n' | '\r' | '\t' ->
        if Buffer.length b > 0 then pending_space := true
      | c ->
        if !pending_space
        then (
          Buffer.add_char b ' ';
          pending_space := false);
        Buffer.add_char b c)
    s;
  Buffer.contents b

let remove_numeric_path_stamps s =
  let b = Buffer.create (String.length s) in
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let rec loop i =
    if i >= String.length s
    then ()
    else
      match s.[i] with
      | '/' when i + 1 < String.length s && is_digit s.[i + 1] ->
        let rec skip_digits j =
          if j < String.length s && is_digit s.[j]
          then skip_digits (j + 1)
          else j
        in
        loop (skip_digits (i + 1))
      | c ->
        Buffer.add_char b c;
        loop (i + 1)
  in
  loop 0;
  Buffer.contents b

let format_jkind_single_line env jkind =
  Format_doc.asprintf "%a" (Jkind.format env) jkind
  |> collapse_whitespace |> remove_numeric_path_stamps

let pp_breakable_words ppf s =
  s |> String.split_on_char ' '
  |> List.filter (fun s -> not (String.equal s ""))
  |> Format_doc.pp_print_list
       ~pp_sep:(fun ppf () -> Format_doc.fprintf ppf "@ ")
       Format_doc.pp_print_string ppf

let split_on_first_substring ~sub s =
  let len = String.length s in
  let sub_len = String.length sub in
  let rec loop i =
    if i + sub_len > len
    then None
    else if String.equal (String.sub s i sub_len) sub
    then
      let before = String.sub s 0 i in
      let after = String.sub s (i + sub_len) (len - i - sub_len) in
      Some (before, after)
    else loop (i + 1)
  in
  loop 0

let pp_breakable_jkind_annotation ppf s =
  match split_on_first_substring ~sub:" with " s with
  | None -> pp_breakable_words ppf s
  | Some (before, after) ->
    Format_doc.fprintf ppf "@[<v 2>%s@;with %s@]" before after

let is_bot_poly poly = Axis_lattice.equal (Ldd.round_up poly) Axis_lattice.bot

let axes_in_violation_order ~violating_axes axes =
  (* Keep only axes that the overall comparison actually failed on, ordered as
     in [violating_axes]. A fragment residual can be non-top on a non-violating
     axis because [round_down] under-approximates the symbolic implication, so
     intersecting here prevents blaming a fragment on an axis the whole
     comparison passes on. *)
  List.filter
    (fun violating_axis -> List.exists (same_axis violating_axis) axes)
    violating_axes

type mode_crossing_error =
  { origin : string option;
    sub_jkind : Types.jkind_l;
    super_jkind : Types.jkind_l;
    sub_poly : Ldd.node;
    super_poly : Ldd.node;
    failing_poly : Ldd.node;
    fast_path : string;
    provenance_names : Ldd.Name.t list;
    violating_axes : Jkind_axis.Axis.packed list
  }

type subjkind_error =
  | Jkind_error of Jkind.Violation.t
  | Mode_crossing_error of mode_crossing_error

type provenance_residual =
  { ty : string;
    mode_bounds : Axis_lattice.t;
    axes : Jkind_axis.Axis.packed list
  }

let provenance_ty_of_name (name : Ldd.Name.t) =
  match name with
  | Provenance { ty; _ } -> Some ty
  | Atom _ | KAtom _ | Param _ | Unknown _ -> None

let add_provenance_residual entries { ty; mode_bounds; axes } =
  match List.partition (fun entry -> String.equal entry.ty ty) entries with
  | [], rest -> { ty; mode_bounds; axes } :: rest
  | matching, rest ->
    let axes =
      List.fold_left
        (fun axes entry ->
          List.fold_left
            (fun axes axis ->
              if List.exists (same_axis axis) axes then axes else axes @ [axis])
            axes entry.axes)
        axes matching
    in
    let mode_bounds =
      List.fold_left
        (fun mode_bounds entry ->
          Axis_lattice.meet mode_bounds entry.mode_bounds)
        mode_bounds matching
    in
    { ty; mode_bounds; axes } :: rest

let provenance_residuals ~provenance_names ~violating_axes ~sub_poly ~super_poly
    =
  let provenance_vars = List.map Ldd.rigid provenance_names in
  (* For a declaration [type t : bound = rhs], ikind checking compares the
     inferred ikind polynomial for [rhs] against the polynomial for [bound].
     When that comparison fails, we recompute [rhs] with provenance variables
     inserted by meeting a fresh variable into each local contribution. For a
     contribution [coeff] tagged by provenance variable [p], the error
     reporting question is:

       What is the largest assignment to [p] that would make this contribution
       satisfy the required bound?

     This is the right question because the provenance variable stands for the
     mode-crossing behavior we need from the source type. It gives the least
     restrictive bound that would still make the original lattice comparison
     pass.

     We answer the question by first decomposing the provenance-annotated
     polynomial into an untracked base plus one coefficient per provenance
     variable. If the base alone does not satisfy [super_poly], then provenance
     cannot explain the error. Otherwise, for each coefficient [coeff], we need
     the greatest [h] such that:

       meet coeff h <= super_poly

     This is exactly [Ldd.imply coeff super_poly]. The result may still contain
     symbolic variables, so we round it down to a conservative concrete mode
     bound for printing. *)
  let base, coeffs =
    Ldd.decompose_into_linear_terms ~universe:provenance_vars sub_poly
  in
  if Ldd.leq_with_reason base super_poly <> []
  then None
  else
    List.combine provenance_names coeffs
    |> List.filter_map (fun (name, coeff) ->
        if is_bot_poly coeff
        then None
        else
          match provenance_ty_of_name name with
          | None -> None
          | Some ty ->
            let mode_bounds = Ldd.imply coeff super_poly |> Ldd.round_down in
            let non_top_bounds =
              Axis_lattice.co_sub Axis_lattice.top mode_bounds
            in
            if Axis_lattice.equal non_top_bounds Axis_lattice.bot
            then None
            else
              let axes =
                non_top_bounds |> Axis_lattice.non_bot_axes
                |> List.map Axis_lattice.axis_number_to_axis_packed
                |> axes_in_violation_order ~violating_axes
              in
              Some { ty; mode_bounds; axes })
    |> List.fold_left add_provenance_residual []
    |> List.rev |> Option.some

let string_of_required_bound required_bounds (Jkind_axis.Axis.Pack axis) =
  match axis with
  | Modal (Monadic Uniqueness) ->
    Some
      (Format_doc.asprintf "%a" Mode.Uniqueness.Const.print
         (Axis_lattice.uniqueness required_bounds))
  | Modal (Monadic Contention) ->
    Some
      (Format_doc.asprintf "%a" Mode.Contention.Const.print
         (Axis_lattice.contention required_bounds))
  | Modal (Monadic Visibility) ->
    Some
      (Format_doc.asprintf "%a" Mode.Visibility.Const.print
         (Axis_lattice.visibility required_bounds))
  | Modal (Monadic Staticity) ->
    Some
      (Format_doc.asprintf "%a" Mode.Staticity.Const.print
         (Axis_lattice.staticity required_bounds))
  | Modal (Comonadic Areality) ->
    Some
      (Format_doc.asprintf "%a" Mode.Regionality.Const.print
         (Axis_lattice.areality required_bounds))
  | Modal (Comonadic Linearity) ->
    Some
      (Format_doc.asprintf "%a" Mode.Linearity.Const.print
         (Axis_lattice.linearity required_bounds))
  | Modal (Comonadic Portability) ->
    Some
      (Format_doc.asprintf "%a" Mode.Portability.Const.print
         (Axis_lattice.portability required_bounds))
  | Modal (Comonadic Forkable) ->
    Some
      (Format_doc.asprintf "%a" Mode.Forkable.Const.print
         (Axis_lattice.forkable required_bounds))
  | Modal (Comonadic Yielding) ->
    Some
      (Format_doc.asprintf "%a" Mode.Yielding.Const.print
         (Axis_lattice.yielding required_bounds))
  | Modal (Comonadic Statefulness) ->
    Some
      (Format_doc.asprintf "%a" Mode.Statefulness.Const.print
         (Axis_lattice.statefulness required_bounds))
  | Nonmodal Externality ->
    Some
      (Format_doc.asprintf "%a" Jkind_axis.Externality.print
         (Axis_lattice.externality required_bounds))

let pp_provenance_residual ppf { ty; mode_bounds; axes; _ } =
  let modes = List.filter_map (string_of_required_bound mode_bounds) axes in
  Format_doc.fprintf ppf "@[<hov 2>%s is not mod %a@]" ty
    (Format_doc.pp_print_list
       ~pp_sep:(fun ppf () -> Format_doc.fprintf ppf "@ ")
       Format_doc.pp_print_string)
    modes

let pp_provenance_residual_bullets ppf entries =
  List.iteri
    (fun i entry ->
      if i > 0 then Format_doc.fprintf ppf "@;";
      Format_doc.fprintf ppf "- %a" pp_provenance_residual entry)
    entries

let pp_type_definition_kind_annotation env ppf super_jkind =
  let super_jkind_single_line = format_jkind_single_line env super_jkind in
  let single_line_prefix =
    "Error: This type definition does not satisfy its kind annotation "
  in
  let error_prefix_width = String.length "Error: " in
  let continuation_indent = 2 in
  let continuation_prefix_width = error_prefix_width + continuation_indent in
  if
    String.length single_line_prefix + String.length super_jkind_single_line + 1
    <= 88
  then
    Format_doc.fprintf ppf
      "This type definition does not satisfy its kind annotation %s,"
      super_jkind_single_line
  else if
    continuation_prefix_width + String.length super_jkind_single_line + 1 <= 88
  then
    Format_doc.fprintf ppf
      "@[<v 2>This type definition does not satisfy its kind annotation@;%s,@]"
      super_jkind_single_line
  else
    Format_doc.fprintf ppf
      "@[<v 2>This type definition does not satisfy its kind annotation@;\
       @[<hov>%a,@]@]"
      pp_breakable_jkind_annotation super_jkind_single_line

let report_provenance_mode_crossing_error env ppf
    { super_jkind; sub_poly; super_poly; provenance_names; violating_axes; _ } =
  match
    provenance_residuals ~provenance_names ~violating_axes ~sub_poly ~super_poly
  with
  | None | Some [] -> None
  | Some [entry] ->
    Some
      (Format_doc.fprintf ppf "@[<v>%a@;because %a.@]"
         (pp_type_definition_kind_annotation env)
         super_jkind pp_provenance_residual entry)
  | Some entries ->
    Some
      (Format_doc.fprintf ppf "@[<v>%a@;because@;%a@]"
         (pp_type_definition_kind_annotation env)
         super_jkind pp_provenance_residual_bullets entries)

let report_mode_crossing_error ~offender env ppf
    { origin;
      sub_jkind;
      super_jkind;
      sub_poly;
      super_poly;
      failing_poly;
      fast_path;
      provenance_names;
      violating_axes
    } =
  let err =
    { origin;
      sub_jkind;
      super_jkind;
      sub_poly;
      super_poly;
      failing_poly;
      fast_path;
      provenance_names;
      violating_axes
    }
  in
  match report_provenance_mode_crossing_error env ppf err with
  | Some () -> ()
  | None ->
    Format_doc.fprintf ppf
      "@[<v>The mode crossing of %t is not allowed here.@;\
       @[<hov 2>The inferred kind is not below the required kind along %a.@]@]"
      offender pp_axis_list_prose violating_axes

let report_subjkind_error_with_offender ~offender env ppf = function
  | Jkind_error err ->
    Jkind.Violation.report_with_offender ~offender env ppf err
  | Mode_crossing_error err -> report_mode_crossing_error ~offender env ppf err

let report_subjkind_error_with_name ~name env ppf err =
  report_subjkind_error_with_offender
    ~offender:(fun ppf -> Format_doc.fprintf ppf "%s" name)
    env ppf err

let axis_disagreement_reasons (axes : Jkind_axis.Axis.packed list) :
    Jkind.Sub_failure_reason.t list =
  List.map (fun axis -> Jkind.Sub_failure_reason.Axis_disagreement axis) axes

let label_mutability_contribution (lbl : Types.label_declaration) =
  Ldd.const
    (match lbl.ld_mutable with
    | Immutable -> Axis_lattice.immediate
    | Mutable { atomic = Atomic; _ } -> Axis_lattice.sync_data
    | Mutable { atomic = Nonatomic; _ } -> Axis_lattice.mutable_data)

let sum_record_label_contributions ~(base : Ldd.node)
    ~(payload_kind : Types.type_expr -> Ldd.node)
    ?(label_mutability_provenance =
      fun (_ : Types.label_declaration) (poly : Ldd.node) -> poly)
    ~(validate_label : Types.label_declaration -> unit)
    (lbls : Types.label_declaration list) : Ldd.node =
  Ldd.sum lbls ~base ~f:(fun (lbl : Types.label_declaration) ->
      validate_label lbl;
      let mask = Axis_lattice.mask_of_modality lbl.ld_modalities in
      Ldd.join
        (label_mutability_provenance lbl (label_mutability_contribution lbl))
        (Ldd.meet (Ldd.const mask) (payload_kind lbl.ld_type)))

let no_validation (_ : Types.label_declaration) = ()

let validate_immutable_unboxed_label (lbl : Types.label_declaration) =
  match lbl.ld_mutable with
  | Immutable -> ()
  | Mutable _ ->
    failwith "ikind: mutable fields in unboxed records are not supported"

let format_type_expr_single_line ty =
  Format_doc.asprintf "%a" Jkind.format_type_expr ty
  |> String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c)

let label_mutability_provenance (ctx : Solver.ctx)
    (lbl : Types.label_declaration) (poly : Ldd.node) : Ldd.node =
  match lbl.ld_mutable with
  | Immutable -> poly
  | Mutable { atomic = Nonatomic; _ } ->
    Solver.with_provenance_text ctx
      (fun () ->
        Format.sprintf "mutable field %s : %s" (Ident.name lbl.ld_id)
          (format_type_expr_single_line lbl.ld_type))
      poly
  | Mutable { atomic = Atomic; _ } ->
    Solver.with_provenance_text ctx
      (fun () ->
        Format.sprintf "mutable field %s : %s [@atomic]" (Ident.name lbl.ld_id)
          (format_type_expr_single_line lbl.ld_type))
      poly

let decl_base_provenance (ctx : Solver.ctx) source poly =
  Solver.with_provenance_text ctx (fun () -> source) poly

(* Gather constructor-local vars from [tys]. *)
let collect_type_vars (tys : Types.type_expr list) :
    (int, Types.type_expr) Hashtbl.t =
  let vars = Hashtbl.create 16 in
  Types.with_type_mark (fun mark ->
      let super = Btype.type_iterators mark in
      let it =
        { super with
          it_type_expr =
            (fun self ty ->
              match Types.get_desc ty with
              | Types.Tvar _ | Types.Tunivar _ ->
                let id = Types.get_id ty in
                Hashtbl.replace vars id ty
              | _ -> super.it_type_expr self ty)
        }
      in
      List.iter (it.it_type_expr it) tys);
  vars

(* Use each local variable's declared jkind as its fallback bound. *)
let local_var_bounds (ctx : Solver.ctx)
    (local_vars : (int, Types.type_expr) Hashtbl.t) =
  let bounds = Hashtbl.create (Hashtbl.length local_vars) in
  Hashtbl.iter
    (fun id ty ->
      let bound =
        match Types.get_desc ty with
        | Types.Tvar { jkind; _ } | Types.Tunivar { jkind; _ } ->
          Solver.ckind_of_jkind ctx jkind
        | _ -> Ldd.const Axis_lattice.top
      in
      Hashtbl.replace bounds id bound)
    local_vars;
  bounds

(* For a plain-variable result argument, map it directly to [lhs_kind]. *)
let add_plain_var_projection ~(local_vars : ('a, Types.type_expr) Hashtbl.t)
    ~(local_subst : ('a, Ldd.node) Hashtbl.t) ~(lhs_kind : Ldd.node)
    (res_arg : Types.type_expr) : unit =
  match Types.get_desc res_arg with
  | Types.Tvar _ | Types.Tunivar _ ->
    let id = Types.get_id res_arg in
    if Hashtbl.mem local_vars id && not (Hashtbl.mem local_subst id)
    then Hashtbl.add local_subst id lhs_kind
  | _ -> ()

let make_gadt_payload_projector ~(decl_params : Types.type_expr list)
    (ctx : Solver.ctx) :
    Types.constructor_declaration -> Types.type_expr -> Ldd.node =
  (* For a GADT constructor, compute payload kinds under a projection from
     constructor-local vars (existentials/equated vars) to declaration
     parameters. This keeps payload ikinds comparable to the type's declared
     parameters while remaining conservative for unmapped locals.

     Examples:
     - type 'a t = C : 'b -> 'b t
       Here result arg is plain var ['b], aligned with param ['a], so we map
       ['b -> kind('a)] when computing payload kind of ['b].

     - type ('a, 'b) u = C : 'x * 'y -> ('x, int) u
       We map ['x -> kind('a)] from the first result arg. The second result
       arg is [int], so ['y] gets no projection and falls back to its declared
       local bound.

     - type ('a, 'b) same = C : 'x -> ('x, 'x) same
       First hit wins: ['x] is mapped from the first result arg to kind('a);
       the second occurrence does not overwrite it. *)
  let fallback ty = Solver.kind ~use_tables:true ctx ty in
  fun (c : Types.constructor_declaration) ->
    match c.cd_res with
    | None -> fallback
    | Some res -> (
      match Types.get_desc res with
      | Types.Tconstr (_, res_args, _) ->
        let payload_tys = Types.tys_of_constr_args c.cd_args in
        (* Step 1: collect constructor-local vars seen in payload/result.
           GADT constructor vars are in their own scope, distinct from the
           type declaration parameters. *)
        let local_vars = collect_type_vars (payload_tys @ res_args) in
        if Hashtbl.length local_vars = 0
        then fallback
        else
          let local_var_bounds = local_var_bounds ctx local_vars in
          (* Step 2: build a partial substitution local_var -> projected kind
             from the constructor result arguments. Earlier mappings win. *)
          let local_subst = Hashtbl.create (Hashtbl.length local_vars) in
          List.iter2
            (fun decl_param res_arg ->
              add_plain_var_projection ~local_vars ~local_subst
                ~lhs_kind:(Solver.kind ~use_tables:true ctx decl_param)
                res_arg)
            decl_params res_args;
          (* Step 3: apply the substitution to payload kinds:
             - mapped locals use their projected kinds
             - unmapped locals fall back to their declared bounds
             - non-local names are left unchanged. *)
          (* Rewrite projected local vars in payload kinds; unmapped locals
             fall back to their declared bounds. *)
          let map_name (name : Ldd.Name.t) =
            match name with
            | Ldd.Name.Param id -> (
              match Hashtbl.find_opt local_subst id with
              | Some projected -> projected
              | None ->
                if Hashtbl.mem local_vars id
                then
                  match Hashtbl.find_opt local_var_bounds id with
                  | Some bound -> bound
                  | None -> Ldd.const Axis_lattice.top
                else Solver.node_of_name ctx name)
            | Ldd.Name.Unknown _ | Ldd.Name.Provenance _ | Ldd.Name.Atom _
            | Ldd.Name.KAtom _ ->
              Solver.node_of_name ctx name
          in
          fun ty ->
            let raw_kind = Solver.kind ~use_tables:true ctx ty in
            Ldd.map_rigid map_name raw_kind
      | _ ->
        failwith
          "ikind: expected GADT constructor result to be a type constructor")

let type_decl_allows_any_crossing (decl : Types.type_declaration) =
  match decl.type_kind with
  | Types.Type_record (_, _, umc_opt)
  | Types.Type_record_unboxed_product (_, _, umc_opt)
  | Types.Type_variant (_, _, umc_opt) ->
    Option.is_some umc_opt
  | Types.Type_abstract _ | Types.Type_open -> false

let type_decl_rhs_kind_poly (ctx : Solver.ctx) (decl : Types.type_declaration) :
    Ldd.node =
  match decl.type_manifest with
  | Some body_ty -> Solver.kind ~use_tables:true ctx body_ty
  | None -> (
    let use_decl_jkind () = Solver.ckind_of_jkind ctx decl.type_jkind in
    match decl.type_kind with
    (* For abstract types and allow_any_crossing types, derive the ikind from
       the jkind annotation instead of computing it from the type declaration's
       body. *)
    | _ when type_decl_allows_any_crossing decl -> use_decl_jkind ()
    | Types.Type_abstract _ | Types.Type_open -> use_decl_jkind ()
    | Types.Type_record (lbls, rep, _umc_opt) ->
      let base =
        Ldd.const
          (match rep with
          | Types.Record_unboxed -> Axis_lattice.immediate
          (* CR box: This will no longer be [non_float] once we update the
             representation of singleton float64 records *)
          | _ -> Axis_lattice.immutable_data)
        |> decl_base_provenance ctx "this record type"
      in
      sum_record_label_contributions ~base
        ~payload_kind:(fun ty -> Solver.kind ~use_tables:true ctx ty)
        ~label_mutability_provenance:(label_mutability_provenance ctx)
        ~validate_label:no_validation lbls
    | Types.Type_record_unboxed_product (lbls, _rep, _umc_opt) ->
      let base =
        Ldd.const Axis_lattice.immediate
        |> decl_base_provenance ctx "this unboxed record type"
      in
      sum_record_label_contributions ~base
        ~payload_kind:(fun ty -> Solver.kind ~use_tables:true ctx ty)
        ~label_mutability_provenance:(label_mutability_provenance ctx)
        ~validate_label:validate_immutable_unboxed_label lbls
    | Types.Type_variant (_cstrs, Types.Variant_with_null, _umc_opt) ->
      (* [Variant_with_null] (i.e. [or_null]) has semantics that are not
         captured by its constructors: nullability/separability and
         mode-crossing are baked into its representation. We defer to jkinds
         because ikinds cannot express this today. This deferral can be removed
         once separability and nullability become layout properties rather than
         modal axes. *)
      use_decl_jkind ()
    | Types.Type_variant (cstrs, rep, _umc_opt) ->
      (* Choose base: immediate for void-only variants; otherwise immutable. *)
      let all_args_void =
        List.for_all
          (fun (c : Types.constructor_declaration) ->
            match c.cd_args with
            | Types.Cstr_tuple args ->
              List.for_all
                (fun (arg : Types.constructor_argument) ->
                  match arg.ca_sort with
                  | Some sort -> Jkind_types.Sort.Const.all_void sort
                  | None -> false)
                args
            | Types.Cstr_record lbls ->
              List.for_all
                (fun (lbl : Types.label_declaration) ->
                  match lbl.ld_sort with
                  | Some sort -> Jkind_types.Sort.Const.all_void sort
                  | None -> false)
                lbls)
          cstrs
      in
      let base =
        let base_lat =
          match rep with
          | Types.Variant_unboxed -> Axis_lattice.immediate
          | _ ->
            if all_args_void
            then Axis_lattice.immediate
            else Axis_lattice.immutable_data
        in
        Ldd.const base_lat |> decl_base_provenance ctx "this variant type"
      in
      let payload_kind_of_constructor =
        make_gadt_payload_projector ~decl_params:decl.type_params ctx
      in
      let constructor_contrib (c : Types.constructor_declaration) =
        let payload_kind = payload_kind_of_constructor c in
        match c.cd_args with
        | Types.Cstr_tuple args ->
          Ldd.sum args ~base:Ldd.bot
            ~f:(fun (arg : Types.constructor_argument) ->
              let mask = Axis_lattice.mask_of_modality arg.ca_modalities in
              Ldd.meet (Ldd.const mask) (payload_kind arg.ca_type))
        | Types.Cstr_record lbls ->
          sum_record_label_contributions ~base:Ldd.bot ~payload_kind
            ~label_mutability_provenance:(label_mutability_provenance ctx)
            ~validate_label:no_validation lbls
      in
      Ldd.sum cstrs ~base ~f:constructor_contrib)

let type_decl_constr_decl (decl : Types.type_declaration) : Solver.constr_decl =
  let abstract =
    match decl.type_manifest, decl.type_kind with
    | None, Types.Type_abstract _ -> not (Jkind.is_best decl.type_jkind)
    | _ -> false
  in
  let kind ctx = type_decl_rhs_kind_poly ctx decl in
  Solver.Ty { args = decl.type_params; kind; abstract }

(* Lookup function supplied to the solver.
   We prefer a stored ikind (when present) and otherwise recompute from the
   type declaration in [env]. *)
let lookup_of_env ~(env : Env.t) (path : Path.t) : Solver.constr_decl =
  match Env.find_type path env with
  | exception Not_found ->
    (* Format.eprintf "ERROR: unknown constructor %a@." Path.print path; *)
    (* WE CANNOT ACTUALLY GIVE AN ERROR HERE! *)
    (* Explanation: build systems sometimes heuristically do not
       include all cmis for performance reasons. Because of that,
       we could encounter types that appear not to exist. We must
       treat those as abstract unknowns. *)
    (* Fallback for unknown constructors: treat them as abstract,
       non-recursive values. *)
    let unknown = Ldd.Name.unknown (fresh_unknown_uid ()) in
    let kind : Solver.ckind = fun _ctx -> Ldd.node_of_var (Ldd.rigid unknown) in
    Solver.Ty { args = []; kind; abstract = true }
  | type_decl ->
    (* Here we can switch to using the cached ikind or not. *)
    let fallback () =
      (* When we have no stored ikind, compute from the declaration through the
         same RHS builder used for bound diagnostics. *)
      type_decl_constr_decl type_decl
    in
    (* Prefer a stored constructor ikind if one is present and enabled. *)
    let ikind =
      match type_decl.type_ikind with
      | Types.Constructor_ikind { base; coeffs } when !Clflags.ikinds ->
        Solver.Poly (base, coeffs)
      | Types.No_constructor_ikind reason ->
        if !Clflags.ikinds_debug then Format.eprintf "[ikind-miss] %s@." reason;
        fallback ()
      | Types.Constructor_ikind _ -> fallback ()
    in
    (if !Clflags.ikinds_debug
     then
       let ikind_msg =
         match ikind with
         | Solver.Ty _ -> "Ty"
         | Solver.Poly (base, coeffs) ->
           let coeffs =
             coeffs |> Array.map Ldd.pp |> Array.to_list |> String.concat "; "
           in
           Format.asprintf "Poly(base=%s; coeffs=[%s])" (Ldd.pp base) coeffs
       in
       Format.eprintf "[ikind] %a: %s@."
         (Format_doc.compat Path.print)
         path ikind_msg);
    ikind

(* Package the above into a full evaluation context. *)
let create_ctx ~(mode : Solver.mode) ~(env : Env.t option) =
  Solver.create_ctx ~mode ~env ~add_provenance:false
    ~lookup_of_env:(fun env path -> lookup_of_env ~env path)

let normalize ~(env : Env.t option) (jkind : Types.jkind_l) : Ldd.node =
  let ctx = create_ctx ~mode:Solver.Normal ~env in
  Solver.normalize (Solver.ckind_of_jkind ctx jkind)

let type_declaration_ikind ~(env : Env.t option) ~(path : Path.t) :
    Types.constructor_ikind =
  let ctx = create_ctx ~mode:Solver.Normal ~env in
  let base, coeffs = Solver.constr_kind_poly ctx path in
  constructor_ikind ~base ~coeffs

let type_declaration_ikind_gated ~(env : Env.t option) ~(path : Path.t) :
    Types.type_ikind =
  (* This function gets called separately for each
    type definition of a mutually recursive group. This is
    safe but computationally wasteful. In the future we might
    want to give this function a list of paths and compute the
    ikind for all of them at once. Alternatively, keep the cache
    between calls to this function from the same mutually recursive
    group. *)
  with_ikinds_enabled (fun () ->
      let ikind = type_declaration_ikind ~env ~path in
      (if !Clflags.ikinds_debug
       then
         let stored_jkind =
           match env with
           | None -> "?"
           | Some env -> (
             match Env.find_type path env with
             | exception Not_found -> "?"
             | _decl -> "<stored-jkind>")
         in
         Format.eprintf "[ikind] %a: stored=%s, base=%s, coeffs=[%s]@."
           (Format_doc.compat Path.print)
           path stored_jkind (Ldd.pp ikind.base) (pp_coeffs ikind.coeffs));
      ikind)

let type_declaration_ikind_of_jkind ~(env : Env.t option)
    ~(params : Types.type_expr list) (type_jkind : Types.jkind_l) :
    Types.type_ikind =
  with_ikinds_enabled (fun () ->
      let poly = normalize ~env type_jkind in
      let rigid_vars =
        List.map (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty))) params
      in
      let base, coeffs =
        Ldd.decompose_into_linear_terms ~universe:rigid_vars poly
      in
      let coeffs = Array.of_list coeffs in
      let payload = constructor_ikind ~base ~coeffs in
      if !Clflags.ikinds_debug
      then
        Format.eprintf "[ikind] from jkind: base=%s; coeffs=[%s]@."
          (Ldd.pp payload.base) (pp_coeffs payload.coeffs);
      payload)

let predef_ikind_of_jkind ~params type_jkind =
  type_declaration_ikind_of_jkind ~env:None ~params type_jkind

let () = Predef.set_ikind_of_jkind predef_ikind_of_jkind

type subcheck_fast_path =
  | No_fast_path
  | Rhs_top_fast_path
  | Lhs_mod_bounds_floor_fast_path

let string_of_subcheck_fast_path = function
  | No_fast_path -> "none"
  | Rhs_top_fast_path -> "rhs_top"
  | Lhs_mod_bounds_floor_fast_path -> "lhs_mod_bounds_floor"

type subcheck_polys =
  { lhs_for_leq : Ldd.node;
    rhs_for_leq : Ldd.node;
    fast_path : subcheck_fast_path
  }

let compute_bound_polys env (super : ('l2 * 'r2) Types.jkind)
    ~(lhs_floor : (Solver.ctx -> Ldd.node option) option)
    ~(lhs : Solver.ctx -> Ldd.node) : subcheck_polys =
  Provenance.reset ();
  let ctx = create_ctx ~mode:Solver.Normal ~env:(Some env) in
  let super_poly = Solver.ckind_of_jkind ctx super in
  let super_is_constant =
    Ldd.solve_pending ();
    Ldd.is_const super_poly
  in
  if
    super_is_constant
    && Axis_lattice.equal (Ldd.round_up super_poly) Axis_lattice.top
  then
    { lhs_for_leq = Ldd.bot;
      rhs_for_leq = super_poly;
      fast_path = Rhs_top_fast_path
    }
  else
    let floor_fast_path =
      if super_is_constant
      then
        match lhs_floor with
        | None -> None
        | Some lhs_floor -> (
          match lhs_floor ctx with
          | None -> None
          | Some lhs_floor ->
            let lhs_floor_or_super = Ldd.join lhs_floor super_poly in
            if
              Axis_lattice.equal
                (Ldd.round_up lhs_floor_or_super)
                Axis_lattice.top
            then Some lhs_floor
            else None)
      else None
    in
    match floor_fast_path with
    | Some lhs_floor ->
      { lhs_for_leq = lhs_floor;
        rhs_for_leq = super_poly;
        fast_path = Lhs_mod_bounds_floor_fast_path
      }
    | None ->
      let lhs_ctx =
        if super_is_constant
        then Solver.reset_for_mode ctx ~mode:Solver.Round_up
        else ctx
      in
      let lhs_poly = lhs lhs_ctx in
      { lhs_for_leq = lhs_poly;
        rhs_for_leq = super_poly;
        fast_path = No_fast_path
      }

(* Compute polynomials for a subcheck:
   - compute [super] in Normal mode
   - fast path: if [super] is constant top, no need to compute [sub]
   - otherwise, if [super] is constant, try the lhs mod-bounds floor fast path
   - otherwise, only round up [sub] if [super] is constant *)
let compute_subcheck_polys ~context:_ env (sub : ('l1 * 'r1) Types.jkind)
    (super : ('l2 * 'r2) Types.jkind) : subcheck_polys =
  compute_bound_polys env super
    ~lhs_floor:(Some (fun ctx -> Solver.mod_bounds_floor_of_jkind ctx sub))
    ~lhs:(fun ctx -> Solver.ckind_of_jkind ctx sub)

let compute_provenance_bound_polys env ~(lhs : Solver.ctx -> Ldd.node)
    (bound : Types.jkind_l) : subcheck_polys =
  compute_bound_polys env bound ~lhs_floor:None ~lhs:(fun ctx ->
      let ctx = Solver.reset_for_provenance ctx ~add_provenance:true in
      lhs ctx)

let check_mode_crossing_polys ~origin ~sub_jkind ~super_jkind
    { lhs_for_leq = sub_poly; rhs_for_leq = super_poly; fast_path } =
  let violating_axes = Ldd.leq_with_reason sub_poly super_poly in
  let failing_poly = Ldd.sub_subsets sub_poly super_poly in
  match violating_axes with
  | [] -> Ok ()
  | _ ->
    Error
      (Mode_crossing_error
         { origin;
           sub_jkind;
           super_jkind;
           sub_poly;
           super_poly;
           failing_poly;
           fast_path = string_of_subcheck_fast_path fast_path;
           provenance_names = Provenance.all_names ();
           violating_axes
         })

let subjkind_error_has_provenance_residuals = function
  | Jkind_error _ -> false
  | Mode_crossing_error
      { sub_poly; super_poly; provenance_names; violating_axes; _ } -> (
    match
      provenance_residuals ~provenance_names ~violating_axes ~sub_poly
        ~super_poly
    with
    | None | Some [] -> false
    | Some (_ :: _) -> true)

let same_axis_set axes1 axes2 =
  List.length axes1 = List.length axes2
  && List.for_all (fun axis1 -> List.exists (same_axis axis1) axes2) axes1

let subjkind_errors_have_same_violating_axes error1 error2 =
  match error1, error2 with
  | ( Mode_crossing_error { violating_axes = axes1; _ },
      Mode_crossing_error { violating_axes = axes2; _ } ) ->
    same_axis_set axes1 axes2
  | Jkind_error _, _ | _, Jkind_error _ -> false

let best_effort_provenance_error ?fallback_error ~origin ~sub_jkind ~super_jkind
    actual_error make_polys =
  let fallback error =
    match fallback_error with
    | None -> Error error
    | Some fallback_error -> (
      match fallback_error () with
      | None -> Error error
      | Some fallback_error -> Error fallback_error)
  in
  let provenance_check =
    match make_polys () with
    | provenance_polys ->
      check_mode_crossing_polys ~origin ~sub_jkind ~super_jkind provenance_polys
    | exception _ -> Ok ()
  in
  match provenance_check with
  | Ok () -> fallback actual_error
  | Error provenance_error ->
    if
      subjkind_errors_have_same_violating_axes actual_error provenance_error
      && subjkind_error_has_provenance_residuals provenance_error
    then Error provenance_error
    else fallback actual_error

let sub_jkind_l ?allow_any_crossing ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) env (sub : Types.jkind_l)
    (super : Types.jkind_l) : (unit, subjkind_error) result =
  let open Misc.Stdlib.Monad.Result.Syntax in
  if not (enable_sub_jkind_l && !Clflags.ikinds)
  then
    Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context env sub super
    |> Result.map_error (fun err -> Jkind_error err)
  else
    (* Check layouts first; if that fails, print both sides with full
       info and return the error. *)
    let* () =
      match Jkind.sub_layout_or_error ~context env sub super with
      | Ok () -> Ok ()
      | Error v -> Error (Jkind_error v)
    in
    let allow_any =
      match allow_any_crossing with Some true -> true | _ -> false
    in
    if allow_any
    then (
      (if !Clflags.ikinds_debug
       then
         let origin_suffix = origin_suffix_of origin in
         Format.eprintf "[ikind-subjkind] call%s allow_any=true@." origin_suffix);
      Ok ())
    else
      let { lhs_for_leq = sub_poly; rhs_for_leq = super_poly; fast_path } =
        compute_subcheck_polys ~context env sub super
      in
      match
        check_mode_crossing_polys ~origin ~sub_jkind:sub ~super_jkind:super
          { lhs_for_leq = sub_poly; rhs_for_leq = super_poly; fast_path }
      with
      | Ok () -> Ok ()
      | Error ikind_error -> (
        if !Clflags.ikinds_debug
        then Error ikind_error
        else
          match
            Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context env sub
              super
          with
          | Ok () -> Error ikind_error
          | Error jkind_error -> Error (Jkind_error jkind_error))

let check_bound ?allow_any_crossing ?origin ~type_equal ~context env ~actual
    ~bound ~provenance_lhs =
  if not (enable_sub_jkind_l && !Clflags.ikinds)
  then
    sub_jkind_l ?allow_any_crossing ?origin ~type_equal ~context env actual
      bound
  else
    let open Misc.Stdlib.Monad.Result.Syntax in
    let* () =
      match Jkind.sub_layout_or_error ~context env actual bound with
      | Ok () -> Ok ()
      | Error v -> Error (Jkind_error v)
    in
    let allow_any =
      match allow_any_crossing with Some true -> true | _ -> false
    in
    if allow_any
    then Ok ()
    else
      let actual_polys = compute_subcheck_polys ~context env actual bound in
      match
        check_mode_crossing_polys ~origin ~sub_jkind:actual ~super_jkind:bound
          actual_polys
      with
      | Ok () -> Ok ()
      | Error actual_error ->
        let fallback_error () =
          match
            Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context env
              actual bound
          with
          | Ok () ->
            let violating_axes =
              match actual_error with
              | Mode_crossing_error { violating_axes; _ } -> violating_axes
              | Jkind_error _ -> []
            in
            let axis_reasons = axis_disagreement_reasons violating_axes in
            Some
              (Jkind_error
                 (Jkind.Violation.of_ ~context env
                    (Jkind.Violation.Not_a_subjkind (actual, bound, axis_reasons))))
          | Error jkind_error -> Some (Jkind_error jkind_error)
        in
        best_effort_provenance_error ~fallback_error ~origin ~sub_jkind:actual
          ~super_jkind:bound actual_error (fun () ->
            compute_provenance_bound_polys env ~lhs:provenance_lhs bound)

let check_type_expr_bound ?origin ~type_equal ~context env ~ty ~actual ~bound =
  check_bound ?origin ~type_equal ~context env ~actual ~bound
    ~provenance_lhs:(fun ctx -> Solver.kind ~use_tables:true ctx ty)

let check_type_decl_bound ?allow_any_crossing ?origin ~type_equal ~context env
    ~decl ~actual ~bound =
  check_bound ?allow_any_crossing ?origin ~type_equal ~context env ~actual
    ~bound ~provenance_lhs:(fun ctx -> type_decl_rhs_kind_poly ctx decl)

let crossing_of_jkind ~(context : Jkind.jkind_context) env
    (jkind : ('l * 'r) Types.jkind) : Mode.Crossing.t =
  if not (enable_crossing && !Clflags.ikinds)
  then Jkind.get_mode_crossing ~context env jkind
  else
    let with_bounds_is_empty : type l r. (l * r) Types.with_bounds -> bool =
      function
      | No_with_bounds -> true
      | With_bounds _ -> false
    in
    match jkind.jkind.base with
    | Types.Layout _ when with_bounds_is_empty jkind.jkind.with_bounds ->
      Jkind.get_mode_crossing ~context env jkind
    | _ ->
      let ctx = create_ctx ~mode:Solver.Round_up ~env:(Some env) in
      let lat = Solver.round_up (Solver.ckind_of_jkind ctx jkind) in
      Axis_lattice.to_mode_crossing lat

let round_up_type env (ty : Types.type_expr) : Axis_lattice.t =
  let ctx = create_ctx ~mode:Solver.Round_up ~env:(Some env) in
  Solver.round_up (Solver.kind ~use_tables:false ctx ty)

let crossing_of_type env (ty : Types.type_expr) : Mode.Crossing.t =
  let lat = round_up_type env ty in
  Axis_lattice.to_mode_crossing lat

type sub_or_intersect = Jkind.sub_or_intersect

let with_bounds_is_empty : type l r. (l * r) Types.with_bounds -> bool =
  function
  | Types.No_with_bounds -> true
  | Types.With_bounds _ -> false

let fast_sub_of_value_sub : type r.
    Axis_lattice.t -> (Allowance.allowed * r) Types.jkind -> bool =
 fun super_lat (sub : (Allowance.allowed * r) Types.jkind) ->
  if Axis_lattice.equal super_lat Axis_lattice.top
  then true
  else if not (with_bounds_is_empty sub.jkind.with_bounds)
  then false
  else
    let sub_lat = Jkind.Mod_bounds.to_axis_lattice sub.jkind.mod_bounds in
    Axis_lattice.leq sub_lat super_lat

let fast_sub_of_any_super : type r.
    Types.mod_bounds -> (Allowance.allowed * r) Types.jkind -> bool =
 fun mod_bounds sub ->
  match sub.jkind.base with
  | Types.Layout
      (Jkind_types.Layout.Sort (_sub_sort, { nullability = _; separability = _ }))
    ->
    fast_sub_of_value_sub (Jkind.Mod_bounds.to_axis_lattice mod_bounds) sub
  | Types.Layout _ | Types.Kconstr _ -> false

let fast_sub_of_sort_super : type r.
    Jkind_types.Sort.t ->
    Types.mod_bounds ->
    (Allowance.allowed * r) Types.jkind ->
    bool =
 fun super_sort mod_bounds sub ->
  match sub.jkind.base with
  | Types.Layout
      (Jkind_types.Layout.Sort (sub_sort, { nullability = _; separability = _ }))
    ->
    if not (Jkind_types.Sort.equate sub_sort super_sort)
    then false
    else fast_sub_of_value_sub (Jkind.Mod_bounds.to_axis_lattice mod_bounds) sub
  | Types.Layout _ | Types.Kconstr _ -> false

let fast_sub : type r1 l2.
    context:Jkind.jkind_context ->
    Env.t ->
    (Allowance.allowed * r1) Types.jkind ->
    (l2 * Allowance.allowed) Types.jkind ->
    bool =
 fun ~context:_ _env (sub : (Allowance.allowed * r1) Types.jkind)
     (super : (l2 * Allowance.allowed) Types.jkind) ->
  match super.jkind with
  | { base =
        (* CR rtjoa for jujacobs: I guessed you want [max] here? *)
        Types.Layout
          (Jkind_types.Layout.Sort
             ( super_sort,
               { separability = Jkind_axis.Separability.Maybe_separable;
                 nullability = Jkind_axis.Nullability.Maybe_null
               } ));
      mod_bounds;
      with_bounds = Types.No_with_bounds
    } ->
    fast_sub_of_sort_super super_sort mod_bounds sub
  | { base =
        Types.Layout
          (Jkind_types.Layout.Any
             { separability = Jkind_axis.Separability.Maybe_separable;
               nullability = Jkind_axis.Nullability.Maybe_null
             });
      mod_bounds;
      with_bounds = Types.No_with_bounds
    } ->
    fast_sub_of_any_super mod_bounds sub
  | _ -> false

let sub_or_intersect ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) env
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) : sub_or_intersect =
  let debug_polys ?polys ~outcome () =
    if !Clflags.ikinds_debug
    then
      let sub_poly, super_poly =
        match polys with
        | Some polys -> polys
        | None ->
          let subcheck = compute_subcheck_polys ~context env t1 t2 in
          subcheck.lhs_for_leq, subcheck.rhs_for_leq
      in
      let origin_suffix = origin_suffix_of origin in
      Format.eprintf
        "[ikind-sub-or-intersect] outcome=%s%s@;@;sub_poly=%s@;super_poly=%s@."
        outcome origin_suffix (Ldd.pp sub_poly) (Ldd.pp super_poly)
  in
  let generic_sub_or_intersect () =
    (* Old behavior adapted to abstract kinds:
       1) gate on env-aware layout subchecking
       2) if layouts are compatible, decide based on ikind polynomials *)
    match Jkind.sub_layout_or_error ~context env t1 t2 with
    | Error _ -> (
      (* Keep Jkind as the source of Disjoint vs May_have_intersection
         classification when layouts fail. *)
      match Jkind.sub_or_intersect ~type_equal ~context env t1 t2 with
      | Jkind.Disjoint _ as disjoint ->
        debug_polys ~outcome:"Disjoint" ();
        disjoint
      | Jkind.May_have_intersection _ as maybe ->
        debug_polys ~outcome:"May_have_intersection" ();
        maybe
      | Jkind.Sub ->
        debug_polys ~outcome:"Sub" ();
        Jkind.Sub)
    | Ok () -> (
      let subcheck = compute_subcheck_polys ~context env t1 t2 in
      let sub_poly = subcheck.lhs_for_leq in
      let super_poly = subcheck.rhs_for_leq in
      match Ldd.leq_with_reason sub_poly super_poly with
      | [] ->
        debug_polys ~polys:(sub_poly, super_poly) ~outcome:"Sub" ();
        Jkind.Sub
      | violating_axes ->
        (if !Clflags.ikinds_debug
         then
           let axes = pp_axes violating_axes in
           Format.eprintf
             "[ikind-sub-or-intersect] outcome=May_have_intersection \
              axes=[%s]@."
             axes);
        debug_polys ~polys:(sub_poly, super_poly)
          ~outcome:"May_have_intersection" ();
        let reasons : Jkind.Sub_failure_reason.t Misc.Nonempty_list.t =
          match axis_disagreement_reasons violating_axes with
          | [] -> [Jkind.Sub_failure_reason.Layout_disagreement]
          | hd :: tl -> hd :: tl
        in
        Jkind.May_have_intersection reasons)
  in
  if not (enable_sub_or_intersect && !Clflags.ikinds)
  then Jkind.sub_or_intersect ~type_equal ~context env t1 t2
  else if fast_sub ~context env t1 t2
  then (
    (if !Clflags.ikinds_debug
     then
       let origin_suffix = origin_suffix_of origin in
       Format.eprintf "[ikind-sub-or-intersect] outcome=Sub%s fast_sub=true@."
         origin_suffix);
    Jkind.Sub)
  else generic_sub_or_intersect ()

let sub_or_error ?origin:_origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) env
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) :
    (unit, Jkind.Violation.t) result =
  if not (enable_sub_or_error && !Clflags.ikinds)
  then Jkind.sub_or_error ~type_equal ~context env t1 t2
  else
    let { lhs_for_leq = sub_poly; rhs_for_leq = super_poly; _ } =
      compute_subcheck_polys ~context env t1 t2
    in
    match Ldd.leq_with_reason sub_poly super_poly with
    | [] -> Ok ()
    | _ ->
      (* Delegate to Jkind for detailed error reporting. *)
      Jkind.sub_or_error ~type_equal ~context env t1 t2

(** Substitute constructor ikinds according to [lookup] without requiring Env.
*)

let poly_of_type_function_in_identity_env ~(params : Types.type_expr list)
    ~(body : Types.type_expr) : Ldd.node * Ldd.node array =
  (* Approximate type-function substitution by evaluating in an identity
     environment, i.e. every constructor contributes an independent rigid
     atom. *)
  let ctx = create_ctx ~mode:Solver.Normal ~env:None in
  let poly = Solver.normalize (Solver.kind ~use_tables:true ctx body) in
  let rigid_vars =
    List.map (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty))) params
  in
  let base, coeffs =
    Ldd.decompose_into_linear_terms ~universe:rigid_vars poly
  in
  base, Array.of_list coeffs

let substitute_decl_ikind_with_lookup
    ~(lookup_type : Path.t -> Subst.Ikind_substitution.type_lookup_result)
    ~(lookup_jkind : Path.t -> Subst.Ikind_substitution.jkind_lookup_result)
    (ikind_entry : Types.type_ikind) : Types.type_ikind =
  (* Inline type functions in an identity environment (no Env). *)
  match ikind_entry with
  | No_constructor_ikind _ -> ikind_entry
  | Constructor_ikind _ when reset_constructor_ikind_on_substitution ->
    Types.ikinds_todo "ikind substitution reset"
  | Constructor_ikind packed ->
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
      | Provenance _ -> Ldd.node_of_var (Ldd.rigid name)
      | Unknown _ -> Ldd.node_of_var (Ldd.rigid name)
      | KAtom path -> (
        match lookup_jkind path with
        | Subst.Ikind_substitution.Lookup_jkind_identity ->
          Ldd.node_of_var (Ldd.rigid name)
        | Subst.Ikind_substitution.Lookup_jkind_path alias_path ->
          Ldd.node_of_var (Ldd.rigid (Ldd.Name.katom alias_path))
        | Subst.Ikind_substitution.Lookup_jkind_const jkind_const ->
          let raw =
            let ctx = create_ctx ~mode:Solver.Normal ~env:None in
            Solver.normalize (Solver.ckind_of_jkind_desc ctx jkind_const)
          in
          map_poly expanding raw)
      | Atom { constr = path; arg_index } -> (
        match lookup_type path with
        | Subst.Ikind_substitution.Lookup_identity ->
          Ldd.node_of_var (Ldd.rigid name)
        | Subst.Ikind_substitution.Lookup_path alias_path ->
          Ldd.node_of_var (Ldd.rigid (Ldd.Name.atomic alias_path arg_index))
        | Subst.Ikind_substitution.Lookup_type_fun (params, body) ->
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

let () =
  Subst.Ikind_substitution.substitute_decl_ikind_with_lookup
    := substitute_decl_ikind_with_lookup
