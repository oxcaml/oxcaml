(* This forces ikinds globally on. *)
Clflags.ikinds := true

(* Types.ikind_debug := true *)
let enable_crossing = true
let enable_sub_jkind_l = true
let enable_sub_or_intersect = false
let enable_sub_or_error = false

module Ldd = Ikind.Ldd

module JK = struct
  (* The JKind solver specialized to Ikind.Ldd and Types.type_expr. *)

  module Ldd = Ikind.Ldd

  type ty = Types.type_expr

  type constr = Ldd.constr

  type lat = Ldd.lat

  type poly = Ldd.node

  type kind = Ldd.node

  (* Hash tables avoiding polymorphic structural comparison on deep values. *)
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

  type ops =
    { const : lat -> kind;
      join : kind -> kind -> kind;
      meet : kind -> kind -> kind;
      modality : lat -> kind -> kind;
      constr : constr -> kind list -> kind;
      kind_of : ty -> kind;
      rigid : ty -> kind
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
    Ldd.clear_memos ();
    (* Define all the trivial ops *)
    let const l = Ldd.const l in
    let join a b = Ldd.join a b in
    let modality l k = Ldd.meet (Ldd.const l) k in
    let meet a b = Ldd.meet a b in
    (* Create hash table mapping ty to kind for memoization *)
    let ty_to_kind = TyTbl.create 0 in
    let rigid t =
      let param = Types.get_id t in
      Ldd.var (Ldd.rigid (Ldd.Name.param param))
    in
    (* And hash table mapping constructor to coefficients *)
    let constr_to_coeffs = ConstrTbl.create 0 in
    (* Define kind_of and constr ops *)
    let rec kind_of (t : ty) : kind =
      match TyTbl.find_opt ty_to_kind t with
      | Some k -> k
      | None ->
        (* Pre-insert lattice solver var for this type *)
        let v = Ldd.new_var () in
        TyTbl.add ty_to_kind t (Ldd.var v);
        let kind = env.kind_of t ops in
        (* Always solve LFPs here to avoid correctness issues *)
        Ldd.solve_lfp v kind;
        kind
    and constr_kind c =
      match ConstrTbl.find_opt constr_to_coeffs c with
      | Some base_and_coeffs -> base_and_coeffs
      | None -> (
        match env.lookup c with
        | Poly (base, coeffs) ->
          (* CR jujacobs: cycle-breaker for cached polynomials
             --------------------------------------------------
             We install placeholder LDD variables for the base and each
             coefficient before rehydrating the polynomial via map_rigid.
             This prevents recursive rehydration from looping when cached
             polynomials reference each other cyclically (common in recursive
             type groups). Once placeholders are published to the cache, we
             rehydrate the right-hand sides and solve LFPs to define them.

             A more principled approach would construct these polynomials in a
             strictly topologically-safe order or use an explicit fixpoint
             builder for constructor coefficients; this hack should be replaced
             by such a mechanism when the solver grows a dedicated interface. *)
          let base_var = Ldd.new_var () in
          let coeff_vars =
            List.init (List.length coeffs) (fun _ -> Ldd.new_var ())
          in
          ConstrTbl.add constr_to_coeffs c
            (Ldd.var base_var, List.map Ldd.var coeff_vars);
          let instantiate (name : Ldd.Name.t) : kind =
            match name with
            | Ldd.Name.Param _ -> Ldd.var (Ldd.rigid name)
            | Ldd.Name.Atom { constr = constr'; arg_index } ->
              if Path.compare constr' c = 0
              then Ldd.var (Ldd.rigid name)
              else
                let base', coeffs' = constr_kind constr' in
                if arg_index = 0
                then base'
                else
                  match List.nth_opt coeffs' (arg_index - 1) with
                  | Some coeff -> coeff
                  | None -> Ldd.var (Ldd.rigid name)
          in
          let rehydrate node = Ldd.map_rigid instantiate node in
          let base_rhs = rehydrate base in
          let coeffs_rhs = List.map rehydrate coeffs in
          Ldd.solve_lfp base_var base_rhs;
          List.iter2 (fun v rhs -> Ldd.solve_lfp v rhs) coeff_vars coeffs_rhs;
          Ldd.var base_var, List.map Ldd.var coeff_vars
        | Ty { args; kind; abstract } ->
          let base = Ldd.new_var () in
          (* Allocate coefficient vars based on declared arity, not on ks
             length *)
          let coeffs = List.init (List.length args) (fun _ -> Ldd.new_var ()) in
          ConstrTbl.add constr_to_coeffs c
            (Ldd.var base, List.map Ldd.var coeffs);
          (* Recursively compute the kind of the body *)
          let rigid_vars =
            List.map
              (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty)))
              args
          in
          List.iter2
            (fun ty var -> TyTbl.add ty_to_kind ty (Ldd.var var))
            args rigid_vars;
          (* Compute body kind *)
          (* CR jujacobs: we shouldn't need to compute the kind if
             we are in Right mode and abstract=true, but currently this is
             needed to correctly populate the cache. *)
          let kind' = kind ops in
          (* Extract coeffs' from kind' *)
          let base', coeffs' =
            Ldd.decompose_linear ~universe:rigid_vars kind'
          in
          if List.length coeffs <> List.length coeffs'
          then
            failwith
              (Printf.sprintf
                 "jkind_solver: coeffs mismatch for constr %s (length %d vs %d)"
                 (constr_to_string c) (List.length coeffs) (List.length coeffs'));
          if abstract
          then (
            (* We need to assert that kind' is less than or equal to the base *)
            Ldd.enqueue_gfp base
              (Ldd.meet base' (Ldd.var (Ldd.rigid (Ldd.Name.atomic c 0))));
            let i = ref 0 in
            List.iter2
              (fun coeff coeff' ->
                let idx = !i in
                let rhs = Ldd.join coeff' base' in
                let bound =
                  Ldd.meet rhs
                    (Ldd.var (Ldd.rigid (Ldd.Name.atomic c (idx + 1))))
                in
                incr i;
                Ldd.enqueue_gfp coeff bound)
              coeffs coeffs')
          else (
            Ldd.solve_lfp base base';
            List.iter2
              (fun coeff coeff' -> Ldd.solve_lfp coeff coeff')
              coeffs coeffs');
          Ldd.var base, List.map Ldd.var coeffs)
    and constr c ks =
      let base, coeffs = constr_kind c in
      (* Meet each arg with the corresponding coeff *)
      let ks' =
        List.mapi
          (fun i coeff ->
            match List.nth_opt ks i with
            | Some k -> Ldd.meet k coeff
            | None -> failwith "Missing arg")
          coeffs
      in
      (* Join all the ks'' plus the base *)
      List.fold_left Ldd.join base ks'
    and ops =
      { const;
        join;
        meet;
        modality;
        constr;
        kind_of;
        rigid
      }
    in
    let constr_kind_poly c =
      let base, coeffs = constr_kind c in
      (* Ensure any pending fixpoints are installed before inspecting. *)
      Ldd.solve_pending ();
      let coeffs_minus_base =
        List.map (fun p -> Ldd.sub_subsets p base) coeffs
      in
      base, coeffs_minus_base
    in
    { ops; constr_kind_poly }

  let normalize (solver : solver) (k : ckind) : poly =
    let k' = k solver.ops in
    Ldd.solve_pending ();
    k'

  let constr_kind_poly (solver : solver) (c : constr) : poly * poly list =
    solver.constr_kind_poly c

  let leq_with_reason (solver : solver) (k1 : ckind) (k2 : ckind) :
      int list option =
    let k2' = k2 solver.ops in
    let k1' = k1 solver.ops in
    Ldd.solve_pending ();
    Ldd.leq_with_reason k1' k2'

  let round_up (solver : solver) (k : ckind) : lat =
    let k' = k solver.ops in
    Ldd.round_up k'

end

let ikind_reset : string -> Types.type_ikind = Types.ikind_reset

let ckind_of_jkind (j : ('l * 'r) Types.jkind) : JK.ckind =
 fun (ops : JK.ops) ->
  (* Base is the modality bounds stored on this jkind. *)
  let base =
    ops.const
      (Axis_lattice.of_mod_bounds j.jkind.mod_bounds)
  in
  (* For each with-bound (ty, axes), contribute
     modality(axes_mask, kind_of ty). *)
  Jkind.With_bounds.to_seq j.jkind.with_bounds
  |> List.of_seq
  |> List.fold_left (fun acc (ty, info) ->
         let axes = Jkind.With_bounds.type_info_relevant_axes info in
         let mask = Axis_lattice.of_axis_set axes in
         let kty = ops.kind_of ty in
         ops.join acc (ops.modality mask kty))
       base

let ckind_of_jkind_l (j : Types.jkind_l) : JK.ckind = ckind_of_jkind j

let ckind_of_jkind_r (j : Types.jkind_r) : JK.ckind =
 fun (ops : JK.ops) ->
  (* For r-jkinds used in sub checks, with-bounds are not present
     on the right (see Jkind_desc.sub's precondition). So only the
     base mod-bounds matter. *)
  let base = ops.const (Axis_lattice.of_mod_bounds j.jkind.mod_bounds) in
  base

let kind_of_depth = ref 0

let kind_of_counter = ref 0

let kind_of ~(context : Jkind.jkind_context) (ty : Types.type_expr) : JK.ckind =
 fun (ops : JK.ops) ->
  ignore context;
  incr kind_of_depth;
  if !kind_of_depth > 50 then failwith "kind_of_depth too deep" else ();
  incr kind_of_counter;
  if !kind_of_counter > 10000000 then failwith "kind_of_counter too big" else ();
  let res =
    match Types.get_desc ty with
    | Types.Tvar { name = _name; jkind } | Types.Tunivar { name = _name; jkind }
      ->
      (* TODO: allow general jkinds here (including with-bounds) *)
      let jkind_l = Jkind.disallow_right jkind in
      let ckind = ckind_of_jkind_l jkind_l in
      ops.meet (ops.rigid ty) (ckind ops)
    | Types.Tconstr (p, args, _abbrev_memo) ->
      let arg_kinds = List.map ops.kind_of args in
      ops.constr p arg_kinds
    | Types.Ttuple elts ->
      (* Boxed tuples: immutable_data base + per-element contributions
         under id modality. *)
      let base = ops.const Axis_lattice.immutable_data in
      List.fold_left
        (fun acc (_lbl, t) ->
          let mask = Axis_lattice.mask_shallow in
          ops.join acc (ops.modality mask (ops.kind_of t)))
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
          ops.join acc (ops.modality mask (ops.kind_of t)))
        (ops.const Axis_lattice.bot) elts
    | Types.Tarrow (_lbl, _t1, _t2, _commu) ->
      (* Arrows use the dedicated per-axis bounds (no with-bounds). *)
      ops.const Axis_lattice.arrow
    | Types.Tlink _ -> failwith "Tlink shouldn't appear in kind_of"
    | Types.Tsubst _ -> failwith "Tsubst shouldn't appear in kind_of"
    | Types.Tpoly _ ->
      ops.const Axis_lattice.value
    | Types.Tof_kind jkind ->
      ckind_of_jkind jkind ops
    | Types.Tobject _ ->
      ops.const Axis_lattice.object_legacy
    | Types.Tfield _ ->
      failwith "Tfield shouldn't appear in kind_of"
      (* ops.const Axis_lattice.value *)
    | Types.Tnil ->
      failwith "Tnil shouldn't appear in kind_of"
      (* ops.const Axis_lattice.value *)
    | Types.Tvariant row ->
      if Btype.tvariant_not_immediate row
      then
        if Btype.static_row row
        then
          (* Closed, boxed polymorphic variant: immutable_data base plus
             per-constructor args. *)
          let base = ops.const Axis_lattice.immutable_data in
          let mask = Axis_lattice.mask_shallow in
          Btype.fold_row
            (fun acc ty ->
              let k_ty = ops.kind_of ty in
              let k = ops.modality mask k_ty in
              ops.join acc k)
            base row
        else
          (* Open row: conservative non-float value (boxed). *)
          ops.const Axis_lattice.nonfloat_value
      else
        (* All-constant (immediate) polymorphic variant. *)
        ops.const Axis_lattice.immediate
    | Types.Tpackage _ ->
      ops.const Axis_lattice.nonfloat_value
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

type constructor_ikind_payload =
  { base : JK.poly;
    coeffs : JK.poly array
  }

let pack_constructor_ikind (payload : constructor_ikind_payload) :
    Types.constructor_ikind =
  Obj.magic payload

let unpack_constructor_ikind (packed : Types.constructor_ikind) :
    constructor_ikind_payload =
  Obj.magic packed

let constructor_ikind_polynomial
    (packed : Types.constructor_ikind) : JK.poly * JK.poly list =
  let payload = unpack_constructor_ikind packed in
  payload.base, Array.to_list payload.coeffs

let lookup_of_context ~(context : Jkind.jkind_context) (p : Path.t) :
    JK.constr_decl =
  (* We may need to be careful here to look up the right thing: what happens on GADT-installed equations? *)
  match context.lookup_type p with
  | None ->
    (* CR jujacobs: unknown-constructor fallback
       ----------------------------------------
       When we don't find a declaration in [context.lookup_type], we treat the
       constructor as abstract with a conservative [value] base, ignoring
       arguments. This avoids deep recursion and speculation about arities.
       Longer-term, we should ensure contexts always supply enough lookup
       information, or thread Env through places that need it, and delete this
       fallback. *)
    let kind : JK.ckind = fun (ops : JK.ops) -> ops.const Axis_lattice.value in
    JK.Ty { args = []; kind; abstract = true }
  | Some decl ->
    (* Here we can switch to using the cached ikind or not. *)
    let fallback () =
      match decl.type_manifest with
      | None ->
        begin
        (* No manifest: may still be concrete (record/variant/...). Build
           ckind. *)
        match decl.type_kind with
        | Types.Type_abstract _ ->
          let kind : JK.ckind = ckind_of_jkind_l decl.type_jkind in
          JK.Ty { args = decl.type_params; kind; abstract = true }
        | Types.Type_record (lbls, rep, _umc_opt) ->
          (* Build from components: base (non-float value) + per-label
             contributions. *)
          let base_lat =
            if has_mutable_label lbls
            then Axis_lattice.mutable_data
            else Axis_lattice.immutable_data
          in
          let relevant_for_shallow = relevance_of_rep (`Record rep) in
          let kind : JK.ckind =
           fun (ops : JK.ops) ->
            let base = ops.const base_lat in
            List.fold_left
              (fun acc (lbl : Types.label_declaration) ->
                let mask =
                  Axis_lattice.mask_of_modality
                    ~relevant_for_shallow
                    lbl.ld_modalities
                in
                ops.join acc (ops.modality mask (ops.kind_of lbl.ld_type)))
              base lbls
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_record_unboxed_product (lbls, _rep, _umc_opt) ->
          (* Unboxed products: non-float base; shallow axes relevant only
             for arity = 1. *)
          let base_lat =
            if has_mutable_label lbls
            then Axis_lattice.mutable_data
            else Axis_lattice.nonfloat_value
          in
          let kind : JK.ckind =
           fun (ops : JK.ops) ->
            let base = ops.const base_lat in
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
                ops.join acc (ops.modality mask (ops.kind_of lbl.ld_type)))
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
           fun (ops : JK.ops) ->
            let base = ops.const base_lat in
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
                      ops.join acc (ops.modality mask (ops.kind_of arg.ca_type)))
                    acc args
                | Types.Cstr_record lbls ->
                  List.fold_left
                    (fun acc (lbl : Types.label_declaration) ->
                      let mask =
                        Axis_lattice.mask_of_modality
                          ~relevant_for_shallow
                          lbl.ld_modalities
                      in
                      ops.join acc (ops.modality mask (ops.kind_of lbl.ld_type)))
                    acc lbls)
              base cstrs
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_open ->
          let kind : JK.ckind =
           fun ops ->
            ops.const Axis_lattice.value
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        end
      | Some body_ty ->
        (* Concrete: compute kind of body. *)
        let args = decl.type_params in
        let kind : JK.ckind =
         fun ops ->
          ops.kind_of body_ty
        in
        JK.Ty { args; kind; abstract = false }
    in
    match decl.type_ikind with
    | Types.Constructor_ikind constructor when !Clflags.ikinds ->
      let base, coeffs =
        constructor_ikind_polynomial constructor
      in
      JK.Poly (base, coeffs)
    | Types.No_constructor_ikind reason ->
      (* Print the reason *)
      ignore reason;
      (*= Format.eprintf "[ikind-miss] %s@." reason; *)
      fallback ()
    | Types.Constructor_ikind _ -> fallback ()

(* Package the above into a full solver environment. *)
let make_solver ~(context : Jkind.jkind_context) : JK.solver =
  JK.make_solver
    { kind_of = kind_of ~context; lookup = lookup_of_context ~context }

let normalize ~(context : Jkind.jkind_context) (jkind : Types.jkind_l) :
    Ikind.Ldd.node =
  let solver = make_solver ~context in
  JK.normalize solver (ckind_of_jkind_l jkind)

let type_declaration_ikind ~(context : Jkind.jkind_context)
    ~(path : Path.t) : Types.constructor_ikind =
  let solver = make_solver ~context in
  let base, coeffs = JK.constr_kind_poly solver path in
  let coeffs_array = Array.of_list coeffs in
  pack_constructor_ikind { base; coeffs = coeffs_array }

let type_declaration_ikind_gated ~(context : Jkind.jkind_context)
    ~(path : Path.t) : Types.type_ikind =
  if not !Clflags.ikinds
  then Types.ikind_reset "ikinds disabled"
  else
    let ikind = type_declaration_ikind ~context ~path in
    let payload = unpack_constructor_ikind ikind in
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

let sub_jkind_l ?allow_any_crossing ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) (sub : Types.jkind_l)
    (super : Types.jkind_l) : (unit, Jkind.Violation.t) result =
  ignore origin;
  let open Misc.Stdlib.Monad.Result.Syntax in
  (* Check layouts first; if that fails, print both sides with full
     info and return the error. *)
  let* () =
    match Jkind.sub_jkind_l_layout ~context sub super with
    | Ok () -> Ok ()
    | Error v -> Error v
  in
  if not (enable_sub_jkind_l && !Clflags.ikinds)
  then Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context sub super
  else
    let solver = make_solver ~context in
    let allow_any =
      match allow_any_crossing with Some true -> true | _ -> false
    in
    if allow_any
    then Ok ()
    else
      let sub_ckind = ckind_of_jkind_l sub in
      let super_ckind = ckind_of_jkind_l super in
      let ik_leq = JK.leq_with_reason solver sub_ckind super_ckind in
      match ik_leq with
      | None -> Ok ()
      | Some violating_axes ->
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
    ~(context : Jkind.jkind_context) (sub : Types.jkind_l)
    (super : Types.jkind_r) : bool =
  let _ = type_equal, origin in
  if not !Clflags.ikinds
  then Jkind.sub ~type_equal ~context sub super
  else
    let solver = make_solver ~context in
    match
      JK.leq_with_reason solver (ckind_of_jkind_l sub) (ckind_of_jkind_r super)
    with
    | None -> true
    | Some _ -> false

(* CR jujacobs: this is really slow when enabled. Fix performance. *)
let crossing_of_jkind ~(context : Jkind.jkind_context)
    (jkind : ('l * 'r) Types.jkind) : Mode.Crossing.t =
  if not (enable_crossing && !Clflags.ikinds) (* CR jujacobs: fix this *)
  then Jkind.get_mode_crossing ~context jkind
  else
    let solver = make_solver ~context in
    let lat = JK.round_up solver (ckind_of_jkind jkind) in
    let mb = Axis_lattice.to_mod_bounds lat in
    Jkind.Mod_bounds.to_mode_crossing mb

(* Intentionally no ikind versions of sub_or_intersect / sub_or_error.
   Keep Jkind as the single source for classification and error reporting. *)
(* CR jujacobs: fix this *)
type sub_or_intersect = Ikind.sub_or_intersect

(* CR jujacobs: performance issue here. *)
let sub_or_intersect ?origin:_origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) : sub_or_intersect =
  if not (enable_sub_or_intersect && !Clflags.ikinds) (* CR jujacobs: fix this *)
  then Jkind.sub_or_intersect ~type_equal ~context t1 t2
  else
    (* CR jujacobs: enable this *)
    let _ik =
      sub ~type_equal ~context (Jkind.disallow_right t1)
        (Jkind.disallow_left t2)
    in
    (* Preserve canonical Jkind classification for now. *)
    Jkind.sub_or_intersect ~type_equal ~context t1 t2

let sub_or_error ?origin:_origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) :
    (unit, Jkind.Violation.t) result =
  if not (enable_sub_or_error && !Clflags.ikinds)
  then Jkind.sub_or_error ~type_equal ~context t1 t2
  else if sub ~type_equal ~context (Jkind.disallow_right t1)
            (Jkind.disallow_left t2)
  then Ok ()
  else
    (* Delegate to Jkind for detailed error reporting. *)
    Jkind.sub_or_error ~type_equal ~context t1 t2

(**
  Substitution over constructor ikinds (polynomials)

  We sometimes need to apply a Subst-style mapping of type constructors to
  other constructors or to type functions, but without access to an Env. For
  ikinds, we can interpret such substitutions directly on the cached
  polynomials (constructor_ikind) by mapping rigid atoms via Ldd.map_rigid.

  The caller supplies a [lookup] function that describes the substitution:
  - None: the path is unchanged (identity)
  - `Path q: replace occurrences of constructor [p] with [q]
  - `Type_fun (params, body): inline a type function; we convert [body] to a
    polynomial in an identity environment (where every constructor [r] has a
    polynomial consisting solely of its self-atoms) and use its base/coefficient
    polynomials. This avoids Env while keeping results stable.
*)

let rec max_arity_in_type (acc : int Path.Map.t) (ty : Types.type_expr) =
  let open Types in
  match get_desc ty with
  | Tconstr (p, args, _) ->
      let n = List.length args in
      let prev = match Path.Map.find_opt p acc with None -> 0 | Some m -> m in
      let acc = if n > prev then Path.Map.add p n acc else acc in
      List.fold_left max_arity_in_type acc args
  | Ttuple elts -> List.fold_left (fun a (_, t) -> max_arity_in_type a t) acc elts
  | Tunboxed_tuple elts -> List.fold_left (fun a (_, t) -> max_arity_in_type a t) acc elts
  | Tarrow (_, t1, t2, _) -> max_arity_in_type (max_arity_in_type acc t1) t2
  | Tpoly (t, ts) -> List.fold_left max_arity_in_type (max_arity_in_type acc t) ts
  | Tobject (t, _) -> max_arity_in_type acc t
  | Tfield (_, _, t1, t2) -> max_arity_in_type (max_arity_in_type acc t1) t2
  | Tvar _ | Tunivar _ | Tlink _ | Tsubst _ | Tnil | Tvariant _ | Tpackage _
  | Tof_kind _ -> acc

let identity_lookup_from_arity_map (arity : int Path.Map.t) (p : Path.t)
    : JK.constr_decl =
  let open Ldd in
  let n = match Path.Map.find_opt p arity with None -> 0 | Some m -> m in
  let base = var (rigid (Name.atomic p 0)) in
  let coeffs =
    List.init n (fun i -> var (rigid (Name.atomic p (i + 1))))
  in
  JK.Poly (base, coeffs)

let poly_of_type_function_in_identity_env ~(params : Types.type_expr list)
    ~(body : Types.type_expr) : JK.poly * JK.poly list =
  (* CR jujacobs: identity-environment evaluation of type functions
     --------------------------------------------------------------
     We approximate type-function substitution for ikinds without Env by
     evaluating the body in an "identity environment" where every constructor
     [p] is mapped to a polynomial consisting solely of its own rigid atoms
     (one base and as many coefficients as observed arity in [body]). This
     intentionally does not recurse into other environments or unfold further
     information; it is a local, stable interpretation suitable for
     substitution. If/when Env is available here, we should replace this with a
     proper evaluation against real declarations and cached constructor ikinds. *)
  let arity = max_arity_in_type Path.Map.empty body in
  let lookup p = identity_lookup_from_arity_map arity p in
  let dummy_context =
    (* dummy context; currently ignored *)
    { Jkind.jkind_of_type = (fun _ -> None)
    ; is_abstract = (fun _ -> false)
    ; lookup_type = (fun _ -> None)
    ; debug_print_env = (fun _ppf -> ())
    }
  in
  let kind_of_identity = kind_of ~context:dummy_context in
  let env : JK.env = { kind_of = kind_of_identity; lookup } in
  let solver = JK.make_solver env in
  let poly = JK.normalize solver (kind_of_identity body)
  in
  let rigid_vars =
    List.map (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty))) params
  in
  Ldd.decompose_linear ~universe:rigid_vars poly

let substitute_decl_ikind_with_lookup
    ~(lookup : Path.t -> [ `Path of Path.t
                         | `Type_fun of Types.type_expr list * Types.type_expr ] option)
    (entry : Types.type_ikind) : Types.type_ikind =
  match entry with
  | Types.No_constructor_ikind _ -> entry
  | Types.Constructor_ikind packed ->
      let payload = unpack_constructor_ikind packed in
      let memo : (Path.t, (JK.poly * JK.poly list)) Hashtbl.t = Hashtbl.create 17 in
      let map_name (name : Ldd.Name.t) : Ldd.node =
        match name with
        | Ldd.Name.Param _ -> Ldd.var (Ldd.rigid name)
        | Ldd.Name.Atom { constr = p; arg_index } ->
            (match lookup p with
             | None -> Ldd.var (Ldd.rigid name)
             | Some (`Path q) ->
                 Ldd.var (Ldd.rigid (Ldd.Name.atomic q arg_index))
             | Some (`Type_fun _ as tf) ->
                 let base, coeffs =
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
                 if arg_index = 0 then base
                 else
                   match List.nth_opt coeffs (arg_index - 1) with
                   | Some k -> k
                   | None ->
                       (* Fallback: if coefficient missing, keep original atom *)
                       Ldd.var (Ldd.rigid name))
      in
      let base' = Ldd.map_rigid map_name payload.base in
      let coeffs' = Array.map (Ldd.map_rigid map_name) payload.coeffs in
      Types.Constructor_ikind (pack_constructor_ikind { base = base'; coeffs = coeffs' })
