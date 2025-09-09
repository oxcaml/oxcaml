(* Minimal kind constructor over real Types.type_expr, inspired by infer6. *)

module TyM = struct
  type t = Types.type_expr

  let compare (t1 : t) (t2 : t) =
    Int.compare (Types.get_id t1) (Types.get_id t2)

  (* Avoid depending on Printtyp to prevent module cycles. *)
  let to_string (t : t) : string = Printf.sprintf "ty#%d" (Types.get_id t)
end

module ConstrM = struct
  type t = Path.t

  let compare = Path.compare

  let to_string (p : t) : string = Format.asprintf "%a" Path.print p
end

module JK = Ldd_jkind_solver.Make (Axis_lattice) (TyM) (ConstrM)

  (* Optional ambient tag to disambiguate higher-level call sites
     (e.g. includecore). Other modules can bracket calls with
     [with_origin_tag] to add this suffix. *)
let __ikind_origin_tag : string option ref = ref None

let with_origin_tag (tag : string) (f : unit -> 'a) : 'a =
  let prev = !__ikind_origin_tag in
  __ikind_origin_tag := Some tag;
  Fun.protect ~finally:(fun () -> __ikind_origin_tag := prev) f

let ckind_of_jkind (j : ('l * 'r) Types.jkind) : JK.ckind =
  fun (ops : JK.ops) ->
    (* Base is the modality bounds stored on this jkind. *)
    let base = ops.const (Axis_lattice.of_mod_bounds j.jkind.mod_bounds) in
    (* For each with-bound (ty, axes), contribute
       modality(axes_mask, kind_of ty). *)
    let contribs =
      Jkind.With_bounds.to_seq j.jkind.with_bounds
      |> List.of_seq
      |> List.map (fun (ty, info) ->
              let axes = Jkind.With_bounds.type_info_relevant_axes info in
              let mask = Axis_lattice.of_axis_set axes in
              let kty = ops.kind_of ty in
              ops.modality mask kty)
    in
    ops.join (base :: contribs)

let ckind_of_jkind_l (j : Types.jkind_l) : JK.ckind = ckind_of_jkind j

let ckind_of_jkind_r (j : Types.jkind_r) : JK.ckind =
  fun (ops : JK.ops) ->
    (* For r-jkinds used in sub checks, with-bounds are not present
       on the right (see Jkind_desc.sub's precondition). So only the
       base mod-bounds matter. *)
    let base = ops.const (Axis_lattice.of_mod_bounds j.jkind.mod_bounds) in
    base

let kind_of_depth = ref 0 

let kind_of ~(context : Jkind.jkind_context) (ty : Types.type_expr) : JK.ckind =
 fun (ops : JK.ops) ->
  incr kind_of_depth;
  if !kind_of_depth > 50 then failwith "kind_of_depth too deep" else ();
  let res = (match Types.get_desc ty with
  | Types.Tvar {name=_name; jkind=jkind}
  | Types.Tunivar {name=_name; jkind=jkind} ->
    (* TODO: allow general jkinds here (including with-bounds) *)
    let jkind_l = Jkind.disallow_right jkind in
    let ckind = ckind_of_jkind_l jkind_l in
    ops.meet (ops.rigid ty) (ckind ops)
  | Types.Tconstr (p, args, _abbrev_memo) ->
    let arg_kinds = List.map (fun t -> ops.kind_of t) args in
    let p' = context.normalize_path p in
    ops.constr p' arg_kinds
  | Types.Ttuple elts ->
    (* Boxed tuples: immutable_data base + per-element contributions
       under id modality. *)
    let base = ops.const Axis_lattice.immutable_data in
    let contribs =
      List.map
        (fun (_lbl, t) ->
           let mask =
             Axis_lattice.mask_of_modality ~relevant_for_shallow:`Irrelevant
               Mode.Modality.Const.id
           in
           ops.modality mask (ops.kind_of t))
        elts
    in
    ops.join (base :: contribs)
  | Types.Tunboxed_tuple elts ->
    (* Unboxed tuples: non-float base + per-element contributions with
       shallow axes relevant. *)
    let contribs =
      let relevant_for_shallow =
        match List.length elts with 1 -> `Relevant | _ -> `Irrelevant
      in
      List.map
        (fun (_lbl, t) ->
           let mask =
             Axis_lattice.mask_of_modality ~relevant_for_shallow
               Mode.Modality.Const.id
           in
           ops.modality mask (ops.kind_of t))
        elts
    in
    ops.join contribs
  | Types.Tarrow (_lbl, _t1, _t2, _commu) ->
    (* Arrows use the dedicated per-axis bounds (no with-bounds). *)
    ops.const Axis_lattice.arrow
  | Types.Tlink _ -> failwith "Tlink shouldn't appear in kind_of"
  | Types.Tsubst _ -> failwith "Tsubst shouldn't appear in kind_of"
  | Types.Tpoly _ -> ops.const Axis_lattice.value
  | Types.Tof_kind jkind -> ckind_of_jkind jkind ops
  | Types.Tobject _ -> ops.const Axis_lattice.object_legacy
  | Types.Tfield _ -> ops.const Axis_lattice.value
  | Types.Tnil -> ops.const Axis_lattice.value
  | Types.Tvariant row ->
    if Btype.tvariant_not_immediate row then (
      if Btype.static_row row then (
        (* Closed, boxed polymorphic variant: immutable_data base plus
           per-constructor args. *)
        let base = ops.const Axis_lattice.immutable_data in
        let contribs =
          Btype.fold_row
            (fun acc ty ->
               let mask =
                 Axis_lattice.mask_of_modality ~relevant_for_shallow:`Irrelevant
                   Mode.Modality.Const.id
               in
               let k = ops.modality mask (ops.kind_of ty) in
               k :: acc)
            []
            row
          |> List.rev
        in
        ops.join (base :: contribs)
      ) else
        (* Open row: conservative non-float value (boxed). *)
        ops.const Axis_lattice.nonfloat_value
    ) else
      (* All-constant (immediate) polymorphic variant. *)
      ops.const Axis_lattice.immediate
  | Types.Tpackage _ -> ops.const Axis_lattice.nonfloat_value)
  in
  decr kind_of_depth;
  res




let has_mutable_label lbls =
  List.exists
    (fun (lbl : Types.label_declaration) ->
      match lbl.ld_mutable with Immutable -> false | Mutable _ -> true)
    lbls

  
    
let lookup_of_context ~(context : Jkind.jkind_context) (p : Path.t)
    : JK.constr_decl =
  match context.lookup_type p with
  | None ->
    failwith
      (Format.asprintf "Ikind.lookup: unknown constructor %a" Path.print p)
  | Some decl -> (
      match decl.type_manifest with
      | None ->
        (* No manifest: may still be concrete (record/variant/...). Build ckind. *)
        begin match decl.type_kind with
        | Types.Type_abstract _ ->
          let kind : JK.ckind = ckind_of_jkind_l decl.type_jkind in
          JK.Ty { args = decl.type_params; kind; abstract = true }
        | Types.Type_record (lbls, _rep, _umc_opt) ->
          (* Build from components: base (non-float value) + per-label contributions. *)
          let base_lat =
            if has_mutable_label lbls then Axis_lattice.mutable_data
            else Axis_lattice.immutable_data
          in
          let kind : JK.ckind =
            fun (ops : JK.ops) ->
              let base = ops.const base_lat in
              let contribs =
                List.map
                  (fun (lbl : Types.label_declaration) ->
                     let mask =
                       Axis_lattice.mask_of_modality
                         ~relevant_for_shallow:`Irrelevant
                         lbl.ld_modalities
                     in
                     ops.modality mask (ops.kind_of lbl.ld_type))
                  lbls
              in
              ops.join (base :: contribs)
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
              let contribs =
                let relevant_for_shallow =
                  match List.length lbls with 1 -> `Relevant | _ -> `Irrelevant
                in
                List.map
                  (fun (lbl : Types.label_declaration) ->
                     let mask =
                       Axis_lattice.mask_of_modality
                         ~relevant_for_shallow
                         lbl.ld_modalities
                     in
                     ops.modality mask (ops.kind_of lbl.ld_type))
                  lbls
              in
              ops.join (base :: contribs)
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_variant (cstrs, _rep, _umc_opt) ->
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
                 | Types.Cstr_record lbls -> List.for_all
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
            if all_args_void then Axis_lattice.immediate
            else if has_mutable then Axis_lattice.mutable_data
            else Axis_lattice.immutable_data
          in
          let kind : JK.ckind =
            fun (ops : JK.ops) ->
              let base = ops.const base_lat in
              let contribs =
                List.concat_map
                  (fun (c : Types.constructor_declaration) ->
                     match c.cd_args with
                     | Types.Cstr_tuple args ->
                       List.map
                         (fun (arg : Types.constructor_argument) ->
                           let mask =
                             Axis_lattice.mask_of_modality
                               ~relevant_for_shallow:`Irrelevant
                               arg.ca_modalities
                           in
                           ops.modality mask (ops.kind_of arg.ca_type))
                         args
                     | Types.Cstr_record lbls ->
                       List.map
                         (fun (lbl : Types.label_declaration) ->
                           let mask =
                             Axis_lattice.mask_of_modality
                               ~relevant_for_shallow:`Irrelevant
                               lbl.ld_modalities
                           in
                           ops.modality mask (ops.kind_of lbl.ld_type))
                         lbls)
                  cstrs
              in
              ops.join (base :: contribs)
          in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        | Types.Type_open ->
          let kind : JK.ckind = fun ops -> ops.const Axis_lattice.value in
          JK.Ty { args = decl.type_params; kind; abstract = false }
        end
      | Some body_ty ->
        (* Concrete: compute kind of body. *)
        let args = decl.type_params in
        let kind : JK.ckind = fun ops -> ops.kind_of body_ty in
        JK.Ty { args; kind; abstract = false })

(* Package the above into a full solver environment. *)
let make_solver ~(context : Jkind.jkind_context) : JK.solver =
  JK.make_solver
    { kind_of = kind_of ~context; lookup = lookup_of_context ~context }



let sub_jkind_l
    ?allow_any_crossing
    ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (sub : Types.jkind_l)
    (super : Types.jkind_l)
    : (unit, Jkind.Violation.t) result =
  let _ = origin in
  let open Misc.Stdlib.Monad.Result.Syntax in
  (* Check layouts first; if that fails, print both sides with full
     info and return the error. *)
  let* () =
    match Jkind.sub_jkind_l_layout ~context sub super with
    | Ok () -> Ok ()
    | Error v ->
        (* Format.eprintf
          "[ikind] sub_jkind_l LAYOUT FAILED%s@.@[<v 2>  sub = %a@, \
           super = %a@]@."
          (match origin with Some s -> Printf.sprintf " at %s" s
           | None -> "")
          Jkind.Debug_printers.t sub
          Jkind.Debug_printers.t super;
        if Language_extension.is_enabled Ikinds then (
          let solver = make_solver ~context in
          let sub_poly = JK.normalize solver (ckind_of_jkind_l sub) in
          let super_poly = JK.normalize solver (ckind_of_jkind_l super) in
          Format.eprintf "[ikind]   sub poly   = %s@." (JK.pp sub_poly);
          Format.eprintf "[ikind]   super poly = %s@." (JK.pp super_poly);
        ); *)
        Error v
  in
    let use_ik = !Clflags.ikinds in
    if not use_ik then
      Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context sub super
    else
      let solver = make_solver ~context in
      let allow_any =
        match allow_any_crossing with Some true -> true | _ -> false
      in
      if allow_any then Ok ()
      else
        let ik_leq =
          JK.leq_with_reason solver
            (ckind_of_jkind_l sub)
            (ckind_of_jkind_l super)
        in
        match ik_leq with
        | None -> Ok ()
        | Some violating_axis ->
          let violating_axis_name =
            Axis_lattice.axis_number_to_axis_packed violating_axis
          in
          (* Print full jkinds on failure for debugging. *)
          (* let pp_axis ppf (Jkind_axis.Axis.Pack ax) =
            Format.fprintf ppf "%s" (Jkind_axis.Axis.name ax)
          in
          (* Also show the normalized ikind polys for both sides. *)
          let sub_poly = JK.normalize solver (ckind_of_jkind_l sub) in
          let super_poly = JK.normalize solver (ckind_of_jkind_l super) in
          Format.eprintf
            "[ikind] sub_jkind_l AXIS FAILED%s (axis=%a)@.@[<v 2>  sub = %a@,  super = %a@]@."
            (match origin with Some s -> Printf.sprintf " at %s" s | None -> "")
            pp_axis violating_axis_name
            Jkind.Debug_printers.t sub
            Jkind.Debug_printers.t super;
          Format.eprintf "[ikind]   sub poly   = %s@." (JK.pp sub_poly);
          Format.eprintf "[ikind]   super poly = %s@." (JK.pp super_poly);
          (* Debug: print a snapshot of the typing environment for context. *)
          context.debug_print_env Format.err_formatter; *)
          (* Do not try to adjust allowances; Violation.Not_a_subjkind
             accepts an r-jkind. *)
          Error
            (Jkind.Violation.of_ ~context
               (Jkind.Violation.Not_a_subjkind
                  ( sub,
                    super,
                    [ Jkind.Sub_failure_reason.Axis_disagreement
                        violating_axis_name ])))

let sub
    ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (sub : Types.jkind_l)
    (super : Types.jkind_r)
    : bool =
  let _ = (type_equal, origin) in
  if not (!Clflags.ikinds) then
    Jkind.sub ~type_equal ~context sub super
  else
    let solver = make_solver ~context in
    match JK.leq_with_reason solver
            (ckind_of_jkind_l sub)
            (ckind_of_jkind_r super)
    with
    | None -> true
    | Some _violating_axis -> false

let crossing_of_jkind ~(context : Jkind.jkind_context) (jkind : ('l * 'r) Types.jkind)
  : Mode.Crossing.t =
  if not (!Clflags.ikinds) then
    Jkind.get_mode_crossing ~context jkind
  else
    let solver = make_solver ~context in
    let lat = JK.round_up solver (ckind_of_jkind jkind) in
    let mb = Axis_lattice.to_mod_bounds lat in
    Jkind.Mod_bounds.to_mode_crossing mb

(* Intentionally no ikind versions of sub_or_intersect / sub_or_error.
   Keep Jkind as the single source for classification and error reporting. *)
type sub_or_intersect = Jkind.sub_or_intersect =
  | Sub
  | Disjoint of Jkind.Sub_failure_reason.t Misc.Nonempty_list.t
  | Has_intersection of Jkind.Sub_failure_reason.t Misc.Nonempty_list.t

let sub_or_intersect
    ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind)
    : sub_or_intersect =
  let _ = origin in
  if not (!Clflags.ikinds) then
    Jkind.sub_or_intersect ~type_equal ~context t1 t2
  else begin
    let _ik =
      sub ~type_equal ~context
        (Jkind.disallow_right t1)
        (Jkind.disallow_left t2)
    in
    (* Preserve canonical Jkind classification for now. *)
    Jkind.sub_or_intersect ~type_equal ~context t1 t2
  end

let sub_or_error
    ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind)
    : (unit, Jkind.Violation.t) result =
  let _ = origin in
  if not (!Clflags.ikinds) then
    Jkind.sub_or_error ~type_equal ~context t1 t2
  else if
    sub ~type_equal ~context
      (Jkind.disallow_right t1)
      (Jkind.disallow_left t2)
  then
    Ok ()
  else
    (* Delegate to Jkind for detailed error reporting. *)
    Jkind.sub_or_error ~type_equal ~context t1 t2

(* Developer probe stub: set IKIND_POLY_PROBE to enable future tests. No-op by default. *)
let () =
  match Sys.getenv_opt "IKIND_POLY_PROBE" with
  | Some v when (v = "1" || String.lowercase_ascii v = "true") ->
    Printf.eprintf "[ikind] IKIND_POLY_PROBE enabled (no-op stub)\n%!"
  | _ -> ()
