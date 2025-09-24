(* This forces ikinds globally on. *)
(* Clflags.ikinds := true *)

module Ldd = Ikind.Ldd

module TyM = struct
  type t = Types.type_expr

  let compare (t1 : t) (t2 : t) =
    Int.compare (Types.get_id t1) (Types.get_id t2)

  let unique_id (t : t) : int = Types.get_id t
end

module ConstrM = struct
  type t = Ldd.constr

  let compare = Path.compare

  let to_string (p : t) : string = Format.asprintf "%a" Path.print p
end

module JK = Ldd_jkind_solver.Make (Ldd) (TyM) (ConstrM)

(* Optional ambient tag to disambiguate higher-level call sites
   (e.g. includecore). Other modules can bracket calls with
   [with_origin_tag] to add this suffix. *)
let __ikind_origin_tag : string option ref = ref None

let with_origin_tag (tag : string) (f : unit -> 'a) : 'a =
  let prev = !__ikind_origin_tag in
  __ikind_origin_tag := Some tag;
  Fun.protect ~finally:(fun () -> __ikind_origin_tag := prev) f

(* Debug logging helper with indentation (2 spaces per level).
   Enable by setting IKIND_DEBUG=1|true|yes in the environment. *)
let __ikind_debug : bool ref = ref false

let __ikind_log_depth = ref 0

let log ?pp (msg : string) (f : unit -> 'a) : 'a =
  if not !__ikind_debug
  then f ()
  else
    let indent = String.make (!__ikind_log_depth * 2) ' ' in
    let suffix =
      match !__ikind_origin_tag with
      | None -> ""
      | Some s -> Printf.sprintf " @(%s)" s
    in
    Format.eprintf "%s[ikind]%s %s@." indent suffix msg;
    incr __ikind_log_depth;
    match f () with
    | r ->
      decr __ikind_log_depth;
      let indent' = String.make (!__ikind_log_depth * 2) ' ' in
      (match pp with
      | None -> Format.eprintf "%s[ikind] end %s@." indent' msg
      | Some to_string ->
        let s = to_string r in
        Format.eprintf "%s[ikind] end %s => %s@." indent' msg s);
      r
    | exception exn ->
      decr __ikind_log_depth;
      let indent' = String.make (!__ikind_log_depth * 2) ' ' in
      Format.eprintf "%s[ikind] end %s@." indent' msg;
      raise exn

let ckind_of_jkind (j : ('l * 'r) Types.jkind) : JK.ckind =
 fun (ops : JK.ops) ->
  log ~pp:ops.pp_kind "ckind_of_jkind" (fun () ->
      (* Base is the modality bounds stored on this jkind. *)
      let base =
        ops.const
          (Axis_lattice_bits.of_mod_bounds j.jkind.mod_bounds)
      in
      (* For each with-bound (ty, axes), contribute
         modality(axes_mask, kind_of ty). *)
      let contribs =
        Jkind.With_bounds.to_seq j.jkind.with_bounds
        |> List.of_seq
        |> List.map (fun (ty, info) ->
               let axes = Jkind.With_bounds.type_info_relevant_axes info in
               let mask = Axis_lattice_bits.of_axis_set axes in
               let mask2 =
                Axis_lattice_bits.mask_of_modality
                  ~relevant_for_shallow:`Irrelevant Mode.Modality.Const.id
              in
               log ~pp:ops.pp_kind "with-bound" (fun () ->
                   let kty = ops.kind_of ty in
                   ops.modality mask2 (ops.modality mask kty)))
      in
      ops.join (base :: contribs))

let ckind_of_jkind_l (j : Types.jkind_l) : JK.ckind = ckind_of_jkind j

let ckind_of_jkind_r (j : Types.jkind_r) : JK.ckind =
 fun (ops : JK.ops) ->
  (* For r-jkinds used in sub checks, with-bounds are not present
     on the right (see Jkind_desc.sub's precondition). So only the
     base mod-bounds matter. *)
  let base = ops.const (Axis_lattice_bits.of_mod_bounds j.jkind.mod_bounds) in
  base

let kind_of_depth = ref 0

let kind_of_counter = ref 0

let kind_of ~(context : Jkind.jkind_context) (ty : Types.type_expr) : JK.ckind =
 fun (ops : JK.ops) ->
  ignore context;
  incr kind_of_depth;
  if !kind_of_depth > 5000 then failwith "kind_of_depth too deep" else ();
  incr kind_of_counter;
  (* if !kind_of_counter > 10000000 then __ikind_debug := true else (); *)
  let res =
    match Types.get_desc ty with
    | Types.Tvar { name = _name; jkind } | Types.Tunivar { name = _name; jkind }
      ->
      log ~pp:ops.pp_kind "Tvar/Tunivar" (fun () ->
          (* TODO: allow general jkinds here (including with-bounds) *)
          let jkind_l = Jkind.disallow_right jkind in
          let ckind = ckind_of_jkind_l jkind_l in
          ops.meet (ops.rigid ty) (ckind ops))
    | Types.Tconstr (p, args, _abbrev_memo) ->
      let p_str = Format.asprintf "%a" Path.print p in
      log ~pp:ops.pp_kind
        (Printf.sprintf "Tconstr %s (%d args)" p_str (List.length args))
        (fun () ->
          let arg_kinds =
            List.mapi
              (fun i t ->
                log ~pp:ops.pp_kind (Printf.sprintf "arg %d" i) (fun () ->
                    ops.kind_of t))
              args
          in
          ops.constr p arg_kinds)
    | Types.Ttuple elts ->
      log ~pp:ops.pp_kind
        (Printf.sprintf "Ttuple %d elts" (List.length elts))
        (fun () ->
          (* Boxed tuples: immutable_data base + per-element contributions
             under id modality. *)
          let base = ops.const Axis_lattice_bits.immutable_data in
          let contribs =
            List.map
              (fun (_lbl, t) ->
                let mask =
                  Axis_lattice_bits.mask_of_modality
                    ~relevant_for_shallow:`Irrelevant Mode.Modality.Const.id
                in
                log ~pp:ops.pp_kind "tuple elt" (fun () ->
                    ops.modality mask (ops.kind_of t)))
              elts
          in
          ops.join (base :: contribs))
    | Types.Tunboxed_tuple elts ->
      log ~pp:ops.pp_kind
        (Printf.sprintf "Tunboxed_tuple %d elts" (List.length elts))
        (fun () ->
          (* Unboxed tuples: per-element contributions; shallow axes relevant
             only for arity = 1. *)
          let contribs =
            let relevant_for_shallow =
              match List.length elts with 1 -> `Relevant | _ -> `Irrelevant
            in
            List.map
              (fun (_lbl, t) ->
                let mask =
                  Axis_lattice_bits.mask_of_modality ~relevant_for_shallow
                    Mode.Modality.Const.id
                in
                log ~pp:ops.pp_kind "unboxed tuple elt" (fun () ->
                    ops.modality mask (ops.kind_of t)))
              elts
          in
          ops.join contribs)
    | Types.Tarrow (_lbl, _t1, _t2, _commu) ->
      log ~pp:ops.pp_kind "Tarrow" (fun () ->
          (* Arrows use the dedicated per-axis bounds (no with-bounds). *)
          ops.const Axis_lattice_bits.arrow)
    | Types.Tlink _ -> failwith "Tlink shouldn't appear in kind_of"
    | Types.Tsubst _ -> failwith "Tsubst shouldn't appear in kind_of"
    | Types.Tpoly _ ->
      log ~pp:ops.pp_kind "Tpoly" (fun () -> ops.const Axis_lattice_bits.value)
    | Types.Tof_kind jkind ->
      log ~pp:ops.pp_kind "Tof_kind" (fun () -> ckind_of_jkind jkind ops)
    | Types.Tobject _ ->
      log ~pp:ops.pp_kind "Tobject" (fun () ->
          ops.const Axis_lattice_bits.object_legacy)
    | Types.Tfield _ ->
      failwith "Tfield shouldn't appear in kind_of"
      (* ops.const Axis_lattice_bits.value *)
    | Types.Tnil ->
      failwith "Tnil shouldn't appear in kind_of"
      (* ops.const Axis_lattice_bits.value *)
    | Types.Tvariant row ->
      log ~pp:ops.pp_kind "Tvariant" (fun () ->
          if Btype.tvariant_not_immediate row
          then
            if Btype.static_row row
            then
              log ~pp:ops.pp_kind "closed boxed" (fun () ->
                  (* Closed, boxed polymorphic variant: immutable_data base plus
                     per-constructor args. *)
                  let base = ops.const Axis_lattice_bits.immutable_data in
                  let mask =
                    Axis_lattice_bits.mask_of_modality
                      ~relevant_for_shallow:`Irrelevant Mode.Modality.Const.id
                  in
                  Btype.fold_row
                    (fun acc ty ->
                      let k =
                        log ~pp:ops.pp_kind "constructor arg" (fun () ->
                            let k_ty = ops.kind_of ty in
                            ops.modality mask k_ty)
                      in
                      ops.join [k; acc])
                    base row)
            else
              log ~pp:ops.pp_kind "open boxed" (fun () ->
                  (* Open row: conservative non-float value (boxed). *)
                  ops.const Axis_lattice_bits.nonfloat_value)
          else
            log ~pp:ops.pp_kind "immediate" (fun () ->
                (* All-constant (immediate) polymorphic variant. *)
                ops.const Axis_lattice_bits.immediate))
    | Types.Tpackage _ ->
      log ~pp:ops.pp_kind "Tpackage" (fun () ->
          ops.const Axis_lattice_bits.nonfloat_value)
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

let always_use_stored_jkind = false

let lookup_of_context ~(context : Jkind.jkind_context) (p : Path.t) :
    JK.constr_decl =
  match context.lookup_type p with
  | None ->
    failwith
      (Format.asprintf "Ikinds.lookup: unknown constructor %a" Path.print p)
  | Some decl -> (
    if always_use_stored_jkind
    then
      let abstract =
        match decl.type_manifest, decl.type_kind with
        | None, Types.Type_abstract _ -> true
        | _ -> false
      in
      let kind : JK.ckind = ckind_of_jkind_l decl.type_jkind in
      JK.Ty { args = decl.type_params; kind; abstract }
    else
      match decl.type_manifest with
      | None -> (
        (* No manifest: may still be concrete (record/variant/...). Build
           ckind. *)
        match decl.type_kind with
        | Types.Type_abstract _ ->
          log "lookup Type_abstract" (fun () ->
              let kind : JK.ckind = ckind_of_jkind_l decl.type_jkind in
              JK.Ty { args = decl.type_params; kind; abstract = true })
        | Types.Type_record (lbls, rep, _umc_opt) ->
          log "lookup Type_record" (fun () ->
              (* Build from components: base (non-float value) + per-label
                 contributions. *)
              let base_lat =
                if has_mutable_label lbls
                then Axis_lattice_bits.mutable_data
                else Axis_lattice_bits.immutable_data
              in
              let relevant_for_shallow = relevance_of_rep (`Record rep) in
              let kind : JK.ckind =
               fun (ops : JK.ops) ->
                log ~pp:ops.pp_kind "record kind" (fun () ->
                    let base = ops.const base_lat in
                    let contribs =
                      List.map
                        (fun (lbl : Types.label_declaration) ->
                          let mask =
                            Axis_lattice_bits.mask_of_modality
                              ~relevant_for_shallow
                              lbl.ld_modalities
                          in
                          log ~pp:ops.pp_kind
                            (Printf.sprintf "label %s" (Ident.name lbl.ld_id))
                            (fun () ->
                              ops.modality mask (ops.kind_of lbl.ld_type)))
                        lbls
                    in
                    ops.join (base :: contribs))
              in
              JK.Ty { args = decl.type_params; kind; abstract = false })
        | Types.Type_record_unboxed_product (lbls, _rep, _umc_opt) ->
          log "lookup Type_record_unboxed_product" (fun () ->
              (* Unboxed products: non-float base; shallow axes relevant only
                 for arity = 1. *)
              let base_lat =
                if has_mutable_label lbls
                then Axis_lattice_bits.mutable_data
                else Axis_lattice_bits.nonfloat_value
              in
              let kind : JK.ckind =
               fun (ops : JK.ops) ->
                log ~pp:ops.pp_kind "record_unboxed_product kind" (fun () ->
                    let base = ops.const base_lat in
                    let contribs =
                      let relevant_for_shallow =
                        match List.length lbls with
                        | 1 -> `Relevant
                        | _ -> `Irrelevant
                      in
                      List.map
                        (fun (lbl : Types.label_declaration) ->
                          let mask =
                            Axis_lattice_bits.mask_of_modality
                              ~relevant_for_shallow lbl.ld_modalities
                          in
                          log ~pp:ops.pp_kind
                            (Printf.sprintf "label %s" (Ident.name lbl.ld_id))
                            (fun () ->
                              ops.modality mask (ops.kind_of lbl.ld_type)))
                        lbls
                    in
                    ops.join (base :: contribs))
              in
              JK.Ty { args = decl.type_params; kind; abstract = false })
        | Types.Type_variant (cstrs, rep, _umc_opt) ->
          log "lookup Type_variant" (fun () ->
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
                then Axis_lattice_bits.immediate
                else if has_mutable
                then Axis_lattice_bits.mutable_data
                else Axis_lattice_bits.immutable_data
              in
              let relevant_for_shallow = relevance_of_rep (`Variant rep) in
              let kind : JK.ckind =
               fun (ops : JK.ops) ->
                log ~pp:ops.pp_kind "variant kind" (fun () ->
                    let base = ops.const base_lat in
                    let contribs =
                      List.concat_map
                        (fun (c : Types.constructor_declaration) ->
                          match c.cd_args with
                          | Types.Cstr_tuple args ->
                            List.mapi
                              (fun i (arg : Types.constructor_argument) ->
                                let mask =
                                  Axis_lattice_bits.mask_of_modality
                                    ~relevant_for_shallow
                                    arg.ca_modalities
                                in
                                log ~pp:ops.pp_kind
                                  (Printf.sprintf "cstr arg %d" i) (fun () ->
                                    ops.modality mask
                                      (ops.kind_of arg.ca_type)))
                              args
                          | Types.Cstr_record lbls ->
                            List.map
                              (fun (lbl : Types.label_declaration) ->
                                let mask =
                                  Axis_lattice_bits.mask_of_modality
                                    ~relevant_for_shallow
                                    lbl.ld_modalities
                                in
                                log ~pp:ops.pp_kind
                                  (Printf.sprintf "cstr label %s"
                                     (Ident.name lbl.ld_id))
                                  (fun () ->
                                    ops.modality mask
                                      (ops.kind_of lbl.ld_type)))
                              lbls)
                        cstrs
                    in
                    ops.join (base :: contribs))
              in
              JK.Ty { args = decl.type_params; kind; abstract = false })
        | Types.Type_open ->
          log "lookup Type_open" (fun () ->
              let kind : JK.ckind =
               fun ops ->
                log ~pp:ops.pp_kind "Type_open kind" (fun () ->
                    ops.const Axis_lattice_bits.value)
              in
              JK.Ty { args = decl.type_params; kind; abstract = false }))
      | Some body_ty ->
        log "lookup manifest body" (fun () ->
            (* Concrete: compute kind of body. *)
            let args = decl.type_params in
            let kind : JK.ckind =
             fun ops ->
              log ~pp:ops.pp_kind "manifest kind" (fun () ->
                  ops.kind_of body_ty)
            in
            JK.Ty { args; kind; abstract = false }))

(* Package the above into a full solver environment. *)
let make_solver ~(context : Jkind.jkind_context) : JK.solver =
  JK.make_solver
    { kind_of = kind_of ~context; lookup = lookup_of_context ~context }

let normalize ~(context : Jkind.jkind_context) (jkind : Types.jkind_l) :
    Ikind.Ldd.node =
  let solver = make_solver ~context in
  JK.normalize solver (ckind_of_jkind_l jkind)

type constructor_ikind_payload =
  { base : JK.poly;
    coeffs : JK.poly array
  }

let pack_poly (poly : Ikind.Ldd.node) : Types.constructor_ikind = Obj.magic poly

let unpack_poly (packed : Types.constructor_ikind) : Ikind.Ldd.node = Obj.magic packed

let pack_constructor_ikind (payload : constructor_ikind_payload) :
    Types.constructor_ikind =
  Obj.magic payload

let unpack_constructor_ikind (packed : Types.constructor_ikind) :
    constructor_ikind_payload =
  Obj.magic packed

let normalize_and_pack ~(context : Jkind.jkind_context) ~(path : Path.t)
    (jkind : Types.jkind_l) : Types.constructor_ikind =
  let poly = normalize ~context jkind in
  let poly_str = JK.pp poly in
  if false then Format.eprintf "[ikind-store] %a => %s@." Path.print path poly_str else ();
  pack_poly poly

let type_declaration_ikind ~(context : Jkind.jkind_context)
    ~(path : Path.t) : Types.constructor_ikind =
  let solver = make_solver ~context in
  let base, coeffs = JK.constr_kind_poly solver path in
  let coeffs_array = Array.of_list coeffs in
  if false || !__ikind_debug
  then (
    let base_str = JK.pp base in
    let coeffs_str =
      coeffs_array
      |> Array.to_list
      |> List.mapi (fun i coeff -> Format.asprintf "%d:%s" i (JK.pp coeff))
      |> String.concat ", "
    in
    Format.eprintf "[ikind-install] %a base=%s coeffs=[%s]@."
      Path.print path base_str coeffs_str);
  pack_constructor_ikind { base; coeffs = coeffs_array }

let rehydrate_constructor_ikind ~(context : Jkind.jkind_context)
    (payload : constructor_ikind_payload) : constructor_ikind_payload =
  ignore context;
  payload

let apply_constructor_ikind ~(context : Jkind.jkind_context)
    (packed : Types.constructor_ikind) (args : Ikind.Ldd.node list) :
    Ikind.Ldd.node =
  let payload = rehydrate_constructor_ikind ~context (unpack_constructor_ikind packed) in
  let arity = Array.length payload.coeffs in
  if List.length args <> arity
  then
    Misc.fatal_errorf
      "ikinds: constructor arity mismatch (expected %d, got %d)"
      arity (List.length args);
  let contributions =
    List.mapi
      (fun i arg -> Ikind.Ldd.meet arg payload.coeffs.(i))
      args
  in
  List.fold_left Ikind.Ldd.join payload.base contributions

let sub_jkind_l ?allow_any_crossing ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) (sub : Types.jkind_l)
    (super : Types.jkind_l) : (unit, Jkind.Violation.t) result =
  let _ = origin in
  let open Misc.Stdlib.Monad.Result.Syntax in
  (* Check layouts first; if that fails, print both sides with full
     info and return the error. *)
  let* () =
    match Jkind.sub_jkind_l_layout ~context sub super with
    | Ok () -> Ok ()
    | Error v ->
      (* On layout failure, show normalized polys for both sides. *)
      (let solver = make_solver ~context in
       ignore
         (log ~pp:JK.pp "sub poly (layout failure)" (fun () ->
              JK.normalize solver (ckind_of_jkind_l sub)));
       ignore
         (log ~pp:JK.pp "super poly (layout failure)" (fun () ->
              JK.normalize solver (ckind_of_jkind_l super))));
      Error v
  in
  if not !Clflags.ikinds
  then Jkind.sub_jkind_l ?allow_any_crossing ~type_equal ~context sub super
  else
    let solver = make_solver ~context in
    let allow_any =
      match allow_any_crossing with Some true -> true | _ -> false
    in
    if allow_any
    then Ok ()
    else
      let ik_leq =
        JK.leq_with_reason solver (ckind_of_jkind_l sub)
          (ckind_of_jkind_l super)
      in
      match ik_leq with
      | None -> Ok ()
      | Some violating_axes ->
        let violating_axis_names =
          List.map Axis_lattice_bits.axis_number_to_axis_packed
            violating_axes
        in
        (* Also show the normalized ikind polys for both sides. *)
        ignore
          (log ~pp:JK.pp "sub poly" (fun () ->
               JK.normalize solver (ckind_of_jkind_l sub)));
        ignore
          (log ~pp:JK.pp "super poly" (fun () ->
               JK.normalize solver (ckind_of_jkind_l super)));
        (* Do not try to adjust allowances; Violation.Not_a_subjkind
           accepts an r-jkind. *)
        let axis_reasons =
          List.map
            (fun axis_name ->
              Jkind.Sub_failure_reason.Axis_disagreement axis_name)
            violating_axis_names
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
  if not (true && !Clflags.ikinds) (* CR jujacobs: fix this *)
  then Jkind.get_mode_crossing ~context jkind
  else
    let solver = make_solver ~context in
    let lat = JK.round_up solver (ckind_of_jkind jkind) in
    let mb = Axis_lattice_bits.to_mod_bounds lat in
    Jkind.Mod_bounds.to_mode_crossing mb

(* Intentionally no ikind versions of sub_or_intersect / sub_or_error.
   Keep Jkind as the single source for classification and error reporting. *)
(* CR jujacobs: fix this *)
type sub_or_intersect = Ikind.sub_or_intersect

(* CR jujacobs: performance issue here. *)
let sub_or_intersect ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) : sub_or_intersect =
  let _ = origin in
  if not (true && !Clflags.ikinds) (* CR jujacobs: fix this *)
  then Jkind.sub_or_intersect ~type_equal ~context t1 t2
  else
    (* CR jujacobs: enable this *)
    let _ik =
      sub ~type_equal ~context (Jkind.disallow_right t1)
        (Jkind.disallow_left t2)
    in
    (* Preserve canonical Jkind classification for now. *)
    Jkind.sub_or_intersect ~type_equal ~context t1 t2

let sub_or_error ?origin
    ~(type_equal : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context)
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) :
    (unit, Jkind.Violation.t) result =
  let _ = origin in
  if not !Clflags.ikinds
  then Jkind.sub_or_error ~type_equal ~context t1 t2
  else if sub ~type_equal ~context (Jkind.disallow_right t1)
            (Jkind.disallow_left t2)
  then Ok ()
  else
    (* Delegate to Jkind for detailed error reporting. *)
    Jkind.sub_or_error ~type_equal ~context t1 t2

(* Developer probe stub: set IKIND_POLY_PROBE to enable future tests. No-op by
   default. *)
let () =
  match Sys.getenv_opt "IKIND_POLY_PROBE" with
  | Some v when v = "1" || String.lowercase_ascii v = "true" ->
    Format.eprintf "[ikind] IKIND_POLY_PROBE enabled (no-op stub)\n%!"
  | _ -> ()
