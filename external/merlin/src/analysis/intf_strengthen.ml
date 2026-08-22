(* CR-someday ggray: expose functionality to overwrite existing annotations
   with the stronger version that the implementation supports. We'd likely
   want to expose this differently for modes, modalities, and kinds. *)
open Std

module Crossing = Mode.Crossing

let ok_exn ~msg res =
  match res with
  | Ok () -> ()
  | Error _ -> failwith msg

(* Mirrors [Ctype.moregen_variance], but the strengthener
   needs the two variances reachable by walking arrow types. *)
module Variance = struct
  type t = Covariant | Contravariant

  let neg = function
    | Covariant -> Contravariant
    | Contravariant -> Covariant
end

module Arrow_pos = struct
  type dir = Here | In_arg of dir | In_ret of dir

  type role = Arg | Ret

  type t = { dir : dir; nesting : int; role : role; variance : Variance.t }
end

type arrow_diff =
  { path : Arrow_pos.t; impl : Mode.Alloc.Const.t; intf : Mode.Alloc.Const.t }

let guard b = if b then Some () else None

module Weakness = struct
  type t =
    | Value of
        { loc : Location.t;
          modality_diff :
            (impl:Mode.Modality.Const.t * intf:Mode.Modality.Const.t) option;
          arrow_diffs : arrow_diff list
        }
    | Type of { loc : Location.t; suggested_jkind : Types.jkind_l }
end

(* Arity is derived from the interface type, not the implementation's:
   the suggestions are written into the interface, so its syntactic arrow
   spine is what positions can be named. *)
module Arrow_pass : sig
  val run :
    env:Env.t ->
    variance:Variance.t ->
    name:string ->
    impl:Types.value_description ->
    intf:Types.value_description ->
    unit ->
    arrow_diff list
end = struct
  (* The type with all [Tpoly] wrappers peeled off. *)
  let rec strip_poly ty =
    match Types.get_desc ty with
    | Tpoly (inner, _) -> strip_poly inner
    | _ -> ty

  type zap_entry =
    { pos : Arrow_pos.t;
      impl_modes : Mode.Alloc.lr;
      intf_modes : Mode.Alloc.lr;
      ty : Types.type_expr
    }

  let rec collect_positions ~variance ~nesting ~path impl_ty intf_ty =
    let open Arrow_pos in
    let impl_vd = Types.get_desc (strip_poly impl_ty)
    and intf_vd = Types.get_desc (strip_poly intf_ty) in
    match (impl_vd, intf_vd) with
    | ( Tarrow ((_, impl_arg, impl_ret), impl_arg_ty, impl_ret_ty, _),
        Tarrow ((_, intf_arg, intf_ret), intf_arg_ty, intf_ret_ty, _) ) ->
      let intf_arg_ty = strip_poly intf_arg_ty in
      let intf_ret_ty = strip_poly intf_ret_ty in
      let arg =
        { pos =
            { nesting;
              variance = Variance.neg variance;
              role = Arg;
              dir = path (In_arg Here)
            };
          impl_modes = impl_arg;
          intf_modes = intf_arg;
          ty = intf_arg_ty
        }
      in
      let ret =
        match Types.get_desc intf_ret_ty with
        | Tarrow _ -> []
        | _ ->
          [ { pos = { nesting; variance; role = Ret; dir = path (In_ret Here) };
              impl_modes = impl_ret;
              intf_modes = intf_ret;
              ty = intf_ret_ty
            }
          ]
      in
      let inside_arg =
        collect_positions ~variance:(Variance.neg variance)
          ~nesting:(nesting + 1)
          ~path:(fun p -> path (In_arg p))
          impl_arg_ty intf_arg_ty
      in
      let inside_return =
        collect_positions ~variance ~nesting
          ~path:(fun p -> path (In_ret p))
          impl_ret_ty intf_ret_ty
      in
      ((arg :: ret) @ inside_arg) @ inside_return
    | _, _ -> []

  let collect_positions ~variance impl_ty intf_ty =
    collect_positions ~variance ~nesting:0 ~path:Fun.id impl_ty intf_ty

  (* Zap one entry to its extremal bound and produce an [arrow_diff] if a
     nontrivial strengthening remains after crossing is masked. *)
  let zap_and_diff ~env diffs entry =
    let is_ret =
      match entry.pos.role with
      | Ret -> true
      | Arg -> false
    in
    let impl_var = Mode.Alloc.newvar () in
    let intf_var = Mode.Alloc.newvar () in
    let ok_exn = ok_exn ~msg:"Intf_strengthen.Arrow_pass.zap_and_diff" in
    (* Read both sides at the same extremal end as the comparison
       direction (floor for covariant, ceil for contravariant). *)
    (match entry.pos.variance with
    | Covariant ->
      Ctype.submode_with_cross env ~is_ret entry.ty impl_var entry.impl_modes
      |> ok_exn;
      Ctype.submode_with_cross env ~is_ret entry.ty intf_var entry.intf_modes
      |> ok_exn;
      let (_ : Mode.Alloc.Const.t) = Mode.Alloc.zap_to_floor entry.impl_modes in
      let (_ : Mode.Alloc.Const.t) = Mode.Alloc.zap_to_floor entry.intf_modes in
      ()
    | Contravariant ->
      Ctype.submode_with_cross env ~is_ret entry.ty entry.impl_modes impl_var
      |> ok_exn;
      Ctype.submode_with_cross env ~is_ret entry.ty entry.intf_modes intf_var
      |> ok_exn;
      let (_ : Mode.Alloc.Const.t) = Mode.Alloc.zap_to_ceil entry.impl_modes in
      let (_ : Mode.Alloc.Const.t) = Mode.Alloc.zap_to_ceil entry.intf_modes in
      ());
    let impl_c = Mode.Alloc.zap_to_legacy impl_var in
    let intf_c = Mode.Alloc.zap_to_legacy intf_var in
    match Mode.Alloc.Const.equal impl_c intf_c with
    | true -> diffs
    | false -> { path = entry.pos; impl = impl_c; intf = intf_c } :: diffs

  (* Run `moregen`, pretending that intermediate arrows don't exist. *)
  (* CR-someday ggray: we should allow users to toggle this functionality,
     which will likely result in no strengthened modes, but modalities and
     kinds may still be strengthened. *)
  let nary_moregen ~env ~variance ~(impl : Types.value_description)
      ~(intf : Types.value_description) =
    let pat_ty, pat_lpoly, subj_ty, subj_lpoly =
      let impl_ty = impl.val_type
      and impl_lpoly = Types.Lpoly.get_exn impl.val_lpoly
      and intf_ty = intf.val_type
      and intf_lpoly = Types.Lpoly.get_exn intf.val_lpoly in
      match (variance : Variance.t) with
      | Covariant -> (impl_ty, impl_lpoly, intf_ty, intf_lpoly)
      | Contravariant -> (intf_ty, intf_lpoly, impl_ty, impl_lpoly)
    in
    match
      Ctype.moregeneral ~ret_modes:Skip_intermediate_ret_modes env true
        pat_lpoly subj_lpoly pat_ty subj_ty
    with
    | (_ : Jkind.sort option list) -> Ok ()
    | exception Ctype.Moregen _ -> Error ()

  (*  The mode analysis of one value's arrow type: find every arrow
      position where the implementation supports a strictly stronger
      mode than its interface declares.

      preconditions for calling this function:
      P1. The current pipeline typed the implementation: its body
          constraints are live in the solver.
      P2. No inclusion (moregen) constraints against the interface
          are live on those variables.
      P3. [intf_ty] is already substituted into the
          implementation's namespace (see [Pairing]).
      P4. [variance] is the ambient variance of the declaration site.
      P5. The input typechecks, i.e. full inclusion already holds for this pair. *)
  let run ~env ~variance ~name ~impl ~intf () =
    match nary_moregen ~env ~variance ~impl ~intf with
    | Ok () ->
      (* The core insight of this analysis is how to expand the constraint
        graph without breaking any clients. We zap returns before parameters
        so that return types constrain parameters and not the other way
        around, which would not guarantee an interface widening. *)
      (* The schedule: per nesting level, returns then arguments, each left
         to right — the sort is stable and [collect_positions] emits left to
         right. *)
      let priority entry =
        match entry.pos.role with
        | Ret -> 0
        | Arg -> 1
      in
      collect_positions ~variance impl.val_type intf.val_type
      |> List.stable_sort ~cmp:(fun a b ->
          compare (a.pos.nesting, priority a) (b.pos.nesting, priority b))
      |> List.fold_left ~init:[] ~f:(zap_and_diff ~env)
      |> List.rev
    | Error () ->
      (* We assume that the code typechecked, so if module inclusion with
       *fewer* submode constraints fails then some assumption has been broken *)
      Logger.notify ~section:"intf-strengthen"
        "intf-weakness: selective moregen re-run failed for [%s]; skipping \
         this value."
        name;
      []
end

(*  Matches interface items to implementation items. The analysis
    walks the interface signature; for each item it needs the
    same-named implementation declaration and a substitution
    reconciling the two namespaces the way the compiler's own
    inclusion check does. *)
module Pairing : sig
  type t =
    { impl_vds : Types.value_description String.Map.t;
      impl_tds : (path:Path.t * decl:Types.type_declaration) String.Map.t;
      impl_prefix : Path.t option;
      impl_sig : Types.signature;
      subst : Subst.t
    }

  val create :
    impl_prefix:Path.t option ->
    subst:Subst.t ->
    impl_sig:Types.signature ->
    intf_sig:Types.signature ->
    t

  val find_impl_module_mty :
    Types.signature -> string -> (Ident.t * Types.module_type) option
end = struct
  (* Everything the walk knows about one signature level: name-indexed
     implementation lookups, and the interface -> implementation ident
     substitution accumulated across the levels above and including this
     one. *)
  type t =
    { impl_vds : Types.value_description String.Map.t;
      impl_tds : (path:Path.t * decl:Types.type_declaration) String.Map.t;
      impl_prefix : Path.t option;
      impl_sig : Types.signature;
      subst : Subst.t
    }

  let find_impl_module_mty impl_sig name =
    List.find_map_opt impl_sig ~f:(fun (item : Types.signature_item) ->
        match item with
        | Sig_module (id, _, md, _, _) when Ident.name id = name ->
          Some (id, md.md_type)
        | _ -> None)

  (* One pass over the implementation signature: the value/type payloads
     for the lookup tables, and the idents the substitution pairs by
     name. *)
  type impl_index =
    { vds : Types.value_description String.Map.t;
      tds :
        (id:Ident.t * path:Path.t * decl:Types.type_declaration) String.Map.t;
      module_ids : Ident.t String.Map.t;
      modtype_ids : Ident.t String.Map.t;
      jkind_ids : Ident.t String.Map.t
    }

  let index_impl_sig ~impl_prefix impl_sig =
    List.fold_left impl_sig
      ~init:
        { vds = String.Map.empty;
          tds = String.Map.empty;
          module_ids = String.Map.empty;
          modtype_ids = String.Map.empty;
          jkind_ids = String.Map.empty
        } ~f:(fun acc (item : Types.signature_item) ->
        match item with
        | Sig_value (id, vd, _) ->
          { acc with vds = String.Map.add ~key:(Ident.name id) ~data:vd acc.vds }
        | Sig_type (id, td, _, _) ->
          let name = Ident.name id in
          let path : Path.t =
            match impl_prefix with
            | None -> Pident id
            | Some p -> Pdot (p, name)
          in
          { acc with
            tds = String.Map.add ~key:name ~data:(~id, ~path, ~decl:td) acc.tds
          }
        | Sig_module (id, _, _, _, _) ->
          { acc with
            module_ids = String.Map.add ~key:(Ident.name id) ~data:id acc.module_ids
          }
        | Sig_modtype (id, _, _) ->
          { acc with
            modtype_ids =
              String.Map.add ~key:(Ident.name id) ~data:id acc.modtype_ids
          }
        | Sig_jkind (id, _, _) ->
          { acc with
            jkind_ids = String.Map.add ~key:(Ident.name id) ~data:id acc.jkind_ids
          }
        | _ -> acc)

  (* The interface and the implementation declare *distinct* idents for
     their same-named components. The compiler's inclusion check reconciles
     them by building a substitution while matching signatures
     ([Includemod.signatures]); mirror that, so interface-side types can be
     rewritten into the implementation's namespace before any mode or kind
     comparison. Unpaired items stay unmapped: comparisons involving them
     fail conservatively. *)
  let extend_subst subst index ~intf_sig =
    List.fold_left intf_sig ~init:subst
      ~f:(fun subst (item : Types.signature_item) ->
        let pair impl_id id add =
          match impl_id with
          | Some impl_id -> add id (Path.Pident impl_id) subst
          | None -> subst
        in
        let find ids id = String.Map.find_opt (Ident.name id) ids in
        match item with
        | Sig_type (id, _, _, _) ->
          pair
            (Option.map (find index.tds id) ~f:(fun (~id, ~path:_, ~decl:_) ->
                 id))
            id Subst.add_type
        | Sig_module (id, _, _, _, _) ->
          pair (find index.module_ids id) id Subst.add_module
        | Sig_modtype (id, _, _) ->
          pair (find index.modtype_ids id) id Subst.add_modtype
        | Sig_jkind (id, _, _) ->
          pair (find index.jkind_ids id) id Subst.add_jkind
        | _ -> subst)

  (* Entry point: pair one signature level. [impl_prefix] is the module
     path of this level in the implementation's namespace ([None] at the
     top level, where idents are bound in the environment directly). *)
  let create ~impl_prefix ~subst ~impl_sig ~intf_sig =
    let index = index_impl_sig ~impl_prefix impl_sig in
    { impl_vds = index.vds;
      impl_tds =
        String.Map.map ~f:(fun (~id:_, ~path, ~decl) -> (~path, ~decl)) index.tds;
      impl_prefix;
      impl_sig;
      subst = extend_subst subst index ~intf_sig
    }
end

module Analyze : sig
  val analyze :
    env:Env.t ->
    impl_sig:Types.signature ->
    intf_sig:Types.signature ->
    unit ->
    Weakness.t list
end = struct
  (* Where the walk currently is: typing environment and ambient variance
     (flipped inside functor parameters). *)
  type analysis_context = { env : Env.t; variance : Variance.t }

  (* The modality evidence for one value: [impl] is the interface's const
     overlaid with every axis the implementation provably supports more
     strongly (axes whose type crossing makes the claim vacuous are
     discharged), and [intf] the interface's const itself. [impl = intf]
     when there is nothing new to claim. *)
  let compute_modality_diff ~crossing impl_mod intf_mod =
    let claim side =
      let var = Mode.Value.newvar () in
      Mode.Value.submode
        (Crossing.apply_left crossing
           (Mode.Modality.Const.apply_left side Mode.Value.max))
        (Crossing.apply_right crossing var)
      |> ok_exn ~msg:"Intf_strengthen.Arrow_pass.compute_modality_diff";
      Mode.Modality.zap_to_floor
        (Mode.Modality.infer ~md_mode:Mode.Value.max ~mode:var)
    in
    let impl = claim (Mode.Modality.zap_to_floor impl_mod) in
    let intf = claim (Mode.Modality.to_const_exn intf_mod) in
    (* [claim] under-approximates: zapping an inferred modality worst-cases
       the module mode (mode.ml's [Diff] zap uses [mm]'s ceil), so [impl]
       can come out below what the item provably supports — and [intf] is
       proven support (the unit typechecked against it). Join the two
       evidence sources per axis so no consumer ever sees a weakening. *)
    let impl =
      List.fold_left (Mode.Modality.Const.diff intf impl) ~init:intf
        ~f:(fun acc (Mode.Modality.Atom (ax, v)) ->
          let stronger =
            Mode.Modality.(
              sub (of_const (Mode.Modality.Const.set ax v intf)) (of_const intf))
          in
          match stronger with
          | Ok () -> Mode.Modality.Const.set ax v acc
          | Error _ -> acc)
    in
    (* Returned even when [impl] claims nothing new: rendering needs the
       recorded interface const of every analyzed value to spell hoisting
       exemptions, and the printer's emptiness gate already drops claim-free
       per-item edits. *)
    Some (~impl, ~intf)

  (* The modality strengthening of one value, or [None].

     No snapshot here: the modality pin is deliberately read in the same
     world as — and after — this value's arrow commitments, so consequences
     of the arrow pins (e.g. a return committed to its floor) constrain what
     the modality may still claim. The single backtrack lives in
     [analyze]. *)
  let modality_diff_for_value ~(variance : Variance.t) ~crossing
      (impl_vd : Types.value_description) (intf_vd : Types.value_description) =
    (* Modalities are only strengthened in covariant position; in a contravariant
       (like a functor-parameter) context the floor/ceil framework does not apply, so
       they are skipped. *)
    match variance with
    | Contravariant -> None
    | Covariant ->
      compute_modality_diff ~crossing impl_vd.val_modalities
        intf_vd.val_modalities

  (* Analyze one interface [val] declaration: find the same-named
     implementation value, compare arrow alloc modes at each arrow position,
     then compare value modalities (covariant contexts only). Returns a
     weakness only if at least one arrow-mode or modality diff remains. *)
  let analyze_value { env; variance } (pairing : Pairing.t) ~id
      ~(intf_vd : Types.value_description) =
    let open Option.Infix in
    let name = Ident.name id in
    let* impl_vd = String.Map.find_opt name pairing.impl_vds in
    let intf_vd_orig = intf_vd in
    let intf_vd = Subst.value_description pairing.subst intf_vd in
    (* The crossing used to mask suggestiosn is computed on the substituted type. *)
    (* CR-someday ggray: there's the type we'd suggest in the implemetnation, [t_1],
       and the type that we inferred from the implementation, [t_0]. [t_0] is *stronger*
       than [t_1]; meaning, [t_0] might know more about abstract types then [t_1].
       In theory, this means that it's possible that [crossing] is wider than if we
       used [t_1] to compute it; if that's true, then we would suppress modality
       suggestions that we should suggest. It's unclear if that can happen in practice
       because the only difference is the presence of abstract types in the with-bounds. *)
    let crossing = Ctype.crossing_of_ty env intf_vd.val_type in
    let arrow_diffs =
      Arrow_pass.run ~env ~variance ~name ~impl:impl_vd ~intf:intf_vd ()
    in
    let modality_diff =
      modality_diff_for_value ~variance ~crossing impl_vd intf_vd
    in
    match (modality_diff, arrow_diffs) with
    | None, [] -> None
    | _ ->
      Some
        (Weakness.Value
           { (* identity/keying data from the original declaration: the
                 substitution must not affect how rendering locates it *)
             loc = intf_vd_orig.val_loc;
             modality_diff;
             arrow_diffs
           })

  (* Is the interface's declared kind already at least as strong as [best]?
     Compared under the same [always_principal] jkind context the compiler's
     inclusion check uses. *)
  let is_intf_kind_already_as_strong ~env ~intf_jkind best =
    let context = Ctype.mk_jkind_context_always_principal env in
    let type_equal = Ctype.type_equal env in
    match
      Ikind.sub_jkind_l ~origin:"intf-strengthen:kind-weakness" ~type_equal
        ~context env intf_jkind best
    with
    | Ok () -> true
    | Error _ -> false

  (* Report a kind weakness iff the implementation's best interface-expressible
     kind is strictly stronger than the interface's declared kind. *)
  let analyze_type { env; variance } (pairing : Pairing.t) ~id
      ~(intf_decl : Types.type_declaration) =
    let open Option.Infix in
    let* () =
      (* Only strengthen in covariant contexts *)
      match (variance : Variance.t) with
      | Covariant -> Some ()
      | Contravariant -> None
    in
    let* () =
      (* If a type is exposed (e.g., [type a = int]) then we don't need to strengthen *)
      match intf_decl with
      | { type_kind = Type_abstract _; type_manifest = None; _ } -> Some ()
      | _ -> None
    in
    let* ~path:impl_path, ~decl:impl_decl =
      String.Map.find_opt (Ident.name id) pairing.impl_tds
    in
    (* Rewrite types using the interface environment *)
    let intf_jkind =
      (Subst.type_declaration pairing.subst intf_decl).type_jkind
    in
    let impl_ikind =
      Ikind.type_declaration_ikind_gated ~env:(Some env) ~path:impl_path
    in
    let* suggested_jkind =
      Parametric_kind.restrict_to_parameters ~env ~decl:impl_decl impl_ikind
    in
    let* () =
      guard
        (not (is_intf_kind_already_as_strong ~env ~intf_jkind suggested_jkind))
    in
    Some (Weakness.Type { loc = intf_decl.type_loc; suggested_jkind })

  let rec analyze_sig context ~impl_prefix ~subst ~impl_sig ~intf_sig =
    let context =
      { context with
        env = Env.add_signature impl_sig (Env.in_signature true context.env)
      }
    in
    let pairing = Pairing.create ~impl_prefix ~subst ~impl_sig ~intf_sig in
    List.concat_map intf_sig ~f:(analyze_signature_item context pairing)

  and analyze_signature_item context pairing item =
    match (item : Types.signature_item) with
    | Sig_value (id, intf_vd, _) ->
      analyze_value context pairing ~id ~intf_vd |> Option.to_list
    | Sig_type (id, intf_decl, _, _) ->
      analyze_type context pairing ~id ~intf_decl |> Option.to_list
    | Sig_module (id, _, md, _, _) ->
      analyze_module_item context pairing ~id ~md
    | _ -> []

  and analyze_module_item context (pairing : Pairing.t) ~id ~md =
    match Pairing.find_impl_module_mty pairing.impl_sig (Ident.name id) with
    | None -> []
    | Some (impl_id, impl_mty) ->
      let impl_prefix : Path.t option =
        Some
          (match pairing.impl_prefix with
          | None -> Pident impl_id
          | Some p -> Pdot (p, Ident.name impl_id))
      in
      analyze_mty context ~subst:pairing.subst ~impl_prefix ~impl_mty
        ~intf_mty:md.md_type

  and head_of_mty env subst (mty : Types.module_type) =
    match mty with
    | Mty_signature sg -> Some (`Signature sg)
    | Mty_functor (param, body, _) -> Some (`Functor (param, body))
    | Mty_strengthen (inner, _, _) -> head_of_mty env subst inner
    | Mty_ident _ | Mty_alias _ -> (
      let mty = Subst.modtype Subst.Keep subst mty in
      match Mtype.scrape_alias env mty with
      | Mty_ident _ | Mty_alias _ | Mty_for_hole -> None
      | (Mty_signature _ | Mty_functor _ | Mty_strengthen _) as scraped ->
        (* Already in the implementation's namespace: no further subst. *)
        head_of_mty env Subst.identity scraped
      | exception _ -> None)
    | Mty_for_hole -> None

  and analyze_mty context ~subst ~impl_prefix ~impl_mty ~intf_mty =
    match
      ( head_of_mty context.env subst intf_mty,
        head_of_mty context.env Subst.identity impl_mty )
    with
    | Some (`Signature intf_sig), Some (`Signature impl_sig) ->
      analyze_sig context ~impl_prefix ~subst ~impl_sig ~intf_sig
    | ( Some (`Functor (intf_param, intf_body)),
        Some (`Functor (impl_param, impl_body)) ) ->
      let param_weaknesses =
        analyze_functor_param context ~subst ~intf_param ~impl_param
      in
      let env, subst =
        match
          ( (intf_param : Types.functor_parameter),
            (impl_param : Types.functor_parameter) )
        with
        | Named (intf_id, _, _, _), Named (impl_id, impl_pmty, _, _) ->
          let env =
            match impl_id with
            | Some impl_id ->
              Env.add_module impl_id Types.Mp_present impl_pmty context.env
            | None -> context.env
          in
          let subst =
            match (intf_id, impl_id) with
            | Some intf_id, Some impl_id ->
              Subst.add_module intf_id (Path.Pident impl_id) subst
            | (Some _ | None), _ -> subst
          in
          (env, subst)
        | (Unit | Named _), _ -> (context.env, subst)
      in
      param_weaknesses
      @ analyze_mty { context with env } ~subst ~impl_prefix ~impl_mty:impl_body
          ~intf_mty:intf_body
    | _, _ -> []

  and analyze_functor_param context ~subst ~intf_param ~impl_param =
    match
      ( (intf_param : Types.functor_parameter),
        (impl_param : Types.functor_parameter) )
    with
    | Named (_, intf_pmty, _, _), Named (_, impl_pmty, _, _) -> (
      match
        ( head_of_mty context.env subst intf_pmty,
          head_of_mty context.env Subst.identity impl_pmty )
      with
      | Some (`Signature intf_sig), Some (`Signature impl_sig) ->
        analyze_sig
          { context with variance = Variance.neg context.variance }
          ~impl_prefix:None ~subst ~impl_sig ~intf_sig
      | _, _ -> [])
    | (Unit | Named _), _ -> []

  let analyze ~env ~impl_sig ~intf_sig () =
    let snap = Btype.snapshot () in
    Fun.protect
      ~finally:(fun () -> Btype.backtrack snap)
      (fun () ->
        let context = { env; variance = Covariant } in
        analyze_sig context ~impl_prefix:None ~subst:Subst.identity ~impl_sig
          ~intf_sig)
end

module Abstract : sig
  type diff =
    | Kind_annotation of string
    | Mode_diffs of
        { modality_diff :
            (impl:Mode.Modality.Const.t * intf:Mode.Modality.Const.t) option;
          arrow_diffs : arrow_diff list
        }

  (* CR-someday ggray: currently we don't hold abstract data
     across pipelines, but when we expand the analysis to work
     for arbitrary module types and modules implementations, we will. *)
  (* A [t] is safe to hold across Merlin pipelines, when a module type
     has multiple implementations, we hold the abstract types across
     pipelines so that we can later union all the results. *)
  type t = { decl_loc : Location.t; diff : diff }

  val of_weakness : env:Env.t -> Weakness.t -> t
end = struct
  type diff =
    | Kind_annotation of string
    | Mode_diffs of
        { modality_diff :
            (impl:Mode.Modality.Const.t * intf:Mode.Modality.Const.t) option;
          arrow_diffs : arrow_diff list
        }

  type t = { decl_loc : Location.t; diff : diff }

  let of_weakness ~env (w : Weakness.t) : t =
    match w with
    | Value { loc; modality_diff; arrow_diffs } ->
      { decl_loc = loc; diff = Mode_diffs { modality_diff; arrow_diffs } }
    | Type { loc; suggested_jkind } ->
      let printed =
        Printtyp.wrap_printing_env env (fun () ->
            Format_doc.asprintf "%a" (Jkind.format env) suggested_jkind)
      in
      (*The formatter wraps long kinds, edits appear less scary as a
         single long line edit than a poorly formatted mutli-line edit *)
      let diff =
        Kind_annotation
          (String.concat ~sep:" "
             (List.map (String.split_on_char ~sep:'\n' printed) ~f:String.trim))
      in
      { decl_loc = loc; diff }
end

module Render : sig
  val actions_of_weaknesses :
    intf_file:string ->
    intf:Parsetree.signature ->
    Abstract.t list ->
    Query_protocol.Intf_weakness.code_action list
end = struct
  module Intf_weakness = Query_protocol.Intf_weakness

  let transl_modalities ms =
    Typemode.transl_modalities ~maturity:Stable Types.Immutable ms

  module Lookup = struct
    module Parsenode_id = struct
      type t = { file : string; span_start : int; span_end : int }

      let of_loc (l : Location.t) : t =
        { file = Filename.basename l.loc_start.pos_fname;
          span_start = l.loc_start.pos_cnum;
          span_end = l.loc_end.pos_cnum
        }
    end

    let is_intf_file ~intf_file (l : Location.t) =
      String.equal
        (Filename.basename intf_file)
        (Filename.basename l.loc_start.pos_fname)

    type t =
      { intf_file : string;
        annotations : (Parsenode_id.t, Parsetree.modalities) Hashtbl.t;
        enclosing : (Parsenode_id.t, Parsenode_id.t list) Hashtbl.t
      }

    let create ~intf_file (sign : Parsetree.signature) : t =
      let t =
        { intf_file;
          annotations = Hashtbl.create 64;
          enclosing = Hashtbl.create 64
        }
      in
      let annotate loc (modalities : Parsetree.modalities) =
        let id = Parsenode_id.of_loc loc in
        Hashtbl.add t.annotations id modalities;
        id
      in
      let enclosing = ref [] in
      let within id f =
        let outer = !enclosing in
        enclosing := id :: outer;
        f ();
        enclosing := outer
      in
      let signature this (sg : Parsetree.signature) =
        within (annotate sg.psg_loc sg.psg_modalities) (fun () ->
            Ast_iterator.default_iterator.signature this sg)
      in
      let module_declaration this (md : Parsetree.module_declaration) =
        within (annotate md.pmd_loc md.pmd_modalities) (fun () ->
            Ast_iterator.default_iterator.module_declaration this md)
      in
      let value_description this (vd : Parsetree.value_description) =
        Hashtbl.add t.enclosing
          (annotate vd.pval_loc vd.pval_modalities)
          !enclosing;
        Ast_iterator.default_iterator.value_description this vd
      in
      let iterator =
        { Ast_iterator.default_iterator with
          signature;
          module_declaration;
          value_description
        }
      in
      iterator.signature iterator sign;
      t

    let index_weaknesses t (ws : Abstract.t list) =
      let tbl : (Parsenode_id.t, Abstract.t list) Hashtbl.t =
        Hashtbl.create 16
      in
      List.iter ws ~f:(fun (a : Abstract.t) ->
          if is_intf_file ~intf_file:t.intf_file a.decl_loc then begin
            let key = Parsenode_id.of_loc a.decl_loc in
            let prev = Option.value ~default:[] (Hashtbl.find_opt tbl key) in
            Hashtbl.replace tbl key (a :: prev)
          end);
      fun loc ->
        Hashtbl.find_opt tbl (Parsenode_id.of_loc loc)
        |> Option.value ~default:[] |> List.rev

    let annotations_id t id =
      Hashtbl.find_opt t.annotations id |> Option.value ~default:[]

    let enclosing_annotations_id t id =
      Hashtbl.find_opt t.enclosing id
      |> Option.value ~default:[]
      |> List.filter_map ~f:(Hashtbl.find_opt t.annotations)
      |> List.cons (annotations_id t id)

    let enclosing_annotations t loc =
      enclosing_annotations_id t (Parsenode_id.of_loc loc)

    let annotations t loc = annotations_id t (Parsenode_id.of_loc loc)
  end

  (* [Additions] are a structured notion of what we could add into an interface.
     There is one addition type for each place we could add an annotation. *)
  module Additions = struct
    type value_addition =
      { vd : Parsetree.value_description;
        weaknesses : Abstract.t list (* the [Mode_diffs] rows joined to [vd] *)
      }

    type type_addition =
      { td : Parsetree.type_declaration;
        weaknesses :
          Abstract.t list (* the [Kind_annotation] rows joined to [td] *)
      }

    type item_addition =
      | Opaque
      | Value of value_addition
      | Type of type_addition
      | Module of module_addition
      | Module_type of { bodies : signature_addition list }

    and module_addition =
      { md : Parsetree.module_declaration;
        exemption : Mode.Modality.atom list;
        body : signature_addition option;
        floating : signature_addition list
      }

    and signature_addition =
      { sg : Parsetree.signature;
        clause : Mode.Modality.atom list;
        items : item_addition list
      }

    (* The weaknesses [ws] are a flat list of weaknesses found by
       analyzing the typed tree. We need to now analyze the data
       alongside the parse tree. Here we walk the parse tree and
       pair the weaknesses with their items in a structure that
       preserves the module nesting structure. *)
    let of_weaknesses ~lookup ~(intf : Parsetree.signature) ws =
      let weaknesses_at = Lookup.index_weaknesses lookup ws in
      let value_weaknesses loc =
        List.filter (weaknesses_at loc) ~f:(fun (a : Abstract.t) ->
            match a.diff with
            | Mode_diffs _ -> true
            | Kind_annotation _ -> false)
      in
      let type_weaknesses loc =
        List.filter (weaknesses_at loc) ~f:(fun (a : Abstract.t) ->
            match a.diff with
            | Kind_annotation _ -> true
            | Mode_diffs _ -> false)
      in
      let items = ref [] in
      let emit item = items := item :: !items in
      let collect traverse =
        let outer = !items in
        items := [];
        traverse ();
        let collected = List.rev !items in
        items := outer;
        collected
      in
      let of_signature (this : Ast_iterator.iterator) (sg : Parsetree.signature)
          : signature_addition =
        { sg; clause = []; items = collect (fun () -> this.signature this sg) }
      in
      (* The signatures inside a module type that are not contained in the module,
         e.g., functor parameters and results. We consider these a root of their
         own tree. *)
      let floating_signatures this mty =
        let found = ref [] in
        let module_type search (mty : Parsetree.module_type) =
          match mty.pmty_desc with
          | Pmty_signature sg -> found := of_signature this sg :: !found
          | _ -> Ast_iterator.default_iterator.module_type search mty
        in
        let search = { Ast_iterator.default_iterator with module_type } in
        search.module_type search mty;
        List.rev !found
      in
      let rec split_mty this (mty : Parsetree.module_type) :
          signature_addition option * signature_addition list =
        match mty.pmty_desc with
        | Pmty_signature sg -> (Some (of_signature this sg), [])
        | Pmty_with (body, _) | Pmty_strengthen (body, _) -> split_mty this body
        | Pmty_functor _ -> (None, floating_signatures this mty)
        | Pmty_ident _ | Pmty_alias _ | Pmty_typeof _ | Pmty_extension _ ->
          (None, [])
      in
      let value_description _this (vd : Parsetree.value_description) =
        emit (Value { vd; weaknesses = value_weaknesses vd.pval_loc })
      in
      let type_declaration _this (td : Parsetree.type_declaration) =
        emit (Type { td; weaknesses = type_weaknesses td.ptype_loc })
      in
      let module_declaration this (md : Parsetree.module_declaration) =
        let body, floating = split_mty this md.pmd_type in
        emit (Module { md; exemption = []; body; floating })
      in
      let module_type_declaration this (mtd : Parsetree.module_type_declaration)
          =
        match mtd.pmtd_type with
        | None -> ()
        | Some mty ->
          let body, floating = split_mty this mty in
          emit (Module_type { bodies = Option.to_list body @ floating })
      in
      let signature_item this (item : Parsetree.signature_item) =
        match item.psig_desc with
        | Psig_value _ | Psig_type _ | Psig_module _ | Psig_modtype _ ->
          Ast_iterator.default_iterator.signature_item this item
        (* Items with no modality story of their own. *)
        | Psig_open _
        | Psig_attribute _
        | Psig_typesubst _
        | Psig_modsubst _
        | Psig_modtypesubst _ -> ()
        (* [include] and anything unmodeled. *)
        | _ -> emit Opaque
      in
      let iterator =
        { Ast_iterator.default_iterator with
          signature_item;
          value_description;
          type_declaration;
          module_declaration;
          module_type_declaration
        }
      in
      of_signature iterator intf
  end

  module Hoist : sig
    val hoist : Additions.signature_addition -> Additions.signature_addition
  end = struct
    open Additions

    let modality_const modalities =
      (transl_modalities modalities).moda_modalities

    let apply_modalities moda modalities =
      List.fold_left moda ~init:modalities
        ~f:(fun acc (Mode.Modality.Atom (axis, value)) ->
          Mode.Modality.Const.set axis value acc)

    let supports_atom modalities (Mode.Modality.Atom (axis, value)) =
      let required = Mode.Modality.Const.set axis value modalities in
      match
        Mode.Modality.sub
          (Mode.Modality.of_const modalities)
          (Mode.Modality.of_const required)
      with
      | Ok () -> true
      | Error _ -> false

    let strengthens_atom modalities (Mode.Modality.Atom (axis, value)) =
      let strengthened = Mode.Modality.Const.set axis value modalities in
      (not (List.is_empty (Mode.Modality.Const.diff modalities strengthened)))
      &&
      match
        Mode.Modality.sub
          (Mode.Modality.of_const strengthened)
          (Mode.Modality.of_const modalities)
      with
      | Ok () -> true
      | Error _ -> false

    let strengthen_modalities moda modalities =
      List.fold_left moda ~init:modalities ~f:(fun acc atom ->
          if supports_atom acc atom then acc
          else
            let (Mode.Modality.Atom (axis, value)) = atom in
            Mode.Modality.Const.set axis value acc)

    let strongest_modalities moda =
      strengthen_modalities moda Mode.Modality.Const.id

    let unique_modalities moda = List.sort_uniq ~cmp:compare moda

    let value_modality_diff (value : value_addition) =
      List.filter_map value.weaknesses ~f:(fun (weakness : Abstract.t) ->
          match weakness.diff with
          | Mode_diffs { modality_diff = Some diff; _ } -> Some diff
          | Kind_annotation _ | Mode_diffs { modality_diff = None; _ } -> None)
      |> List.hd_opt

    let flip_weakness moda (weakness : Abstract.t) =
      match weakness.diff with
      | Kind_annotation _ | Mode_diffs { modality_diff = None; _ } -> weakness
      | Mode_diffs { modality_diff = Some (~impl, ~intf); arrow_diffs } ->
        let intf = apply_modalities moda intf in
        { weakness with
          diff = Mode_diffs { modality_diff = Some (~impl, ~intf); arrow_diffs }
        }

    let flip_value moda (value : value_addition) =
      { value with
        weaknesses = List.map value.weaknesses ~f:(flip_weakness moda)
      }

    let module_modalities module_ body_modalities =
      apply_modalities
        (Mode.Modality.Const.diff Mode.Modality.Const.id
           (modality_const module_.md.pmd_modalities))
        body_modalities

    let inverse_modalities current moda =
      List.map moda ~f:(fun (Mode.Modality.Atom (axis, _)) ->
          Mode.Modality.Atom (axis, Mode.Modality.Const.proj axis current))

    let flip_item moda (item, modalities, _, _) =
      match (item, modalities) with
      | Module module_, Some modalities ->
        let unsupported =
          List.filter moda ~f:(fun atom -> not (supports_atom modalities atom))
        in
        let current = modality_const module_.md.pmd_modalities in
        Module
          { module_ with
            exemption =
              unique_modalities
                (inverse_modalities current unsupported @ module_.exemption)
          }
      | (Opaque | Value _ | Type _ | Module_type _ | Module _), _ -> item

    let extend_signature_modalities sign moda =
      { sign with clause = unique_modalities (moda @ sign.clause) }

    let flip_modalities moda sign items =
      { sign with items = List.map items ~f:(flip_item moda) }

    let hoist_modalities moda sign items =
      flip_modalities moda (extend_signature_modalities sign moda) items

    let majority supports atom =
      let count = List.length supports in
      let supporting =
        List.fold_left supports ~init:0 ~f:(fun count modalities ->
            if supports_atom modalities atom then count + 1 else count)
      in
      supporting * 2 > count

    let hoistable_modalities ambient supports candidates ~blocked =
      match (blocked, supports) with
      | true, _ | false, [] -> []
      | false, _ ->
        let strengthened =
          candidates
          |> List.filter ~f:(strengthens_atom ambient)
          |> unique_modalities
          |> List.filter ~f:(majority supports)
          |> fun moda -> strengthen_modalities moda ambient
        in
        Mode.Modality.Const.diff ambient strengthened

    let shared_modalities ambient supports ~blocked =
      let candidates =
        Mode.Modality.Const.diff Mode.Modality.Const.id ambient
        @
        if blocked then []
        else
          List.concat_map supports
            ~f:(Mode.Modality.Const.diff Mode.Modality.Const.id)
      in
      candidates |> unique_modalities
      |> List.filter ~f:(fun atom ->
          List.for_all supports ~f:(fun modalities ->
              supports_atom modalities atom))
      |> strongest_modalities

    let same_axis (Mode.Modality.Atom (left, _)) (Mode.Modality.Atom (right, _))
        =
      let (Mode.Value.Axis.P left) =
        Mode.Modality.Axis.to_value (Mode.Modality.Axis.P left)
      in
      let (Mode.Value.Axis.P right) =
        Mode.Modality.Axis.to_value (Mode.Modality.Axis.P right)
      in
      Mode.Value.Axis.compare left right = 0

    let canonical_modalities moda =
      Mode.Modality.Const.diff Mode.Modality.Const.id
        (strongest_modalities moda)

    let rec apply_hoists inherited sign =
      let provided = strongest_modalities inherited in
      let clause =
        List.filter sign.clause ~f:(fun atom ->
            not (supports_atom provided atom))
      in
      let effective = canonical_modalities (clause @ inherited) in
      { sign with
        clause;
        items = List.map sign.items ~f:(apply_hoists_item effective)
      }

    and apply_hoists_item effective = function
      | Value value -> Value (flip_value effective value)
      | Module module_ ->
        let composed =
          List.filter effective ~f:(fun atom ->
              not (List.exists module_.exemption ~f:(same_axis atom)))
        in
        Module
          { module_ with
            body = Option.map module_.body ~f:(apply_hoists composed)
          }
      | (Opaque | Type _ | Module_type _) as item -> item

    let rec hoist_item ambient = function
      | Opaque -> (Opaque, None, [], true)
      | Type type_ -> (Type type_, None, [], false)
      | Value value -> (
        match value_modality_diff value with
        | Some (~impl, ~intf) ->
          let candidates =
            Mode.Modality.Const.diff intf impl
            |> List.filter ~f:(strengthens_atom ambient)
          in
          (Value value, Some impl, candidates, false)
        | None -> (Value value, None, [], true))
      | Module_type _ as item -> (item, None, [], false)
      | Module module_ -> (
        match module_.body with
        | None -> (Module module_, None, [], true)
        | Some body ->
          let body_ambient = module_modalities module_ ambient in
          let body, modalities = hoist_with_modalities body_ambient body in
          let modalities = module_modalities module_ modalities in
          (Module { module_ with body = Some body }, Some modalities, [], false)
        )

    and hoist_with_modalities inherited sign =
      let ambient =
        strengthen_modalities
          (Mode.Modality.Const.diff Mode.Modality.Const.id
             (modality_const sign.sg.psg_modalities))
          inherited
      in
      let items = List.map sign.items ~f:(hoist_item ambient) in
      let supports =
        List.filter_map items ~f:(fun (_, modalities, _, _) -> modalities)
      in
      let candidates = List.concat_map items ~f:(fun (_, _, moda, _) -> moda) in
      let blocked = List.exists items ~f:(fun (_, _, _, blocked) -> blocked) in
      let sign =
        { sign with items = List.map items ~f:(fun (item, _, _, _) -> item) }
      in
      let moda = hoistable_modalities ambient supports candidates ~blocked in
      let sign = hoist_modalities moda sign items in
      (sign, shared_modalities ambient supports ~blocked)

    (* Hoist modalities of items into the signature.
       We recursively hoist, starting with the leaves and then
       work our way up. *)
    let hoist sign =
      hoist_with_modalities Mode.Modality.Const.id sign
      |> fst |> apply_hoists []
  end

  module Print = struct
    let modality_atoms ~impl ~intf ~explicit =
      List.fold_right explicit ~init:impl
        ~f:(fun (moda : Typemode.modalities) acc ->
          List.fold_left moda.moda_desc ~init:acc
            ~f:(fun acc (m : _ Location.loc) ->
              let (Mode.Modality.Atom (ax, _)) = m.txt in
              Mode.Modality.Const.set ax (Mode.Modality.Const.proj ax intf) acc))
      |> Out_type.tree_of_modalities_with_default ~default:intf

    let written_atoms ~intf ~explicit =
      match explicit with
      | [] -> []
      | (own : Typemode.modalities) :: _ ->
        List.map own.moda_desc ~f:(fun (m : _ Location.loc) ->
            let (Mode.Modality.Atom (ax, _)) = m.txt in
            Format_doc.asprintf "%a"
              (Mode.Modality.Per_axis.print ax)
              (Mode.Modality.Const.proj ax intf))

    let alloc_atoms ~impl ~explicit =
      List.fold_left explicit ~init:impl
        ~f:(fun (acc : Mode.Alloc.Const.t) (m : _ Location.loc) ->
          let (Mode.Alloc.Atom (ax, v)) = m.txt in
          match ax with
          | Comonadic Areality -> { acc with areality = v }
          | Comonadic Linearity -> { acc with linearity = v }
          | Comonadic Portability -> { acc with portability = v }
          | Comonadic Forkable -> { acc with forkable = v }
          | Comonadic Yielding -> { acc with yielding = v }
          | Comonadic Statefulness -> { acc with statefulness = v }
          | Monadic Uniqueness -> { acc with uniqueness = v }
          | Monadic Contention -> { acc with contention = v }
          | Monadic Visibility -> { acc with visibility = v }
          | Monadic Staticity -> { acc with staticity = v })
      |> Out_type.tree_of_modes

    (* [None] when the edit doesn't add new text, but emits
       something equivalent to what's there *)
    let modality_text ~impl ~intf ~explicit =
      match modality_atoms ~impl ~intf ~explicit with
      | [] -> None
      | claims ->
        Some
          (claims @ written_atoms ~intf ~explicit
          |> List.fast_sort ~cmp:String.compare
          |> String.concat ~sep:" ")

    let arrow_modes_text ~impl ~explicit =
      alloc_atoms ~impl ~explicit
      |> List.fast_sort ~cmp:String.compare
      |> String.concat ~sep:" "
  end

  module Loc = struct
    (* A zero-width non-ghost location at the requested position. *)
    let insertion_at (pos : Lexing.position) : Location.t =
      { loc_start = pos; loc_end = pos; loc_ghost = false }

    (* The span from the first location's start to the last location's
       end; [None] for an empty list. *)
    let span_locs (locs : Location.t list) : Location.t option =
      match locs with
      | [] -> None
      | first :: rest ->
        let last = List.fold_left rest ~init:first ~f:(fun _ l -> l) in
        Some
          { loc_start = first.loc_start;
            loc_end = last.loc_end;
            loc_ghost = false
          }
  end

  module Edit = struct
    (* Replace the existing annotation's span when there is one, otherwise
       insert [prefix ^ text] at [at], which is the only non-insertion edit shape. *)
    let replace_or_insert ~existing_locs ~at ~prefix text :
        Intf_weakness.text_edit =
      match Loc.span_locs existing_locs with
      | None -> { edit_loc = Loc.insertion_at at; edit_text = prefix ^ text }
      | Some span -> { edit_loc = span; edit_text = text }

    type arrow_target =
      { reference_pos : Lexing.position;
        existing : Mode.Alloc.Const.t Typemode.modes
      }

    (* Navigate a parsed core type along an [Arrow_pos.dir] and find the
       source insertion/replacement target for that arrow mode.
       Arrow-shaped return targets that are currently ambiguous to render
       are dropped. *)
    let locate_target (ct : Parsetree.core_type) (dir : Arrow_pos.dir) :
        arrow_target option =
      let rec is_arrow_shaped (ct : Parsetree.core_type) =
        match ct.ptyp_desc with
        | Ptyp_arrow _ -> true
        | Ptyp_poly (_, inner) | Ptyp_alias (inner, _, _) | Ptyp_open (_, inner)
          -> is_arrow_shaped inner
        | _ -> false
      in
      let rec walk (ct : Parsetree.core_type) (dir : Arrow_pos.dir) =
        match ct.ptyp_desc with
        | Ptyp_arrow (_, arg_ct, ret_ct, arg_modes, ret_modes) -> (
          match dir with
          | In_arg Here ->
            let pos = arg_ct.ptyp_loc.loc_end in
            (* A parenthesized arrow arg's closing paren is not part of
                [arg_ct]'s loc; skip it. Assumes canonical formatting where
                [)] immediately follows the inner type. *)
            let reference_pos =
              if is_arrow_shaped arg_ct then
                { pos with pos_cnum = pos.pos_cnum + 1 }
              else pos
            in
            Some
              { reference_pos; existing = Typemode.transl_alloc_mode arg_modes }
          | In_ret Here ->
            Some
              { reference_pos = ret_ct.ptyp_loc.loc_end;
                existing = Typemode.transl_alloc_mode ret_modes
              }
          | In_arg rest -> walk arg_ct rest
          | In_ret rest -> walk ret_ct rest
          | Here -> None)
        | Ptyp_poly (_, inner)
        | Ptyp_alias (inner, _, _)
        | Ptyp_open (_, inner)
        | Ptyp_repr (_, inner)
        | Ptyp_newlayout (_, inner) -> walk inner dir
        | _ -> None
      in
      walk ct dir

    (* The edit for one arrow-mode diff, or [None] when the target cannot
       be located or the rendered text is empty. *)
    let arrow_diff_edit (vd : Parsetree.value_description) (d : arrow_diff) =
      let open Option.Infix in
      let* target = locate_target vd.pval_type d.path.dir in
      let explicit = target.existing.mode_desc in
      let text = Print.arrow_modes_text ~impl:d.impl ~explicit in
      let* () = guard (not (String.equal text "")) in
      Some
        (replace_or_insert
           ~existing_locs:
             (List.map explicit ~f:(fun (m : _ Location.loc) -> m.loc))
           ~at:target.reference_pos ~prefix:" @ " text)

    let modality_diff_edit ~lookup (vd : Parsetree.value_description)
        modality_diff =
      let open Option.Infix in
      let* ~impl, ~intf = modality_diff in
      let* () = guard (List.is_empty vd.pval_prim) in
      let explicit =
        Lookup.enclosing_annotations lookup vd.pval_loc
        |> List.map ~f:transl_modalities
      in
      let+ text = Print.modality_text ~impl ~intf ~explicit in
      replace_or_insert
        ~existing_locs:
          (List.map (Lookup.annotations lookup vd.pval_loc) ~f:(fun m ->
               m.Location.loc))
        ~at:vd.pval_type.ptyp_loc.loc_end ~prefix:" @@ " text

    let edits_for_value (vd : Parsetree.value_description) ~lookup
        ~(modality_diff :
           (impl:Mode.Modality.Const.t * intf:Mode.Modality.Const.t) option)
        ~(arrow_diffs : arrow_diff list) =
      let arrow_edits = List.filter_map arrow_diffs ~f:(arrow_diff_edit vd) in
      let modality_edits =
        modality_diff_edit ~lookup vd modality_diff |> Option.to_list
      in
      (* It's important the modality edit goes after the arrow edits so that the modality
         annotation goes after a mode annotation on the return value. *)
      arrow_edits @ modality_edits

    (* The kind-annotation edit for a type declaration: insert
       [ : <kind>] immediately after the type's name. Declarations already
       carrying a kind annotation are skipped. *)
    let kind_annotation_edit (td : Parsetree.type_declaration) annotation :
        Intf_weakness.text_edit option =
      match td.ptype_jkind_annotation with
      | Some _ -> None
      | None ->
        Some
          { edit_loc = Loc.insertion_at td.ptype_name.loc.loc_end;
            edit_text = " : " ^ annotation
          }

    (* The floating-clause edit of a hoisting signature: extend the
       existing clause in place (re-printing its written atoms — never
       deleted, never touched on their axes), or insert a new clause
       before the first item. The printer keeps the spelling minimal:
       an atom implied by a stronger hoisted one is dropped, exactly as a
       reader's translation would reintroduce it. *)
    let clause_edit (s : Additions.signature_addition) :
        Intf_weakness.text_edit option =
      let open Option.Infix in
      match s.clause with
      | [] -> None
      | hoisted -> (
        let clause = transl_modalities s.sg.psg_modalities in
        let ambient = clause.moda_modalities in
        let hoisted_const =
          List.fold_left hoisted ~init:ambient
            ~f:(fun acc (Mode.Modality.Atom (ax, v)) ->
              Mode.Modality.Const.set ax v acc)
        in
        let* text =
          Print.modality_text ~impl:hoisted_const ~intf:ambient
            ~explicit:[ clause ]
        in
        match
          Loc.span_locs
            (List.map s.sg.psg_modalities ~f:(fun (m : _ Location.loc) -> m.loc))
        with
        | Some span -> Some { Intf_weakness.edit_loc = span; edit_text = text }
        | None -> (
          match s.sg.psg_items with
          | [] -> None
          | first :: _ ->
            let start = first.psig_loc.loc_start in
            let indent = String.make (start.pos_cnum - start.pos_bol) ' ' in
            Some
              { Intf_weakness.edit_loc = Loc.insertion_at start;
                edit_text = "@@ " ^ text ^ "\n\n" ^ indent
              }))

    let module_exemption_edit ~hoisted (m : Additions.module_addition) :
        Intf_weakness.text_edit option =
      let open Option.Infix in
      match m.exemption with
      | [] -> None
      | unsupported ->
        let written = transl_modalities m.md.pmd_modalities in
        let current = written.moda_modalities in
        (* What the declaration would read as with no exemption: the
           clause's atoms are its per-item default. *)
        let defaulted =
          List.fold_left hoisted ~init:current
            ~f:(fun acc (Mode.Modality.Atom (ax, v)) ->
              Mode.Modality.Const.set ax v acc)
        in
        (* The declaration's target: unsupported axes back at their
           status quo, supported ones kept. *)
        let target =
          List.fold_left unsupported ~init:defaulted
            ~f:(fun acc (Mode.Modality.Atom (ax, _)) ->
              Mode.Modality.Const.set ax
                (Mode.Modality.Const.proj ax current)
                acc)
        in
        let* text =
          Print.modality_text ~impl:target ~intf:defaulted ~explicit:[ written ]
        in
        Some
          (replace_or_insert
             ~existing_locs:
               (List.map m.md.pmd_modalities ~f:(fun (x : _ Location.loc) ->
                    x.loc))
             ~at:m.md.pmd_type.pmty_loc.loc_end ~prefix:" @@ " text)
  end

  let value_addition_edits ~lookup (v : Additions.value_addition) =
    List.concat_map v.weaknesses ~f:(fun (a : Abstract.t) ->
        match a.diff with
        | Kind_annotation _ -> []
        | Mode_diffs { modality_diff; arrow_diffs } ->
          Edit.edits_for_value v.vd ~lookup ~modality_diff ~arrow_diffs)

  let type_addition_edits (t : Additions.type_addition) =
    List.concat_map t.weaknesses ~f:(fun (a : Abstract.t) ->
        match a.diff with
        | Mode_diffs _ -> []
        | Kind_annotation annotation ->
          Option.to_list (Edit.kind_annotation_edit t.td annotation))

  let rec signature_edits ~lookup ~hoisted (s : Additions.signature_addition) =
    let same_axis (Mode.Modality.Atom (left, _)) (Mode.Modality.Atom (right, _))
        =
      let (Mode.Value.Axis.P left) =
        Mode.Modality.Axis.to_value (Mode.Modality.Axis.P left)
      in
      let (Mode.Value.Axis.P right) =
        Mode.Modality.Axis.to_value (Mode.Modality.Axis.P right)
      in
      Mode.Value.Axis.compare left right = 0
    in
    let hoisted = s.clause @ hoisted in
    let of_item (item : Additions.item_addition) =
      match item with
      | Opaque -> []
      | Value v -> value_addition_edits ~lookup v
      | Type t -> type_addition_edits t
      | Module m ->
        (* An exempted axis does not compose into the module; the others
           do, and the members print against them. Functor signatures:
           no clause reaches them. *)
        let composed =
          List.filter hoisted ~f:(fun a ->
              not (List.exists m.exemption ~f:(fun e -> same_axis e a)))
        in
        Option.to_list (Edit.module_exemption_edit ~hoisted m)
        @ List.concat_map (Option.to_list m.body)
            ~f:(signature_edits ~lookup ~hoisted:composed)
        @ List.concat_map m.floating ~f:(signature_edits ~lookup ~hoisted:[])
      | Module_type { bodies } ->
        (* Module type bodies: shared declarations, no clause reaches
           them. *)
        List.concat_map bodies ~f:(signature_edits ~lookup ~hoisted:[])
    in
    Option.to_list (Edit.clause_edit s) @ List.concat_map s.items ~f:of_item

  (* CR-someday ggray: right now we'll just assume that the entire interface
     is strengthened at once. There are many different use cases, likely agentic,
     so we'll have to think about that later. *)
  let code_action ~intf_file edits =
    match edits with
    | [] -> []
    | _ :: _ -> [ { Intf_weakness.intf_file; edits } ]

  let actions_of_weaknesses ~intf_file ~intf ws =
    let lookup = Lookup.create ~intf_file intf in
    Additions.of_weaknesses ~lookup ~intf ws
    |> Hoist.hoist
    |> signature_edits ~lookup ~hoisted:[]
    |> code_action ~intf_file
end

let analyze ~env ~impl_sig ~intf_sig () =
  Analyze.analyze ~env ~impl_sig ~intf_sig ()
  |> List.map ~f:(Abstract.of_weakness ~env)

let render = Render.actions_of_weaknesses

let code_actions ~env ~impl_sig ~intf_sig ~intf_file ~intf () =
  analyze ~env ~impl_sig ~intf_sig () |> render ~intf_file ~intf
