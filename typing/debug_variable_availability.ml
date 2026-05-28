(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Simon Spies, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module VA = Type_shape.Variable_availability

module Walk = struct
  (* [seen_idents] records the first [Shape.Uid.t] under which each
     [Ident.t] was registered. When a later registration uses the same
     [Ident.t] with a different uid, the new variable has been merged
     with the original (e.g. by [enter_orpat_variables] alpha-renaming
     the right arm of an or-pattern). Same-id-same-uid revisits are
     just the iterator re-entering a node already registered by an
     enclosing handler (function parameters, comprehension indices,
     etc.) and are ignored. *)
  type t =
    { mutable current_function : VA.source_function;
      seen_idents : (Ident.t, Shape.Uid.t) Hashtbl.t
    }

  let note_ident t ~id ~uid =
    match Hashtbl.find_opt t.seen_idents id with
    | None -> Hashtbl.add t.seen_idents id uid
    | Some prev_uid ->
      if not (Shape.Uid.equal prev_uid uid)
      then
        VA.register_dropped_intentionally ~uid
          ~reason:(Merged_with prev_uid)

  let register_var t ~id ~uid ~name ~location ~kind =
    VA.register_source_variable t.current_function ~uid ~name ~location ~kind;
    note_ident t ~id ~uid

  let enter_function ~display_name ~location =
    VA.register_source_function ~display_name ~location

  let with_function t f body =
    let prev = t.current_function in
    t.current_function <- f;
    let r = body () in
    t.current_function <- prev;
    r

  let function_param t it (p : Typedtree.function_param) ~index =
    register_var t ~id:p.fp_param ~uid:p.fp_param_debug_uid
      ~name:(Ident.name p.fp_param) ~location:p.fp_loc
      ~kind:(Parameter index);
    match p.fp_kind with
    | Tparam_pat pat -> it.Tast_iterator.pat it pat
    | Tparam_optional_default (pat, default, _sort) ->
      it.Tast_iterator.pat it pat;
      it.Tast_iterator.expr it default

  let function_cases t it (fc : Typedtree.function_cases) ~index =
    register_var t ~id:fc.fc_param ~uid:fc.fc_param_debug_uid
      ~name:(Ident.name fc.fc_param) ~location:fc.fc_loc
      ~kind:(Parameter index);
    List.iter (it.Tast_iterator.case it) fc.fc_cases

  let comprehension t it (c : Typedtree.comprehension) =
    List.iter
      (fun cl ->
        match cl with
        | Typedtree.Texp_comp_when e -> it.Tast_iterator.expr it e
        | Typedtree.Texp_comp_for bindings ->
          List.iter
            (fun { Typedtree.comp_cb_iterator; comp_cb_attributes = _ } ->
              match comp_cb_iterator with
              | Typedtree.Texp_comp_range
                  { ident;
                    ident_debug_uid;
                    pattern;
                    start;
                    stop;
                    direction = _
                  } ->
                register_var t ~id:ident ~uid:ident_debug_uid
                  ~name:(Ident.name ident) ~location:pattern.ppat_loc
                  ~kind:Comprehension_index;
                it.Tast_iterator.expr it start;
                it.Tast_iterator.expr it stop
              | Texp_comp_in { pattern; sequence } ->
                it.Tast_iterator.pat it pattern;
                it.Tast_iterator.expr it sequence)
            bindings)
      c.comp_clauses;
    it.Tast_iterator.expr it c.comp_body

  let enter_anonymous_function t it (e : Typedtree.expression) params body =
    let display_name =
      Format.asprintf "<anon at %a>" Location.print_loc e.exp_loc
    in
    let f = enter_function ~display_name ~location:e.exp_loc in
    with_function t f (fun () ->
        List.iteri (fun i p -> function_param t it p ~index:i) params;
        match (body : Typedtree.function_body) with
        | Tfunction_body e -> it.Tast_iterator.expr it e
        | Tfunction_cases fc ->
          function_cases t it fc ~index:(List.length params))

  let expr t it (e : Typedtree.expression) =
    match e.exp_desc with
    | Texp_function { params; body; _ } ->
      enter_anonymous_function t it e params body
    | Texp_for { for_id; for_debug_uid; for_pat; for_from; for_to; for_body; _ }
      ->
      register_var t ~id:for_id ~uid:for_debug_uid ~name:(Ident.name for_id)
        ~location:for_pat.ppat_loc ~kind:For_index;
      it.Tast_iterator.expr it for_from;
      it.Tast_iterator.expr it for_to;
      it.Tast_iterator.expr it for_body
    | Texp_letop { let_; ands; param; param_debug_uid; body; _ } ->
      it.Tast_iterator.binding_op it let_;
      List.iter (it.Tast_iterator.binding_op it) ands;
      register_var t ~id:param ~uid:param_debug_uid ~name:(Ident.name param)
        ~location:body.c_lhs.pat_loc ~kind:Letop_param;
      it.Tast_iterator.case it body
    | Texp_list_comprehension c -> comprehension t it c
    | Texp_array_comprehension (_, _, c) -> comprehension t it c
    | _ -> Tast_iterator.default_iterator.expr it e

  let display_name_from_pat (pat : Typedtree.pattern) =
    match pat.pat_desc with
    | Tpat_var { name; _ } -> Some name.txt
    | _ -> None

  let value_binding t it (vb : Typedtree.value_binding) =
    match display_name_from_pat vb.vb_pat, vb.vb_expr.exp_desc with
    | Some name, Texp_function { params; body; _ } ->
      it.Tast_iterator.pat it vb.vb_pat;
      let f = enter_function ~display_name:name ~location:vb.vb_loc in
      with_function t f (fun () ->
          List.iteri (fun i p -> function_param t it p ~index:i) params;
          match body with
          | Tfunction_body e -> it.Tast_iterator.expr it e
          | Tfunction_cases fc ->
            function_cases t it fc ~index:(List.length params))
    | _ -> Tast_iterator.default_iterator.value_binding it vb

  let pat (type k) t it (pat : k Typedtree.general_pattern) =
    (match pat.pat_desc with
    | Tpat_var { id; name; uid; _ } ->
      register_var t ~id ~uid ~name:name.txt ~location:pat.pat_loc ~kind:Local
    | Tpat_alias { id; name; uid; _ } ->
      register_var t ~id ~uid ~name:name.txt ~location:pat.pat_loc ~kind:Alias
    | Tpat_fun_layout { id; name; uid; _ } ->
      register_var t ~id ~uid ~name:name.txt ~location:pat.pat_loc ~kind:Local
    | _ -> ());
    Tast_iterator.default_iterator.pat it pat

  let implementation ~unit_name (impl : Typedtree.implementation) =
    VA.set_unit_name unit_name;
    let module_scope =
      VA.register_source_module ~display_name:unit_name ~location:Location.none
    in
    let t =
      { current_function = module_scope;
        seen_idents = Hashtbl.create 64
      }
    in
    let open Tast_iterator in
    (* The handlers need open recursion through the iterator, so we tie
       the knot via a ref rather than a let-rec. *)
    let it = ref default_iterator in
    it
      := { default_iterator with
           pat = (fun it p -> pat t it p);
           expr = (fun it e -> expr t it e);
           value_binding = (fun it vb -> value_binding t it vb)
         };
    !it.structure !it impl.structure
end

let collect_from_typedtree ~unit_name impl =
  if VA.is_enabled () then Walk.implementation ~unit_name impl

let record_lambda_uid ~checkpoint uid =
  if not (Shape.Uid.equal uid Shape.Uid.internal_not_actually_unique)
  then VA.record_observation ~checkpoint (VA.Source_uid uid)

let rec walk_lambda ~checkpoint (l : Lambda.lambda) =
  match l with
  | Lvar _ | Lmutvar _ | Lconst _ | Lsplice _ -> ()
  | Lapply { ap_func; ap_args; _ } ->
    walk_lambda ~checkpoint ap_func;
    List.iter (walk_lambda ~checkpoint) ap_args
  | Lfunction { params; body; _ } ->
    List.iter
      (fun (p : Lambda.lparam) -> record_lambda_uid ~checkpoint p.debug_uid)
      params;
    walk_lambda ~checkpoint body
  | Llet (_, _, _id, duid, def, body) | Lmutlet (_, _id, duid, def, body) ->
    record_lambda_uid ~checkpoint duid;
    walk_lambda ~checkpoint def;
    walk_lambda ~checkpoint body
  | Lletrec (bindings, body) ->
    List.iter
      (fun (b : Lambda.rec_binding) ->
        record_lambda_uid ~checkpoint b.debug_uid;
        walk_lambda ~checkpoint (Lambda.Lfunction b.def))
      bindings;
    walk_lambda ~checkpoint body
  | Lprim (_, args, _) -> List.iter (walk_lambda ~checkpoint) args
  | Lswitch (arg, sw, _, _) ->
    walk_lambda ~checkpoint arg;
    List.iter (fun (_, e) -> walk_lambda ~checkpoint e) sw.sw_consts;
    List.iter (fun (_, e) -> walk_lambda ~checkpoint e) sw.sw_blocks;
    Option.iter (walk_lambda ~checkpoint) sw.sw_failaction
  | Lstringswitch (arg, cases, default, _, _) ->
    walk_lambda ~checkpoint arg;
    List.iter (fun (_, e) -> walk_lambda ~checkpoint e) cases;
    Option.iter (walk_lambda ~checkpoint) default
  | Lstaticraise (_, args) -> List.iter (walk_lambda ~checkpoint) args
  | Lstaticcatch (body, (_, params), handler, _, _) ->
    walk_lambda ~checkpoint body;
    List.iter
      (fun (_, duid, _) -> record_lambda_uid ~checkpoint duid)
      params;
    walk_lambda ~checkpoint handler
  | Ltrywith (body, _id, duid, handler, _) ->
    walk_lambda ~checkpoint body;
    record_lambda_uid ~checkpoint duid;
    walk_lambda ~checkpoint handler
  | Lifthenelse (c, t, e, _) ->
    walk_lambda ~checkpoint c;
    walk_lambda ~checkpoint t;
    walk_lambda ~checkpoint e
  | Lsequence (a, b) ->
    walk_lambda ~checkpoint a;
    walk_lambda ~checkpoint b
  | Lwhile { wh_cond; wh_body } ->
    walk_lambda ~checkpoint wh_cond;
    walk_lambda ~checkpoint wh_body
  | Lfor { for_debug_uid; for_from; for_to; for_body; _ } ->
    record_lambda_uid ~checkpoint for_debug_uid;
    walk_lambda ~checkpoint for_from;
    walk_lambda ~checkpoint for_to;
    walk_lambda ~checkpoint for_body
  | Lassign (_, e) -> walk_lambda ~checkpoint e
  | Lsend (_, met, obj, args, _, _, _, _) ->
    List.iter (walk_lambda ~checkpoint) (met :: obj :: args)
  | Levent (e, _) | Lifused (_, e) | Lregion (e, _) | Lexclave e ->
    walk_lambda ~checkpoint e

let observe_lambda_program ~checkpoint (program : Lambda.program) =
  if VA.is_enabled () then walk_lambda ~checkpoint program.code
