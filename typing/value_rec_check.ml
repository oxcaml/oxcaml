(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Jeremy Yallop, University of Cambridge                   *)
(*               Gabriel Scherer, Project Parsifal, INRIA Saclay          *)
(*               Alban Reynaud, ENS Lyon                                  *)
(*                                                                        *)
(*   Copyright 2017 Jeremy Yallop                                         *)
(*   Copyright 2018 Alban Reynaud                                         *)
(*   Copyright 2018 INRIA                                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Static checking of recursive declarations.

    The names bound by a [let rec] may only be used where they are not read
    while the group is being defined. In a group
    {[
      let rec x1 = e1 and ... and xn = en
    ]}
    each right-hand side [ei] that mentions the group consists of a preamble
    followed by a definition.

    The preamble is a sequence of [let ... in], [let open ... in],
    [let module ... in], [let exception ... in] and [e;] (see
    [split_preamble]). An item of the preamble that does not mention the
    group is evaluated before the group. The variables bound by a [let] of the
    preamble that mentions the group join the group: their definitions are
    checked like those of [x1 ... xn].

    A definition is one of:
    {[
      M ::= fun ...          (body arbitrary)
          | lazy e           (e arbitrary)
          | B                (a block, whose fields are constructions)
          | e                (e does not mention the group)
      B ::= K (C, ..) | (C, ..) | { l = C; .. } | `A C | ref C
      C ::= B | [| C; .. |] | x | constant | fun ... | lazy e
    ]}
    where only the fields of [B] that mention the group need to be
    constructions. Constructions only allocate: functions and lazy values in
    them are stored, not evaluated. A block is therefore allocated with
    placeholders in the fields that mention the group, which are assigned once
    the whole group is defined.

    Finally, every cycle in the data created by the group must go through a
    function, a lazy block or a mutable field: the names of the group that
    occur in the fields of a block, outside of functions, lazy expressions and
    mutable fields (including references and arrays), must not form a cycle
    between blocks. Definitions such as [let rec xs = 0 :: xs] or
    [let rec a = Some b and b = Some a] are thus rejected, whereas
    [let rec r = { f = fun () -> r }] is accepted. See
    [Translcore.transl_let] for the compilation scheme.

    Recursive class definitions follow the same principle: the let-bindings
    in the spine of a class expression are evaluated when the class is
    defined, so they may not refer to the classes of the group. Method
    bodies and the other parts of the class body are only evaluated later
    and are unrestricted. *)

open Typedtree

type error =
  | Illegal_reference of Location.t * Ident.t
  | Illegal_cycle of Location.t * Ident.t list

exception Illegal of error

exception Found of Ident.t

let is_member ids id = List.exists (Ident.same id) ids

(* Identifiers in the typed tree are unique, so a plain traversal suffices
   to find references: there is no shadowing to account for. *)
let check_path ids path =
  match path with
  | Path.Pident id when is_member ids id -> raise (Found id)
  | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> ()

(* Values can only be referred to through [Texp_ident] and the operators of
   [Texp_letop]. *)
let value_reference_iterator ids =
  let open Tast_iterator in
  let expr sub e =
    begin match e.exp_desc with
    | Texp_ident { path; _ } -> check_path ids path
    | _ -> ()
    end;
    default_iterator.expr sub e
  in
  let binding_op sub bop =
    check_path ids bop.bop_op_path;
    default_iterator.binding_op sub bop
  in
  { default_iterator with expr; binding_op }

(* Classes can only be referred to through [Texp_new] and [Tcl_ident]. *)
let class_reference_iterator ids =
  let open Tast_iterator in
  let expr sub e =
    begin match e.exp_desc with
    | Texp_new (path, _, _, _) -> check_path ids path
    | _ -> ()
    end;
    default_iterator.expr sub e
  in
  let class_expr sub ce =
    begin match ce.cl_desc with
    | Tcl_ident (path, _, _) -> check_path ids path
    | _ -> ()
    end;
    default_iterator.class_expr sub ce
  in
  { default_iterator with expr; class_expr }

let find_reference iterator traverse =
  match traverse iterator with
  | () -> None
  | exception Found id -> Some id

let find_value_reference ids e =
  let iterator = value_reference_iterator ids in
  find_reference iterator (fun (it : Tast_iterator.iterator) -> it.expr it e)

let find_module_reference ids me =
  let iterator = value_reference_iterator ids in
  find_reference iterator
    (fun (it : Tast_iterator.iterator) -> it.module_expr it me)

let illegal_reference loc id = raise (Illegal (Illegal_reference (loc, id)))

let split_preamble e =
  match e.exp_desc with
  | Texp_let (_, _, body)
  | Texp_open (_, body)
  | Texp_letmodule (_, _, _, _, body)
  | Texp_letexception (_, body)
  | Texp_sequence (_, _, body) -> Some body
  | _ -> None

(* The contents of a reference cell allocated by a direct application of
   the [%makemutable] primitive, such as [ref] or [Atomic.make]. Its single
   field is mutable, like that of [{ contents }]. *)
let ref_contents e =
  match e.exp_desc with
  | Texp_apply
      ({ exp_desc =
           Texp_ident
             { desc = { val_kind = Val_prim { prim_name = "%makemutable"; _ };
                        _ };
               _ };
         _ },
       [ (_, Arg (contents, _)) ], _, _, _, _) ->
    Some contents
  | _ -> None

type field =
  { contents : expression;
    mutable_ : bool;
    label : Data_types.label_description option }

let record_fields fields (representation : Types.record_representation) =
  match representation with
  | Record_unboxed
  | Record_inlined (_, _, (Variant_unboxed | Variant_with_null)) -> None
  | Record_undetermined | Record_dummy _ | Record_variable _ -> None
  | Record_boxed | Record_float | Record_ufloat | Record_mixed _
  | Record_inlined (_, _, (Variant_boxed _ | Variant_extensible)) ->
    Some
      (List.filter_map
         (fun ((lbl : Data_types.label_description), _, definition) ->
            match definition with
            | Kept _ -> None
            | Overridden (_, contents) ->
              Some { contents; mutable_ = Types.is_mutable lbl.lbl_mut;
                     label = Some lbl })
         (Array.to_list fields))

let immutable_field contents = { contents; mutable_ = false; label = None }

(* The fields of a block literal (the [B] of the grammar above). *)
let block_fields e =
  match e.exp_desc with
  | Texp_record { fields; representation; extended_expression = None; _ } ->
    record_fields fields representation
  | Texp_construct (_, cstr, _, args, _) ->
    begin match cstr.cstr_repr, cstr.cstr_inlined, args with
    | (Variant_unboxed | Variant_with_null), _, _ -> None
    | (Variant_boxed _ | Variant_extensible), Some _,
      [ (_, { exp_desc =
                Texp_record
                  { fields; representation; extended_expression = None; _ };
              _ }) ] ->
      record_fields fields representation
    | (Variant_boxed _ | Variant_extensible), Some _, _ -> None
    | (Variant_boxed _ | Variant_extensible), None, _ ->
      Some (List.map (fun (_, e) -> immutable_field e) args)
    end
  | Texp_tuple (args, _) ->
    Some (List.map (fun (_, e, _) -> immutable_field e) args)
  | Texp_variant (_, Some (e, _)) -> Some [ immutable_field e ]
  | _ ->
    match ref_contents e with
    | Some contents -> Some [ { contents; mutable_ = true; label = None } ]
    | None -> None

(* The state of the check of a group: the names bound by the group,
   including the members of preambles, the kinds of their definitions, and
   the references between blocks that are not through a function, a lazy
   expression or a mutable field. *)
type state =
  { mutable ids : Ident.t list;
    mutable kinds : (Ident.t * Value_rec_types.definition) list;
    mutable edges : (Ident.t * Ident.t * Location.t) list }

exception Not_a_construction

(* Checks that [e], in a field of the block [src], is a construction (the [C]
   of the grammar above), and records the references to the group in it. *)
let rec construction st ~src ~mutable_ e =
  let construction ?(mutable_ = mutable_) e =
    construction st ~src ~mutable_ e
  in
  match e.exp_desc with
  | Texp_function _ | Texp_lazy _ | Texp_constant _ -> ()
  | Texp_ident { path = Path.Pident id; _ } when is_member st.ids id ->
    if not mutable_ then st.edges <- (src, id, e.exp_loc) :: st.edges
  | Texp_ident _ -> ()
  | Texp_apply_layout (e, _) -> construction e
  | Texp_construct (_, _, _, args, _) ->
    List.iter (fun (_, e) -> construction e) args
  | Texp_tuple (args, _) -> List.iter (fun (_, e, _) -> construction e) args
  | Texp_variant (_, arg) -> Option.iter (fun (e, _) -> construction e) arg
  | Texp_array (mut, _, args, _) ->
    let mutable_ = mutable_ || Types.is_mutable mut in
    List.iter (construction ~mutable_) args
  | Texp_record { fields; extended_expression = None; _ } ->
    Array.iter
      (fun ((lbl : Data_types.label_description), _, definition) ->
         match definition with
         | Kept _ -> ()
         | Overridden (_, e) ->
           construction ~mutable_:(mutable_ || Types.is_mutable lbl.lbl_mut) e)
      fields
  | _ ->
    match ref_contents e with
    | Some contents -> construction ~mutable_:true contents
    | None -> raise Not_a_construction

let classify_field st ~src { contents; mutable_; label } =
  match find_value_reference st.ids contents with
  | None -> ()
  | Some id ->
    begin match construction st ~src ~mutable_ contents with
    | () -> ()
    | exception Not_a_construction -> illegal_reference contents.exp_loc id
    end;
    (* The field is assigned once the group is defined. *)
    Option.iter
      (fun (lbl : Data_types.label_description) ->
         if mutable_ then Env.mark_label_used Mutation lbl.lbl_uid)
      label

let classify_definition st id e : Value_rec_types.definition =
  match find_value_reference st.ids e with
  | None -> Value
  | Some ref_id ->
    match e.exp_desc with
    | Texp_function _ -> Function
    | Texp_lazy _ -> Lazy
    | _ ->
      match block_fields e with
      | Some fields ->
        List.iter (classify_field st ~src:id) fields;
        Construction
      | None -> illegal_reference e.exp_loc ref_id

(* A reference to the group in an item of a preamble that cannot join the
   group. *)
let preamble_item_reference st e =
  let in_module_expr me =
    Option.map (fun id -> me.Typedtree.mod_loc, id)
      (find_module_reference st.ids me)
  in
  match e.exp_desc with
  | Texp_let (_, vbs, _) ->
    List.find_map
      (fun vb ->
         Option.map (fun id -> vb.vb_expr.exp_loc, id)
           (find_value_reference st.ids vb.vb_expr))
      vbs
  | Texp_open (od, _) -> in_module_expr od.open_expr
  | Texp_letmodule (_, _, _, me, _) -> in_module_expr me
  | Texp_sequence (e1, _, _) ->
    Option.map (fun id -> e1.exp_loc, id) (find_value_reference st.ids e1)
  | _ -> None

let binding_ident vb =
  match vb.vb_pat.pat_desc with
  | Tpat_var { id; _ } -> Some id
  | _ -> None

let rec classify_binding st id e : Value_rec_types.recursive_binding_kind =
  let kind : Value_rec_types.recursive_binding_kind =
    match e.exp_desc, find_value_reference st.ids e with
    | Texp_function _, _ -> Recursive { preamble = []; definition = Function }
    | _, None -> Not_recursive
    | _, Some _ ->
      let preamble, definition = classify_rhs st id e in
      Recursive { preamble; definition }
  in
  let definition : Value_rec_types.definition =
    match kind with
    | Not_recursive -> Value
    | Recursive { definition; _ } -> definition
  in
  st.kinds <- (id, definition) :: st.kinds;
  kind

and classify_rhs st id e =
  match split_preamble e with
  | None -> [], classify_definition st id e
  | Some body ->
    let item : Value_rec_types.preamble_item =
      match preamble_item_reference st e, e.exp_desc with
      | None, _ -> Hoisted
      | Some (loc, ref_id), Texp_let (_, vbs, _) ->
        let ids =
          List.map
            (fun vb ->
               match binding_ident vb with
               | Some id -> id
               | None -> illegal_reference loc ref_id)
            vbs
        in
        st.ids <- ids @ st.ids;
        Members (List.map2 (fun id vb -> classify_binding st id vb.vb_expr)
                   ids vbs)
      | Some (loc, ref_id), _ -> illegal_reference loc ref_id
    in
    let items, definition = classify_rhs st id body in
    item :: items, definition

let definition_kind st id =
  List.find_map
    (fun (id', kind) -> if Ident.same id id' then Some kind else None)
    st.kinds

(* Checks that the references between blocks do not form a cycle. *)
let check_cycles st =
  let edges =
    List.filter
      (fun (_, dst, _) ->
         match definition_kind st dst with
         | Some Construction -> true
         | Some (Function | Lazy | Value) | None -> false)
      (List.rev st.edges)
  in
  let finished = ref [] in
  (* [path] is the current path of the search, the most recent node first,
     with the location of the reference to it. *)
  let rec visit path id =
    if not (is_member !finished id) then begin
      List.iter
        (fun (src, dst, loc) ->
           if Ident.same src id then
             match
               List.find_index (fun (id, _) -> Ident.same id dst) path
             with
             | Some i ->
               let cycle =
                 List.rev (List.filteri (fun j _ -> j <= i) path)
               in
               raise (Illegal (Illegal_cycle
                               (loc, List.map fst cycle @ [ dst ])))
             | None -> visit ((dst, loc) :: path) dst)
        edges;
      finished := id :: !finished
    end
  in
  List.iter (fun (id, _) -> visit [ (id, Location.none) ] id) st.kinds

let classify_recursive_bindings bindings =
  let st = { ids = List.map fst bindings; kinds = []; edges = [] } in
  match
    let kinds =
      List.map (fun (id, e) -> classify_binding st id e) bindings
    in
    check_cycles st;
    kinds
  with
  | kinds -> Ok kinds
  | exception Illegal error -> Error error

let check_class_expr ids ce =
  let iterator = class_reference_iterator ids in
  let rec spine ce =
    match ce.cl_desc with
    | Tcl_let (_, bindings, _, ce) ->
      let traverse (it : Tast_iterator.iterator) =
        List.iter (it.value_binding it) bindings
      in
      begin match find_reference iterator traverse with
      | Some id -> Error id
      | None -> spine ce
      end
    | Tcl_constraint (ce, _, _, _, _) | Tcl_open (_, ce) -> spine ce
    | Tcl_ident _ | Tcl_structure _ | Tcl_fun _ | Tcl_apply _ -> Ok ()
  in
  spine ce
