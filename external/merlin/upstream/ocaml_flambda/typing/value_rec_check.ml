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

    The names bound by a [let rec] may only be used by the functions it
    binds, or in the mutable fields of the records it binds. In a group
    {[
      let rec x1 = e1 and ... and xn = en
    ]}
    each right-hand side [ei] is thus one of:
    - a syntactic function ([fun] or [function]), whose body may refer
      freely to [x1 ... xn] because it is only evaluated when the function
      is called;
    - a record literal, possibly the inline record of a constructor, whose
      fields referring to [x1 ... xn] are mutable constructions (see
      [construction_iterator]). Such a record is allocated with placeholders
      in these fields, which are assigned once the whole group is defined;
    - a reference cell [ref e] (or another application of the
      [%makemutable] primitive), where [e] is a construction if it refers to
      [x1 ... xn]. Like a record, it is allocated with a placeholder;
    - a [lazy] expression, whose body may refer freely to [x1 ... xn]
      because it is only evaluated when forced. The lazy block is allocated
      with a placeholder, and its thunk is assigned once the whole group is
      defined;
    - an expression that does not mention [x1 ... xn] at all, which is an
      ordinary definition that merely shares the [let rec] with the others.

    Hence every cycle in the data created by a [let rec] goes through a
    mutable field or a lazy block: no cyclic data is created without
    mutation. Definitions
    such as [let rec xs = 0 :: xs] or [let rec r = { f = fun () -> r }] are
    rejected. See [Translcore.transl_let] for the compilation scheme.

    Recursive class definitions follow the same principle: the let-bindings
    in the spine of a class expression are evaluated when the class is
    defined, so they may not refer to the classes of the group. Method
    bodies and the other parts of the class body are only evaluated later
    and are unrestricted. *)

open Typedtree

exception Found of Ident.t

(* Identifiers in the typed tree are unique, so a plain traversal suffices
   to find references: there is no shadowing to account for. *)
let check_path ids path =
  match path with
  | Path.Pident id when List.exists (Ident.same id) ids -> raise (Found id)
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

(* Constructions only allocate, so they can be evaluated once the group is
   defined without reading through it: variables, constants, functions,
   lazy expressions, and constructors, tuples, arrays and records of
   constructions. *)
exception Not_a_construction

let construction_iterator =
  let open Tast_iterator in
  let expr sub e =
    match e.exp_desc with
    | Texp_function _ | Texp_lazy _ -> ()
    | Texp_ident _ | Texp_constant _ | Texp_apply_layout _ | Texp_construct _
    | Texp_tuple _ | Texp_variant _ | Texp_array _
    | Texp_record { extended_expression = None; _ } ->
      default_iterator.expr sub e
    | _ -> raise Not_a_construction
  in
  { default_iterator with expr }

let is_construction e =
  match construction_iterator.expr construction_iterator e with
  | () -> true
  | exception Not_a_construction -> false

(* The fields of a record literal, looking through the constructor of an
   inline record. *)
let record_fields e =
  let fields e =
    match e.exp_desc with
    | Texp_record { fields; extended_expression = None; _ } -> Some fields
    | _ -> None
  in
  match e.exp_desc with
  | Texp_construct (_, cstr, _, [ (_, arg) ], _)
    when Option.is_some cstr.cstr_inlined -> fields arg
  | _ -> fields e

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

(* Contents that refer to the group, in a record field or a reference cell,
   must be a mutable construction; they are assigned once the group is
   defined. Returns whether they need to be assigned. *)
let classify_contents ids ~mutable_ e =
  match find_value_reference ids e with
  | None -> Ok false
  | Some id ->
    if mutable_ && is_construction e then Ok true else Error (e.exp_loc, id)

let classify_field ids
      ((lbl : Data_types.label_description), _, definition) patched =
  match patched, definition with
  | Error _, _ | _, Kept _ -> patched
  | Ok patched, Overridden (_, e) ->
    let mutable_ = Types.is_mutable lbl.lbl_mut in
    match classify_contents ids ~mutable_ e with
    | Ok false -> Ok patched
    | Ok true ->
      Env.mark_label_used Mutation lbl.lbl_uid;
      Ok (lbl.lbl_pos :: patched)
    | Error _ as error -> error

let classify_recursive_binding ids expr =
  match expr.exp_desc, record_fields expr with
  | Texp_function _, _ -> Ok Value_rec_types.Recursive_function
  | Texp_lazy e, _ ->
    begin match find_value_reference ids e with
    | None -> Ok Value_rec_types.Not_recursive
    | Some _ -> Ok Value_rec_types.Recursive_lazy
    end
  | _, Some fields ->
    begin match Array.fold_right (classify_field ids) fields (Ok []) with
    | Ok [] -> Ok Value_rec_types.Not_recursive
    | Ok patched -> Ok (Value_rec_types.Recursive_record patched)
    | Error _ as error -> error
    end
  | _, None ->
    match ref_contents expr with
    | Some contents ->
      Result.map
        (fun patched ->
           if patched then Value_rec_types.Recursive_ref
           else Value_rec_types.Not_recursive)
        (classify_contents ids ~mutable_:true contents)
    | None ->
      match find_value_reference ids expr with
      | None -> Ok Value_rec_types.Not_recursive
      | Some id -> Error (expr.exp_loc, id)

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
