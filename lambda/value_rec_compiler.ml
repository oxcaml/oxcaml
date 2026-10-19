(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Compilation of recursive definitions

    [Lletrec] only binds syntactic functions, whereas a [let rec] can also
    bind lazy values and blocks, as in
    {[
      let rec f = fun x -> ... g ... r ...
      and g = fun y -> ... f ...
      and l = lazy (... f ...)
      and r = { name = "r"; callback = f }
      and n = 42
    ]}
    [Value_rec_check] only accepts groups whose members are not read before
    the whole group is defined: the group is only referred to from the bodies
    of functions and lazy values, and from the fields of blocks, where it is
    stored and not read. It also guarantees that the fields of blocks that
    refer to the group only allocate (blocks, functions and lazy values), and
    that every cycle in the data goes through a function, a lazy value or a
    mutable field.

    [Translcore.transl_let] translates the members of the group and the
    preambles of their definitions (such as [let x = e in] or [e;]) to a list
    of items, in evaluation order. The group is then compiled in three steps:

    + Allocation. The items other than functions are evaluated in order.
      Values and preamble items are evaluated as usual. Each block is
      allocated with a placeholder in every field that refers to the group,
      and each lazy value with a placeholder instead of its thunk.
    + Definition of the functions, in a single [Lletrec]. Every member of the
      group is bound at this point.
    + Patching. The placeholders are replaced with the contents of these
      fields and with the thunks, which can now be evaluated.

    For the example above, this gives
    {[
      let n = 42 in
      let l = Pmakelazyblock () in
      let r = Pmakeblock Mutable ("r", ()) in
      letrec f = fun x -> ... g ... r ...
      and g = fun y -> ... f ... in
      l.0 <- (fun () -> ... f ...);
      r.1 <- f;
      body
    ]}

    Blocks are allocated mutable, even if their fields are immutable, so that
    the optimisers do not assume that the placeholders are the final contents
    of the fields.

    Recursive classes are compiled in a similar way. The classes that may
    refer to the group are pre-allocated with placeholders. Their definitions
    are then evaluated in order, and the fields of the resulting class blocks
    are copied into the pre-allocated blocks. *)

open Lambda

let modify_mode alloc_mode =
  if is_heap_mode alloc_mode then modify_heap else modify_maybe_stack

let boxed_float_zero = Lconst (Const_base (Const_float "0."))

(* Splits the allocation [def] of the block [id] into its allocation with
   placeholders in the fields that refer to the group, and the assignments
   of these fields. *)
let rec split_block ~refers_to_group id def =
  let split ~alloc ~placeholder ~set_field args loc =
    let args = List.mapi (fun i arg -> i, arg, refers_to_group arg) args in
    let alloc_args =
      List.map
        (fun (i, arg, patched) -> if patched then placeholder i else arg)
        args
    in
    let patches =
      List.filter_map
        (fun (i, arg, patched) ->
           if patched then Some (Lprim (set_field i, [Lvar id; arg], loc))
           else None)
        args
    in
    Lprim (alloc, alloc_args, loc), patches
  in
  match def with
  | Levent (def, ev) ->
    let alloc, patches = split_block ~refers_to_group id def in
    Levent (alloc, ev), patches
  | Lprim (Pmakeblock (tag, _, All_value, mode), args, loc) ->
    split ~alloc:(Pmakeblock (tag, Mutable, All_value, mode))
      ~placeholder:(fun _ -> lambda_unit)
      ~set_field:(fun i ->
        Psetfield (i, Pointer, Assignment (modify_mode mode)))
      args loc
  | Lprim (Pmakeblock (tag, _, Shape shape, mode), args, loc) ->
    (* Only value fields can refer to the group. The placeholders of the
       fields of other kinds of values are not of these kinds. *)
    let shape =
      Array.of_list
        (List.map2
           (fun (element : _ mixed_block_element) arg ->
              match element with
              | Value _ when refers_to_group arg -> Value generic_value
              | element -> element)
           (Array.to_list shape) args)
    in
    let all_values =
      Array.for_all (function Value _ -> true | _ -> false) shape
    in
    split ~alloc:(Pmakeblock (tag, Mutable, Shape shape, mode))
      ~placeholder:(fun i ->
        match shape.(i) with
        | Value _ -> lambda_unit
        | Float_boxed () -> boxed_float_zero
        | _ ->
          Misc.fatal_errorf
            "Value_rec_compiler: field %d of %a is not a value" i
            Ident.print id)
      ~set_field:(fun i ->
        if all_values then Psetfield (i, Pointer, Assignment (modify_mode mode))
        else Psetmixedfield ([i], shape, Assignment (modify_mode mode)))
      args loc
  | Lprim (Pmakefloatblock (_, mode), args, loc) ->
    split ~alloc:(Pmakefloatblock (Mutable, mode))
      ~placeholder:(fun _ -> boxed_float_zero)
      ~set_field:(fun i -> Psetfloatfield (i, Assignment (modify_mode mode)))
      args loc
  | _ ->
    Misc.fatal_errorf "Value_rec_compiler: the definition of %a is not a \
                       block:@ %a"
      Ident.print id Printlambda.lambda def

type item =
  | Preamble of (lambda -> lambda)
  | Value of
      { id : Ident.t; debug_uid : debug_uid; layout : layout; def : lambda }
  | Function of { wrap : lambda -> lambda; binding : rec_binding }
  | Lazy of
      { id : Ident.t; debug_uid : debug_uid; thunk : lambda;
        loc : Debuginfo.Scoped_location.t }
  | Block of
      { id : Ident.t; debug_uid : debug_uid; layout : layout; def : lambda }

let compile_letrec items body =
  let group =
    List.filter_map
      (function
        | Preamble _ -> None
        | Value { id; _ } | Lazy { id; _ } | Block { id; _ }
        | Function { binding = { id; _ }; _ } -> Some id)
      items
    |> Ident.Set.of_list
  in
  let refers_to_group lam =
    not (Ident.Set.disjoint (free_variables lam) group)
  in
  (* The allocation of each item, which binds it around the rest of the code
     (step 1), and the assignments that patch it (step 3). *)
  let allocate_and_patch item : (lambda -> lambda) * lambda list =
    match item with
    | Preamble wrap -> wrap, []
    | Value { id; debug_uid; layout; def } ->
      (fun body -> Llet (Strict, layout, id, debug_uid, def, body)), []
    | Function _ -> Fun.id, []
    | Lazy { id; debug_uid; thunk; loc } ->
      let alloc = Lprim (Pmakelazyblock Lazy_tag, [lambda_unit], loc) in
      (fun body -> Llet (Strict, layout_lazy, id, debug_uid, alloc, body)),
      [Lprim (Psetfield (0, Pointer, Assignment modify_heap),
              [Lvar id; thunk], loc)]
    | Block { id; debug_uid; layout; def } ->
      let alloc, patches = split_block ~refers_to_group id def in
      (fun body -> Llet (Strict, layout, id, debug_uid, alloc, body)), patches
  in
  let allocations, patches = List.split (List.map allocate_and_patch items) in
  let functions =
    List.filter_map
      (function
        | Function { wrap; binding } -> Some (wrap, binding)
        | Preamble _ | Value _ | Lazy _ | Block _ -> None)
      items
  in
  let body =
    List.fold_right (fun patch body -> Lsequence (patch, body))
      (List.concat patches) body
  in
  (* Step 2: the functions are defined between the allocations and the
     patches. *)
  let body =
    match functions with
    | [] -> body
    | _ :: _ ->
      List.fold_left (fun body (wrap, _) -> wrap body)
        (Lletrec (List.map snd functions, body))
        functions
  in
  List.fold_right (fun allocate body -> allocate body) allocations body

type class_kind =
  | Static
  | Dynamic

let class_block_size = 3

let compile_class_bindings bindings body =
  let static, dynamic =
    List.partition
      (fun (_, _, kind, _) ->
         match kind with Static -> true | Dynamic -> false)
      bindings
  in
  let define (id, debug_uid, _, def) body =
    Llet (Strict, layout_block, id, debug_uid, def, body)
  in
  let allocate (id, debug_uid, _, _) body =
    let placeholder =
      Lprim (Pmakeblock (0, Mutable, All_value, alloc_heap),
             List.init class_block_size (fun _ -> lambda_unit), Loc_unknown)
    in
    Llet (Strict, layout_block, id, debug_uid, placeholder, body)
  in
  let copy (id, _, _, def) body =
    let tmp = Ident.create_local (Ident.name id) in
    let copy_field i body =
      Lsequence
        (Lprim (Psetfield (i, Pointer, Assignment modify_heap),
                [Lvar id;
                 Lprim (Pfield (i, Pointer, Reads_vary), [Lvar tmp],
                        Loc_unknown)],
                Loc_unknown),
         body)
    in
    Llet (Strict, layout_block, tmp, debug_uid_none, def,
          List.fold_right copy_field (List.init class_block_size Fun.id) body)
  in
  List.fold_right define dynamic
    (List.fold_right allocate static (List.fold_right copy static body))
