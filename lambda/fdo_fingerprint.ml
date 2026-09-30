open Lambda
module H = Fdo_prehash

(* Every node is hashed from a tag of its own (starting at 1, since a leading 0
   would not contribute), its fields and its children, so that distinct trees
   get distinct hashes. Lists include their length. *)
let node tag fields = List.fold_left H.combine (H.int tag) fields

let list f xs =
  List.fold_left (fun acc x -> H.combine acc (f x)) (H.int (List.length xs)) xs

let option f = function None -> H.int 0 | Some x -> node 1 [f x]

let int64 n =
  H.combine
    (H.int (Int64.to_int (Int64.shift_right_logical n 32)))
    (H.int (Int64.to_int (Int64.logand n 0xffffffffL)))

(* Identifiers by name: their stamps depend on everything compiled before. *)
let ident id = H.string (Ident.name id)

let constant (c : constant) =
  match c with
  | Const_int n -> node 1 [H.int n]
  | Const_char c -> node 2 [H.int (Char.code c)]
  | Const_untagged_char n -> node 3 [H.int n]
  | Const_string (s, _, _) -> node 4 [H.string s]
  | Const_float s -> node 5 [H.string s]
  | Const_float32 s -> node 6 [H.string s]
  | Const_unboxed_float s -> node 7 [H.string s]
  | Const_unboxed_float32 s -> node 8 [H.string s]
  | Const_int8 n -> node 9 [H.int n]
  | Const_int16 n -> node 10 [H.int n]
  | Const_int32 n -> node 11 [H.int (Int32.to_int n)]
  | Const_int64 n -> node 12 [int64 n]
  | Const_nativeint n -> node 13 [int64 (Int64.of_nativeint n)]
  | Const_untagged_int n -> node 14 [H.int n]
  | Const_untagged_int8 n -> node 15 [H.int n]
  | Const_untagged_int16 n -> node 16 [H.int n]
  | Const_unboxed_int32 n -> node 17 [H.int (Int32.to_int n)]
  | Const_unboxed_int64 n -> node 18 [int64 n]
  | Const_unboxed_nativeint n -> node 19 [int64 (Int64.of_nativeint n)]

let rec structured_constant = function
  | Const_base c -> node 1 [constant c]
  | Const_block (tag, fields) ->
    node 2 [H.int tag; list structured_constant fields]
  | Const_mixed_block (tag, _, fields) ->
    node 3 [H.int tag; list structured_constant fields]
  | Const_float_array fields -> node 4 [list H.string fields]
  | Const_immstring s -> node 5 [H.string s]
  | Const_float_block fields -> node 6 [list H.string fields]
  | Const_null -> node 7 []

(* A primitive by its name and the payload that selects the operation: field
   index, block tag, C function or global. Other payloads (shapes, modes, kinds)
   are left out. *)
let primitive (prim : primitive) =
  let payload =
    match prim with
    | Pfield (n, _, _)
    | Psetfield (n, _, _)
    | Pmakeblock (n, _, _, _)
    | Pfloatfield (n, _, _)
    | Psetfloatfield (n, _)
    | Pufloatfield (n, _)
    | Psetufloatfield (n, _) ->
      [H.int n]
    | Pmixedfield (path, _, _) | Psetmixedfield (path, _, _) -> [list H.int path]
    | Pccall { prim_name; _ } -> [H.string prim_name]
    | Pgetglobal (cu, _) -> [H.string (Compilation_unit.full_path_as_string cu)]
    | Pgetpredef id -> [ident id]
    | _ -> []
  in
  List.fold_left H.combine
    (H.string (Printlambda.name_of_primitive prim))
    payload

(* Source positions, identifier stamps, static handler numbers, layouts, modes
   and attributes vary between compilations of the same code, or do not affect
   the translation order that counters are numbered in, and are left out.
   Debugger events are transparent. *)
let rec term lam =
  match lam with
  | Lvar id -> node 1 [ident id]
  | Lmutvar id -> node 2 [ident id]
  | Lconst c -> node 3 [structured_constant c]
  | Lapply { ap_func; ap_args; _ } -> node 4 [term ap_func; list term ap_args]
  | Lfunction fn -> node 5 [lfunction fn]
  | Llet (_, _, id, _, arg, body) -> node 6 [ident id; term arg; term body]
  | Lmutlet (_, id, _, arg, body) -> node 7 [ident id; term arg; term body]
  | Lletrec (bindings, body) ->
    node 8
      [ list
          (fun (b : rec_binding) -> H.combine (ident b.id) (lfunction b.def))
          bindings;
        term body ]
  | Lprim (prim, args, _) -> node 9 [primitive prim; list term args]
  | Lswitch (arg, sw, _, _) ->
    let cases = list (fun (key, case) -> H.combine (H.int key) (term case)) in
    node 10
      [ term arg;
        H.int sw.sw_numconsts;
        cases sw.sw_consts;
        H.int sw.sw_numblocks;
        cases sw.sw_blocks;
        option term sw.sw_failaction ]
  | Lstringswitch (arg, cases, default, _, _) ->
    node 11
      [ term arg;
        list (fun (key, case) -> H.combine (H.string key) (term case)) cases;
        option term default ]
  | Lstaticraise (_, args) -> node 12 [list term args]
  | Lstaticcatch (body, (_, params), handler, _, _) ->
    node 13 [term body; list (fun (id, _, _) -> ident id) params; term handler]
  | Ltrywith (body, id, _, handler, _) ->
    node 14 [term body; ident id; term handler]
  | Lifthenelse (cond, ifso, ifnot, _) ->
    node 15 [term cond; term ifso; term ifnot]
  | Lsequence (first, second) -> node 16 [term first; term second]
  | Lwhile { wh_cond; wh_body } -> node 17 [term wh_cond; term wh_body]
  | Lfor { for_id; for_from; for_to; for_dir; for_body; _ } ->
    node 18
      [ ident for_id;
        term for_from;
        term for_to;
        H.int (match for_dir with Upto -> 0 | Downto -> 1);
        term for_body ]
  | Lassign (id, e) -> node 19 [ident id; term e]
  | Lsend (kind, met, obj, args, _, _, _, _, _) ->
    node 20
      [ H.int (match kind with Self -> 0 | Public -> 1 | Cached -> 2);
        term met;
        term obj;
        list term args ]
  | Levent (e, _) -> term e
  | Lifused (id, e) -> node 21 [ident id; term e]
  | Lregion (e, _) -> node 22 [term e]
  | Lexclave e -> node 23 [term e]
  | Lsplice _ -> node 24 []
  | Lkindtemplate { ktmpl_body; _ } -> node 25 [lfunction ktmpl_body]
  | Lkindinstantiate { kinst_func; _ } -> node 26 [term kinst_func]
  | Ltemplate { tmpl_func; _ } -> node 27 [lfunction tmpl_func]
  | Linstantiate { ap_func; ap_args; _ } ->
    node 28 [term ap_func; list term ap_args]

and lfunction ({ params; body; _ } : lfunction) =
  H.combine (list (fun (p : lparam) -> ident p.name) params) (term body)

let of_function ~params body =
  Fdo_counter.Function_body_hash.of_int32
    (H.to_int32 (H.combine (list ident params) (term body)))

(* A compact spelling of a primitive: its tag or offset where it has one, the
   C function's name for a call, else the first word of its usual printing. *)
let primitive_token (prim : primitive) =
  match prim with
  | Pfield (n, _, _) -> "field" ^ string_of_int n
  | Psetfield (n, _, _) -> "setfield" ^ string_of_int n
  | Pmakeblock (tag, _, _, _) -> "block" ^ string_of_int tag
  | Pccall p -> p.prim_name
  | Pgetglobal (cu, _) -> Compilation_unit.name_as_string cu
  | Pgetpredef id -> Ident.name id
  | _ -> (
    let printed = Format.asprintf "%a" Printlambda.primitive prim in
    match String.index_opt printed ' ' with
    | Some i -> String.sub printed 0 i
    | None -> printed)

let leading_tokens ~params body =
  let limit = 5 in
  let tokens = ref [] in
  let exception Enough in
  let mention token =
    if not (List.mem token !tokens)
    then (
      tokens := token :: !tokens;
      if List.length !tokens >= limit then raise Enough)
  in
  let ident id = mention (Ident.name id) in
  let rec term lam =
    match lam with
    | Lprim (Pfield (n, _, _), [Lprim (Pgetglobal (cu, _), [], _)], _) ->
      (* A value of another compilation unit. *)
      mention (Compilation_unit.name_as_string cu ^ "." ^ string_of_int n)
    | _ ->
      (match lam with
      | Lvar id
      | Lmutvar id
      | Llet (_, _, id, _, _, _)
      | Lmutlet (_, id, _, _, _)
      | Ltrywith (_, id, _, _, _)
      | Lassign (id, _)
      | Lifused (id, _) ->
        ident id
      | Lfor { for_id; _ } -> ident for_id
      | Lfunction { params; _ } ->
        List.iter (fun (p : lparam) -> ident p.name) params
      | Lletrec (bindings, _) ->
        List.iter (fun (b : rec_binding) -> ident b.id) bindings
      | Lstaticcatch (_, (_, params), _, _, _) ->
        List.iter (fun (id, _, _) -> ident id) params
      | Lprim (prim, _, _) -> mention (primitive_token prim)
      | Lconst (Const_base (Const_int n)) -> mention (string_of_int n)
      | Lconst
          ( Const_base _ | Const_block _ | Const_mixed_block _
          | Const_float_array _ | Const_immstring _ | Const_float_block _
          | Const_null )
      | Lapply _ | Lswitch _ | Lstringswitch _ | Lstaticraise _ | Lifthenelse _
      | Lsequence _ | Lwhile _ | Lsend _ | Levent _ | Lregion _ | Lexclave _
      | Lsplice _ | Lkindtemplate _ | Lkindinstantiate _ | Ltemplate _
      | Linstantiate _ ->
        ());
      shallow_iter ~tail:term ~non_tail:term lam
  in
  (try
     List.iter ident params;
     term body
   with Enough -> ());
  List.rev !tokens
