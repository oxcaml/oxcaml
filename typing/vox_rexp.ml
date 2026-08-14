(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Copyright 2026 Jane Street Group LLC                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types

(* Folding over interior types *)

let rec fold_types f acc rexp =
  match rexp.rexp_desc with
  | Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> acc
  | Rexp_apply (fn, args) ->
      List.fold_left
        (fun acc (_, arg) -> fold_types f acc arg)
        (fold_types f acc fn) args
  | Rexp_tuple components ->
      List.fold_left
        (fun acc (_, component) -> fold_types f acc component)
        acc components
  | Rexp_construct (_, _, arg) ->
      Option.fold ~none:acc ~some:(fold_types f acc) arg
  | Rexp_field (e, _) -> fold_types f acc e
  | Rexp_ifthenelse (cond, ifso, ifnot) ->
      let acc = fold_types f acc cond in
      let acc = fold_types f acc ifso in
      Option.fold ~none:acc ~some:(fold_types f acc) ifnot
  | Rexp_let ({ rb_ident = _; rb_expr }, body) ->
      fold_types f (fold_types f acc rb_expr) body
  | Rexp_fun (_, body) -> fold_types f acc body
  | Rexp_match (scrutinee, cases) ->
      List.fold_left (fold_types_case f) (fold_types f acc scrutinee) cases
  | Rexp_constraint (e, ty) -> f (fold_types f acc e) ty

and fold_types_case f acc { rc_lhs = _; rc_guard; rc_rhs } =
  (* Patterns carry no interior types. *)
  let acc = Option.fold ~none:acc ~some:(fold_types f acc) rc_guard in
  fold_types f acc rc_rhs

let iter_types f rexp = fold_types (fun () ty -> f ty) () rexp

(* Rebuilding *)

let map ?(rename = Ident.Map.empty) ?(freshen = false) ?value_path
    ?constructor_path ~type_expr rexp =
  let rename_var rename id =
    match Ident.Map.find_opt id rename with Some id' -> id' | None -> id
  in
  let bind rename id =
    if freshen then
      let id' = Ident.rename id in
      Ident.Map.add id id' rename, id'
    else rename, id
  in
  let rec map_rexp rename rexp =
    let rexp_desc =
      match rexp.rexp_desc with
      | Rexp_hole -> Rexp_hole
      | Rexp_var id -> Rexp_var (rename_var rename id)
      | Rexp_ident (path, lid) ->
          let path =
            match value_path with Some f -> f path | None -> path
          in
          Rexp_ident (path, lid)
      | Rexp_constant _ as desc -> desc
      | Rexp_apply (fn, args) ->
          Rexp_apply
            ( map_rexp rename fn,
              List.map (fun (lbl, arg) -> lbl, map_rexp rename arg) args )
      | Rexp_tuple components ->
          Rexp_tuple
            (List.map (fun (lbl, c) -> lbl, map_rexp rename c) components)
      | Rexp_construct (path, lid, arg) ->
          let path =
            match constructor_path with Some f -> f path | None -> path
          in
          Rexp_construct (path, lid, Option.map (map_rexp rename) arg)
      | Rexp_field (e, lid) -> Rexp_field (map_rexp rename e, lid)
      | Rexp_ifthenelse (cond, ifso, ifnot) ->
          Rexp_ifthenelse
            ( map_rexp rename cond,
              map_rexp rename ifso,
              Option.map (map_rexp rename) ifnot )
      | Rexp_let ({ rb_ident; rb_expr }, body) ->
          let rb_expr = map_rexp rename rb_expr in
          let rename, rb_ident = bind rename rb_ident in
          Rexp_let ({ rb_ident; rb_expr }, map_rexp rename body)
      | Rexp_fun (param, body) ->
          let rename, param = bind rename param in
          Rexp_fun (param, map_rexp rename body)
      | Rexp_match (scrutinee, cases) ->
          Rexp_match (map_rexp rename scrutinee, List.map (map_case rename) cases)
      | Rexp_constraint (e, ty) ->
          Rexp_constraint (map_rexp rename e, type_expr ty)
    in
    { rexp with rexp_desc }
  and map_case rename { rc_lhs; rc_guard; rc_rhs } =
    let rename, rc_lhs = map_pat rename rc_lhs in
    { rc_lhs;
      rc_guard = Option.map (map_rexp rename) rc_guard;
      rc_rhs = map_rexp rename rc_rhs }
  and map_pat rename pat =
    let rename, rpat_desc =
      match pat.rpat_desc with
      | (Rpat_any | Rpat_constant _) as desc -> rename, desc
      | Rpat_var id ->
          let rename, id = bind rename id in
          rename, Rpat_var id
      | Rpat_tuple components ->
          let rename, components =
            List.fold_left_map
              (fun rename (lbl, p) ->
                let rename, p = map_pat rename p in
                rename, (lbl, p))
              rename components
          in
          rename, Rpat_tuple components
      | Rpat_construct (path, lid, arg) ->
          let path =
            match constructor_path with Some f -> f path | None -> path
          in
          let rename, arg =
            match arg with
            | None -> rename, None
            | Some p ->
                let rename, p = map_pat rename p in
                rename, Some p
          in
          rename, Rpat_construct (path, lid, arg)
      | Rpat_alias (p, id) ->
          let rename, p = map_pat rename p in
          let rename, id = bind rename id in
          rename, Rpat_alias (p, id)
    in
    rename, { pat with rpat_desc }
  in
  map_rexp rename rexp

(* Alpha-equivalence *)

(* [Pconst_string] carries the location of the string contents inside the
   description; it is not part of the syntax and must not be part of type
   identity. *)
let constant_equal (c1 : Parsetree.constant) (c2 : Parsetree.constant) =
  match c1.pconst_desc, c2.pconst_desc with
  | Pconst_string (s1, _, d1), Pconst_string (s2, _, d2) ->
      String.equal s1 s2 && Option.equal String.equal d1 d2
  | desc1, desc2 -> desc1 = desc2

let equal ~type_eq ~pairs rexp1 rexp2 =
  (* [pairs] pairs the binders of the left predicate with the binders of
     the right one, innermost first. *)
  let var_eq pairs id1 id2 =
    let rec find = function
      | [] -> Ident.same id1 id2
      | (l, r) :: rest ->
          if Ident.same id1 l then Ident.same id2 r
          else if Ident.same id2 r then false
          else find rest
    in
    find pairs
  in
  let rec eq pairs rexp1 rexp2 =
    match rexp1.rexp_desc, rexp2.rexp_desc with
    | Rexp_hole, Rexp_hole -> true
    | Rexp_var id1, Rexp_var id2 -> var_eq pairs id1 id2
    | Rexp_ident (p1, _), Rexp_ident (p2, _) -> Path.same p1 p2
    | Rexp_constant c1, Rexp_constant c2 -> constant_equal c1 c2
    | Rexp_apply (f1, args1), Rexp_apply (f2, args2) ->
        eq pairs f1 f2
        && List.compare_lengths args1 args2 = 0
        && List.for_all2
             (fun (l1, a1) (l2, a2) -> l1 = l2 && eq pairs a1 a2)
             args1 args2
    | Rexp_tuple c1, Rexp_tuple c2 ->
        List.compare_lengths c1 c2 = 0
        && List.for_all2
             (fun (l1, e1) (l2, e2) -> l1 = l2 && eq pairs e1 e2)
             c1 c2
    | Rexp_construct (p1, _, arg1), Rexp_construct (p2, _, arg2) ->
        Path.same p1 p2
        && Option.equal (eq pairs) arg1 arg2
    | Rexp_field (e1, lid1), Rexp_field (e2, lid2) ->
        lid1.txt = lid2.txt && eq pairs e1 e2
    | Rexp_ifthenelse (c1, t1, e1), Rexp_ifthenelse (c2, t2, e2) ->
        eq pairs c1 c2 && eq pairs t1 t2 && Option.equal (eq pairs) e1 e2
    | Rexp_let (b1, body1), Rexp_let (b2, body2) ->
        eq pairs b1.rb_expr b2.rb_expr
        && eq ((b1.rb_ident, b2.rb_ident) :: pairs) body1 body2
    | Rexp_fun (p1, body1), Rexp_fun (p2, body2) ->
        eq ((p1, p2) :: pairs) body1 body2
    | Rexp_match (s1, cases1), Rexp_match (s2, cases2) ->
        eq pairs s1 s2
        && List.compare_lengths cases1 cases2 = 0
        && List.for_all2 (eq_case pairs) cases1 cases2
    | Rexp_constraint (e1, ty1), Rexp_constraint (e2, ty2) ->
        eq pairs e1 e2 && type_eq ty1 ty2
    | ( ( Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _
        | Rexp_apply _ | Rexp_tuple _ | Rexp_construct _ | Rexp_field _
        | Rexp_ifthenelse _ | Rexp_let _ | Rexp_fun _ | Rexp_match _
        | Rexp_constraint _ ), _ ) ->
        false
  and eq_case pairs case1 case2 =
    match eq_pat pairs case1.rc_lhs case2.rc_lhs with
    | None -> false
    | Some pairs ->
        Option.equal (eq pairs) case1.rc_guard case2.rc_guard
        && eq pairs case1.rc_rhs case2.rc_rhs
  and eq_pat pairs pat1 pat2 =
    match pat1.rpat_desc, pat2.rpat_desc with
    | Rpat_any, Rpat_any -> Some pairs
    | Rpat_var id1, Rpat_var id2 -> Some ((id1, id2) :: pairs)
    | Rpat_constant c1, Rpat_constant c2 ->
        if constant_equal c1 c2 then Some pairs else None
    | Rpat_tuple c1, Rpat_tuple c2 ->
        if List.compare_lengths c1 c2 = 0 then
          List.fold_left2
            (fun pairs (l1, p1) (l2, p2) ->
              Option.bind pairs (fun pairs ->
                  if l1 = l2 then eq_pat pairs p1 p2 else None))
            (Some pairs) c1 c2
        else None
    | Rpat_construct (c1, _, arg1), Rpat_construct (c2, _, arg2) ->
        if Path.same c1 c2 then
          match arg1, arg2 with
          | None, None -> Some pairs
          | Some p1, Some p2 -> eq_pat pairs p1 p2
          | None, Some _ | Some _, None -> None
        else None
    | Rpat_alias (p1, id1), Rpat_alias (p2, id2) ->
        Option.map
          (fun pairs -> (id1, id2) :: pairs)
          (eq_pat pairs p1 p2)
    | ( ( Rpat_any | Rpat_var _ | Rpat_constant _ | Rpat_tuple _
        | Rpat_construct _ | Rpat_alias _ ), _ ) ->
        None
  in
  eq pairs rexp1 rexp2

(* Back to surface syntax *)

let untype ~var_name ~value_ident ~constructor_ident ~core_type rexp =
  let open Ast_helper in
  let lid_of_name name = Location.mknoloc (Longident.Lident name) in
  let rec untype_rexp rexp =
    let loc = rexp.rexp_loc in
    match rexp.rexp_desc with
    | Rexp_hole -> Exp.mk ~loc Pexp_hole
    | Rexp_var id -> Exp.ident ~loc (lid_of_name (var_name id))
    | Rexp_ident (path, _) ->
        (* Render from the resolved path: the source longident may not
           resolve at the printing site, and substitution rewrites only the
           path. *)
        Exp.ident ~loc (value_ident path)
    | Rexp_constant const -> Exp.constant ~loc const
    | Rexp_apply (fn, args) ->
        Exp.apply ~loc (untype_rexp fn)
          (List.map (fun (lbl, arg) -> lbl, untype_rexp arg) args)
    | Rexp_tuple components ->
        Exp.tuple ~loc
          (List.map (fun (lbl, c) -> lbl, untype_rexp c) components)
    | Rexp_construct (path, _, arg) ->
        Exp.construct ~loc (constructor_ident path)
          (Option.map untype_rexp arg)
    | Rexp_field (e, lid) -> Exp.field ~loc (untype_rexp e) lid
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        Exp.ifthenelse ~loc (untype_rexp cond) (untype_rexp ifso)
          (Option.map untype_rexp ifnot)
    | Rexp_let ({ rb_ident; rb_expr }, body) ->
        Exp.let_ ~loc Immutable Nonrecursive
          [ Vb.mk
              (Pat.var (Location.mknoloc (var_name rb_ident)))
              (untype_rexp rb_expr) ]
          (untype_rexp body)
    | Rexp_fun (param, body) ->
        Exp.function_ ~loc
          [ { pparam_desc =
                Pparam_val
                  ( Asttypes.Nolabel, None,
                    Pat.var (Location.mknoloc (var_name param)) );
              pparam_loc = Location.none } ]
          { mode_annotations = [];
            ret_mode_annotations = [];
            ret_type_constraint = None }
          (Pfunction_body (untype_rexp body))
    | Rexp_match (scrutinee, cases) ->
        Exp.match_ ~loc (untype_rexp scrutinee) (List.map untype_case cases)
    | Rexp_constraint (e, ty) ->
        Exp.constraint_ ~loc (untype_rexp e) (Some (core_type ty)) []
  and untype_case { rc_lhs; rc_guard; rc_rhs } =
    Exp.case (untype_pat rc_lhs)
      ?guard:(Option.map untype_rexp rc_guard)
      (untype_rexp rc_rhs)
  and untype_pat pat =
    let loc = pat.rpat_loc in
    match pat.rpat_desc with
    | Rpat_any -> Pat.any ~loc ()
    | Rpat_var id -> Pat.var ~loc (Location.mknoloc (var_name id))
    | Rpat_constant const -> Pat.constant ~loc const
    | Rpat_tuple components ->
        Pat.tuple ~loc
          (List.map (fun (lbl, p) -> lbl, untype_pat p) components)
          Asttypes.Closed
    | Rpat_construct (path, _, arg) ->
        Pat.construct ~loc (constructor_ident path)
          (Option.map (fun p -> [], untype_pat p) arg)
    | Rpat_alias (p, id) ->
        Pat.alias ~loc (untype_pat p) (Location.mknoloc (var_name id))
  in
  untype_rexp rexp

(* Occurrence checks used by the printer *)

let exists_rexp pred rexp =
  let exception Found in
  let rec walk rexp =
    if pred rexp then raise Found;
    match rexp.rexp_desc with
    | Rexp_hole | Rexp_var _ | Rexp_ident _ | Rexp_constant _ -> ()
    | Rexp_apply (fn, args) ->
        walk fn;
        List.iter (fun (_, arg) -> walk arg) args
    | Rexp_tuple components -> List.iter (fun (_, c) -> walk c) components
    | Rexp_construct (_, _, arg) -> Option.iter walk arg
    | Rexp_field (e, _) -> walk e
    | Rexp_ifthenelse (cond, ifso, ifnot) ->
        walk cond; walk ifso; Option.iter walk ifnot
    | Rexp_let ({ rb_expr; _ }, body) -> walk rb_expr; walk body
    | Rexp_fun (_, body) -> walk body
    | Rexp_match (scrutinee, cases) ->
        walk scrutinee;
        List.iter
          (fun { rc_guard; rc_rhs; _ } ->
            Option.iter walk rc_guard;
            walk rc_rhs)
          cases
    | Rexp_constraint (e, _) -> walk e
  in
  match walk rexp with () -> false | exception Found -> true

let find_value_path (f : Path.t -> 'a option) rexp : 'a option =
  let result = ref None in
  let check path =
    match f path with
    | Some _ as found ->
        result := found;
        true
    | None -> false
  in
  ignore
    (exists_rexp
       (fun r ->
         match r.rexp_desc with
         | Rexp_ident (path, _) | Rexp_construct (path, _, _) -> check path
         | Rexp_match (_, cases) ->
             let rec pat_path p =
               match p.rpat_desc with
               | Rpat_construct (path, _, arg) ->
                   check path
                   || Option.fold ~none:false ~some:pat_path arg
               | Rpat_alias (p, _) -> pat_path p
               | Rpat_tuple ps -> List.exists (fun (_, p) -> pat_path p) ps
               | Rpat_any | Rpat_var _ | Rpat_constant _ -> false
             in
             List.exists (fun c -> pat_path c.rc_lhs) cases
         | _ -> false)
       rexp
     : bool);
  !result

let mentions_ident id rexp =
  exists_rexp
    (fun r ->
      match r.rexp_desc with
      | Rexp_var id' -> Ident.same id id'
      | _ -> false)
    rexp
