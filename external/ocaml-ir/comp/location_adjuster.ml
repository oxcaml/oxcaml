open! Ocaml_ir_common.Std
module Location = Ocaml_ir_common.Location

type t = (int * int, Warnings.loc) Hashtbl.t

let create () = Hashtbl.create 100
let key (loc : Warnings.loc) = loc.loc_start.pos_cnum, loc.loc_end.pos_cnum

let record ~from ~to_ t =
  match to_.Warnings.loc_start.pos_fname with
  | "_none_" -> ()
  | _ -> Hashtbl.add t (key from) to_
;;

let map' ~(from : Location.Simple.t) t =
  Hashtbl.find_opt t (from.loc_start.pos_cnum, from.loc_end.pos_cnum)
  |> Option.map ~f:(fun loc -> Location.Simple.of_warnings_loc loc)
  |> Option.value ~default:from
;;

let map ~from t = Hashtbl.find_opt t (key from) |> Option.value ~default:from

let take_longest_keyword ~file_content ~from =
  let len = String.length file_content in
  let rec aux i =
    if i >= 0 && i < len
    then (
      match file_content.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '`' | '_' | '.' | '%' -> aux (i + 1)
      | _ -> i)
    else i
  in
  let to_ = aux from in
  String.sub file_content ~pos:from ~len:(to_ - from), to_
;;

let advance_loc_by_one (loc : Warnings.loc) =
  let aux (l : Lexing.position) = { l with pos_cnum = l.pos_cnum + 1 } in
  let open Warnings in
  { loc with loc_end = aux loc.loc_end; loc_start = aux loc.loc_start }
;;

let rec ensure_captures_no_parenthesis ~file_content (loc : Warnings.loc) =
  let p = loc.loc_start.pos_cnum in
  if p >= 0 && p < String.length file_content
  then (
    match file_content.[p] with
    | '(' -> ensure_captures_no_parenthesis ~file_content (advance_loc_by_one loc)
    | _ -> loc)
  else loc
;;

let shrink_for_datatypes ~file_content (parse_tree : Parsetree.structure option) t =
  let open Parsetree in
  let iterator =
    { Ast_iterator.default_iterator with
      expr =
        (fun this e ->
          (match e.pexp_desc with
           | Pexp_construct (ident, _) ->
             let rec length acc = function
               | Longident.Lapply (a, b) -> length (length (acc + 2) a) b
               | Longident.Ldot (a, s) -> length (acc + String.length s + 1) a
               | Longident.Lident s -> acc + String.length s
             in
             let to_ =
               { e.pexp_loc with
                 loc_end =
                   { e.pexp_loc.loc_start with
                     pos_cnum =
                       e.pexp_loc.loc_start.pos_cnum + length 0 ident.Asttypes.txt
                   }
               }
               |> ensure_captures_no_parenthesis ~file_content
             in
             record ~from:e.pexp_loc ~to_ t
           | Pexp_record _ | Pexp_tuple _ ->
             let to_ =
               { e.pexp_loc with
                 loc_start =
                   { e.pexp_loc.loc_start with
                     pos_cnum = e.pexp_loc.loc_start.pos_cnum - 1
                   }
               ; loc_end =
                   { e.pexp_loc.loc_start with
                     pos_cnum = e.pexp_loc.loc_start.pos_cnum + 2
                   }
               }
             in
             record ~from:e.pexp_loc ~to_ t
           | _ -> ());
          Ast_iterator.default_iterator.expr this e)
    }
  in
  Option.iter (iterator.structure iterator) parse_tree
;;

let default_for_fundecl ~file_content (parse_tree : Parsetree.structure option) t =
  let open Parsetree in
  let rec find_deepest_module c =
    match c.pmod_desc with
    | Pmod_functor (_, c) -> find_deepest_module c
    | _ -> c.pmod_loc
  in
  let iterator =
    { Ast_iterator.default_iterator with
      module_expr =
        (fun this c ->
          (match c.pmod_desc with
           | Pmod_structure _ ->
             record
               ~from:c.pmod_loc
               ~to_:
                 { c.pmod_loc with
                   loc_end =
                     { c.pmod_loc.loc_end with
                       pos_cnum = c.pmod_loc.loc_start.pos_cnum + 6
                     }
                 }
               t
           | _ -> ());
          Ast_iterator.default_iterator.module_expr this c)
    ; module_binding =
        (fun this c ->
          (match c.pmb_expr.pmod_desc with
           | Pmod_functor _ -> record ~from:c.pmb_expr.pmod_loc ~to_:c.pmb_name.loc t
           | _ -> ());
          Ast_iterator.default_iterator.module_binding this c)
    ; structure_item =
        (fun this c ->
          (match c.pstr_desc with
           | Pstr_module m ->
             let from = find_deepest_module m.pmb_expr in
             record ~from ~to_:m.pmb_name.loc t;
             record ~from:c.pstr_loc ~to_:m.pmb_name.loc t
           | Pstr_value (_, bindings) ->
             List.iter
               ~f:(fun binding ->
                 record ~from:binding.pvb_expr.pexp_loc ~to_:binding.pvb_pat.ppat_loc t)
               bindings;
             List.iter bindings ~f:(fun binding ->
               match binding.pvb_expr.pexp_desc with
               | Pexp_function _ ->
                 record ~from:binding.pvb_expr.pexp_loc ~to_:binding.pvb_pat.ppat_loc t
               | _ -> ())
           | _ -> ());
          Ast_iterator.default_iterator.structure_item this c)
    ; expr =
        (fun this e ->
          (match e.pexp_desc with
           | Pexp_function
               ( []
               , { ret_type_constraint = None; ret_mode_annotations = []; _ }
               , Pfunction_cases _ ) ->
             if not e.pexp_loc.loc_ghost
             then (
               let to_ =
                 { e.pexp_loc with
                   loc_end =
                     { e.pexp_loc.loc_start with
                       pos_cnum = e.pexp_loc.loc_start.pos_cnum + String.length "function"
                     }
                 }
               in
               record ~from:e.pexp_loc ~to_ t)
           | Pexp_function _ ->
             if not e.pexp_loc.loc_ghost
             then (
               let to_ =
                 { e.pexp_loc with
                   loc_end =
                     { e.pexp_loc.loc_start with
                       pos_cnum = e.pexp_loc.loc_start.pos_cnum + String.length "fun"
                     }
                 }
                 |> ensure_captures_no_parenthesis ~file_content
               in
               record ~from:e.pexp_loc ~to_ t)
           | Pexp_letop _ -> ()
           | Pexp_let (_, bindings, _) ->
             List.iter
               ~f:(fun binding ->
                 match binding.pvb_expr.pexp_desc with
                 | Pexp_function _ ->
                   record ~from:binding.pvb_expr.pexp_loc ~to_:binding.pvb_pat.ppat_loc t
                 | _ -> ())
               bindings
           | _ -> ());
          Ast_iterator.default_iterator.expr this e)
    }
  in
  Option.iter (iterator.structure iterator) parse_tree
;;

let default_for_call ~file_content (parse_tree : Parsetree.structure option) t =
  let open Parsetree in
  let adjust_record_if_apply_is_let_bind_ppx ~file_content (apply : Parsetree.expression) =
    match apply.pexp_desc with
    | Pexp_ident loc_ident ->
      let is_let_syntax_ppx =
        List.exists ~f:(String.equal "Let_syntax") (Longident.flatten loc_ident.txt)
      in
      if is_let_syntax_ppx && loc_ident.loc.loc_ghost
      then (
        let pattern, to_ =
          take_longest_keyword ~file_content ~from:loc_ident.loc.loc_start.pos_cnum
        in
        if String.contains pattern '%'
        then (
          let fake_loc =
            let open Warnings in
            { loc_ident.loc with
              loc_end = { loc_ident.loc.loc_start with pos_cnum = to_ }
            }
          in
          record ~from:loc_ident.loc ~to_:fake_loc t))
    | _ -> ()
  in
  let iterator =
    { Ast_iterator.default_iterator with
      module_expr =
        (fun this me ->
          (match me.pmod_desc with
           | Pmod_apply (f, _) -> record ~from:me.pmod_loc ~to_:f.pmod_loc t
           | _ -> ());
          Ast_iterator.default_iterator.module_expr this me)
    ; class_expr =
        (fun this c ->
          (match c.pcl_desc with
           | Pcl_fun _ -> ()
           | Pcl_apply (f, _) -> record ~from:c.pcl_loc ~to_:f.pcl_loc t
           | _ -> ());
          Ast_iterator.default_iterator.class_expr this c)
    ; expr =
        (fun this e ->
          (match e.pexp_desc with
           | Pexp_apply (f, _) ->
             record ~from:e.pexp_loc ~to_:f.pexp_loc t;
             adjust_record_if_apply_is_let_bind_ppx ~file_content f
           | _ -> ());
          Ast_iterator.default_iterator.expr this e)
    }
  in
  Option.iter (iterator.structure iterator) parse_tree
;;
