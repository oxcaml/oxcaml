(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Values as patterns pretty printer *)

open Asttypes
open Typedtree
<<<<<<< HEAD
open Types
||||||| 23e84b8c4d
open Types
open Format
=======
open Data_types
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
open Format_doc

let is_cons = function
| {cstr_name = "::"} -> true
| _ -> false

let pretty_const c = match c with
| Const_int i -> Printf.sprintf "%d" i
| Const_char c -> Printf.sprintf "%C" c
| Const_untagged_char c -> Printf.sprintf "#%C" c
| Const_string (s, _, _) -> Printf.sprintf "%S" s
| Const_float f -> Printf.sprintf "%s" f
| Const_float32 f -> Printf.sprintf "%s" f
| Const_unboxed_float f -> Printf.sprintf "%s" (Misc.format_as_unboxed_literal f)
| Const_unboxed_float32 f -> Printf.sprintf "%ss" (Misc.format_as_unboxed_literal f)
| Const_int8 i -> Printf.sprintf "%ds" i
| Const_int16 i -> Printf.sprintf "%dS" i
| Const_int32 i -> Printf.sprintf "%ldl" i
| Const_int64 i -> Printf.sprintf "%LdL" i
| Const_nativeint i -> Printf.sprintf "%ndn" i
| Const_untagged_int i ->
  Printf.sprintf "%sm" (Misc.format_as_unboxed_literal (Int.to_string i))
| Const_untagged_int8 i ->
  Printf.sprintf "%ss" (Misc.format_as_unboxed_literal (Int.to_string i))
| Const_untagged_int16 i ->
  Printf.sprintf "%sS" (Misc.format_as_unboxed_literal (Int.to_string i))
| Const_unboxed_int32 i ->
  Printf.sprintf "%sl" (Misc.format_as_unboxed_literal (Int32.to_string i))
| Const_unboxed_int64 i ->
  Printf.sprintf "%sL" (Misc.format_as_unboxed_literal (Int64.to_string i))
| Const_unboxed_nativeint i ->
  Printf.sprintf "%sn" (Misc.format_as_unboxed_literal (Nativeint.to_string i))

let bool ppf = function
  | false -> fprintf ppf "false"
  | true -> fprintf ppf "true"

let pretty_extra ppf (cstr, _loc, _attrs) pretty_rest rest =
  match cstr with
  | Tpat_unpack ->
     fprintf ppf "@[(module %a)@]" pretty_rest rest
  | Tpat_constraint _ ->
     fprintf ppf "@[(%a : _)@]" pretty_rest rest
  | Tpat_type _ ->
     fprintf ppf "@[(# %a)@]" pretty_rest rest
  | Tpat_open _ ->
     fprintf ppf "@[(# %a)@]" pretty_rest rest
  | Tpat_inspected_type _ ->
     fprintf ppf "%a" pretty_rest rest

let rec pretty_val : type k . _ -> k general_pattern -> _ = fun ppf v ->
  match v.pat_extra with
    | extra :: rem ->
       pretty_extra ppf extra
         pretty_val { v with pat_extra = rem }
    | [] ->
  let pretty_record ~unboxed lvs =
    let filtered_lvs = List.filter
        (function
          | (_,_,{pat_desc=Tpat_any}) -> false (* do not show lbl=_ *)
          | _ -> true) lvs in
    let hash = if unboxed then "#" else "" in
    begin match filtered_lvs with
    | [] -> fprintf ppf "%s{ _ }" hash
    | (_, lbl, _) :: q ->
        let elision_mark ppf =
          (* we assume that there is no label repetitions here *)
            if Array.length lbl.lbl_all > 1 + List.length q then
              fprintf ppf ";@ _@ "
            else () in
        fprintf ppf "@[%s{%a%t}@]"
          hash pretty_lvals filtered_lvs elision_mark
    end
  in
  match v.pat_desc with
  | Tpat_any -> fprintf ppf "_"
<<<<<<< HEAD
  | Tpat_var (x,_,_,_,_) -> fprintf ppf "%s" (Ident.name x)
||||||| 23e84b8c4d
  | Tpat_var (x,_) -> fprintf ppf "%s" (Ident.name x)
=======
  | Tpat_var (x,_,_) -> fprintf ppf "%s" (Ident.name x)
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  | Tpat_constant c -> fprintf ppf "%s" (pretty_const c)
  | Tpat_unboxed_unit -> fprintf ppf "#()"
  | Tpat_unboxed_bool b -> fprintf ppf "#%a" bool b
  | Tpat_tuple vs ->
      fprintf ppf "@[(%a)@]" (pretty_list pretty_labeled_val ",") vs
<<<<<<< HEAD
  | Tpat_unboxed_tuple vs ->
      fprintf ppf "@[#(%a)@]" (pretty_list pretty_labeled_val_sort ",") vs
||||||| 23e84b8c4d
      fprintf ppf "@[(%a)@]" (pretty_vals ",") vs
=======
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  | Tpat_construct (_, cstr, [], _) ->
      fprintf ppf "%s" cstr.cstr_name
  | Tpat_construct (_, cstr, [w], None) ->
      fprintf ppf "@[<2>%s@ %a@]" cstr.cstr_name pretty_arg w
  | Tpat_construct (_, cstr, vs, vto) ->
      let name = cstr.cstr_name in
      begin match (name, vs, vto) with
        ("::", [v1;v2], None) ->
          fprintf ppf "@[%a::@,%a@]" pretty_car v1 pretty_cdr v2
      | (_, _, None) ->
          fprintf ppf "@[<2>%s@ @[(%a)@]@]" name (pretty_vals ",") vs
      | (_, _, Some ([], _t)) ->
          fprintf ppf "@[<2>%s@ @[(%a : _)@]@]" name (pretty_vals ",") vs
      | (_, _, Some (vl, _t)) ->
          let vars = List.map (fun (x, _) -> Ident.name x.txt) vl in
          fprintf ppf "@[<2>%s@ (type %s)@ @[(%a : _)@]@]"
            name (String.concat " " vars) (pretty_vals ",") vs
      end
  | Tpat_variant (l, None, _) ->
      fprintf ppf "`%s" l
  | Tpat_variant (l, Some w, _) ->
      fprintf ppf "@[<2>`%s@ %a@]" l pretty_arg w
<<<<<<< HEAD
  | Tpat_record (lvs,_) -> pretty_record ~unboxed:false lvs
  | Tpat_record_unboxed_product (lvs,_) -> pretty_record ~unboxed:true lvs
  | Tpat_array (am, _arg_sort, vs) ->
      let punct = if Types.is_mutable am then '|' else ':' in
      fprintf ppf "@[[%c %a %c]@]" punct (pretty_vals " ;") vs punct
||||||| 23e84b8c4d
  | Tpat_record (lvs,_) ->
      let filtered_lvs = List.filter
          (function
            | (_,_,{pat_desc=Tpat_any}) -> false (* do not show lbl=_ *)
            | _ -> true) lvs in
      begin match filtered_lvs with
      | [] -> fprintf ppf "{ _ }"
      | (_, lbl, _) :: q ->
          let elision_mark ppf =
            (* we assume that there is no label repetitions here *)
             if Array.length lbl.lbl_all > 1 + List.length q then
               fprintf ppf ";@ _@ "
             else () in
          fprintf ppf "@[{%a%t}@]"
            pretty_lvals filtered_lvs elision_mark
      end
  | Tpat_array vs ->
      fprintf ppf "@[[| %a |]@]" (pretty_vals " ;") vs
=======
  | Tpat_record (lvs,_) ->
      let filtered_lvs = List.filter
          (function
            | (_,_,{pat_desc=Tpat_any}) -> false (* do not show lbl=_ *)
            | _ -> true) lvs in
      begin match filtered_lvs with
      | [] -> fprintf ppf "{ _ }"
      | (_, lbl, _) :: q ->
          let elision_mark ppf =
            (* we assume that there is no label repetitions here *)
             if Array.length lbl.lbl_all > 1 + List.length q then
               fprintf ppf ";@ _@ "
             else () in
          fprintf ppf "@[{%a%t}@]"
            pretty_lvals filtered_lvs elision_mark
      end
  | Tpat_array (_, vs) ->
      fprintf ppf "@[[| %a |]@]" (pretty_vals " ;") vs
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  | Tpat_lazy v ->
      fprintf ppf "@[<2>lazy@ %a@]" pretty_arg v
<<<<<<< HEAD
  | Tpat_alias (v, x, _, _, _, _, _) ->
||||||| 23e84b8c4d
  | Tpat_alias (v, x,_) ->
      fprintf ppf "@[(%a@ as %a)@]" pretty_val v Ident.print x
=======
  | Tpat_alias (v, x,_,_,_) ->
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
      fprintf ppf "@[(%a@ as %a)@]" pretty_val v Ident.doc_print x
  | Tpat_value v ->
      fprintf ppf "%a" pretty_val (v :> pattern)
  | Tpat_exception v ->
      fprintf ppf "@[<2>exception@ %a@]" pretty_arg v
  | Tpat_or _ ->
      fprintf ppf "@[(%a)@]" pretty_or v

and pretty_car ppf v = match v.pat_desc with
| Tpat_construct (_,cstr, [_ ; _], None)
    when is_cons cstr ->
      fprintf ppf "(%a)" pretty_val v
| _ -> pretty_val ppf v

and pretty_cdr ppf v = match v.pat_desc with
| Tpat_construct (_,cstr, [v1 ; v2], None)
    when is_cons cstr ->
      fprintf ppf "%a::@,%a" pretty_car v1 pretty_cdr v2
| _ -> pretty_val ppf v

and pretty_arg ppf v = match v.pat_desc with
| Tpat_construct (_,_,_::_,None)
| Tpat_variant (_, Some _, _) -> fprintf ppf "(%a)" pretty_val v
|  _ -> pretty_val ppf v

and pretty_or : type k . _ -> k general_pattern -> _ = fun ppf v ->
  match v.pat_desc with
  | Tpat_or (v,w,_) ->
      fprintf ppf "%a|@,%a" pretty_or v pretty_or w
  | _ -> pretty_val ppf v

and pretty_list : type k . (_ -> k -> _) -> _ -> _ -> k list -> _ =
  fun print_val sep ppf ->
    function
    | [] -> ()
    | [v] -> print_val ppf v
    | v::vs ->
        fprintf ppf "%a%s@ %a" print_val v sep (pretty_list print_val sep) vs
<<<<<<< HEAD
||||||| 23e84b8c4d
and pretty_vals sep ppf = function
  | [] -> ()
  | [v] -> pretty_val ppf v
  | v::vs ->
      fprintf ppf "%a%s@ %a" pretty_val v sep (pretty_vals sep) vs
=======

and pretty_vals sep = pretty_list pretty_val sep

and pretty_labeled_val ppf (l, p) =
  begin match l with
  | Some s -> fprintf ppf "~%s:" s
  | None -> ()
  end;
  pretty_val ppf p
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

and pretty_vals sep = pretty_list pretty_val sep

and pretty_labeled_val ppf (l, p) =
  begin match l with
  | Some s -> fprintf ppf "~%s:" s
  | None -> ()
  end;
  pretty_val ppf p

and pretty_labeled_val_sort ppf (l, p, _) =
  begin match l with
  | Some s -> fprintf ppf "~%s:" s
  | None -> ()
  end;
  pretty_val ppf p

and pretty_lvals : 'a. _ -> (_ * 'a gen_label_description * _) list -> _ =
  fun ppf -> function
  | [] -> ()
  | [_,lbl,v] ->
      fprintf ppf "%s=%a" lbl.lbl_name pretty_val v
  | (_, lbl,v)::rest ->
      fprintf ppf "%s=%a;@ %a"
        lbl.lbl_name pretty_val v pretty_lvals rest

let top_pretty ppf v =
  fprintf ppf "@[%a@]" pretty_val v

let pretty_pat ppf p =
  top_pretty ppf p ;
  pp_print_flush ppf ()

type 'k matrix = 'k general_pattern list list

let pretty_line ppf line =
  fprintf ppf "@[";
  List.iter (fun p ->
      fprintf ppf "<%a>@ "
        pretty_val p
    ) line;
  fprintf ppf "@]"

let pretty_matrix ppf (pss : 'k matrix) =
  fprintf ppf "@[<v 2>  %a@]"
    (pp_print_list ~pp_sep:pp_print_cut pretty_line)
    pss

module Compat = struct
  let pretty_pat ppf x = compat pretty_pat ppf x
  let pretty_line ppf x = compat pretty_line ppf x
  let pretty_matrix ppf x = compat pretty_matrix ppf x
end
