(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Jacques Garrigue, Graduate School of Mathematics, Nagoya University   *)
(*                                                                        *)
(*   Copyright 2003 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


(* Print a raw type expression, with sharing *)

open Format
open Types
open Mode
let longident = Pprintast.longident

let raw_list pr ppf = function
    [] -> fprintf ppf "[]"
  | a :: l ->
      fprintf ppf "@[<1>[%a%t]@]" pr a
        (fun ppf -> List.iter (fun x -> fprintf ppf ";@,%a" pr x) l)

let kind_vars = ref []
let kind_count = ref 0

let string_of_field_kind v =
  match field_kind_repr v with
  | Fpublic -> "Fpublic"
  | Fabsent -> "Fabsent"
  | Fprivate -> "Fprivate"

let rec safe_repr v t =
  match Transient_expr.coerce t with
    {desc = Tlink t} when not (List.memq t v) ->
      safe_repr (t::v) t
  | t' -> t'

let rec list_of_memo = function
    Mnil -> []
  | Mcons (_priv, p, _t1, _t2, rem) -> p :: list_of_memo rem
  | Mlink rem -> list_of_memo !rem

let print_name ppf = function
    None -> fprintf ppf "None"
  | Some name -> fprintf ppf "\"%s\"" name

let path = Format_doc.compat Path.print

let string_of_label : Types.arg_label -> string = function
    Nolabel -> ""
  | Labelled s | Position s -> s
  | Optional s -> "?"^s

let visited = ref []
let rec raw_type ppf ty =
  let ty = safe_repr [] ty in
  if List.memq ty !visited then fprintf ppf "{id=%d}" ty.id else begin
    visited := ty :: !visited;
    fprintf ppf "@[<1>{id=%d;level=%d;scope=%d;marks=%x;desc=@,%a}@]"
      ty.id ty.level
      (Transient_expr.get_scope ty) (Transient_expr.get_marks ty)
      raw_type_desc ty.desc
  end
and labeled_type ppf (label, ty) =
  begin match label with
  | Some s -> fprintf ppf "label=\"%s\" " s
  | None -> ()
  end;
  raw_type ppf ty
and raw_type_list tl = raw_list raw_type tl
and labeled_type_list tl = raw_list labeled_type tl
and raw_lid_type_list tl =
  raw_list (fun ppf (lid, typ) ->
             let lid = Longident.unflatten lid |> Option.get in
             fprintf ppf "(@,%a,@,%a)" longident lid raw_type typ)
    tl
and raw_row_desc ppf row =
  let Row {fields; more; name; fixed; closed} = row_repr row in
  fprintf ppf
    "@[<hov1>{@[%s@,%a;@]@ @[%s@,%a;@]@ %s%B;@ %s%a;@ @[<1>%s%t@]}@]"
    "row_fields="
    (raw_list (fun ppf (l, f) ->
       fprintf ppf "@[%s,@ %a@]" l raw_field f))
    fields
    "row_more=" raw_type more
    "row_closed=" closed
    "row_fixed=" raw_row_fixed fixed
    "row_name="
    (fun ppf ->
       match name with None -> fprintf ppf "None"
                     | Some(p,tl) ->
                       fprintf ppf "Some(@,%a,@,%a)" path p raw_type_list tl)
and raw_type_desc ppf = function
    Tvar { name; jkind } ->
      fprintf ppf "Tvar (@,%a,@,%a)"
        print_name name (Format_doc.compat Jkind.format) jkind
  | Tarrow((l,arg,ret),t1,t2,c) ->
      fprintf ppf "@[<hov1>Tarrow((\"%s\",%a,%a),@,%a,@,%a,@,%s)@]"
        (string_of_label l)
        (Format_doc.compat (Alloc.print ~verbose:true ())) arg
        (Format_doc.compat (Alloc.print ~verbose:true ())) ret
        raw_type t1 raw_type t2
        (if is_commu_ok c then "Cok" else "Cunknown")
  | Ttuple tl ->
      fprintf ppf "@[<1>Ttuple@,%a@]" labeled_type_list tl
  | Tunboxed_tuple tl ->
      fprintf ppf "@[<1>Tunboxed_tuple@,%a@]" labeled_type_list tl
  | Tconstr (p, tl, abbrev) ->
      fprintf ppf "@[<hov1>Tconstr(@,%a,@,%a,@,%a)@]" path p
        raw_type_list tl
        (raw_list path) (list_of_memo !abbrev)
  | Tobject (t, nm) ->
      fprintf ppf "@[<hov1>Tobject(@,%a,@,@[<1>ref%t@])@]" raw_type t
        (fun ppf ->
          match !nm with None -> fprintf ppf " None"
          | Some(p,tl) ->
              fprintf ppf "(Some(@,%a,@,%a))" path p raw_type_list tl)
  | Tquote t ->
      fprintf ppf "@[Tquote@ %a@]" raw_type t
  | Tsplice t ->
      fprintf ppf "@[Tsplice@ %a@]" raw_type t
  | Tfield (f, k, t1, t2) ->
      fprintf ppf "@[<hov1>Tfield(@,%s,@,%s,@,%a,@;<0 -1>%a)@]" f
        (string_of_field_kind k)
        raw_type t1 raw_type t2
  | Tnil -> fprintf ppf "Tnil"
  | Tlink t -> fprintf ppf "@[<1>Tlink@,%a@]" raw_type t
  | Tsubst (t, None) -> fprintf ppf "@[<1>Tsubst@,(%a,None)@]" raw_type t
  | Tsubst (t, Some t') ->
      fprintf ppf "@[<1>Tsubst@,(%a,@ Some%a)@]" raw_type t raw_type t'
  | Tunivar { name; jkind } ->
      fprintf ppf "Tunivar (@,%a,@,%a)"
        print_name name (Format_doc.compat Jkind.format) jkind
  | Tpoly (t, tl) ->
      fprintf ppf "@[<hov1>Tpoly(@,%a,@,%a)@]"
        raw_type t
        raw_type_list tl
  | Trepr (t, sort_vars) ->
      let print_sort_univar ppf (uv : Jkind_types.Sort.univar) =
        match uv.name with
        | Some n -> fprintf ppf "%s" n
        | None -> fprintf ppf "_"
      in
      fprintf ppf "@[<hov1>Trepr(@,%a,@,[@[%a@]])@]"
        raw_type t
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
          print_sort_univar) sort_vars
  | Tvariant row ->
    raw_row_desc ppf row
  | Tpackage pack ->
    fprintf ppf "@[<hov1>Tpackage(@,%a,@,%a)@]"
      path pack.pack_path
      raw_lid_type_list pack.pack_cstrs
  | Tof_kind jkind ->
    fprintf ppf "(type@ :@ %a)" (Format_doc.compat Jkind.format) jkind
and raw_row_fixed ppf = function
| None -> fprintf ppf "None"
| Some Types.Fixed_private -> fprintf ppf "Some Fixed_private"
| Some Types.Rigid -> fprintf ppf "Some Rigid"
| Some Types.Univar t -> fprintf ppf "Some(Univar(%a))" raw_type t
| Some Types.Reified p -> fprintf ppf "Some(Reified(%a))" path p
| Some Types.Fixed_existential -> fprintf ppf "Some Fixed_existential"

and raw_field ppf rf =
  match_row_field
    ~absent:(fun _ -> fprintf ppf "RFabsent")
    ~present:(function
      | None ->
          fprintf ppf "RFpresent None"
      | Some t ->
          fprintf ppf  "@[<1>RFpresent(Some@,%a)@]" raw_type t)
    ~either:(fun c tl m (_,e) ->
      fprintf ppf "@[<hov1>RFeither(%B,@,%a,@,%B,@,@[<1>ref%t@])@]" c
        raw_type_list tl m
        (fun ppf ->
          match e with None -> fprintf ppf " RFnone"
          | Some f -> fprintf ppf "@,@[<1>(%a)@]" raw_field f))
    rf
let type_expr ppf t =
  visited := []; kind_vars := []; kind_count := 0;
  raw_type ppf t;
  visited := []; kind_vars := []
let row_field = raw_field
let row_desc = raw_row_desc

let () = Btype.print_raw := type_expr
let () = Jkind.set_raw_type_expr type_expr
