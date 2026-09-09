(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module C = Cmm_helpers
module String = Misc.Stdlib.String

type data_group =
  | Required of Cmm.data_item list
  | Specialisation_site of
      { data : Cmm.data_item list;
        symbols : Cmm.symbol list
      }

type t =
  { gc_roots : Symbol.t list;
    data_list : data_group list;
    functions : Cmm.fundecl list;
    current_data : Cmm.data_item list;
    reachable_names : Name_occurrences.t;
    symbols : Cmm.symbol String.Map.t;
    (* This map is only used for symbols not directly translated from
       [Symbol.t], e.g. module entry point names. *)
    module_symbol : Symbol.t;
    module_symbol_defined : bool;
    invalid_message_symbols : Symbol.t String.Map.t
  }

let create ~module_symbol ~reachable_names =
  { gc_roots = [];
    data_list = [];
    functions = [];
    current_data = [];
    reachable_names;
    symbols = String.Map.empty;
    module_symbol;
    module_symbol_defined = false;
    invalid_message_symbols = String.Map.empty
  }

(* Symbol handling

   These functions are there to ensure that a given symbol is: 1) given an
   appropriate locality, and 2) **always** given the same locality *)
let raw_symbol res ~global:sym_global sym_name : t * Cmm.symbol =
  match String.Map.find_opt sym_name res.symbols with
  | None ->
    let sym : Cmm.symbol = { sym_name; sym_global } in
    let symbols = String.Map.add sym_name sym res.symbols in
    { res with symbols }, sym
  | Some sym ->
    if Cmm.equal_is_global sym_global sym.sym_global
    then res, sym
    else
      Misc.fatal_errorf "The symbol %s is declared as both local and global"
        sym_name

let symbol res sym =
  let sym_name = Linkage_name.to_string (Symbol.linkage_name sym) in
  let sym_global =
    if
      Current_unit.is_current (Symbol.compilation_unit sym)
      && not (Name_occurrences.mem_symbol res.reachable_names sym)
    then Cmm.Local
    else Cmm.Global
  in
  let s : Cmm.symbol = { sym_name; sym_global } in
  s

let symbol_of_code_id res code_id ~currently_in_inlined_body : Cmm.symbol =
  let sym_name = Linkage_name.to_string (Code_id.linkage_name code_id) in
  let () =
    (* In classic mode, ensure that all .cmx files have been loaded, so that the
       zero-alloc check can see the function summaries. We only need to do this
       for inlined bodies, which are not traversed during [Lambda_to_flambda].
       (When using [Simplify], all inlined bodies are traversed and any
       referenced .cmx files will have been loaded.) *)
    if Flambda_features.classic_mode () && currently_in_inlined_body
    then Compilenv.require_global (Code_id.get_compilation_unit code_id)
    else ()
  in
  let sym_global =
    if
      Current_unit.is_current (Code_id.get_compilation_unit code_id)
      && not (Name_occurrences.mem_code_id res.reachable_names code_id)
    then Cmm.Local
    else Cmm.Global
  in
  { sym_name; sym_global }

(* *)

let check_for_module_symbol t symbol =
  if Symbol.equal symbol t.module_symbol
  then (
    if t.module_symbol_defined
    then
      Misc.fatal_errorf
        "check_for_module_symbol %a: Module block symbol (%a) already defined"
        Symbol.print symbol Symbol.print t.module_symbol;
    { t with module_symbol_defined = true })
  else t

let defined_symbol data =
  match (data : Cmm.data_item) with
  | Cdefine_symbol sym -> Some sym
  | Cint8 _ | Cint16 _ | Cint32 _ | Cint _ | Csingle _ | Cdouble _ | Cvec128 _
  | Cvec256 _ | Cvec512 _ | Csymbol_address _ | Csymbol_offset _ | Cstring _
  | Cskip _ | Calign _ ->
    None

let add_to_data_list ?(is_specialisation_site = false) x l =
  match x with
  | [] -> l
  | _ :: _ ->
    if not (List.exists (fun data -> Option.is_some (defined_symbol data)) x)
    then
      Misc.fatal_errorf
        "data list does not define any symbol, its elements will be unusable: \
         %a"
        Printcmm.data x;
    let group =
      if is_specialisation_site
      then
        Specialisation_site
          { data = x; symbols = List.filter_map defined_symbol x }
      else Required x
    in
    group :: l

let archive_data r =
  { r with
    current_data = [];
    data_list = add_to_data_list r.current_data r.data_list
  }

let update_data r f = { r with current_data = f r.current_data }

let set_data r l =
  update_data r (function
    | [] -> l
    | _ ->
      Misc.fatal_errorf "To_cmm_result.set_data: %s"
        "about to lose some translated static data items")

let add_archive_data_items r l =
  { r with data_list = add_to_data_list l r.data_list }

let add_specialisation_site_data r data =
  { r with
    data_list = add_to_data_list ~is_specialisation_site:true data r.data_list
  }

let add_gc_roots r l = { r with gc_roots = l @ r.gc_roots }

let add_function r f = { r with functions = f :: r.functions }

type result =
  { data_items : Cmm.phrase list;
    gc_roots : Cmm.symbol list;
    functions : Cmm.phrase list
  }

let define_module_symbol_if_missing r =
  if r.module_symbol_defined
  then r
  else
    let linkage_name =
      Linkage_name.to_string (Symbol.linkage_name r.module_symbol)
    in
    let sym : Cmm.symbol = { sym_name = linkage_name; sym_global = Global } in
    let l = C.emit_block sym (C.black_block_header 0 0) [] in
    set_data r l

let add_invalid_message_symbol t symbol ~message =
  { t with
    invalid_message_symbols =
      String.Map.add message symbol t.invalid_message_symbols
  }

let invalid_message_symbol t ~message =
  String.Map.find_opt message t.invalid_message_symbols

let add_data_references used data =
  List.fold_left
    (fun used (item : Cmm.data_item) ->
      match item with
      | Csymbol_address sym | Csymbol_offset (sym, _) ->
        String.Set.add sym.sym_name used
      | Cdefine_symbol _ | Cint8 _ | Cint16 _ | Cint32 _ | Cint _ | Csingle _
      | Cdouble _ | Cvec128 _ | Cvec256 _ | Cvec512 _ | Cstring _ | Cskip _
      | Calign _ ->
        used)
    used data

let add_expr_references used expr =
  let used = ref used in
  let add (sym : Cmm.symbol) = used := String.Set.add sym.sym_name !used in
  let rec visit (expr : Cmm.expression) =
    (match expr with
    | Cconst_symbol (sym, _) | Cinvalid { symbol = sym; _ } -> add sym
    | Cphantom_let (_, defining_expr, _) -> (
      match defining_expr with
      | Some (Cphantom_const_symbol sym | Cphantom_read_symbol_field { sym; _ })
        ->
        add sym
      | None
      | Some
          ( Cphantom_const_int _ | Cphantom_var _ | Cphantom_offset_var _
          | Cphantom_read_field _ | Cphantom_block _ ) ->
        ())
    | Cop (_, _, _) ->
      (* Operations only name C functions and probe handlers, never closure
         data; call targets are operands. *)
      ()
    | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _ | Cconst_mask _
    | Cvar _ | Clet _ | Cname_for_debugger _ | Ctuple _ | Csequence _
    | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ ->
      ());
    Cmm.iter_shallow visit expr
  in
  visit expr;
  !used

let add_phrase_references used (phrase : Cmm.phrase) =
  match phrase with
  | Cfunction func -> add_expr_references used func.fun_body
  | Cdata data -> add_data_references used data

let site_is_required used symbols =
  List.exists
    (fun (sym : Cmm.symbol) ->
      match sym.sym_global with
      | Global -> true
      | Local -> String.Set.mem sym.sym_name used)
    symbols

let filter_specialisation_sites data_list roots ~extra_phrases =
  let site_symbols =
    List.fold_left
      (fun all_symbols -> function
        | Required _ -> all_symbols
        | Specialisation_site { symbols; _ } ->
          List.fold_left
            (fun all_symbols (sym : Cmm.symbol) ->
              String.Set.add sym.sym_name all_symbols)
            all_symbols symbols)
      String.Set.empty data_list
  in
  let used, roots =
    if String.Set.is_empty site_symbols
    then String.Set.empty, roots
    else
      (* Sites have no fields for the GC to scan, so they need no roots, which
         would otherwise keep their data. *)
      let roots =
        List.filter
          (fun (sym : Cmm.symbol) ->
            not (String.Set.mem sym.sym_name site_symbols))
          roots
      in
      let used =
        List.fold_left
          (fun used (sym : Cmm.symbol) -> String.Set.add sym.sym_name used)
          String.Set.empty roots
      in
      let used = List.fold_left add_phrase_references used extra_phrases in
      let used =
        List.fold_left
          (fun used -> function
            | Required data -> add_data_references used data
            | Specialisation_site _ -> used)
          used data_list
      in
      (* Sites only point to code, never to other sites, so one pass
         suffices. *)
      used, roots
  in
  let data_items =
    List.filter_map
      (function
        | Required data -> Some (C.cdata data)
        | Specialisation_site { data; symbols } ->
          if site_is_required used symbols then Some (C.cdata data) else None)
      data_list
  in
  data_items, roots

let to_cmm r ~extra_phrases =
  (* Make sure the module symbol is defined *)
  let r = define_module_symbol_if_missing r in
  (* Make sure we do not forget any current data *)
  let r = archive_data r in
  let sorted_functions =
    let functions = List.rev r.functions in
    match !Oxcaml_flags.function_layout with
    | Topological -> functions
    | Source ->
      (* Sort functions according to debuginfo, to get a stable ordering. Use
         reversed r.functions to preserve order for equal debuginfos *)
      List.sort
        (fun (f1 : Cmm.fundecl) (f2 : Cmm.fundecl) ->
          Debuginfo.compare f1.fun_dbg f2.fun_dbg)
        functions
  in
  let function_phrases = List.map (fun f -> C.cfunction f) sorted_functions in
  (* Translate roots to Cmm symbols *)
  let roots = List.map (symbol r) r.gc_roots in
  let data_items, gc_roots =
    filter_specialisation_sites r.data_list roots
      ~extra_phrases:(function_phrases @ extra_phrases)
  in
  { data_items; gc_roots; functions = function_phrases }
