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

type t =
  { gc_roots : Symbol.t list;
    data_list : Cmm.phrase list;
    functions : Cmm.fundecl list;
    current_data : Cmm.data_item list;
    reachable_names : Name_occurrences.t;
    symbols : Cmm.symbol String.Map.t;
    (* This map is only used for symbols not directly translated from
       [Symbol.t], e.g. module entry point names. *)
    invalid_message_symbols : Symbol.t String.Map.t
  }

let create ~reachable_names =
  { gc_roots = [];
    data_list = [];
    functions = [];
    current_data = [];
    reachable_names;
    symbols = String.Map.empty;
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

let defined_symbol_name data =
  match (data : Cmm.data_item) with
  | Cdefine_symbol sym -> Some sym.sym_name
  | Cint8 _ | Cint16 _ | Cint32 _ | Cint _ | Csingle _ | Cdouble _ | Cvec128 _
  | Cvec256 _ | Cvec512 _ | Csymbol_address _ | Csymbol_offset _ | Cstring _
  | Cskip _ | Calign _ ->
    None

let defines_a_symbol data = Option.is_some (defined_symbol_name data)

let add_to_data_list x l =
  match x with
  | [] -> l
  | _ :: _ ->
    if not (List.exists defines_a_symbol x)
    then
      Misc.fatal_errorf
        "data list does not define any symbol, its elements will be unusable: \
         %a"
        Printcmm.data x;
    C.cdata x :: l

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

let add_gc_roots r l = { r with gc_roots = l @ r.gc_roots }

let add_function r f = { r with functions = f :: r.functions }

(* The roots are defined in the initialiser's return continuation, which is
   dropped when the initialiser cannot return, while other units still refer to
   them. The placeholders are never read; their Abstract tag keeps the GC from
   scanning them. *)
let define_missing_symbols r symbols_with_sizes =
  let r = archive_data r in
  let defined =
    List.fold_left
      (fun defined (phrase : Cmm.phrase) ->
        match phrase with
        | Cfunction _ -> defined
        | Cdata items ->
          List.fold_left
            (fun defined item ->
              match defined_symbol_name item with
              | Some name -> String.Set.add name defined
              | None -> defined)
            defined items)
      String.Set.empty r.data_list
  in
  List.fold_left
    (fun r (sym, size) ->
      let sym = symbol r sym in
      if String.Set.mem sym.sym_name defined
      then r
      else
        let header = C.block_header Obj.abstract_tag size in
        let fields = List.init size (fun _ -> Cmm.Cint 0n) in
        add_archive_data_items r (C.emit_block sym header fields))
    r symbols_with_sizes

type result =
  { data_items : Cmm.phrase list;
    gc_roots : Cmm.symbol list;
    functions : Cmm.phrase list
  }

let add_invalid_message_symbol t symbol ~message =
  { t with
    invalid_message_symbols =
      String.Map.add message symbol t.invalid_message_symbols
  }

let invalid_message_symbol t ~message =
  String.Map.find_opt message t.invalid_message_symbols

let to_cmm r =
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
  (* Return the data list, gc roots and function declarations *)
  { data_items = r.data_list; gc_roots = roots; functions = function_phrases }
