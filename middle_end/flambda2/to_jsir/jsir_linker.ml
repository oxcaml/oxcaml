(******************************************************************************
 *                                  OxCaml                                    *
 *                           Leo Lee, Jane Street                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Jsoo_imports.Import
module Symbol_table = Jsir.Linkage_name.Hashtbl

(* A unit whose variables and block addresses have been renumbered so as not to
   clash with those of the other units. *)
type unit_ =
  { name : string;
    start : Jsir.Addr.t;
    blocks : Jsir.block Jsir.Addr.Map.t;
    symbol_imports : (Jsir.Var.t * Jsir.Linkage_name.t) list;
    symbol_exports : (Jsir.Linkage_name.t * Jsir.Var.t) list;
    exits : Jsir.Addr.t list
        (** The blocks of the toplevel code (reachable without entering
            closures) ending in [Stop]: its normal exits. *)
  }

(* The variables of a unit are given the indices starting at [first_var], and
   its blocks the addresses starting at [first_addr]. Returns the unit and the
   first indices available to the next one. *)
let renumber (file : Jsir.jsir_file) ~first_var ~first_addr =
  let program = Jsir.Marshalable_program.to_program file.program in
  let max_var = ref (-1) in
  let var v =
    let i = Jsir.Var.idx v in
    if i > !max_var then max_var := i;
    Jsir.Var.of_idx (i + first_var)
  in
  let addr pc = pc + first_addr in
  let addr_map =
    Jsir.Addr.Map.fold
      (fun pc _ addr_map -> Jsir.Addr.Map.add pc (addr pc) addr_map)
      program.blocks Jsir.Addr.Map.empty
  in
  let blocks =
    Jsir.Addr.Map.fold
      (fun pc block blocks ->
        Jsir.Addr.Map.add (addr pc)
          (Jsir_subst.Including_Binders.And_Continuations.block addr_map var
             block)
          blocks)
      program.blocks Jsir.Addr.Map.empty
  in
  List.iter
    (fun (v, name) -> Jsir.Variable_name.set (var v) name)
    file.var_names;
  let symbol_imports =
    List.map (fun (v, symbol) -> var v, symbol) file.symbol_imports
  in
  let symbol_exports =
    List.map (fun (symbol, v) -> symbol, var v) file.symbol_exports
  in
  let start = addr program.start in
  let exits =
    Jsir.traverse
      { Jsir.fold = Jsir.fold_children }
      (fun pc exits ->
        match (Jsir.Addr.Map.find pc blocks).branch with
        | Stop -> pc :: exits
        | Return _ | Raise _ | Branch _ | Cond _ | Switch _ | Pushtrap _
        | Poptrap _ ->
          exits)
      start blocks []
  in
  let free_pc =
    if Jsir.Addr.Map.is_empty program.blocks
    then program.free_pc
    else max program.free_pc (fst (Jsir.Addr.Map.max_binding program.blocks) + 1)
  in
  ( { name = file.exported_compilation_unit;
      start;
      blocks;
      symbol_imports;
      symbol_exports;
      exits
    },
    first_var + !max_var + 1,
    first_addr + free_pc )

(* Removing instructions may create consecutive events, which JSOO doesn't
   like. *)
let rec remove_consecutive_events (body : Jsir.instr list) : Jsir.instr list =
  match body with
  | [] -> []
  | (Event _ as instr) :: body -> (
    let body = remove_consecutive_events body in
    match body with
    | Jsir.Event _ :: _ -> body
    | (Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _) :: _ | [] ->
      instr :: body)
  | ((Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _) as instr)
    :: body ->
    instr :: remove_consecutive_events body

let registers_symbol (instr : Jsir.instr) =
  match instr with
  | Let (_, Prim (Extern (prim, _), _)) ->
    String.equal prim "caml_register_symbol"
  | Let
      ( _,
        ( Prim
            ( (Vectlength _ | Array_get | Not | IsInt | Eq | Neq | Lt | Le | Ult),
              _ )
        | Apply _ | Block _ | Field _ | Closure _ | Constant _ | Special _ ) )
  | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
    false

let rec reachable_units (units : unit_ list) =
  match units with
  | [] -> []
  | unit_ :: next_units -> (
    match unit_.exits with
    | [] -> [unit_]
    | _ :: _ -> unit_ :: reachable_units next_units)

(* Maps each symbol to the name of the unit exporting it. *)
let exporters (units : unit_ list) =
  let exporters = Symbol_table.create 1024 in
  let add_export (unit_ : unit_) (symbol, _) =
    match Symbol_table.find_opt exporters symbol with
    | Some first_unit ->
      Misc.fatal_errorf
        "Jsir_linker.link: the symbol %s is defined by both %s and %s"
        (Jsir.Linkage_name.to_string symbol)
        first_unit unit_.name
    | None -> Symbol_table.add exporters symbol unit_.name
  in
  List.iter
    (fun (unit_ : unit_) -> List.iter (add_export unit_) unit_.symbol_exports)
    units;
  exporters

let unresolved_import ~exporters (unit_ : unit_) symbol =
  let name = Jsir.Linkage_name.to_string symbol in
  match Symbol_table.find_opt exporters symbol with
  | None ->
    Misc.fatal_errorf
      "Jsir_linker.link: %s refers to the symbol %s, which no linked \
       compilation unit defines"
      unit_.name name
  | Some exporter ->
    Misc.fatal_errorf
      "Jsir_linker.link: %s refers to the symbol %s of %s, which is linked \
       after it"
      unit_.name name exporter

(* Resolve the imports of a unit against the exports of the units before it.
   [available] holds the variables of the symbols that the units seen so far
   export, and [subst] maps the variables bound to imports to these. *)
let resolve_imports' ~exporters ~available subst (unit_ : unit_) =
  let resolve_import subst (v, symbol) =
    match Symbol_table.find_opt available symbol with
    | Some d -> Jsir.Var.Map.add v d subst
    | None -> unresolved_import ~exporters unit_ symbol
  in
  let subst = List.fold_left resolve_import subst unit_.symbol_imports in
  let add_export (symbol, d) =
    (* [d] may itself be bound to an import of this unit. *)
    Symbol_table.add available symbol (Jsir_subst.from_map subst d)
  in
  List.iter add_export unit_.symbol_exports;
  subst

(* Maps the variable bound to each import to the variable of the exporting
   unit. *)
let resolve_imports ~exporters (units : unit_ list) =
  List.fold_left
    (resolve_imports' ~exporters ~available:(Symbol_table.create 1024))
    Jsir.Var.Map.empty units

let link (files : Jsir.jsir_file list) =
  if List.compare_length_with files 0 = 0
  then Misc.fatal_error "Jsir_linker.link: no compilation units to link";
  let units, var_count, addr_count =
    List.fold_left
      (fun (units, first_var, first_addr) file ->
        let unit_, next_var, next_addr = renumber file ~first_var ~first_addr in
        unit_ :: units, next_var, next_addr)
      ([], 0, 0) files
  in
  let units = List.rev units in
  let exporters = exporters units in
  let units = reachable_units units in
  let subst = resolve_imports ~exporters units in
  (* Only keep instructions that do not involve the runtime symbol table. *)
  let keep_instr (instr : Jsir.instr) =
    match instr with
    | Let (v, _) when Jsir.Var.Map.mem v subst -> false
    | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
      not (registers_symbol instr)
  in
  let rec merge blocks (units : unit_ list) =
    match units with
    | [] -> blocks
    | unit_ :: next_units ->
      let next_start =
        match next_units with
        | [] -> None
        | next_unit :: _ -> Some next_unit.start
      in
      let exits = Jsir.Addr.Set.of_list unit_.exits in
      let blocks =
        Jsir.Addr.Map.fold
          (fun pc (block : Jsir.block) blocks ->
            let body =
              List.filter keep_instr block.body |> remove_consecutive_events
            in
            let branch : Jsir.last =
              match next_start with
              | Some next_start when Jsir.Addr.Set.mem pc exits ->
                Branch (next_start, [])
              | Some _ | None -> block.branch
            in
            Jsir.Addr.Map.add pc { block with body; branch } blocks)
          unit_.blocks blocks
      in
      merge blocks next_units
  in
  let blocks = merge Jsir.Addr.Map.empty units in
  let program : Jsir.program =
    { start = (List.hd units).start; blocks; free_pc = addr_count }
  in
  Jsir.Var.set_last (var_count - 1);
  Jsir_subst.Excluding_Binders.program (Jsir_subst.from_map subst) program
