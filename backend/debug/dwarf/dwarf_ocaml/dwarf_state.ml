(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare
open Asm_targets
open Dwarf_low
open Dwarf_high
module RS = Runtime_shape
module String = Misc.Stdlib.String

type function_range =
  { start_label : Asm_label.t;
    end_label : Asm_label.t;
    offset_past_end_label : int option;
    function_symbol : Asm_symbol.t
  }

type code_layout =
  | Continuous_code_section of
      { code_begin : Asm_symbol.t;
        code_end : Asm_symbol.t
      }
  | Function_sections

module Diagnostics = struct
  type variable_reduction =
    { shape_size_before_reduction_in_bytes : int;
      shape_size_after_reduction_in_bytes : int;
      shape_size_after_evaluation_in_bytes : int;
      reduction_steps : int;
      evaluation_steps : int;
      type_name : string;
      type_layout : Jkind_types.Sort.Const.t;
      dwarf_die_size : int;
      cms_files_loaded : int;
      cms_files_cached : int;
      cms_files_missing : string list;
      cms_files_unreadable : string list
    }

  type t = { mutable variables : variable_reduction list }
end

(* Context threaded through the translation of runtime shapes to DWARF DIEs: a
   cache of the DIEs generated so far and the recursive-variable environment. *)
module Die_gen_ctx = struct
  (* Recursive-variable environments, deduplicated for use as [Cache] keys. *)
  module Rec_var_env = struct
    include
      Hash_consed.Dedup
        (struct
          type t = Proto_die.reference RS.DeBruijn_env.t

          let hash = RS.DeBruijn_env.hash Asm_targets.Asm_label.hash

          let equal = RS.DeBruijn_env.equal Asm_targets.Asm_label.equal
        end)
        ()

    let empty table = create table RS.DeBruijn_env.empty

    let push table rec_env ref =
      modify table rec_env (fun env -> RS.DeBruijn_env.push env ref)

    let get_opt rec_env ~de_bruijn_index =
      RS.DeBruijn_env.get_opt (value rec_env) ~de_bruijn_index
  end

  module Cache = struct
    type key =
      { runtime_shape : RS.t;
        rec_env : Rec_var_env.t
      }

    module Tbl = Hashtbl.Make (struct
      type t = key

      let equal ({ runtime_shape = s1; rec_env = r1 } : t)
          ({ runtime_shape = s2; rec_env = r2 } : t) =
        RS.equal s1 s2 && Rec_var_env.equal r1 r2

      let hash { runtime_shape; rec_env } =
        Hashtbl.hash (RS.hash runtime_shape, Rec_var_env.hash rec_env)
    end)

    type t = Proto_die.reference Tbl.t

    let create ~initial_size = Tbl.create initial_size

    let find cache ~inp ~rec_env =
      Tbl.find_opt cache { runtime_shape = inp; rec_env }

    let add cache ~inp ~rec_env ~outp =
      Tbl.add cache { runtime_shape = inp; rec_env } outp
  end

  (* DWARF DIE cache for named type shapes. *)
  module Name_cache = struct
    type t = (RS.t * Proto_die.reference) String.Tbl.t

    let create ~initial_size = String.Tbl.create initial_size

    (* Search for the first unused suffix-numbered version of [name]. If we come
       across a type of the same name and runtime shape, then we simply reuse
       that reference. For name conflicts, we search for the next available
       suffix-numbered version of the name, [name/n]. *)
    let find_unused_name_or_cached t name runtime_shape :
        (Proto_die.reference, string) Either.t =
      let rec aux inc : _ Either.t =
        let name_suffix = if inc = 0 then "" else "/" ^ string_of_int inc in
        let name = name ^ name_suffix in
        match String.Tbl.find_opt t name with
        | Some (runtime_shape', reference) ->
          if RS.equal runtime_shape runtime_shape'
          then Left reference
          else aux (inc + 1)
        | None -> Right name
      in
      aux 0

    let add t name runtime_shape reference =
      String.Tbl.add t name (runtime_shape, reference)
  end

  type t =
    { cache : Cache.t;
      name_cache : Name_cache.t;
      rec_env_table : Rec_var_env.table
    }

  let create ~initial_size =
    { cache = Cache.create ~initial_size;
      name_cache = Name_cache.create ~initial_size;
      rec_env_table = Rec_var_env.create_table ~initial_size
    }

  let cache t = t.cache

  let name_cache t = t.name_cache

  let empty_rec_env t = Rec_var_env.empty t.rec_env_table

  let push_rec_binder t rec_env ref =
    Rec_var_env.push t.rec_env_table rec_env ref
end

type t =
  { compilation_unit_header_label : Asm_label.t;
    compilation_unit_proto_die : Proto_die.t;
    code_layout : code_layout;
    debug_loc_table : Debug_loc_table.t;
    debug_ranges_table : Debug_ranges_table.t;
    address_table : Address_table.t;
    location_list_table : Location_list_table.t;
    function_abstract_instances : (Proto_die.t * Asm_symbol.t) Asm_symbol.Tbl.t;
    mutable function_ranges : function_range list;
    get_file_num : string -> int;
    sourcefile : string;
    diagnostics : Diagnostics.t;
    complex_shape_cache : Complex_shape.Shape_cache.t;
    eval_context : Type_shape.Evaluated_shape.Eval_context.t;
    die_gen_ctx : Die_gen_ctx.t
  }

let create ~compilation_unit_header_label ~compilation_unit_proto_die
    ~code_layout debug_loc_table debug_ranges_table address_table
    location_list_table ~get_file_num ~sourcefile =
  { compilation_unit_header_label;
    compilation_unit_proto_die;
    code_layout;
    debug_loc_table;
    debug_ranges_table;
    address_table;
    location_list_table;
    function_abstract_instances = Asm_symbol.Tbl.create 42;
    function_ranges = [];
    get_file_num;
    sourcefile;
    diagnostics = { variables = [] };
    complex_shape_cache = Complex_shape.Shape_cache.create ~initial_size:100;
    eval_context =
      Type_shape.Evaluated_shape.Eval_context.create ~initial_size:256;
    die_gen_ctx = Die_gen_ctx.create ~initial_size:256
  }

let compilation_unit_header_label t = t.compilation_unit_header_label

let compilation_unit_proto_die t = t.compilation_unit_proto_die

let debug_loc_table t = t.debug_loc_table

let debug_ranges_table t = t.debug_ranges_table

let address_table t = t.address_table

let location_list_table t = t.location_list_table

let function_abstract_instances t = t.function_abstract_instances

let can_reference_dies_across_units _t = true

let get_file_num t filename = t.get_file_num filename

let sourcefile t = t.sourcefile

let diagnostics t = t.diagnostics

let complex_shape_cache t = t.complex_shape_cache

let eval_context t = t.eval_context

let die_gen_ctx t = t.die_gen_ctx

let add_variable_reduction_diagnostic t diagnostic =
  t.diagnostics.variables <- diagnostic :: t.diagnostics.variables

let code_layout t = t.code_layout

let function_ranges t = List.rev t.function_ranges

let record_function_range t ~function_symbol ~start_label ~end_label
    ~offset_past_end_label =
  match t.code_layout with
  | Function_sections ->
    let range =
      { start_label; end_label; offset_past_end_label; function_symbol }
    in
    t.function_ranges <- range :: t.function_ranges
  | Continuous_code_section _ ->
    (* Function ranges are not needed for continuous code *)
    ()

module Debug = struct
  let log f =
    match Sys.getenv "DWARF_DEBUG" with
    | exception Not_found -> Format.ifprintf Format.err_formatter f
    | _ -> Format.eprintf f
end
