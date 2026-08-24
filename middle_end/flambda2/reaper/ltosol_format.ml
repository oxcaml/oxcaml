(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                   Miriam Vellacott, Jane Street Europe                 *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Datalog_helpers

module Solution_tables : sig
  type t

  val of_database : Datalog.database -> t

  val to_database : t -> Datalog.database

  val ids_for_export : t -> Ids_for_export.t

  val fields_for_export : t -> Field.Set.t

  val apply_renaming : t -> Renaming.t -> rename_field:(Field.t -> Field.t) -> t
end = struct
  (* We include only the tables that rebuild needs, not everything from the
     Datalog database. *)
  type t =
    { constructor : Maps.Nfn.t;
      parameter : Maps.Ncn.t;
      code_id_my_closure : Maps.Nn.t;
      any_usage : Maps.N.t;
      any_source : Maps.N.t;
      usages : Maps.Nn.t;
      sources : Maps.Nn.t;
      rev_accessor : Maps.Nfn.t;
      has_usage : Maps.N.t;
      has_source : Maps.N.t;
      field_of_constructor_is_used : Maps.Nf.t;
      field_of_constructor_is_used_top : Maps.Nf.t;
      field_of_constructor_is_used_as : Maps.Nfn.t;
      allocation_point_dominator : Maps.Nn.t;
      cannot_change_calling_convention : Maps.N.t
    }

  let of_database db : t =
    let get table = Datalog.get_table table db in
    { constructor = get Global_flow_graph.constructor;
      parameter = get Global_flow_graph.parameter;
      code_id_my_closure = get Global_flow_graph.code_id_my_closure;
      any_usage = get Global_flow_graph.any_usage;
      any_source = get Global_flow_graph.any_source;
      usages = get Points_to_analysis.Relations.usages_table;
      sources = get Points_to_analysis.Relations.sources_table;
      rev_accessor = get Points_to_analysis.Relations.rev_accessor_table;
      has_usage = get Points_to_analysis.Relations.has_usage_table;
      has_source = get Points_to_analysis.Relations.has_source_table;
      field_of_constructor_is_used =
        get Points_to_analysis.Relations.field_of_constructor_is_used_tbl;
      field_of_constructor_is_used_top =
        get Points_to_analysis.Relations.field_of_constructor_is_used_top_table;
      field_of_constructor_is_used_as =
        get Points_to_analysis.Relations.field_of_constructor_is_used_as_table;
      allocation_point_dominator =
        get Points_to_analysis.Relations.allocation_point_dominator_table;
      cannot_change_calling_convention =
        get Unboxing_analysis.cannot_change_calling_convention_table
    }

  let to_database
      ({ constructor;
         parameter;
         code_id_my_closure;
         any_usage;
         any_source;
         usages;
         sources;
         rev_accessor;
         has_usage;
         has_source;
         field_of_constructor_is_used;
         field_of_constructor_is_used_top;
         field_of_constructor_is_used_as;
         allocation_point_dominator;
         cannot_change_calling_convention
       } :
        t) =
    (* CR mvellacott: it would be nice to make reading a table that was not
       serialised a hard error in the future, instead of the empty result
       [Datalog.get_table] returns for missing tables. This is not
       straightforward because the solve code relies on getting empty results
       for tables that have no facts yet. *)
    Datalog.set_table Global_flow_graph.constructor constructor
    @@ Datalog.set_table Global_flow_graph.parameter parameter
    @@ Datalog.set_table Global_flow_graph.code_id_my_closure code_id_my_closure
    @@ Datalog.set_table Global_flow_graph.any_usage any_usage
    @@ Datalog.set_table Global_flow_graph.any_source any_source
    @@ Datalog.set_table Points_to_analysis.Relations.usages_table usages
    @@ Datalog.set_table Points_to_analysis.Relations.sources_table sources
    @@ Datalog.set_table Points_to_analysis.Relations.rev_accessor_table
         rev_accessor
    @@ Datalog.set_table Points_to_analysis.Relations.has_usage_table has_usage
    @@ Datalog.set_table Points_to_analysis.Relations.has_source_table
         has_source
    @@ Datalog.set_table
         Points_to_analysis.Relations.field_of_constructor_is_used_tbl
         field_of_constructor_is_used
    @@ Datalog.set_table
         Points_to_analysis.Relations.field_of_constructor_is_used_top_table
         field_of_constructor_is_used_top
    @@ Datalog.set_table
         Points_to_analysis.Relations.field_of_constructor_is_used_as_table
         field_of_constructor_is_used_as
    @@ Datalog.set_table
         Points_to_analysis.Relations.allocation_point_dominator_table
         allocation_point_dominator
    @@ Datalog.set_table
         Unboxing_analysis.cannot_change_calling_convention_table
         cannot_change_calling_convention
    @@ Datalog.empty

  let ids_for_export
      ({ constructor;
         parameter;
         code_id_my_closure;
         any_usage;
         any_source;
         usages;
         sources;
         rev_accessor;
         has_usage;
         has_source;
         field_of_constructor_is_used;
         field_of_constructor_is_used_top;
         field_of_constructor_is_used_as;
         allocation_point_dominator;
         cannot_change_calling_convention
       } :
        t) =
    let ids = Ids_for_export.empty in
    let ids = Maps.Nfn.add_ids constructor ids in
    let ids = Maps.Ncn.add_ids parameter ids in
    let ids = Maps.Nn.add_ids code_id_my_closure ids in
    let ids = Maps.N.add_ids any_usage ids in
    let ids = Maps.N.add_ids any_source ids in
    let ids = Maps.Nn.add_ids usages ids in
    let ids = Maps.Nn.add_ids sources ids in
    let ids = Maps.Nfn.add_ids rev_accessor ids in
    let ids = Maps.N.add_ids has_usage ids in
    let ids = Maps.N.add_ids has_source ids in
    let ids = Maps.Nf.add_ids field_of_constructor_is_used ids in
    let ids = Maps.Nf.add_ids field_of_constructor_is_used_top ids in
    let ids = Maps.Nfn.add_ids field_of_constructor_is_used_as ids in
    let ids = Maps.Nn.add_ids allocation_point_dominator ids in
    let ids = Maps.N.add_ids cannot_change_calling_convention ids in
    ids

  let fields_for_export
      ({ constructor;
         parameter = _;
         code_id_my_closure = _;
         any_usage = _;
         any_source = _;
         usages = _;
         sources = _;
         rev_accessor;
         has_usage = _;
         has_source = _;
         field_of_constructor_is_used;
         field_of_constructor_is_used_top;
         field_of_constructor_is_used_as;
         allocation_point_dominator = _;
         cannot_change_calling_convention = _
       } :
        t) =
    let fields = Field.Set.empty in
    let fields = Maps.Nfn.add_fields constructor fields in
    let fields = Maps.Nfn.add_fields rev_accessor fields in
    let fields = Maps.Nf.add_fields field_of_constructor_is_used fields in
    let fields = Maps.Nf.add_fields field_of_constructor_is_used_top fields in
    let fields = Maps.Nfn.add_fields field_of_constructor_is_used_as fields in
    fields

  let apply_renaming
      ({ constructor;
         parameter;
         code_id_my_closure;
         any_usage;
         any_source;
         usages;
         sources;
         rev_accessor;
         has_usage;
         has_source;
         field_of_constructor_is_used;
         field_of_constructor_is_used_top;
         field_of_constructor_is_used_as;
         allocation_point_dominator;
         cannot_change_calling_convention
       } :
        t) renaming ~rename_field : t =
    let rename_id = Renaming.apply_code_id_or_name renaming in
    { constructor = Maps.Nfn.rename constructor ~rename_id ~rename_field;
      parameter = Maps.Ncn.rename parameter ~rename_id;
      code_id_my_closure = Maps.Nn.rename code_id_my_closure ~rename_id;
      any_usage = Maps.N.rename any_usage ~rename_id;
      any_source = Maps.N.rename any_source ~rename_id;
      usages = Maps.Nn.rename usages ~rename_id;
      sources = Maps.Nn.rename sources ~rename_id;
      rev_accessor = Maps.Nfn.rename rev_accessor ~rename_id ~rename_field;
      has_usage = Maps.N.rename has_usage ~rename_id;
      has_source = Maps.N.rename has_source ~rename_id;
      field_of_constructor_is_used =
        Maps.Nf.rename field_of_constructor_is_used ~rename_id ~rename_field;
      field_of_constructor_is_used_top =
        Maps.Nf.rename field_of_constructor_is_used_top ~rename_id ~rename_field;
      field_of_constructor_is_used_as =
        Maps.Nfn.rename field_of_constructor_is_used_as ~rename_id ~rename_field;
      allocation_point_dominator =
        Maps.Nn.rename allocation_point_dominator ~rename_id;
      cannot_change_calling_convention =
        Maps.N.rename cannot_change_calling_convention ~rename_id
    }
end

module Serialisable_solution : sig
  type t

  val create : Unboxing_analysis.result -> t

  val deserialise : t -> Unboxing_analysis.result
end = struct
  (* Fields are hashconsed per-process, so the solution is stored with views of
     them in the style of [table_data]. *)
  type t =
    { table_data : Flambda_cmx_format.table_data;
      field_views : (Field.t * Field.view) list;
      solution_tables : Solution_tables.t;
      unboxed_fields : Unboxing_analysis.unboxed Code_id_or_name.Map.t;
      changed_representation :
        (Unboxing_analysis.changed_representation * Code_id_or_name.t)
        Code_id_or_name.Map.t
    }

  let create
      ({ db; unboxed_fields; changed_representation } :
        Unboxing_analysis.result) =
    let solution_tables = Solution_tables.of_database db in
    let ids = Solution_tables.ids_for_export solution_tables in
    let ids =
      Unboxing_analysis.unboxed_fields_ids_for_export unboxed_fields ids
    in
    let ids =
      Unboxing_analysis.changed_representation_ids_for_export
        changed_representation ids
    in
    let fields = Solution_tables.fields_for_export solution_tables in
    let fields =
      Unboxing_analysis.unboxed_fields_fields_for_export unboxed_fields fields
    in
    let fields =
      Unboxing_analysis.changed_representation_fields_for_export
        changed_representation fields
    in
    { table_data = Flambda_cmx_format.create_table_data ids;
      field_views = Field.export_views fields;
      solution_tables;
      unboxed_fields;
      changed_representation
    }

  let deserialise
      { table_data;
        field_views;
        solution_tables;
        unboxed_fields;
        changed_representation
      } : Unboxing_analysis.result =
    (* [used_value_slots] and [original_compilation_unit] only drive value-slot
       pruning, which is only consulted when rewriting Flambda types, and the
       solution contains no types. [code_ids] is only needed by
       [Exported_code.apply_renaming], and the solution contains no code. *)
    let renaming, (_code_ids : Code_id.importer) =
      Flambda_cmx_format.import_renaming ~table_data
        ~used_value_slots:Value_slot.Set.empty
        ~original_compilation_unit:(Symbol.external_symbols_compilation_unit ())
    in
    let rename_field = Field.import_views field_views in
    let db =
      Solution_tables.to_database
        (Solution_tables.apply_renaming solution_tables renaming ~rename_field)
    in
    let unboxed_fields =
      Unboxing_analysis.unboxed_fields_apply_renaming unboxed_fields renaming
        ~rename_field
    in
    let changed_representation =
      Unboxing_analysis.changed_representation_apply_renaming
        changed_representation renaming ~rename_field
    in
    { db; unboxed_fields; changed_representation }
end

module File_contents = struct
  type t =
    { id_stamp_counters : Id_stamp_counters.t;
      participants : Compilation_unit.t list;
      solution : Serialisable_solution.t
    }
end

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

let save ~filename ~participants ~solution =
  let solution = Serialisable_solution.create solution in
  (* We need to store ID stamp counters so that stamp-based ids created during
     rebuild don't conflict with the ones created during solve. *)
  let id_stamp_counters = Id_stamp_counters.save () in
  let file_contents =
    { File_contents.id_stamp_counters; participants; solution }
  in
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.ltosol_magic_number;
      output_value oc file_contents)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let load filename =
  let ic = open_in_bin filename in
  Misc.try_finally
    (fun () ->
      let magic = Config.ltosol_magic_number in
      let format_code = String.sub magic 0 9 in
      let buffer = really_input_string ic (String.length magic) in
      if String.equal buffer magic
      then
        try (input_value ic : File_contents.t) with
        | End_of_file | Failure _ -> raise (Error (Corrupted filename))
        | Error e -> raise (Error e)
      else if String.starts_with ~prefix:format_code buffer
      then raise (Error (Wrong_version filename))
      else raise (Error (Wrong_format filename)))
    ~always:(fun () -> close_in ic)

open Format_doc

let report_error ppf = function
  | Wrong_format filename ->
    fprintf ppf "Expected Ltosol format. Incompatible file %a"
      Location.Doc.quoted_filename filename
  | Wrong_version filename ->
    fprintf ppf "%a@ is not compatible with this version of OCaml"
      Location.Doc.quoted_filename filename
  | Corrupted filename ->
    fprintf ppf "Corrupted format@ %a" Location.Doc.quoted_filename filename
  | Marshal_failed filename ->
    fprintf ppf "Failed to marshal Ltosol to file@ %a"
      Location.Doc.quoted_filename filename

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
