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

  val empty : t

  (** Split by the compilation unit of each table's outermost key. Only units
      that key at least one fact are present in the result. *)
  val partition_by_compilation_unit : t -> t Compilation_unit.Map.t

  (** Union of tables whose key sets are disjoint, as produced by
      [partition_by_compilation_unit]. *)
  val disjoint_union : t -> t -> t
end = struct
  (* We include only the tables that rebuild needs, not everything from the
     Datalog database. *)
  type t =
    { constructor : Serialisation.Nfn.t;
      parameter : Serialisation.Ncn.t;
      code_id_my_closure : Serialisation.Nn.t;
      any_usage : Serialisation.N.t;
      any_source : Serialisation.N.t;
      usages : Serialisation.Nn.t;
      sources : Serialisation.Nn.t;
      rev_accessor : Serialisation.Nfn.t;
      has_usage : Serialisation.N.t;
      has_source : Serialisation.N.t;
      field_of_constructor_is_used : Serialisation.Nf.t;
      field_of_constructor_is_used_top : Serialisation.Nf.t;
      field_of_constructor_is_used_as : Serialisation.Nfn.t;
      allocation_point_dominator : Serialisation.Nn.t;
      cannot_change_calling_convention : Serialisation.N.t
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
    Datalog.empty
    |> Datalog.set_table Global_flow_graph.constructor constructor
    |> Datalog.set_table Global_flow_graph.parameter parameter
    |> Datalog.set_table Global_flow_graph.code_id_my_closure code_id_my_closure
    |> Datalog.set_table Global_flow_graph.any_usage any_usage
    |> Datalog.set_table Global_flow_graph.any_source any_source
    |> Datalog.set_table Points_to_analysis.Relations.usages_table usages
    |> Datalog.set_table Points_to_analysis.Relations.sources_table sources
    |> Datalog.set_table Points_to_analysis.Relations.rev_accessor_table
         rev_accessor
    |> Datalog.set_table Points_to_analysis.Relations.has_usage_table has_usage
    |> Datalog.set_table Points_to_analysis.Relations.has_source_table
         has_source
    |> Datalog.set_table
         Points_to_analysis.Relations.field_of_constructor_is_used_tbl
         field_of_constructor_is_used
    |> Datalog.set_table
         Points_to_analysis.Relations.field_of_constructor_is_used_top_table
         field_of_constructor_is_used_top
    |> Datalog.set_table
         Points_to_analysis.Relations.field_of_constructor_is_used_as_table
         field_of_constructor_is_used_as
    |> Datalog.set_table
         Points_to_analysis.Relations.allocation_point_dominator_table
         allocation_point_dominator
    |> Datalog.set_table
         Unboxing_analysis.cannot_change_calling_convention_table
         cannot_change_calling_convention

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
    let ids = Serialisation.Nfn.add_ids constructor ids in
    let ids = Serialisation.Ncn.add_ids parameter ids in
    let ids = Serialisation.Nn.add_ids code_id_my_closure ids in
    let ids = Serialisation.N.add_ids any_usage ids in
    let ids = Serialisation.N.add_ids any_source ids in
    let ids = Serialisation.Nn.add_ids usages ids in
    let ids = Serialisation.Nn.add_ids sources ids in
    let ids = Serialisation.Nfn.add_ids rev_accessor ids in
    let ids = Serialisation.N.add_ids has_usage ids in
    let ids = Serialisation.N.add_ids has_source ids in
    let ids = Serialisation.Nf.add_ids field_of_constructor_is_used ids in
    let ids = Serialisation.Nf.add_ids field_of_constructor_is_used_top ids in
    let ids = Serialisation.Nfn.add_ids field_of_constructor_is_used_as ids in
    let ids = Serialisation.Nn.add_ids allocation_point_dominator ids in
    let ids = Serialisation.N.add_ids cannot_change_calling_convention ids in
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
    let fields = Serialisation.Nfn.add_fields constructor fields in
    let fields = Serialisation.Nfn.add_fields rev_accessor fields in
    let fields =
      Serialisation.Nf.add_fields field_of_constructor_is_used fields
    in
    let fields =
      Serialisation.Nf.add_fields field_of_constructor_is_used_top fields
    in
    let fields =
      Serialisation.Nfn.add_fields field_of_constructor_is_used_as fields
    in
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
    { constructor =
        Serialisation.Nfn.rename constructor ~rename_id ~rename_field;
      parameter = Serialisation.Ncn.rename parameter ~rename_id;
      code_id_my_closure = Serialisation.Nn.rename code_id_my_closure ~rename_id;
      any_usage = Serialisation.N.rename any_usage ~rename_id;
      any_source = Serialisation.N.rename any_source ~rename_id;
      usages = Serialisation.Nn.rename usages ~rename_id;
      sources = Serialisation.Nn.rename sources ~rename_id;
      rev_accessor =
        Serialisation.Nfn.rename rev_accessor ~rename_id ~rename_field;
      has_usage = Serialisation.N.rename has_usage ~rename_id;
      has_source = Serialisation.N.rename has_source ~rename_id;
      field_of_constructor_is_used =
        Serialisation.Nf.rename field_of_constructor_is_used ~rename_id
          ~rename_field;
      field_of_constructor_is_used_top =
        Serialisation.Nf.rename field_of_constructor_is_used_top ~rename_id
          ~rename_field;
      field_of_constructor_is_used_as =
        Serialisation.Nfn.rename field_of_constructor_is_used_as ~rename_id
          ~rename_field;
      allocation_point_dominator =
        Serialisation.Nn.rename allocation_point_dominator ~rename_id;
      cannot_change_calling_convention =
        Serialisation.N.rename cannot_change_calling_convention ~rename_id
    }

  let empty =
    { constructor = Code_id_or_name.Map.empty;
      parameter = Code_id_or_name.Map.empty;
      code_id_my_closure = Code_id_or_name.Map.empty;
      any_usage = Code_id_or_name.Map.empty;
      any_source = Code_id_or_name.Map.empty;
      usages = Code_id_or_name.Map.empty;
      sources = Code_id_or_name.Map.empty;
      rev_accessor = Code_id_or_name.Map.empty;
      has_usage = Code_id_or_name.Map.empty;
      has_source = Code_id_or_name.Map.empty;
      field_of_constructor_is_used = Code_id_or_name.Map.empty;
      field_of_constructor_is_used_top = Code_id_or_name.Map.empty;
      field_of_constructor_is_used_as = Code_id_or_name.Map.empty;
      allocation_point_dominator = Code_id_or_name.Map.empty;
      cannot_change_calling_convention = Code_id_or_name.Map.empty
    }

  let disjoint_union t1 t2 =
    let u m1 m2 = Code_id_or_name.Map.disjoint_union m1 m2 in
    { constructor = u t1.constructor t2.constructor;
      parameter = u t1.parameter t2.parameter;
      code_id_my_closure = u t1.code_id_my_closure t2.code_id_my_closure;
      any_usage = u t1.any_usage t2.any_usage;
      any_source = u t1.any_source t2.any_source;
      usages = u t1.usages t2.usages;
      sources = u t1.sources t2.sources;
      rev_accessor = u t1.rev_accessor t2.rev_accessor;
      has_usage = u t1.has_usage t2.has_usage;
      has_source = u t1.has_source t2.has_source;
      field_of_constructor_is_used =
        u t1.field_of_constructor_is_used t2.field_of_constructor_is_used;
      field_of_constructor_is_used_top =
        u t1.field_of_constructor_is_used_top
          t2.field_of_constructor_is_used_top;
      field_of_constructor_is_used_as =
        u t1.field_of_constructor_is_used_as t2.field_of_constructor_is_used_as;
      allocation_point_dominator =
        u t1.allocation_point_dominator t2.allocation_point_dominator;
      cannot_change_calling_convention =
        u t1.cannot_change_calling_convention
          t2.cannot_change_calling_convention
    }

  (* Mutable counterpart of [t], so that [partition_by_compilation_unit] can
     distribute each whole-program table in a single pass, without building an
     intermediate partition of each table first. *)
  type accumulator =
    { mutable constructor : Serialisation.Nfn.t;
      mutable parameter : Serialisation.Ncn.t;
      mutable code_id_my_closure : Serialisation.Nn.t;
      mutable any_usage : Serialisation.N.t;
      mutable any_source : Serialisation.N.t;
      mutable usages : Serialisation.Nn.t;
      mutable sources : Serialisation.Nn.t;
      mutable rev_accessor : Serialisation.Nfn.t;
      mutable has_usage : Serialisation.N.t;
      mutable has_source : Serialisation.N.t;
      mutable field_of_constructor_is_used : Serialisation.Nf.t;
      mutable field_of_constructor_is_used_top : Serialisation.Nf.t;
      mutable field_of_constructor_is_used_as : Serialisation.Nfn.t;
      mutable allocation_point_dominator : Serialisation.Nn.t;
      mutable cannot_change_calling_convention : Serialisation.N.t
    }

  let create_accumulator () : accumulator =
    { constructor = Code_id_or_name.Map.empty;
      parameter = Code_id_or_name.Map.empty;
      code_id_my_closure = Code_id_or_name.Map.empty;
      any_usage = Code_id_or_name.Map.empty;
      any_source = Code_id_or_name.Map.empty;
      usages = Code_id_or_name.Map.empty;
      sources = Code_id_or_name.Map.empty;
      rev_accessor = Code_id_or_name.Map.empty;
      has_usage = Code_id_or_name.Map.empty;
      has_source = Code_id_or_name.Map.empty;
      field_of_constructor_is_used = Code_id_or_name.Map.empty;
      field_of_constructor_is_used_top = Code_id_or_name.Map.empty;
      field_of_constructor_is_used_as = Code_id_or_name.Map.empty;
      allocation_point_dominator = Code_id_or_name.Map.empty;
      cannot_change_calling_convention = Code_id_or_name.Map.empty
    }

  let to_solution_tables
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
        accumulator) : t =
    { constructor;
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
    }

  let partition_by_compilation_unit (t : t) =
    let accumulator_by_unit : accumulator Compilation_unit.Tbl.t =
      Compilation_unit.Tbl.create 42
    in
    let accumulator_for_unit cu =
      match Compilation_unit.Tbl.find_opt accumulator_by_unit cu with
      | Some accumulator -> accumulator
      | None ->
        let accumulator = create_accumulator () in
        Compilation_unit.Tbl.add accumulator_by_unit cu accumulator;
        accumulator
    in
    (* Add each binding of one whole-program table to the same table of the
       section for the binding's compilation unit. *)
    let distribute_across_section_tables add table =
      Code_id_or_name.Map.iter
        (fun id value ->
          let accumulator =
            accumulator_for_unit (Code_id_or_name.compilation_unit id)
          in
          add accumulator id value)
        table
    in
    let add map id value = Code_id_or_name.Map.add id value map in
    distribute_across_section_tables
      (fun acc id value -> acc.constructor <- add acc.constructor id value)
      t.constructor;
    distribute_across_section_tables
      (fun acc id value -> acc.parameter <- add acc.parameter id value)
      t.parameter;
    distribute_across_section_tables
      (fun acc id value ->
        acc.code_id_my_closure <- add acc.code_id_my_closure id value)
      t.code_id_my_closure;
    distribute_across_section_tables
      (fun acc id value -> acc.any_usage <- add acc.any_usage id value)
      t.any_usage;
    distribute_across_section_tables
      (fun acc id value -> acc.any_source <- add acc.any_source id value)
      t.any_source;
    distribute_across_section_tables
      (fun acc id value -> acc.usages <- add acc.usages id value)
      t.usages;
    distribute_across_section_tables
      (fun acc id value -> acc.sources <- add acc.sources id value)
      t.sources;
    distribute_across_section_tables
      (fun acc id value -> acc.rev_accessor <- add acc.rev_accessor id value)
      t.rev_accessor;
    distribute_across_section_tables
      (fun acc id value -> acc.has_usage <- add acc.has_usage id value)
      t.has_usage;
    distribute_across_section_tables
      (fun acc id value -> acc.has_source <- add acc.has_source id value)
      t.has_source;
    distribute_across_section_tables
      (fun acc id value ->
        acc.field_of_constructor_is_used
          <- add acc.field_of_constructor_is_used id value)
      t.field_of_constructor_is_used;
    distribute_across_section_tables
      (fun acc id value ->
        acc.field_of_constructor_is_used_top
          <- add acc.field_of_constructor_is_used_top id value)
      t.field_of_constructor_is_used_top;
    distribute_across_section_tables
      (fun acc id value ->
        acc.field_of_constructor_is_used_as
          <- add acc.field_of_constructor_is_used_as id value)
      t.field_of_constructor_is_used_as;
    distribute_across_section_tables
      (fun acc id value ->
        acc.allocation_point_dominator
          <- add acc.allocation_point_dominator id value)
      t.allocation_point_dominator;
    distribute_across_section_tables
      (fun acc id value ->
        acc.cannot_change_calling_convention
          <- add acc.cannot_change_calling_convention id value)
      t.cannot_change_calling_convention;
    Compilation_unit.Tbl.fold
      (fun cu accumulator acc ->
        Compilation_unit.Map.add cu (to_solution_tables accumulator) acc)
      accumulator_by_unit Compilation_unit.Map.empty
end

(* The compilation units of all identifiers in [ids]. Constants and
   continuations are not scoped to compilation units. *)
let compilation_units_of_ids
    ({ symbols; variables; simples; consts = _; code_ids; continuations = _ } :
      Ids_for_export.t) =
  let acc = Compilation_unit.Set.empty in
  let acc =
    Symbol.Set.fold
      (fun symbol acc ->
        Compilation_unit.Set.add (Symbol.compilation_unit symbol) acc)
      symbols acc
  in
  let acc =
    Variable.Set.fold
      (fun var acc ->
        Compilation_unit.Set.add (Variable.compilation_unit var) acc)
      variables acc
  in
  let acc =
    Code_id.Set.fold
      (fun code_id acc ->
        Compilation_unit.Set.add (Code_id.get_compilation_unit code_id) acc)
      code_ids acc
  in
  Ids_for_export.Simple.Set.fold
    (fun simple acc ->
      Ids_for_export.Simple.pattern_match simple
        ~name:(fun name ~coercion:_ ->
          Compilation_unit.Set.add
            (Code_id_or_name.compilation_unit (Code_id_or_name.name name))
            acc)
        ~const:(fun _ -> acc))
    simples acc

(* The part of the solution whose outermost keys belong to one compilation unit,
   stored as its own file section so that rebuilds can read only the units they
   need. *)
module Shard : sig
  type t

  (** Also returns the fields the shard references (for the file-wide view list)
      and the compilation units of all identifiers it references (for computing
      which sections a rebuild needs). *)
  val create :
    solution_tables:Solution_tables.t ->
    unboxed_fields:Unboxing_analysis.unboxed Code_id_or_name.Map.t ->
    changed_representation:
      (Unboxing_analysis.changed_representation * Code_id_or_name.t)
      Code_id_or_name.Map.t ->
    code_changes:Unboxing_analysis.code_changes ->
    t * Field.Set.t * Compilation_unit.Set.t

  val deserialise :
    t ->
    rename_field:(Field.t -> Field.t) ->
    Solution_tables.t
    * Unboxing_analysis.unboxed Code_id_or_name.Map.t
    * (Unboxing_analysis.changed_representation * Code_id_or_name.t)
      Code_id_or_name.Map.t
    * Unboxing_analysis.code_changes
end = struct
  type t =
    { table_data : Flambda_cmx_format.table_data;
      solution_tables : Solution_tables.t;
      unboxed_fields : Unboxing_analysis.unboxed Code_id_or_name.Map.t;
      changed_representation :
        (Unboxing_analysis.changed_representation * Code_id_or_name.t)
        Code_id_or_name.Map.t;
      code_changes : Unboxing_analysis.code_changes
    }

  let create ~solution_tables ~unboxed_fields ~changed_representation
      ~code_changes =
    let ids = Solution_tables.ids_for_export solution_tables in
    let ids =
      Unboxing_analysis.unboxed_fields_ids_for_export unboxed_fields ids
    in
    let ids =
      Unboxing_analysis.changed_representation_ids_for_export
        changed_representation ids
    in
    let ids = Unboxing_analysis.code_changes_ids_for_export code_changes ids in
    let fields = Solution_tables.fields_for_export solution_tables in
    let fields =
      Unboxing_analysis.unboxed_fields_fields_for_export unboxed_fields fields
    in
    let fields =
      Unboxing_analysis.changed_representation_fields_for_export
        changed_representation fields
    in
    let fields =
      Unboxing_analysis.code_changes_fields_for_export code_changes fields
    in
    ( { table_data = Flambda_cmx_format.create_table_data ids;
        solution_tables;
        unboxed_fields;
        changed_representation;
        code_changes
      },
      fields,
      compilation_units_of_ids ids )

  let deserialise
      { table_data;
        solution_tables;
        unboxed_fields;
        changed_representation;
        code_changes
      } ~rename_field =
    (* [used_value_slots] and [original_compilation_unit] only drive value-slot
       pruning, which is only consulted when rewriting Flambda types, and the
       solution contains no types. [code_ids] is only needed by
       [Exported_code.apply_renaming], and the shards contain no code. *)
    let renaming, (_code_ids : Code_id.importer) =
      Flambda_cmx_format.import_renaming ~table_data
        ~used_value_slots:Value_slot.Set.empty
        ~original_compilation_unit:(Symbol.external_symbols_compilation_unit ())
    in
    let solution_tables =
      Solution_tables.apply_renaming solution_tables renaming ~rename_field
    in
    let unboxed_fields =
      Unboxing_analysis.unboxed_fields_apply_renaming unboxed_fields renaming
        ~rename_field
    in
    let changed_representation =
      Unboxing_analysis.changed_representation_apply_renaming
        changed_representation renaming ~rename_field
    in
    let code_changes =
      Unboxing_analysis.code_changes_apply_renaming code_changes renaming
        ~rename_field
    in
    solution_tables, unboxed_fields, changed_representation, code_changes
end

module Header = struct
  type t =
    { id_stamp_counters : Id_stamp_counters.t;
      (* Each participant is paired with the units its dependency graph
         references. [solution_for_members] starts from these when computing
         which sections a rebuild needs. *)
      participants : (Compilation_unit.t * Compilation_unit.Set.t) list;
      (* The units referenced by each section's serialised contents.
         [solution_for_members] takes the transitive closure of this
         relation. *)
      section_references : Compilation_unit.Set.t Compilation_unit.Map.t;
      (* Fields are hashconsed per-process, so the solution is stored with views
         of them in the style of [table_data]. One list serves all sections. *)
      field_views : Fields_for_export.t;
      (* One section per compilation unit that keys any fact (participant or
         not), in section order. *)
      index : (Compilation_unit.t * File_sections.Idx.t) list;
      section_toc : int array;
      (* The slot offsets computed from the solution for the sets of closures of
         all participants. Slots are not hashconsed, so the offsets can be
         stored as is. *)
      slot_offsets : Slot_offsets.result
    }
end

type t =
  { header : Header.t;
    sections : File_sections.t
  }

let id_stamp_counters t = t.header.Header.id_stamp_counters

let participants t = List.map fst t.header.Header.participants

let slot_offsets t = t.header.Header.slot_offsets

type error =
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string
  | Marshal_failed of string

exception Error of error

(* Split a map by the compilation unit of its (outermost) key. *)
let partition_by_cu map =
  Code_id_or_name.Map.fold
    (fun id value acc ->
      let cu = Code_id_or_name.compilation_unit id in
      Compilation_unit.Map.update cu
        (fun part ->
          let part = Option.value part ~default:Code_id_or_name.Map.empty in
          Some (Code_id_or_name.Map.add id value part))
        acc)
    map Compilation_unit.Map.empty

let save ~filename ~participants ~solution ~slot_offsets ~code_changes =
  let ({ db; unboxed_fields; changed_representation }
        : Unboxing_analysis.result) =
    solution
  in
  let tables_by_cu =
    Solution_tables.partition_by_compilation_unit
      (Solution_tables.of_database db)
  in
  let unboxed_by_cu = partition_by_cu unboxed_fields in
  let changed_by_cu = partition_by_cu changed_representation in
  let code_changes_by_cu =
    Unboxing_analysis.partition_code_changes_by_compilation_unit code_changes
  in
  (* Combine the four partitions into one map over the union of their key sets,
     with empty defaults. *)
  let shard_inputs =
    let all_units =
      Compilation_unit.Set.union
        (Compilation_unit.Set.union
           (Compilation_unit.Map.keys tables_by_cu)
           (Compilation_unit.Map.keys code_changes_by_cu))
        (Compilation_unit.Set.union
           (Compilation_unit.Map.keys unboxed_by_cu)
           (Compilation_unit.Map.keys changed_by_cu))
    in
    let find cu map ~default =
      Option.value (Compilation_unit.Map.find_opt cu map) ~default
    in
    Compilation_unit.Map.of_set
      (fun cu ->
        ( find cu tables_by_cu ~default:Solution_tables.empty,
          find cu unboxed_by_cu ~default:Code_id_or_name.Map.empty,
          find cu changed_by_cu ~default:Code_id_or_name.Map.empty,
          find cu code_changes_by_cu
            ~default:Unboxing_analysis.empty_code_changes ))
      all_units
  in
  let builder =
    File_sections.Builder.create (Compilation_unit.Map.cardinal shard_inputs)
  in
  let rev_index, fields, referenced_by_section =
    Compilation_unit.Map.fold
      (fun cu
           ( solution_tables,
             unboxed_fields,
             changed_representation,
             code_changes ) (rev_index, fields, referenced_by_section) ->
        let shard, shard_fields, referenced =
          Shard.create ~solution_tables ~unboxed_fields ~changed_representation
            ~code_changes
        in
        let idx = File_sections.Builder.add builder (Obj.repr shard) in
        ( (cu, idx) :: rev_index,
          Field.Set.union fields shard_fields,
          Compilation_unit.Map.add cu referenced referenced_by_section ))
      shard_inputs
      ([], Field.Set.empty, Compilation_unit.Map.empty)
  in
  let serialized_sections, section_toc, _sections_length =
    File_sections.serialize (File_sections.Builder.build builder)
  in
  (* We need to store ID stamp counters so that stamp-based ids created during
     rebuild don't conflict with the ones created during solve. *)
  let id_stamp_counters = Id_stamp_counters.save () in
  let header =
    { Header.id_stamp_counters;
      participants;
      section_references = referenced_by_section;
      field_views = Fields_for_export.export fields;
      index = List.rev rev_index;
      section_toc;
      slot_offsets
    }
  in
  let oc = open_out_bin filename in
  Misc.try_finally
    (fun () ->
      output_string oc Config.ltosol_magic_number;
      output_value oc (header : Header.t);
      Array.iter (output_string oc) serialized_sections)
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> raise (Error (Marshal_failed filename)))

let load filename =
  let ic = open_in_bin filename in
  (* On success the channel is passed to [File_sections.create] so that sections
     can be read lazily; it must not be closed here. *)
  try
    let magic = Config.ltosol_magic_number in
    let format_code = String.sub magic 0 9 in
    let buffer = really_input_string ic (String.length magic) in
    if String.equal buffer magic
    then
      let header =
        try (input_value ic : Header.t)
        with End_of_file | Failure _ -> raise (Error (Corrupted filename))
      in
      let first_section_offset = pos_in ic in
      let sections =
        File_sections.create header.Header.section_toc filename ic
          ~first_section_offset
      in
      { header; sections }
    else if String.starts_with ~prefix:format_code buffer
    then raise (Error (Wrong_version filename))
    else raise (Error (Wrong_format filename))
  with exn ->
    close_in_noerr ic;
    raise exn

let print_loaded_sections ~members ~total ~loaded =
  let pp_units ppf cus =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
      (fun ppf cu ->
        Format.pp_print_string ppf (Compilation_unit.full_path_as_string cu))
      ppf cus
  in
  Format.eprintf
    "@[<hov 2>ltosol sections for [@[<hov>%a@]]:@ loaded %d of %d:@ \
     [@[<hov>%a@]]@]@."
    pp_units members (List.length loaded) total pp_units loaded

let solution_for_members { header; sections } ~members =
  (* The sections the rebuild needs are the transitive closure of the
     [section_references] relation, starting from the units each member's own
     dependency graph references (plus the member itself).

     Two reference relations are needed because neither contains the other. For
     a participant [P], [referenced_by_graph(P)] (its [Header.participants]
     entry) is computed before the solve: the units whose identifiers appear in
     [P]'s code and graph. [section_references(P)] is computed after the solve:
     the units whose identifiers appear in the facts keyed by [P]. A fact is
     stored in the section of the unit that keys it, which is not always the
     unit whose rebuild reads the fact.

     The starting points must come from [referenced_by_graph] because the
     rebuild queries the solution directly about identifiers in the member's own
     code. For example, if [P] reads a field of a symbol [S] defined in unit
     [X], then [X ∈ referenced_by_graph(P)], and the rebuild queries about [S] —
     the [usages] of [S], [field_of_constructor_is_used (S, 0)], etc. — are
     keyed by [S] and answered by facts in [X]'s section. No fact keyed by [P]'s
     own identifiers needs to mention [S], so possibly [X ∉
     section_references(P)].

     The closure must follow [section_references] because the solve propagates
     identifiers across the whole program. For example, if a value defined in
     [P] flows through other units to a use in unit [Z], then the [usages] of
     [P]'s variable (a fact in [P]'s own section) contain a use-site identifier
     from [Z], so [Z ∈ section_references(P)] even though [Z ∉
     referenced_by_graph(P)]. Deserialising a section gives the rebuild such
     identifiers, and queries about them need the sections of their units.

     Absence of facts is meaningful to rebuild queries, so the closure must
     cover every unit whose identifiers the rebuild can encounter; a section
     outside it is then equivalent to one with no facts. *)
  let seeds =
    List.fold_left
      (fun seeds member ->
        match
          List.find_opt
            (fun (cu, _) -> Compilation_unit.equal cu member)
            header.Header.participants
        with
        | Some (_, referenced_by_graph) ->
          Compilation_unit.Set.add member
            (Compilation_unit.Set.union seeds referenced_by_graph)
        | None ->
          Misc.fatal_errorf "Unit %a is not a participant in the LTO solution"
            (Format_doc.compat Compilation_unit.print)
            member)
      Compilation_unit.Set.empty members
  in
  let needed =
    let rec visit cu visited =
      if Compilation_unit.Set.mem cu visited
      then visited
      else
        let visited = Compilation_unit.Set.add cu visited in
        match
          Compilation_unit.Map.find_opt cu header.Header.section_references
        with
        | None -> visited
        | Some referenced -> Compilation_unit.Set.fold visit referenced visited
    in
    Compilation_unit.Set.fold visit seeds Compilation_unit.Set.empty
  in
  let rename_field = Fields_for_export.import header.Header.field_views in
  let tables, unboxed_fields, changed_representation, code_changes, rev_loaded =
    List.fold_left
      (fun ( tables,
             unboxed_fields,
             changed_representation,
             code_changes,
             rev_loaded ) (cu, idx) ->
        if not (Compilation_unit.Set.mem cu needed)
        then
          ( tables,
            unboxed_fields,
            changed_representation,
            code_changes,
            rev_loaded )
        else
          let shard : Shard.t = Obj.obj (File_sections.get sections idx) in
          let shard_tables, shard_unboxed, shard_changed, shard_code_changes =
            Shard.deserialise shard ~rename_field
          in
          ( Solution_tables.disjoint_union tables shard_tables,
            Code_id_or_name.Map.disjoint_union unboxed_fields shard_unboxed,
            Code_id_or_name.Map.disjoint_union changed_representation
              shard_changed,
            Unboxing_analysis.code_changes_disjoint_union code_changes
              shard_code_changes,
            cu :: rev_loaded ))
      ( Solution_tables.empty,
        Code_id_or_name.Map.empty,
        Code_id_or_name.Map.empty,
        Unboxing_analysis.empty_code_changes,
        [] )
      header.Header.index
  in
  if Flambda_features.debug_reaper "sections"
  then
    print_loaded_sections ~members
      ~total:(List.length header.Header.index)
      ~loaded:(List.rev rev_loaded);
  ( { Unboxing_analysis.db = Solution_tables.to_database tables;
      unboxed_fields;
      changed_representation
    },
    code_changes )

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
