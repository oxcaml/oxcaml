(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Unlike most of the rest of Flambda 2, this file depends on ocamloptcomp,
   meaning it can call [Compilenv]. *)

let get_module_info comp_unit =
  let cmx_name = Compilation_unit.name comp_unit in
  (* Typing information for predefined exceptions should be populated directly
     by the callee. *)
  if Compilation_unit.Name.equal cmx_name Compilation_unit.Name.predef_exn
  then
    Misc.fatal_error
      "get_global_info is not for use with predefined exception compilation \
       units";
  if
    Compilation_unit.Name.equal cmx_name
      (Flambda2_identifiers.Symbol.external_symbols_compilation_unit ()
      |> Compilation_unit.name)
  then None
  else Compilenv.get_unit_export_info comp_unit

let dump_to_target_if_any main_dump_ppf target ~header ~f a =
  match (target : Flambda_features.dump_target) with
  | Nowhere -> ()
  | Main_dump_stream ->
    Format.fprintf main_dump_ppf "\n%t%s:%t@ %a@." Flambda_colours.each_file
      header Flambda_colours.pop f a
  | File filename ->
    Misc.protect_output_to_file filename (fun out ->
        let ppf = Format.formatter_of_out_channel out in
        f ppf a;
        Format.pp_print_flush ppf ())

let dump_if_enabled ppf enabled ~header ~f a =
  let target : Flambda_features.dump_target =
    if enabled then Main_dump_stream else Nowhere
  in
  dump_to_target_if_any ppf target ~header ~f a

let pp_flambda_as_fexpr ppf unit =
  Print_fexpr.flambda_unit ppf (unit |> Flambda_to_fexpr.conv)

let dump_fexpr_annot ~prefixname suffix unit =
  let dump =
    Flambda_features.dump_fexpr_annot ()
    || List.exists (String.equal suffix)
         (Flambda_features.dump_fexpr_annot_after ())
  in
  if dump
  then
    Misc.protect_output_to_file
      (prefixname ^ "." ^ suffix ^ ".fl")
      (fun out ->
        let ppf = Format.formatter_of_out_channel out in
        pp_flambda_as_fexpr ppf unit;
        Format.pp_print_flush ppf ())

let print_rawflambda ppf unit =
  dump_if_enabled ppf
    (Flambda_features.dump_rawflambda ())
    ~header:"After CPS conversion" ~f:Flambda_unit.print unit;
  dump_to_target_if_any ppf
    (Flambda_features.dump_rawfexpr ())
    ~header:"After CPS conversion" ~f:pp_flambda_as_fexpr unit

let print_flambda name condition ppf unit =
  let header = "After " ^ name in
  dump_if_enabled ppf condition ~header ~f:Flambda_unit.print unit

let print_fexpr name target ppf unit =
  let header = "After " ^ name in
  dump_to_target_if_any ppf target ~header ~f:pp_flambda_as_fexpr unit

module NO = Flambda2_nominal.Name_occurrences

type run_result =
  { cmx : Flambda_cmx_format.raw option;
    unit : Flambda_unit.t;
    all_code : Exported_code.t;
    exported_offsets : Exported_offsets.t;
    used_value_slots : Flambda2_identifiers.Value_slot.Set.t;
    reachable_names : NO.t
  }

let build_run_result unit ~prepare_cmx ~all_code
    ({ used_value_slots; exported_offsets } : Slot_offsets.result) : run_result
    =
  let module_symbol = Flambda_unit.module_symbol unit in
  let reachable_names, cmx =
    prepare_cmx ~module_symbol ~used_value_slots ~exported_offsets all_code
  in
  { cmx; unit; all_code; exported_offsets; used_value_slots; reachable_names }

type flambda_result =
  { flambda : Flambda_unit.t;
    all_code : Exported_code.t;
    offsets : Exported_offsets.t;
    reachable_names : NO.t
  }

let finalize_offsets ~free_names ~all_code slot_offsets =
  Slot_offsets.finalize_offsets_from_free_names slot_offsets
    ~get_code_metadata:(Exported_code.get_code_metadata all_code)
    ~free_names

let run_reaper ~ppf ~prefixname ~machine_width ~cmx_loader ~all_code
    ~final_typing_env ~free_names flambda =
  let ((flambda, _, _, _) as result) =
    Profile.record_call ~accumulate:true "reaper" (fun () ->
        Flambda2_reaper.Reaper.run ~machine_width ~cmx_loader ~all_code
          ~final_typing_env ~free_names flambda)
  in
  print_flambda "reaper" (Flambda_features.dump_reaper ()) ppf flambda;
  print_fexpr "reaper"
    (Flambda_features.dump_fexpr (This_pass "reaper"))
    ppf flambda;
  dump_fexpr_annot ~prefixname "reaper" flambda;
  Compiler_hooks.execute Reaped_flambda2 flambda;
  result

let compilation_unit_callbacks = ref []

let register_compilation_unit_callback f =
  compilation_unit_callbacks := f :: !compilation_unit_callbacks

let invoke_compilation_unit_callbacks res =
  List.iter (( |> ) res) !compilation_unit_callbacks;
  compilation_unit_callbacks := []

module Reaper_mode = struct
  (* CR mvellacott: in the future it would be nice to allow running the Reaper
     on the present unit and supporting LTO at the same time, but at the moment
     it isn't safe to run the Reaper twice on the same code. *)
  type t =
    | Single_unit_run
    | Lto_support
    | Disabled

  let of_flags () =
    if Flambda_features.support_lto ()
    then Lto_support
    else if Flambda_features.enable_reaper ()
    then Single_unit_run
    else Disabled
end

let reaper_oclassic = Oxcaml_args.Extra_options.bool __LOC__ "reaper-oclassic"

let flambda_to_flambda0 : type m.
    ppf_dump:Format.formatter ->
    prefixname:string ->
    cmx_loader:Flambda_cmx.loader ->
    machine_width:Target_system.Machine_width.t ->
    mode:m Flambda_features.mode ->
    close_prog_metadata:m Closure_conversion.close_program_metadata ->
    code_slot_offsets:Slot_offsets.t Flambda2_identifiers.Code_id.Map.t ->
    sections:File_sections.Builder.t ->
    Flambda_unit.t ->
    flambda_result =
 fun ~ppf_dump:ppf ~prefixname ~cmx_loader ~machine_width ~mode
     ~close_prog_metadata ~code_slot_offsets ~sections raw_flambda ->
  Compiler_hooks.execute Raw_flambda2 raw_flambda;
  print_rawflambda ppf raw_flambda;
  dump_fexpr_annot ~prefixname "raw" raw_flambda;
  let flambda, all_code, slot_offsets, prepare_cmx, last_pass_name, cmr_payload
      =
    match mode, close_prog_metadata with
    | Classic, Classic (all_code, approxs, free_names, slot_offsets) ->
      (if Flambda_features.inlining_report ()
       then
         let output_prefix = prefixname ^ ".cps_conv" in
         let inlining_tree =
           Inlining_report.output_then_forget_decisions ~output_prefix
         in
         Compiler_hooks.execute Inlining_tree inlining_tree);
      if Flambda_features.enable_reaper () && reaper_oclassic ()
      then
        (* The reaper needs to rewrite the typing environment, so we need to
           convert the value approximations to a real typing environment. *)
        let final_typing_env =
          Flambda2_types.Typing_env.create_from_closure_conversion_approx
            ~machine_width
            ~resolver:(Flambda_cmx.load_cmx_file_contents cmx_loader)
            approxs
        in
        let flambda, all_code, slot_offsets, final_typing_env =
          run_reaper ~ppf ~prefixname ~machine_width ~cmx_loader ~all_code
            ~final_typing_env:(Some final_typing_env) ~free_names raw_flambda
        in
        let prepare_cmx ~module_symbol ~used_value_slots ~exported_offsets
            all_code =
          Flambda_cmx.prepare_cmx_file_contents ~final_typing_env ~module_symbol
            ~used_value_slots ~exported_offsets ~sections all_code
        in
        flambda, all_code, slot_offsets, prepare_cmx, "reaper", None
      else
        let slot_offsets =
          finalize_offsets ~free_names ~all_code slot_offsets
        in
        let prepare_cmx ~module_symbol ~used_value_slots ~exported_offsets
            all_code =
          Flambda_cmx.prepare_cmx_from_approx ~machine_width ~approxs
            ~module_symbol ~exported_offsets ~used_value_slots ~sections
            all_code
        in
        raw_flambda, all_code, slot_offsets, prepare_cmx, "raw", None
    | Normal, Normal ->
      let round = 0 in
      let { Simplify.free_names;
            final_typing_env;
            all_code;
            slot_offsets;
            unit = flambda
          } =
        Profile.record_call ~accumulate:true "simplify" (fun () ->
            Simplify.run ~cmx_loader ~machine_width ~round ~code_slot_offsets
              raw_flambda)
      in
      (if Flambda_features.inlining_report ()
       then
         let output_prefix = Printf.sprintf "%s.%d" prefixname round in
         let inlining_tree =
           Inlining_report.output_then_forget_decisions ~output_prefix
         in
         Compiler_hooks.execute Inlining_tree inlining_tree);
      Compiler_hooks.execute Flambda2 flambda;
      let last_pass_name = "simplify" in
      print_flambda last_pass_name
        (Flambda_features.dump_simplify ())
        ppf flambda;
      print_fexpr "simplify"
        (Flambda_features.dump_fexpr (This_pass "simplify"))
        ppf flambda;
      dump_fexpr_annot ~prefixname "simplify" flambda;
      let ( (flambda, all_code, slot_offsets, final_typing_env),
            last_pass_name,
            cmr_payload ) =
        match Reaper_mode.of_flags () with
        | Disabled ->
          let slot_offsets =
            finalize_offsets ~free_names ~all_code slot_offsets
          in
          ( (flambda, all_code, slot_offsets, final_typing_env),
            last_pass_name,
            None )
        | Single_unit_run ->
          let result =
            run_reaper ~ppf ~prefixname ~machine_width ~cmx_loader ~all_code
              ~final_typing_env ~free_names flambda
          in
          result, "reaper", None
        | Lto_support ->
          let deps, slot_offsets_inputs, solve_inputs, rebuild_data =
            Flambda2_reaper.Reaper.Staged.traverse ~free_names ~cmx_loader
              ~all_code ~closed_world:true flambda
          in
          let cmr_payload =
            Some
              { Flambda2_reaper.Cmr_format.unit_metadata =
                  Flambda_unit.metadata flambda;
                final_typing_env;
                all_code;
                imported_offsets = Exported_offsets.imported_offsets ();
                deps;
                slot_offsets_inputs;
                solve_inputs;
                rebuild_data
              }
          in
          let slot_offsets =
            finalize_offsets ~free_names ~all_code slot_offsets
          in
          ( (flambda, all_code, slot_offsets, final_typing_env),
            last_pass_name,
            cmr_payload )
      in
      let prepare_cmx ~module_symbol ~used_value_slots ~exported_offsets
          all_code =
        Flambda_cmx.prepare_cmx_file_contents ~final_typing_env ~module_symbol
          ~used_value_slots ~exported_offsets ~sections all_code
      in
      flambda, all_code, slot_offsets, prepare_cmx, last_pass_name, cmr_payload
  in
  print_flambda last_pass_name (Flambda_features.dump_flambda ()) ppf flambda;
  print_fexpr last_pass_name (Flambda_features.dump_fexpr Last_pass) ppf flambda;
  let { unit = flambda;
        exported_offsets;
        cmx;
        all_code;
        used_value_slots;
        reachable_names
      } =
    build_run_result flambda ~all_code slot_offsets ~prepare_cmx
  in
  Option.iter
    (Flambda2_reaper.Cmr_format.save ~filename:(prefixname ^ ".cmr")
       ~used_value_slots)
    cmr_payload;
  (match cmx with
  | None ->
    () (* Either opaque was passed, or there is no need to export offsets *)
  | Some cmx -> Compilenv.set_export_info cmx);
  { flambda; offsets = exported_offsets; reachable_names; all_code }

let flambda_to_flambda ~ppf_dump ~prefixname ~machine_width ~code_slot_offsets
    (unit : Flambda_unit.t) =
  (* CR bclement: this does not seem like the right place to set this up. *)
  Misc.Style.setup (Flambda_features.colour ());
  let cmx_loader = Flambda_cmx.create_loader ~get_module_info in
  let mode, close_prog_metadata =
    match Flambda_features.mode () with
    | Mode Normal -> Flambda_features.Normal, Closure_conversion.Normal
    | Mode Classic ->
      Misc.fatal_error "Unsupported classic mode in standalone middle-end pass"
  in
  let sections = Compilenv.current_sections () in
  flambda_to_flambda0 ~ppf_dump ~prefixname ~cmx_loader ~machine_width ~mode
    ~close_prog_metadata ~code_slot_offsets ~sections unit

let lambda_to_flambda ~ppf_dump:ppf ~prefixname ~machine_width
    (program : Lambda.program) =
  let module_repr =
    Lambda.main_module_representation program.main_module_block_format
  in
  let compilation_unit = program.compilation_unit in
  let module_initializer = program.code in
  (* Make sure -linscan is enabled in classic mode. Doing this here to be sure
     it happens exactly when -Oclassic is in effect, which we don't know at CLI
     processing time because there may be an [@@@flambda_oclassic] or
     [@@@flambda_o3] attribute. *)
  if Flambda_features.classic_mode () then Clflags.use_linscan := true;
  Misc.Style.setup (Flambda_features.colour ());
  (* CR-someday mshinwell: Note for future WebAssembly work: this thing about
     the length of arrays will need fixing, I don't think it only applies to the
     Cmm translation.

     This is partially fixed now, but the float array optimization case for
     array length in the Cmm translation assumes the floats are word width. *)
  (* The Flambda 2 code won't currently operate on 32-bit hosts; see
     [Name_occurrences]. *)
  if Sys.word_size <> 64
  then Misc.fatal_error "Flambda 2 can only run on 64-bit hosts at present";
  (* At least one place in the Cmm translation code (for unboxed arrays) cannot
     cope with big-endian systems, and it seems unlikely any such systems will
     have to be supported in the future anyway. *)
  if Arch.big_endian
  then Misc.fatal_error "Flambda2 only supports little-endian hosts";
  (* When the float array optimisation is enabled, the length of an array needs
     to be computed differently according to the array kind, in the case where
     the width of a float is not equal to the machine word width (at present,
     this happens only on 32-bit targets). *)
  if
    Cmm_helpers.wordsize_shift <> Cmm_helpers.numfloat_shift
    && Flambda_features.flat_float_array ()
  then
    Misc.fatal_error
      "Cannot compile on targets where floats are not word-width when the \
       float array optimisation is enabled";
  let cmx_loader = Flambda_cmx.create_loader ~get_module_info in
  let (Mode mode) = Flambda_features.mode () in
  let sections = Compilenv.current_sections () in
  let { Closure_conversion.unit = raw_flambda;
        code_slot_offsets;
        metadata = close_prog_metadata
      } =
    Profile.record_call "lambda_to_flambda" (fun () ->
        Lambda_to_flambda.lambda_to_flambda ~mode ~machine_width
          ~big_endian:Arch.big_endian ~cmx_loader ~compilation_unit ~module_repr
          module_initializer)
  in
  invoke_compilation_unit_callbacks compilation_unit;
  flambda_to_flambda0 ~ppf_dump:ppf ~prefixname ~cmx_loader ~machine_width ~mode
    ~close_prog_metadata ~code_slot_offsets ~sections raw_flambda

let reset_symbol_tables () =
  Compilenv.reset_info_tables ();
  Flambda2_identifiers.Continuation.reset ();
  Flambda2_identifiers.Int_ids.reset ()

let flambda_result_to_cmm ~keep_symbol_tables
    ({ flambda; all_code; offsets; reachable_names } : flambda_result) =
  let cmm =
    Flambda2_to_cmm.To_cmm.unit flambda ~all_code ~offsets ~reachable_names
  in
  if not keep_symbol_tables then reset_symbol_tables ();
  cmm

let lambda_to_cmm ~ppf_dump ~prefixname ~machine_width ~keep_symbol_tables
    (program : Lambda.program) =
  let run () =
    lambda_to_flambda ~ppf_dump ~prefixname ~machine_width program
    |> flambda_result_to_cmm ~keep_symbol_tables
  in
  Profile.record_call "flambda2" run

let reaper_lto_solve ~cmr_files ~ltosol_file =
  (* ID stamp counters are process-global monotonically increasing counters that
     give us an easy way of creating fresh identifiers. These identifiers get
     persisted across processes, and we need to prevent collisions when this
     happens. There are two cases:

     (1) Different processes that operate on different compilation units,
     potentially in parallel. Collisions are prevented here by the fact that
     identifiers are scoped to compilation units, as (CU, number) pairs.

     (2) Different processes that operate on the same compilation units, which
     must always happen in sequence. Collisions are prevented here by saving
     stamp counters in the earlier processes and restoring them in the later
     processes.

     Here we're resuming from many processes that operated on different CUs, and
     we're operating on all of those CUs, so we need to restore stamp counters
     from all of those processes. However stamp counters are global for the
     process, not per-CU. To make this work, we take the maximum across all the
     processes we've resumed from. It is okay that this makes some unused stamps
     get jumped over, the important thing is that they increase monotonically.

     After we're done, rebuild processes will be created to do more work on the
     CUs we touched. To keep counters monotonically increasing, we need to save
     them after our work so that the rebuild processes can restore them. *)
  let cmrs, counters =
    List.split (List.map Flambda2_reaper.Cmr_format.load cmr_files)
  in
  Flambda2_reaper.Id_stamp_counters.restore_for_merge counters;
  let solve_data =
    List.map
      (fun cmr ->
        ( Flambda2_reaper.Cmr_format.Serialisable.compilation_unit cmr,
          Flambda2_reaper.Cmr_format.Serialisable.deserialise_for_solve cmr ))
      cmrs
  in
  (* Reference facts are part of the graph, so they also select the solution
     sections needed by rebuild. *)
  let participants =
    List.map
      (fun (participant, (graph, _, _, inputs)) ->
        ( participant,
          Compilation_unit.Set.union
            (Flambda2_reaper.Global_flow_graph.compilation_units graph)
            (Flambda2_reaper.Reaper.Staged.Solve_inputs
             .referenced_compilation_units inputs) ))
      solve_data
  in
  let combined_graph =
    List.fold_left
      (fun combined (_participant, (graph, _, _, _)) ->
        Flambda2_reaper.Global_flow_graph.union combined graph)
      (Flambda2_reaper.Global_flow_graph.create ())
      solve_data
  in
  let slot_offsets_inputs =
    List.fold_left
      (fun combined (_participant, (_, inputs, _, _)) ->
        Flambda2_reaper.Slot_offsets_analysis.Inputs.union combined inputs)
      Flambda2_reaper.Slot_offsets_analysis.Inputs.empty solve_data
  in
  let solve_inputs =
    List.map
      (fun (_participant, (_, _, _, solve_inputs)) -> solve_inputs)
      solve_data
  in
  let participant_units =
    Compilation_unit.Set.of_list (List.map fst participants)
  in
  let analysis_scope =
    Flambda2_reaper.Analysis.Scope.Lto_participants participant_units
  in
  (* Make the offsets of slots defined by units outside the solve available to
     [Slot_offsets.finalize_offsets]. The offsets of the participants' own slots
     are recomputed from the solution, so the stale ones stored in the .cmr
     files must not be imported. *)
  List.iter
    (fun (_participant, (_, _, imported_offsets, _)) ->
      Exported_offsets.import_offsets
        (Exported_offsets.filter_by_compilation_unit imported_offsets
           ~keep:(fun cu ->
             not
               (Flambda2_reaper.Analysis.Scope.contains_unit analysis_scope cu))))
    solve_data;
  let solution, slot_offsets =
    Flambda2_reaper.Reaper.Staged.solve ~slot_offsets_inputs ~analysis_scope
      ~solve_inputs combined_graph
  in
  Flambda2_reaper.Ltosol_format.save ~filename:ltosol_file ~participants
    ~solution ~slot_offsets

let reaped_flambda2_to_cmm ~ppf_dump:_ ~prefixname:_ ~machine_width
    ~keep_symbol_tables ~ltosol_filename ~cmr_filename =
  let ltosol = Flambda2_reaper.Ltosol_format.load ltosol_filename in
  let id_stamp_counters =
    Flambda2_reaper.Ltosol_format.id_stamp_counters ltosol
  in
  Flambda2_reaper.Id_stamp_counters.restore_for_resume id_stamp_counters;
  (* We expect the stamp counters in the .cmr file to be less than the counters
     in the .ltosol file, because the -reaper-solve invocation begins by taking
     the maximum counters across the .cmr files it reads. Therefore, we can
     ignore these counters. *)
  let cmr_serialisable, cmr_stamp_counters =
    Flambda2_reaper.Cmr_format.load cmr_filename
  in
  if
    Flambda2_reaper.Id_stamp_counters.any_greater_than cmr_stamp_counters
      id_stamp_counters
  then
    Misc.fatal_error
      "The rebuild data contains ID stamp counters greater than those in the \
       the solution file. Stamp counter monotonicity is broken.";
  let cmx_loader = Flambda_cmx.create_loader ~get_module_info in
  let { Flambda2_reaper.Cmr_format.unit_metadata;
        final_typing_env;
        all_code;
        imported_offsets;
        deps = _;
        slot_offsets_inputs = _;
        solve_inputs;
        rebuild_data
      } =
    Flambda2_reaper.Cmr_format.Serialisable.deserialise ~machine_width
      ~resolver:(Flambda_cmx.load_cmx_file_contents cmx_loader)
      cmr_serialisable
  in
  (* Make the paused compilation's imported offsets available for re-export in
     [Flambda_cmx.prepare_cmx_file_contents]. The offsets of the units
     participating in the solve come from the solution file instead. *)
  Exported_offsets.import_offsets imported_offsets;
  (* CR mvellacott: add profiling and debug printing code. *)
  let solution =
    let member =
      Flambda2_identifiers.Symbol.compilation_unit
        (Flambda_unit.Metadata.module_symbol unit_metadata)
    in
    Flambda2_reaper.Ltosol_format.solution_for_members ltosol ~members:[member]
  in
  (* CR sspies: These are the whole-program slot offsets (and used value slot
     set) computed at solve time, so every rebuilt unit exports the entire table
     in its .cmx, and value-slot pruning of the exported typing env uses the
     whole-program set. This is correct but conservative; in the future we may
     want to restrict both to the slots the unit actually references. *)
  let slot_offsets = Flambda2_reaper.Ltosol_format.slot_offsets ltosol in
  let participant_units =
    Compilation_unit.Set.of_list
      (Flambda2_reaper.Ltosol_format.participants ltosol)
  in
  let flambda, all_code, final_typing_env =
    Flambda2_reaper.Reaper.Staged.rebuild ~unit_metadata
      ~traverse_rebuild:rebuild_data ~solution ~code_deps_for_result_types:None
      ~all_sets_of_closures:
        solve_inputs
          .Flambda2_reaper.Reaper.Staged.Solve_inputs.all_sets_of_closures
      ~machine_width ~cmx_loader ~all_code ~final_typing_env
  in
  let { unit = flambda;
        exported_offsets = offsets;
        cmx;
        all_code;
        used_value_slots = _;
        reachable_names
      } =
    let prepare_cmx ~module_symbol ~used_value_slots ~exported_offsets all_code
        =
      (* CR sspies: Offsets of participants' slots are not re-exported here;
         they must come from the solution file's [slot_offsets] instead of the
         (stale) imported offsets. A participant slot that occurs only in
         exported code metadata or the typing env, but not in the used slots
         seen by the solve, is therefore missing from the .reaped.cmx. This is
         fine while .reaped.cmx files are only used for linking, but must be
         revisited if rebuilds were to consume each other's metadata. *)
      Flambda_cmx.prepare_cmx_file_contents
        ~is_local_compilation_unit:(fun cu ->
          Compilation_unit.Set.mem cu participant_units)
        ~final_typing_env ~module_symbol ~used_value_slots
        ~exported_offsets
          (* Pass a mutable reference to the (currently empty) list of .cmx
             sections so that the sections created here get appended. *)
        ~sections:(Compilenv.current_sections ())
        all_code
    in
    build_run_result flambda ~all_code slot_offsets ~prepare_cmx
  in
  Option.iter Compilenv.set_export_info cmx;
  Compiler_hooks.execute Reaped_flambda2 flambda;
  flambda_result_to_cmm ~keep_symbol_tables
    { flambda; all_code; offsets; reachable_names }
