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

let build_run_result unit ~free_names ~final_typing_env ~extra_static_roots
    ~extra_used_value_slots ~extra_used_function_slots ~sections ~all_code
    slot_offsets : run_result =
  let module_symbol = Flambda_unit.module_symbol unit in
  let function_slots_in_normal_projections =
    Flambda2_identifiers.Function_slot.Set.union
      (NO.function_slots_in_normal_projections free_names)
      extra_used_function_slots
  in
  let value_slots_in_normal_projections =
    Flambda2_identifiers.Value_slot.Set.union
      (NO.value_slots_in_normal_projections free_names)
      extra_used_value_slots
  in
  let all_function_slots =
    Flambda2_identifiers.Function_slot.Set.union
      (NO.all_function_slots_at_normal_mode free_names)
      extra_used_function_slots
  in
  let all_value_slots =
    Flambda2_identifiers.Value_slot.Set.union
      (NO.all_value_slots_at_normal_mode free_names)
      extra_used_value_slots
  in
  let ({ used_value_slots; exported_offsets } : Slot_offsets.result) =
    let used_slots : Slot_offsets.used_slots =
      { function_slots_in_normal_projections;
        all_function_slots;
        value_slots_in_normal_projections;
        all_value_slots
      }
    in
    let get_code_metadata code_id =
      Exported_code.find_exn all_code code_id |> Code_or_metadata.code_metadata
    in
    Slot_offsets.finalize_offsets slot_offsets ~get_code_metadata ~used_slots
      ~offsets_from_previous_assignment:Exported_offsets.empty
  in
  let reachable_names, cmx =
    Flambda_cmx.prepare_cmx_file_contents ~final_typing_env ~module_symbol
      ~extra_static_roots ~used_value_slots ~exported_offsets ~sections all_code
  in
  { cmx; unit; all_code; exported_offsets; used_value_slots; reachable_names }

type flambda_result =
  { flambda : Flambda_unit.t;
    all_code : Exported_code.t;
    offsets : Exported_offsets.t;
    reachable_names : NO.t
  }

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
  if Flambda_features.classic_mode ()
  then
    (* CR mvellacott: We want to allow classic mode again in the future. *)
    Misc.fatal_error "failed to prevent classic mode from being enabled";
  Compiler_hooks.execute Raw_flambda2 raw_flambda;
  print_rawflambda ppf raw_flambda;
  dump_fexpr_annot ~prefixname "raw" raw_flambda;
  let flambda, offsets, reachable_names, cmx, all_code =
    match mode, close_prog_metadata with
    | Classic, Classic (code, reachable_names, cmx, offsets) ->
      (if Flambda_features.inlining_report ()
       then
         let output_prefix = prefixname ^ ".cps_conv" in
         let inlining_tree =
           Inlining_report.output_then_forget_decisions ~output_prefix
         in
         Compiler_hooks.execute Inlining_tree inlining_tree);
      raw_flambda, offsets, reachable_names, cmx, code
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
      let ( flambda,
            free_names,
            all_code,
            slot_offsets,
            final_typing_env,
            last_pass_name,
            cmr_payload ) =
        match Reaper_mode.of_flags () with
        | Disabled ->
          ( flambda,
            free_names,
            all_code,
            slot_offsets,
            final_typing_env,
            last_pass_name,
            None )
        | Single_unit_run ->
          let flambda, free_names, all_code, slot_offsets, final_typing_env =
            Profile.record_call ~accumulate:true "reaper" (fun () ->
                Flambda2_reaper.Reaper.run ~machine_width ~cmx_loader ~all_code
                  ~final_typing_env flambda)
          in
          print_flambda "reaper" (Flambda_features.dump_reaper ()) ppf flambda;
          print_fexpr "reaper"
            (Flambda_features.dump_fexpr (This_pass "reaper"))
            ppf flambda;
          dump_fexpr_annot ~prefixname "reaper" flambda;
          ( flambda,
            free_names,
            all_code,
            slot_offsets,
            final_typing_env,
            "reaper",
            None )
        | Lto_support ->
          let deps, rebuild_data =
            Flambda2_reaper.Reaper.Staged.traverse flambda
          in
          let cmr_payload =
            Some
              { Flambda2_reaper.Cmr_format.unit_metadata =
                  Flambda_unit.metadata flambda;
                final_typing_env;
                all_code;
                deps;
                rebuild_data
              }
          in
          ( flambda,
            free_names,
            all_code,
            slot_offsets,
            final_typing_env,
            last_pass_name,
            cmr_payload )
      in
      print_flambda last_pass_name
        (Flambda_features.dump_flambda ())
        ppf flambda;
      print_fexpr last_pass_name
        (Flambda_features.dump_fexpr Last_pass)
        ppf flambda;
      let { unit = flambda;
            exported_offsets;
            cmx;
            all_code;
            used_value_slots;
            reachable_names
          } =
        build_run_result flambda ~free_names ~final_typing_env
          ~extra_static_roots:NO.empty
          ~extra_used_value_slots:Flambda2_identifiers.Value_slot.Set.empty
          ~extra_used_function_slots:
            Flambda2_identifiers.Function_slot.Set.empty ~sections ~all_code
          slot_offsets
      in
      Option.iter
        (Flambda2_reaper.Cmr_format.save ~filename:(prefixname ^ ".cmr")
           ~used_value_slots ~exported_offsets)
        cmr_payload;
      Compiler_hooks.execute Reaped_flambda2 flambda;
      flambda, exported_offsets, reachable_names, cmx, all_code
  in
  (match cmx with
  | None ->
    () (* Either opaque was passed, or there is no need to export offsets *)
  | Some cmx -> Compilenv.set_export_info cmx);
  { flambda; offsets; reachable_names; all_code }

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
          ~sections module_initializer)
  in
  invoke_compilation_unit_callbacks compilation_unit;
  flambda_to_flambda0 ~ppf_dump:ppf ~prefixname ~cmx_loader ~machine_width ~mode
    ~close_prog_metadata ~code_slot_offsets ~sections raw_flambda

let reset_symbol_tables () =
  Compilenv.reset_info_tables ();
  Flambda2_identifiers.Continuation.reset ();
  Flambda2_identifiers.Int_ids.reset ()

let flambda_result_to_cmm ~keep_symbol_tables ~localise_unreachable_symbols
    ({ flambda; all_code; offsets; reachable_names } : flambda_result) =
  let cmm =
    Flambda2_to_cmm.To_cmm.unit flambda ~all_code ~offsets ~reachable_names
      ~localise_unreachable_symbols
  in
  if not keep_symbol_tables then reset_symbol_tables ();
  cmm

let lambda_to_cmm ~ppf_dump ~prefixname ~machine_width ~keep_symbol_tables
    (program : Lambda.program) =
  let run () =
    lambda_to_flambda ~ppf_dump ~prefixname ~machine_width program
    |> flambda_result_to_cmm ~keep_symbol_tables
         ~localise_unreachable_symbols:true
  in
  Profile.record_call "flambda2" run

let reaper_lto_solve ~cmr_files ~ltosol_file =
  (* ID stamp counters are process-global monotonically increasing counters that
     give us an easy way of creating fresh identifiers. These identifiers get
     persisted across processes, and we need to prevent collisions when this
     happens. We have two mechanisms:

     (1) Identifiers are scoped to compilation units, as (CU, number) pairs.
     This means it's fine for different processes to use the same numbers as
     long as they're working on different CUs.

     (2) Saving and restoring stamp counters. We have to ensure that when
     multiple processes operate on the same CUs, these processes happen in
     sequence, and counters increase monotonically along this sequence.

     Here we're resuming from many processes that operated on different CUs, and
     we're operating on all of those CUs, so we use mechanism 2. To make sure
     counters are monotonically increasing, we take the maximum across all the
     process we've resumed from.

     After we're done, rebuild processes will be created to do more work on the
     CUs we touched. To keep counters monotonically increasing, we need to save
     them after our work so that the rebuild processes can restore them. *)
  let cmrs, counters =
    List.split (List.map Flambda2_reaper.Cmr_format.load cmr_files)
  in
  Flambda2_reaper.Id_stamp_counters.restore_for_merge counters;
  let participants =
    List.map Flambda2_reaper.Cmr_format.Serialisable.compilation_unit cmrs
  in
  (* All allocation and access sites of participating units' slots are in the
     combined graph, so they are local; set this before the solve. *)
  Flambda2_reaper.Field.set_locality_scope
    (Compilation_unit.Set.of_list participants);
  let combined_graph =
    (* The lists are in command-line order, which is deterministic, as required
       for reproducible .ltosol output. *)
    Flambda2_reaper.Lto_combine.combine
      (List.map2
         (fun participant cmr ->
           ( participant,
             Flambda2_reaper.Cmr_format.Serialisable.deserialise_deps cmr ))
         participants cmrs)
  in
  (* CR mvellacott: split the resulting solution into per-compilation-unit
     portions. *)
  let solution =
    Flambda2_reaper.Reaper.Staged.solve_whole_program combined_graph
  in
  Flambda2_reaper.Ltosol_format.save ~filename:ltosol_file ~participants
    ~solution

let reaped_flambda2_to_cmm ~machine_width ~ltosol_filename ~batch_members =
  (* Everything up to the function returned below is computed once and shared by
     the whole batch of rebuilds. *)
  let { Flambda2_reaper.Ltosol_format.File_contents.id_stamp_counters;
        participants;
        solution = ltosol_solution
      } =
    Profile.record_call ~accumulate:true "ltosol_load" (fun () ->
        Flambda2_reaper.Ltosol_format.load ltosol_filename)
  in
  Flambda2_reaper.Id_stamp_counters.restore_for_resume id_stamp_counters;
  Compilenv.set_lto_participants participants;
  (* Query the solved tables under the same locality the solve used. *)
  Flambda2_reaper.Field.set_locality_scope
    (Compilation_unit.Set.of_list participants);
  (* Deserialised on first use, which is after the first member's .cmr has been
     deserialised. This matches the identifier creation order of the
     pre-batching rebuild, keeping the first member's output identical to what
     separate invocations produce (under -dcanonical-ids, this makes a reaped
     unit that the Reaper didn't change byte-identical to a plain compile, see
     testsuite/tests/lto). Later members share the already-forced solution. *)
  let solved_dep =
    lazy
      (Profile.record_call ~accumulate:true "ltosol_deserialise" (fun () ->
           Flambda2_reaper.Ltosol_format.Serialisable_solution.deserialise
             ltosol_solution))
  in
  let cmx_loader = Flambda_cmx.create_loader ~get_module_info in
  (* Members of the batch whose rebuild has not started yet. Their .reaped.cmx
     files have not been written by this batch (any such file present on disk is
     stale), so needing one means the batch was not in dependency order. The
     direct imports of each member are checked upfront by the driver code; this
     check also catches violations via indirect dependencies. *)
  let pending_members = ref (Compilation_unit.Set.of_list batch_members) in
  let load_cmx_file_contents comp_unit =
    if Compilation_unit.Set.mem comp_unit !pending_members
    then
      Misc.fatal_errorf
        "-reaper-rebuild: unit %a is needed before its own rebuild; the .cmr \
         files were not given in dependency order"
        (Format_doc.compat Compilation_unit.print)
        comp_unit;
    Flambda_cmx.load_cmx_file_contents cmx_loader comp_unit
  in
  (* Visited set for the eager transitive loading below, shared across the batch
     so that each dependency is only visited once. *)
  let loaded_transitively = ref Compilation_unit.Set.empty in
  fun ~keep_symbol_tables
    ~cmr_filename
    ~paused_imports_cmx
    ~ppf_dump:_
    ~prefixname:_
  ->
    pending_members
      := Compilation_unit.Set.remove
           (Current_unit.get_cu_exn ())
           !pending_members;
    let cmr_serialisable, cmr_id_stamp_counters =
      Profile.record_call ~accumulate:true "cmr_load" (fun () ->
          Flambda2_reaper.Cmr_format.load cmr_filename)
    in
    (* We expect the stamp counters in the .cmr file to be less than the
       counters in the .ltosol file, because the -reaper-solve invocation begins
       by taking the maximum counters across the .cmr files it reads. If they
       are greater, the unit was recompiled after the solve, so the solution is
       stale. *)
    if
      not
        (Flambda2_reaper.Id_stamp_counters.le cmr_id_stamp_counters
           id_stamp_counters)
    then
      Misc.fatal_errorf
        "%s was written after %s (its identifier stamp counters are greater), \
         so the whole-program solution is stale: re-run -reaper-solve"
        cmr_filename ltosol_filename;
    let { Flambda2_reaper.Cmr_format.unit_metadata;
          final_typing_env;
          all_code;
          deps = _;
          rebuild_data
        } =
      Profile.record_call ~accumulate:true "cmr_deserialise" (fun () ->
          Flambda2_reaper.Cmr_format.Serialisable.deserialise ~machine_width
            ~resolver:load_cmx_file_contents cmr_serialisable)
    in
    (* We need post-rebuild exported offsets, code metadata and value slot usage
       from our dependencies that participate in LTO. To make sure we get the
       right version, we load all our transitive dependencies eagerly before
       resuming compilation.

       CR mvellacott: For performance, we should look at alternatives to eagerly
       loading everything. *)
    let () =
      let rec load_transitively imports =
        List.iter
          (fun import ->
            let comp_unit = Import_info.cu import in
            if not (Compilation_unit.Set.mem comp_unit !loaded_transitively)
            then (
              loaded_transitively
                := Compilation_unit.Set.add comp_unit !loaded_transitively;
              let (_ : Flambda2_types.Typing_env.Serializable.t option) =
                load_cmx_file_contents comp_unit
              in
              load_transitively (Compilenv.get_unit_imports comp_unit)))
          imports
      in
      load_transitively paused_imports_cmx
    in
    let solved_dep = Lazy.force solved_dep in
    (* CR mvellacott: add debug printing code. *)
    let flambda, free_names, all_code, slot_offsets, final_typing_env =
      Flambda2_reaper.Reaper.Staged.rebuild ~unit_metadata
        ~traverse_rebuild:rebuild_data ~solved_dep ~machine_width ~cmx_loader
        ~all_code ~final_typing_env
    in
    let unit_compilation_unit =
      Flambda2_identifiers.Symbol.compilation_unit
        (Flambda_unit.Metadata.module_symbol unit_metadata)
    in
    (* Definitions that other units still use must be emitted and exported even
       if this unit no longer references them. *)
    let extra_static_roots =
      Flambda2_reaper.Analysis.used_code_ids_and_symbols_in_unit solved_dep
        ~compilation_unit:unit_compilation_unit
    in
    (* Likewise, slots that other units still read must not be marked dead by
       slot offset finalisation. *)
    let extra_used_value_slots, extra_used_function_slots =
      Flambda2_reaper.Analysis.slots_used_in_unit solved_dep
        ~compilation_unit:unit_compilation_unit
    in
    let { unit = flambda;
          exported_offsets = offsets;
          cmx;
          all_code;
          used_value_slots = _;
          reachable_names
        } =
      build_run_result flambda ~free_names ~final_typing_env ~extra_static_roots
        ~extra_used_value_slots
        ~extra_used_function_slots
          (* Pass a mutable reference to the (currently empty) list of .cmx
             sections so that [build_run_result] can append the sections that it
             creates. *)
        ~sections:(Compilenv.current_sections ())
        ~all_code slot_offsets
    in
    Option.iter Compilenv.set_export_info cmx;
    Compiler_hooks.execute Reaped_flambda2 flambda;
    flambda_result_to_cmm ~keep_symbol_tables ~localise_unreachable_symbols:true
      { flambda; all_code; offsets; reachable_names }
