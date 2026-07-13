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

open Clflags

let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

module Options = Oxcaml_args.Make_optcomp_options
        (Oxcaml_args.Default.Optmain)

(* Opt-in GC pacing for the compiler process itself, intended to be set by
   the build system (e.g. -X gc-space-overhead=200 -X gc-idle-floor=512M).
   The compiler is allocation-heavy and short-lived, so a laxer major-GC
   pace trades bounded extra peak heap for materially less GC work; the
   idle floor additionally lets small compilations finish without doing
   any major-GC marking at all (see runtime/major_gc.c, Idle phase).
   Both knobs are inert unless set. Explicit user settings in
   OCAMLRUNPARAM/CAMLRUNPARAM take precedence, so a single invocation can
   always be re-paced by hand when investigating time or memory issues. *)
module Gc_pacing = struct
  (* The floor is applied through the [caml_gc_set_idle_floor] primitive
     (sets the small_heap_limit tweak AND re-arms the current cycle's
     floor), which the bootstrap toolchain's runtime does not provide; the
     final executables (driver/oxcaml_main.ml) install it here. The
     bootstrap build never passes -X gc-idle-floor, so the failing default
     is unreachable there. *)
  let set_idle_floor_hook : (int -> unit) ref =
    ref (fun _ ->
      Compenv.fatal
        "-X gc-idle-floor is not supported by this executable")

  let space_overhead =
    Oxcaml_args.Extra_options.int __LOC__ "gc-space-overhead" 0

  let idle_floor =
    Oxcaml_args.Extra_options.string __LOC__ "gc-idle-floor" ""

  (* Mirror the runtime's OCAMLRUNPARAM parsing (runtime/startup_aux.c):
     the active variable is OCAMLRUNPARAM if set at all, else CAMLRUNPARAM;
     fields are comma-separated and identified by their leading
     characters. *)
  let env_field_present ~prefix =
    let param =
      match Sys.getenv_opt "OCAMLRUNPARAM" with
      | Some _ as p -> p
      | None -> Sys.getenv_opt "CAMLRUNPARAM"
    in
    match param with
    | None -> false
    | Some s ->
      String.split_on_char ',' s
      |> List.exists (fun field ->
        String.length field >= String.length prefix
        && String.sub field 0 (String.length prefix) = prefix)

  (* Size in bytes: a decimal integer with an optional K/M/G binary
     suffix. Guarded against multiply overflow, and capped at 1 TiB: a
     floor beyond that disables major-GC marking outright on any real
     compilation (unbounded heap growth), and such values are far more
     likely a suffix typo (512G for 512M) than intent. *)
  let max_size_bytes = 1 lsl 40

  let parse_size_bytes s =
    let n = String.length s in
    if n = 0 then None
    else begin
      let mult, digits =
        match s.[n - 1] with
        | 'k' | 'K' -> 1024, String.sub s 0 (n - 1)
        | 'm' | 'M' -> 1024 * 1024, String.sub s 0 (n - 1)
        | 'g' | 'G' -> 1024 * 1024 * 1024, String.sub s 0 (n - 1)
        | _ -> 1, s
      in
      match int_of_string_opt digits with
      | Some i when i >= 0 && i <= max_size_bytes / mult ->
        Some (i * mult)
      | _ -> None
    end

  (* Runs after argument parsing and before any compilation. Late enough
     that -X and OCAMLPARAM's "before" section have been read (knobs in
     OCAMLPARAM's per-file "after" section are NOT honoured: it is only
     read per compilation unit, after this point); early enough that no
     real GC work has happened. Also note [-depend] runs and exits during
     argument parsing, i.e. unpaced, which is fine (it compiles
     nothing). *)
  let apply () =
    begin match space_overhead () with
    | 0 -> ()  (* the "unset" sentinel: 0 cannot be requested explicitly *)
    | n when n < 0 ->
      Compenv.fatal
        (Printf.sprintf "-X gc-space-overhead expects a positive integer, \
got: %d" n)
    | n ->
      if not (env_field_present ~prefix:"o") then
        Gc.set { (Gc.get ()) with Gc.space_overhead = n }
    end;
    match idle_floor () with
    | "" -> ()
    | s ->
      if not (env_field_present ~prefix:"Xsmall_heap_limit=") then begin
        match parse_size_bytes s with
        | Some bytes ->
          (* The primitive takes words; bytes <= 1 TiB so this cannot
             overflow. *)
          !set_idle_floor_hook ((bytes + 7) / 8)
        | None ->
          Compenv.fatal
            ("-X gc-idle-floor expects a byte count with an optional "
             ^ "K/M/G suffix, got: " ^ s)
      end
end

let main unix argv ppf ~flambda2 =
  native_code := true;
  let columns =
    match Sys.getenv "COLUMNS" with
    | exception Not_found -> None
    | columns ->
      try Some (int_of_string columns)
      with _ -> None
  in
  (match columns with
  | None -> ()
  | Some columns ->
    (* Avoid getting too close to the edge just in case we've mismeasured
       the boxes for some reason. *)
    let columns = columns - 5 in
    let set_geometry ppf =
      Format.pp_set_margin ppf columns;
      (* Make sure the max indent is at least 3/4 of the total width. Without
         this, output can be unreadable no matter how wide your screen is. Note
         that [Format.pp_set_margin] already messes with the max indent
         sometimes, so we want to check [Format.pp_get_max_indent] rather than
         make assumptions. *)
      let desired_max_indent = columns * 3 / 4 in
      if Format.pp_get_max_indent ppf () < desired_max_indent then
        Format.pp_set_max_indent ppf desired_max_indent
    in
    set_geometry Format.std_formatter;
    set_geometry Format.err_formatter);
  match
    Compenv.warnings_for_discarded_params := true;
    Compenv.set_extra_params
      (Some Oxcaml_args.Extra_params.read_param);
    Compenv.readenv ppf Before_args;
    Clflags.add_arguments __LOC__ (Arch.command_line_options @ Options.list);
    Clflags.add_arguments __LOC__
      ["-depend", Arg.Unit Makedepend.main_from_option,
       "<options> Compute dependencies \
        (use 'ocamlopt -depend -help' for details)"];
    Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
    Compenv.parse_arguments (ref argv) Compenv.anonymous "ocamlopt";
    Compmisc.read_clflags_from_env ();
    Gc_pacing.apply ();
    (* Set platform-appropriate DWARF fission default when oxcaml-dwarf is
       enabled *)
    if Config.oxcaml_dwarf &&
       !Clflags.dwarf_fission = Clflags.Fission_none &&
       Target_system.is_macos () then
      Clflags.dwarf_fission := Clflags.Fission_dsymutil;
    (* Set up DWARF compression for C compiler invocations *)
    if !Clflags.debug && !Clflags.native_code then
      Clflags.dwarf_c_toolchain_flag :=
        Dwarf_flags.get_dwarf_c_toolchain_flag ();
    if !Oxcaml_flags.gc_timings then Gc_timings.start_collection ();
    if !Clflags.plugin then
      Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
    if !Clflags.requires_metaprogramming
      && not Language_extension.(is_enabled Runtime_metaprogramming) then
        Compenv.fatal "The -requires-metaprogramming flag is only supported \
                       with the runtime metaprogramming extension";
    if !Clflags.uses_metaprogramming
      && not Language_extension.(is_enabled Runtime_metaprogramming) then
        Compenv.fatal "The -uses-metaprogramming flag is only supported \
                       with the runtime metaprogramming extension";
    let (module Compiler : Optcompile.S) =
      Optcompile.native unix ~flambda2
    in
    begin try
      Compenv.process_deferred_actions
        (ppf,
         Compiler.implementation,
         Compiler.interface,
         Compiler.ext_flambda_obj,
         Compiler.ext_flambda_lib);
    with Arg.Bad msg ->
      begin
        prerr_endline msg;
        Clflags.print_arguments usage;
        exit 2
      end
    end;
    Compenv.readenv ppf Before_link;
    if
      List.length (List.filter (fun x -> !x)
                     [make_package; make_archive; shared; instantiate;
                      Compenv.stop_early; output_c_object]) > 1
    then
    begin
      let module P = Clflags.Compiler_pass in
      match !stop_after with
      | None ->
          Compenv.fatal "Please specify at most one of -pack, -a, -shared, -c, \
                         -output-obj, -instantiate";
      | Some ((P.Parsing | P.Typing | P.Lambda | P.Middle_end | P.Linearization
              | P.Simplify_cfg | P.Emit | P.Selection
              | P.Register_allocation | P.Llvmize) as p) ->
        assert (P.is_compilation_pass p);
        Printf.ksprintf Compenv.fatal
          "Options -i and -stop-after (%s) \
           are  incompatible with -pack, -a, -shared, -output-obj"
          (String.concat "|"
             (P.available_pass_names ~filter:(fun _ -> true) ~native:true))
    end;
    if !make_archive then begin
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Compiler.create_archive
        (Compenv.get_objfiles ~with_ocamlparam:false) target;
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Compiler.package_files ~ppf_dump (Compmisc.initial_env ())
          (Compenv.get_objfiles ~with_ocamlparam:false) target);
      Warnings.check_fatal ();
    end
    else if !instantiate then begin
      Compmisc.init_path ();
      (* Requiring [-o] isn't really necessary, but we don't intend for humans
         to be invoking [-instantiate] by hand, and computing the correct value
         here would be awkward *)
      let target = Compenv.extract_output !output_name in
      let src, args =
        match Compenv.get_objfiles ~with_ocamlparam:false with
        | [] | [_] ->
          Printf.ksprintf Compenv.fatal
            "Must specify at least two %s files with -instantiate"
            Compiler.ext_flambda_obj
        | src :: args ->
          src, args
      in
      Compiler.instantiate ~src ~args target;
      Warnings.check_fatal ();
    end
    else if !shared then begin
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Compiler.link_shared ~ppf_dump (Linkenv.create ())
          (Compenv.get_objfiles ~with_ocamlparam:false) target);
      Warnings.check_fatal ();
    end
    else if not !Compenv.stop_early && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = Compenv.extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll)
          then s
          else
            Compenv.fatal
              (Printf.sprintf
                 "The extension of the output file must be %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          Compenv.default_output !output_name
      in
      Compmisc.init_path ();
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          let objs = Compenv.get_objfiles ~with_ocamlparam:true in
          Compiler.link
            ~ppf_dump objs target);
      Warnings.check_fatal ();
    end;
  with
  | exception (Compenv.Exit_with_status n) ->
    n
  | exception x ->
    Location.report_exception ppf x;
    2
  | () ->
    let output_profile_csv ppf_file = Profile.output_to_csv
      ppf_file !Clflags.profile_columns ~timings_precision:!Clflags.timings_precision
    in
    let output_profile_standard ppf =
      if !Oxcaml_flags.gc_timings then begin
        let minor = Gc_timings.gc_minor_ns () in
        let major = Gc_timings.gc_major_ns () in
        let stats = Gc.quick_stat () in
        let secs x = x *. 1e-9 in
        let precision = !Clflags.timings_precision in
        let w2b n = n * (Sys.word_size / 8) in
        let fw2b x = w2b (Float.to_int x) in
        Format.fprintf ppf "%0.*fs gc\n" precision (secs (minor +. major));
        Format.fprintf ppf "  %0.*fs minor\n" precision (secs minor);
        Format.fprintf ppf "  %0.*fs major\n" precision (secs major);
        Format.fprintf ppf "- heap\n";
        (* Having minor + major + promoted = total alloc make more sense for
          hierarchical stats. *)
        Format.fprintf ppf "  %ib alloc\n"
          (fw2b stats.minor_words + (fw2b stats.major_words - fw2b stats.promoted_words));
        Format.fprintf ppf "    %ib minor\n"
          (fw2b stats.minor_words - fw2b stats.promoted_words);
        Format.fprintf ppf "    %ib major\n"
          (fw2b stats.major_words - fw2b stats.promoted_words);
        Format.fprintf ppf "    %ib promoted\n"
          (fw2b stats.promoted_words);
        Format.fprintf ppf "  %ib top\n" (w2b stats.top_heap_words);
        Format.fprintf ppf "  %i collections\n"
          (stats.minor_collections + stats.major_collections);
        Format.fprintf ppf "    %i minor\n" stats.minor_collections;
        Format.fprintf ppf "    %i major\n" stats.major_collections;
      end;
      Profile.print ppf !Clflags.profile_columns ~timings_precision:!Clflags.timings_precision
    in
    (if !Clflags.dump_into_csv then
      let file_prefix =
        Compmisc.get_profile_file_prefix
          ~expected_suffix:".csv" ~default_name:"profile"
      in
      Compmisc.with_ppf_file
        ~file_prefix ~file_extension:".csv" output_profile_csv
    else if !Oxcaml_flags.gc_timings || !Clflags.profile_columns <> [] then
      let file_prefix =
        Compmisc.get_profile_file_prefix
          ~expected_suffix:".dump" ~default_name:"profile"
      in
      Compmisc.with_ppf_dump ~stdout:() ~file_prefix output_profile_standard);
    0
