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

module Options = Main_args.Make_jscomp_options (Main_args.Default.Jsmain)

let main argv ppf =
  Clflags.jsir := true;
  let program = "ocamlj" in
  let columns =
    match Sys.getenv "COLUMNS" with
    | exception Not_found -> None
    | columns -> ( try Some (int_of_string columns) with _ -> None)
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
  Clflags.add_arguments __LOC__ Options.list;
  match
    Compenv.readenv ppf Before_args;
    Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
    Compenv.parse_arguments (ref argv) Compenv.anonymous program;
    Compmisc.read_clflags_from_env ();
    if !Clflags.plugin then
      Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
    try
      Compenv.process_deferred_actions
        (ppf, Jscompile.implementation, Jscompile.interface, ".cmj", ".cmja")
    with Arg.Bad msg ->
      prerr_endline msg;
      Clflags.print_arguments program;
      exit 2
  with
  | exception Compenv.Exit_with_status n -> n
  | () ->
      if !Clflags.make_archive then (
        Compmisc.init_path ();
        Jslibrarian.create_archive
          (Compenv.get_objfiles ~with_ocamlparam:false)
          (Compenv.extract_output !Clflags.output_name);
        Warnings.check_fatal ());
      (* Prevents outputting when using make install to dump CSVs for whole compiler.
         Example use case: scripts/profile-compiler-build.sh *)
      if not !Clflags.dump_into_csv then
        Compmisc.with_ppf_dump ~stdout:() ~file_prefix:"profile" (fun ppf ->
            Profile.print ppf !Clflags.profile_columns
              ~timings_precision:!Clflags.timings_precision);
      0
  | exception x ->
      Location.report_exception ppf x;
      2
