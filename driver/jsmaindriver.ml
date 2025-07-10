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
  let program = "ocamlj" in
  Clflags.add_arguments __LOC__ Options.list;
  match
    Compenv.readenv ppf Before_args;
    Compenv.parse_arguments (ref argv) Compenv.anonymous program;
    Compmisc.read_clflags_from_env ();
    if !Clflags.plugin then
      Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
    begin try
      Compenv.process_deferred_actions
        (ppf,
         Jscompile.implementation,
         Jscompile.interface,
         ".cmo",
         ".cma");
    with Arg.Bad msg ->
      begin
        prerr_endline msg;
        Clflags.print_arguments program;
        exit 2
      end
    end
  with
  | exception (Compenv.Exit_with_status n) ->
    n
  | () ->
    (* Prevents outputting when using make install to dump CSVs for whole compiler.
       Example use case: scripts/profile-compiler-build.sh *)
    if not !Clflags.dump_into_csv then
      Compmisc.with_ppf_dump ~stdout:() ~file_prefix:"profile"
        (fun ppf -> Profile.print ppf !Clflags.profile_columns
          ~timings_precision:!Clflags.timings_precision);
    0
  | exception x ->
    Location.report_exception ppf x;
    2
