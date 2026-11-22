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

(* Eta-expansion gives [create_process] and [waitpid] the unannotated types
   required by [Compiler_owee.Unix_intf.S]. *)
module Unix_for_owee = struct
  include Unix

  let create_process prog args stdin stdout stderr =
    Unix.create_process prog args stdin stdout stderr

  let waitpid flags pid = Unix.waitpid flags pid
end

let write_asm_file = ref false

let compile_file filename =
  if !write_asm_file then begin
    let out_name = Filename.chop_extension filename ^ ".s" in
    Emitaux.output_channel := open_out out_name
  end; (* otherwise, stdout *)
  let compilation_unit = "test" |> Compilation_unit.of_string in
  let unit_info = Unit_info.make_dummy ~input_name:"test" compilation_unit in
  Compilenv.reset unit_info;
  Emit.begin_assembly (module Unix_for_owee : Compiler_owee.Unix_intf.S);
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  lb.Lexing.lex_curr_p <- Lexing.{ lb.lex_curr_p with pos_fname = filename };
  try
    while true do
      Asmgen.compile_phrase ~ppf_dump:Format.std_formatter
        (Parsecmm.phrase Lexcmm.token lb)
    done
  with
      End_of_file ->
        close_in ic; Emit.end_assembly ();
        if !write_asm_file then close_out !Emitaux.output_channel
    | Lexcmm.Error msg ->
        close_in ic; Lexcmm.report_error lb msg
    | Parsing.Parse_error ->
        close_in ic;
        let start_p = Lexing.lexeme_start_p lb in
        let end_p = Lexing.lexeme_end_p lb in
        Printf.eprintf "File \"%s\", line %i, characters %i-%i:\n\
                        Syntax error.\n%!"
          filename
          start_p.Lexing.pos_lnum
          (start_p.Lexing.pos_cnum - start_p.Lexing.pos_bol)
          (end_p.Lexing.pos_cnum - start_p.Lexing.pos_bol)
    | Parsecmmaux.Error msg ->
        close_in ic; Parsecmmaux.report_error msg
    | x ->
        close_in ic; raise x

let usage = "Usage: codegen <options> <files>\noptions are:"

external code_is_fully_polled : unit -> bool
  = "caml_sys_exit" "caml_code_is_fully_polled"
  [@@noalloc]

let main() =
  (* The code is linked with objects built for the codegen variant of this
     executable.  Poll insertion is a property of each unit and has no
     constant: it is the variant's when all the units of this executable
     have it. *)
  Codegen_variant.apply
    { address_sanitizer = Codegen_host_variant.address_sanitizer;
      frame_pointers = Codegen_host_variant.frame_pointers;
      poll_insertion = code_is_fully_polled ();
      stack_checks = Codegen_host_variant.stack_checks
    };
  Arg.parse [
     "-S", Arg.Set write_asm_file,
       " Output file to filename.s (default is stdout)";
     "-g", Arg.Set Clflags.debug, "";
     "-dcfg", Arg.Set Oxcaml_flags.dump_cfg, "";
     "-dcmm", Arg.Set dump_cmm, "";
     "-dcse", Arg.Set dump_cse, "";
     "-dlinear", Arg.Set dump_linear, "";
     "-dtimings", Arg.Unit (fun () -> set_profile_columns [ `Time ]), "";
     "-dcounters", Arg.Unit (fun () -> set_profile_columns [ `Counters ]), "";
     ( "-dgranularity",
        Arg.Symbol
          (Clflags.all_profile_granularity_levels, Clflags.set_profile_granularity),
        "" );
    ] compile_file usage

let () =
  main ();
  Profile.print Format.std_formatter !Clflags.profile_columns ~timings_precision:!Clflags.timings_precision;
  exit 0
