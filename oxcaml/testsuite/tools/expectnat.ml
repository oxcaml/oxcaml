(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  James Rayman, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* See expectcommon.mli for usage *)

module Toplevel = struct
  let override_sys_argv = Opttoploop.override_sys_argv
  let initialize_toplevel_env = Opttoploop.initialize_toplevel_env
  let load_file = Opttoploop.load_file
  let max_printer_depth = Opttoploop.max_printer_depth
  let max_printer_steps = Opttoploop.max_printer_steps

  let isolate_run f =
    let result_file = Filename.temp_file "expectnat" ".run" in
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter ();
    flush_all ();
    match Unix.fork () with
    | 0 ->
      let status =
        match f () with
        | result ->
          Out_channel.with_open_bin result_file (fun oc ->
            Marshal.to_channel oc result []);
          0
        | exception exn ->
          Location.report_exception Format.err_formatter exn;
          2
      in
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      flush_all ();
      Unix._exit status
    | pid ->
      let result =
        match Unix.waitpid [] pid with
        | _, WEXITED 0 ->
          Some (In_channel.with_open_bin result_file Marshal.from_channel)
        | _, (WEXITED _ | WSIGNALED _ | WSTOPPED _) -> None
      in
      Sys.remove result_file;
      match result with
      | Some result -> result
      | None -> exit 2
  let execute_phrase = Opttoploop.execute_phrase
end

let read_anonymous_arg =
  Expectcommon.read_anonymous_arg ~object_extensions:[".cmxs"; ".cmx"; ".cmxa"]

module Options = struct
  include Oxcaml_args.Make_opttop_options (struct
    include Oxcaml_args.Default.Opttopmain
    let _stdin () = (* disabled *) ()
    let _args = Arg.read_arg
    let _args0 = Arg.read_arg0
    let anonymous s = read_anonymous_arg s
  end)

  (* Unlike the interactive toplevel, [expectnat] does not record debugging
     information by default. *)
  let list =
    ("-g", Arg.Set Clflags.debug, " Record debugging information") :: list
end;;

let () =
  Expectcommon.register_assembly_callback :=
    Some Emit.register_expect_asm_callback;
  Expectcommon.set_assembly_whole_function :=
    Some (fun b -> Emit.expect_asm_whole_function := b);
  Expectcommon.register_compilation_unit_callback :=
    Some Flambda2.register_compilation_unit_callback;
  Expectcommon.run
    ~read_anonymous_arg
    ~extra_args:Options.list
    ~extra_init:(fun () ->
      Clflags.set_target Clflags.Target.Native;
      Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler)
    (module Toplevel)
