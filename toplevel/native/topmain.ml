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

open Compenv

let usage =
   "Usage: ocamlnat <options> <object-files> [script-file]\noptions are:"

(* Position of the first non expanded argument *)
let first_nonexpanded_pos = ref 0

let current = ref (!Arg.current)

let argv = ref Sys.argv

(* Test whether the option is part of a responsefile *)
let is_expanded pos = pos < !first_nonexpanded_pos

let expand_position pos len =
  if pos < !first_nonexpanded_pos then
    (* Shift the position *)
    first_nonexpanded_pos := !first_nonexpanded_pos + len
  else
    (* New last position *)
    first_nonexpanded_pos := pos + len + 2

let input_argument (name : Opttoploop.input) =
  let filename = Opttoploop.filename_of_input name in
  let ppf = Format.err_formatter in
  if Filename.check_suffix filename ".cmxs"
    || Filename.check_suffix filename ".cmx"
    || Filename.check_suffix filename ".cmxa"
  then Opttoploop.preload_objects := filename :: !Opttoploop.preload_objects
  else if is_expanded !current then begin
    (* Script files are not allowed in expand options because otherwise the
       check in override arguments may fail since the new argv can be larger
       than the original argv.
    *)
    Printf.eprintf "For implementation reasons, the toplevel does not support\
    \ having script files (here %S) inside expanded arguments passed through\
    \ the -args{,0} command-line option.\n" filename;
    raise (Exit_with_status 2)
  end else begin
    let newargs = Array.sub !argv !Arg.current
                              (Array.length !argv - !Arg.current)
      in
      Compmisc.read_clflags_from_env ();
      if Opttoploop.prepare ppf ~input:name () &&
         Opttoploop.run_script ppf filename newargs
      then raise (Exit_with_status 0)
      else raise (Exit_with_status 2)
    end

let file_argument x = input_argument (Opttoploop.File x)

let wrap_expand f s =
  let start = !current in
  let arr = f s in
  expand_position start (Array.length arr);
  arr

module Options = Oxcaml_args.Make_opttop_options (struct
    include Oxcaml_args.Default.Opttopmain
    let _stdin () = file_argument ""
    let _args = wrap_expand Arg.read_arg
    let _args0 = wrap_expand Arg.read_arg0
    let anonymous s = file_argument s
end);;

let () =
  let extra_paths =
    match Sys.getenv "OCAMLTOP_INCLUDE_PATH" with
    | exception Not_found -> []
    | s -> Misc.split_path_contents s
  in
  Clflags.include_dirs := List.rev_append extra_paths !Clflags.include_dirs

(* Ensure Opttopdirs is linked so that its toplevel directives are registered *)
let () = ignore (Opttopdirs.dir_quit)

let main () =
  let ppf = Format.err_formatter in
  Clflags.native_code := true;
  Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
  let list = ref Options.list in
  begin
    try
      Arg.parse_and_expand_argv_dynamic current argv list file_argument usage;
    with
    | Arg.Bad msg -> Format.fprintf Format.err_formatter "%s%!" msg;
                     raise (Exit_with_status 2)
    | Arg.Help msg -> Format.fprintf Format.std_formatter "%s%!" msg;
                      raise (Exit_with_status 0)
  end;
  Compmisc.read_clflags_from_env ();
  if not (Opttoploop.prepare ppf ()) then raise (Exit_with_status 2);
  Compmisc.init_path ();
  Opttoploop.loop Format.std_formatter

let main () =
  match main () with
  | exception Exit_with_status n -> n
  | () -> 0
