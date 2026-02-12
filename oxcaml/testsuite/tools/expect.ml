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
  let override_sys_argv = Toploop.override_sys_argv
  let initialize_toplevel_env = Toploop.initialize_toplevel_env
  let load_file fmt fname = Topeval.load_file false fmt fname
  let execute_phrase = Toploop.execute_phrase
end

let read_anonymous_arg =
  Expectcommon.read_anonymous_arg ~object_extensions:[".cmo"; ".cma"]

module Options = Main_args.Make_bytetop_options (struct
  include Main_args.Default.Topmain
  let _stdin () = (* disabled *) ()
  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0
  let anonymous s = read_anonymous_arg s
end);;

let () =
  Expectcommon.run
    ~read_anonymous_arg
    ~extra_args:Options.list
    ~extra_init:(fun () -> ())
    (module Toplevel)
