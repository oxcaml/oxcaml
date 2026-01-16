(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Execute a list of phrases from a .ml file and compare the result to the
   expected output, written inside [%%expect ...] nodes. At the end, create
   a .corrected file containing the corrected expectations. The test is
   successful if there is no differences between the two files.

   An [%%expect] node always contains both the expected outcome with and
   without -principal. When the two differ the expectation is written as
   follows:

   {[
     [%%expect {|
     output without -principal
     |}, Principal{|
     output with -principal
     |}]
   ]}

   By default, REPL echoes like [val - : int = 5] are included in the expect
   output. To disable this, use [[%%expect.ignore_echo ..]]
   (see examples in testsuite/tests/typing-local/let_mutable.ml).
   Warnings and errors are still captured with ignore_echo.
*)

module type Toplevel = sig
  val override_sys_argv : string array -> unit
  val initialize_toplevel_env : unit -> unit
  val load_file : Format.formatter -> string -> bool

  (* Either [Toploop.execute_phrase] or [Opttoploop.execute_phrase] *)
  val execute_phrase :
    bool -> Format.formatter -> Parsetree.toplevel_phrase -> bool
end

val read_anonymous_arg : object_extensions:string list -> string -> unit

val run
  :  read_anonymous_arg:(string -> unit)
  -> extra_args:(Arg.key * Arg.spec * Arg.doc) list
  -> extra_init:(unit -> unit)
  -> (module Toplevel)
  -> unit
