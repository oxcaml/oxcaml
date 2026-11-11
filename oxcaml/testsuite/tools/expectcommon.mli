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

   The file is run several times: once without any parameters, and once with
   each of the parameters (currently only [Principal]). An [%%expect] node
   contains the expected output of each run. When the outputs differ, the
   expectation is written as follows:

   {[
     [%%expect {|
     output without parameters
     |}, Principal{|
     output with -principal
     |}]
   ]}

   Several parameters used together are separated with a dot, as in
   [Principal.Rectypes{|...|}], and parameter sets with the same output are
   grouped in a tuple, as in [(Principal, Rectypes){|...|}]. An output that is
   the same as the output without parameters is omitted.
*)

module type Toplevel = sig
  val override_sys_argv : string array -> unit
  val initialize_toplevel_env : unit -> unit
  val load_file : Format.formatter -> string -> bool
  val max_printer_depth : int ref
  val max_printer_steps : int ref

  (* Runs the file with one set of parameters. [expectnat] runs each set in
     its own process, as the native toplevel names the code of each phrase
     using counters that cannot be reset without clashing with the code it has
     already loaded. *)
  val isolate_run : (unit -> 'a) -> 'a

  (* Either [Toploop.execute_phrase] or [Opttoploop.execute_phrase] *)
  val execute_phrase :
    bool -> Format.formatter -> Parsetree.toplevel_phrase -> bool
end

(** Hook to capture assembly output for [%%expect_asm]. This function should be
    set by expectnat.ml. *)
val register_assembly_callback : ((string -> unit) -> unit) option ref

(** Set before compiling each chunk: tells the backend whether the
    [%%expect_asm] capture should extend to the whole function, including the
    trailing out-of-line code. *)
val set_assembly_whole_function : (bool -> unit) option ref

(** Hook to capture phrase compilation unit for [%%expect_fexpr]. This
    function should be set by expectnat.ml. *)
val register_compilation_unit_callback :
  ((Compilation_unit.t -> unit) -> unit) option ref

val read_anonymous_arg : object_extensions:string list -> string -> unit

val run
  :  read_anonymous_arg:(string -> unit)
  -> extra_args:(Arg.key * Arg.spec * Arg.doc) list
  -> extra_init:(unit -> unit)
  -> (module Toplevel)
  -> unit
