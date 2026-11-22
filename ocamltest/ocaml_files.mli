(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Locations of files in the OCaml source tree *)

type runtime_variant =
  | Normal
  | Debug
  | Instrumented

val runtime_variant : unit -> runtime_variant

val ocamlrun : string

val ocamlrun_sys : string

val ocamlc : string

val ocaml : string

val ocamlc_dot_opt : string

val ocamlopt : string

val ocamlopt_dot_opt : string

val js_of_ocaml : string

val wasm_of_ocaml : string

(** Whether [js_of_ocaml] is installed, so that programs can be compiled with
    [ocamlopt -target js_of_ocaml] *)
val jsir_js_available : bool

(** Whether [wasm_of_ocaml] is installed, so that programs can be compiled
    with [ocamlopt -target wasm_of_ocaml] *)
val jsir_wasm_available : bool

(** The JavaScript engine used to run JSIR programs ($NODE, or node) *)
val node : string

(** The flags passed to [node] to run wasm_of_ocaml programs
    ($NODE_WASM_FLAGS) *)
val node_wasm_flags : string

val cmpbyt : string

val expect : string

val ocamllex : string

val ocamlyacc : string

val ocamldoc : string
val ocamldebug : string
val ocamlobjinfo : string
val ocamlmklib : string
val ocamlfilt : string
(* The programs that generate native code, or compile and load it, in
   process: the native expect-test runner, the native toplevel and the
   code generator of the asmgen tests.  Each is built once per codegen
   variant, for that variant. *)
type native_host =
  | Expectnat
  | Ocamlnat
  | Codegen

(* The name a test author knows the program by. *)
val native_host_name : native_host -> string

(* The build of a native host for a codegen variant: the one for the
   install's default variant ([~variant:None]), or the copy for the named
   variant, which install_for_test puts under
   testsuite/tools/variants/<variant>. *)
val native_host : variant:string option -> native_host -> string

val fexprc : string

val asmgen_archmod : string
