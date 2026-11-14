(** The [split] subcommand of [js_of_ocaml] and [wasm_of_ocaml] (see {!Split}). *)

open Js_of_ocaml_compiler

val command :
     info:Cmdliner.Cmd.info
  -> backend:Split.Backend.t
  -> effects_backends:(string * Config.effects_backend) list
  -> default_effects:Config.effects_backend
  -> unit Cmdliner.Cmd.t
