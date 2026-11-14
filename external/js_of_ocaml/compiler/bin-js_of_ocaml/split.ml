open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let info =
  Info.make
    ~name:"split"
    ~doc:"Split a program into eager code and deferred roots"
    ~description:
      "js_of_ocaml-split compiles OCaml bytecode units into eager code and deferred \
       roots, which can each be loaded later, at most once, after the eager code. It \
       writes $(i,main.js), one $(i,root-UNIT.js) per root, $(i,runtime.js) (one runtime \
       for all of them, to load first) and $(i,report.json), which describes the \
       outputs, the configuration and the partition of the units."

let command =
  Jsoo_static_partition.Split_command.command
    ~info
    ~backend:Javascript
    ~effects_backends:
      (List.map Build_info.effects_backends_javascript ~f:(fun (name, effects) ->
           name, (effects :> Config.effects_backend)))
    ~default_effects:`Disabled
