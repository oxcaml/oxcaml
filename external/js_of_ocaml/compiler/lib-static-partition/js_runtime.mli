(** The JavaScript runtime shared by the partitions of a split program. *)

open Js_of_ocaml_compiler
open Js_of_ocaml_compiler.Stdlib

val load_exn : ?after_reset:(unit -> unit) -> string list -> unit
(** Reset the linker and load the standard runtime, then the given runtime files.
    Compiler builtin names such as [+dynlink.js] select builtin files. [after_reset]
    runs after the reset and before loading anything. *)

type t =
  { javascript : string
  ; primitives : StringSet.t
  }

val build_exn :
     primitives:StringSet.t
  -> runtime_files:string list
  -> fs_files:string list
  -> static_env:(string * string) list
  -> profile:Profile.t
  -> t
(** Build one runtime, with the current compiler configuration (which must target
    JavaScript), providing [primitives] and what they depend on, and exporting them as
    [globalThis.jsoo_runtime]. [fs_files] uses [--file HOST:VIRTUAL] specifications.
    A bootstrap initializes predefined exceptions, embedded files, and [static_env]
    before any partition runs. [primitives] in the result includes the bootstrap's
    own primitives. *)
