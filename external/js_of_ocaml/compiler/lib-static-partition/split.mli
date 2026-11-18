(** Compile a program into eager code and independently loaded deferred roots.

    The inputs are [.cmo] and [.cma] files, in initialization order. {!Partition}
    assigns their compilation units to the eager code (the main unit, force-linked
    units, and what they require) and to each root; ordinary units several roots need
    are initialized eagerly, once. All outputs go to one directory:

    - JavaScript: [main.js], the eager code; [root-UNIT.js] for each root; and
      [runtime.js], one runtime for all of them, pruned to the primitives they use
      (plus a bootstrap for predefined exceptions, embedded files and the static
      environment). Load [runtime.js], then [main.js]; a root may then be loaded at any
      time, at most once.
    - Wasm: [main.wasm] (module [partition_main]) and [startup.wasm], which runs
      [prelude.init] then [partition_main.init]; for each root, [root-UNIT.wasm]
      (module [partition_lazy_UNIT], initializer [partition_lazy_UNIT.init], without a
      [caml_main] wrapper). Each Wasm module comes with an ES module,
      [*.fragments.mjs], exporting its fragment namespace map for the runtime's
      [generated] imports. The runtime itself is built separately, with
      [wasm_of_ocaml build-runtime].

    The [late] strategy parses the eager code and all roots as one program, optimizes
    it globally, then splits it with {!Late_partition}; it requires profile 3 and
    either JavaScript without effects or Wasm with JSPI. The [modules] strategy
    compiles each partition separately.

    [report.json] describes the outputs, configuration and partitions. *)

open Js_of_ocaml_compiler

type strategy =
  | Modules
  | Late

module Backend : sig
  type wasm =
    { init : unit -> unit  (** Called after resetting the linker. *)
    ; compile : Code.program -> unit_name:string -> profile:Profile.t -> string * string
          (** Returns optimized Wasm bytes and an ES module exporting the fragment
              namespace map of the module. *)
    ; startup : profile:Profile.t -> string
          (** A module that initializes the prelude and the eager code. *)
    }

  type t =
    | Javascript
    | Wasm of wasm
end

type root =
  { key : string
        (** The name under which a package loads the root (letters, digits, [_] and
            [-]). *)
  ; unit_name : string
  }

type t =
  { backend : Backend.t
  ; strategy : strategy
  ; profile : Profile.t
  ; effects : Config.effects_backend
  ; inputs : string list
  ; runtime_files : string list
  ; fs_files : string list  (** [--file] specifications, embedded in [runtime.js]. *)
  ; static_env : (string * string) list
  ; main_unit : string
  ; roots : root list
  ; require_deferred_units : string list
  ; check_deferred_handles : (string * string) list
        (** [(handle, archive)]: no runtime path from [handle] to a unit of [archive]. *)
  ; output_dir : string
  }

val run : t -> unit
(** Raises [Failure] on invalid inputs, before writing any output. *)

val report_file : string

val root_basename : string -> string
(** [root_basename unit_name] is the basename of the outputs of a root. *)

val wasm_root_module : string -> string

(**/**)

module For_testing : sig
  type late_split =
    | Partitioned of Late_partition.many
    | Terminal_eager of Code.program

  val split_late_exn : Code.program -> late_split
end
