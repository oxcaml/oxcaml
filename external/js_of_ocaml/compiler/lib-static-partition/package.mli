(** Package the outputs of {!Split} for deployment.

    A generated package is an entry script, [ENTRY], next to a directory, [DIR], that
    holds the files to deploy under a content-addressed subdirectory
    [DIR/IDENTITY/]. Loading the entry script loads the eager code, then installs
    [globalThis.__jsoo_imports_v2], with:
    - [ready], a promise resolved once eager initialization is done;
    - [load(KEY)], a promise of the OCaml module value of root [KEY] once it is
      initialized. It loads the root at most once: further calls return the same
      promise, including when it fails. Once a root raises, loading any other root
      fails with the same error, without running it.

    [DIR/bundle-manifest.json] lists the files with their sizes, and the phase
    (eager or deferred) in which they are fetched. *)

module Split_report : sig
  type root =
    { key : string
    ; unit_name : string
    ; file : string
    ; fragments : string option
    ; wasm_module : string option
    ; initializer_ : string option
    }

  type t =
    { directory : string
    ; contents : string
    ; json : Yojson.Safe.t
    ; backend : string
    ; strategy : string
    ; profile : int
    ; effects : string
    ; use_js_string : bool
    ; roots : root list
    }

  val read_exn : string -> t
  (** Read [report.json] in the given output directory of [split]. *)

  val file : t -> string -> string
  (** The path of an output of the split. *)
end

val asset_basenames : backend:[ `Javascript | `Wasm ] -> Split_report.root -> string list
(** The deployed files of a root. *)

val prepare_output_dir_exn : string -> unit
(** Create the directory if needed, and fail unless it is empty. *)

val read_file : string -> string

val write_file : string -> string -> unit

val asset_json : string -> phase:string -> string -> Yojson.Safe.t * int
(** [asset_json directory ~phase name] describes the file [name] of [directory] in a
    manifest, and returns its size. *)

val script_module : string -> string
(** A root's JavaScript, as served: with a [#!] line, which pins content-sniffing file
    servers to JavaScript. *)

val generated_exn :
     backend:[ `Javascript | `Wasm ]
  -> report:Split_report.t
  -> entry:string
  -> output_dir:string
  -> runtime_configuration:(string -> Yojson.Safe.t)
  -> build:(string -> unit)
  -> unit
(** [build directory] writes into [directory] a [loader.js], which loads the eager
    code, and the files of the roots ({!asset_basenames}). For Wasm, [loader.js] must
    define [globalThis.__jsoo_imports_wasm_ready_v1] (a promise of the eager
    initialization) and [globalThis.__jsoo_imports_wasm_load_v1(KEY)].
    [runtime_configuration directory] is recorded in the manifest after building. *)
