(** Resolution of link inputs through manifest files, as the OCaml compiler does for
    [-I-manifest]: a manifest maps bare file names to paths under [$MANIFEST_FILES_ROOT]
    and may reference other manifests. The reader is self-contained and follows
    [Dune_manifests_reader] in the OxCaml compiler's [utils/load_path.ml]: later
    entries take precedence, and each location is read at most once. *)

(** [set manifests] loads the manifests (paths relative to [$MANIFEST_FILES_ROOT]). Does
    nothing when [manifests] is empty. *)
val set : string list -> unit

(** [resolve name] is the location the loaded manifests give for [name] if [name] is a
    bare file name they list; [name] otherwise. The linkers open their inputs through
    this function, so the name given on the command line is what the output embeds (in
    [//# <line> "<file>"] directives and in Wasm asset names). *)
val resolve : string -> string
