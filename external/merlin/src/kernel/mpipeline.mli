type t
val make : Mconfig.t -> Msource.t -> t

(** Create a pipeline with a pre-provided parsetree, bypassing reader and ppx phases.

    The parsetree is used directly for type checking. The source is still needed for
    position resolution (e.g., [get_lexing_pos]).

    This is used for fast-path completion: the caller gets the ppx parsetree from a normal
    pipeline, strips uninteresting parts, then creates a new pipeline with the stripped
    parsetree for cheaper type checking.

    The config should have a modified filename (e.g., append ["__for_completion"]) to avoid
    cache collisions with the normal pipeline.

    The [state] parameter allows reusing the typer state from an existing pipeline,
    which is necessary when called from within [with_pipeline] (where [Local_store] is
    already bound and creating a new state would fail). Use the state from the normal
    pipeline that provided the parsetree. *)
val make_with_parsetree : state:Mocaml.typer_state -> Mconfig.t -> Msource.t -> Mreader.parsetree -> t

val with_pipeline : t -> (unit -> 'a) -> 'a
val for_completion : Msource.position -> t -> t

val raw_source : t -> Msource.t
val typer_state : t -> Mocaml.typer_state

val input_config : t -> Mconfig.t
val input_source : t -> Msource.t
val get_lexing_pos : t -> [< Msource.position ] -> Lexing.position

val reader_config : t -> Mconfig.t
val reader_comments : t -> (string * Location.t) list
val reader_parsetree : t -> Mreader.parsetree
val reader_lexer_keywords : t -> string list
val reader_lexer_errors : t -> exn list
val reader_parser_errors : t -> exn list
val reader_no_labels_for_completion : t -> bool

val ppx_parsetree : t -> Mreader.parsetree
val ppx_errors : t -> exn list

val final_config : t -> Mconfig.t

val typer_result : t -> Mtyper.result
val typer_errors : t -> exn list

val document_overrides : t -> string Overrides.t
val locate_overrides : t -> Lexing.position Overrides.t

val timing_information : t -> (string * float) list
val cache_information : t -> Std.json
