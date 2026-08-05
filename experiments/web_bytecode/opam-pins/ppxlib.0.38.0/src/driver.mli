(** Interaction with the driver, such as getting/setting cookies, adding
    arguments.

    The relevant part in the manual is
    {{!driver.driver_execution} the section on its execution}. *)

open Import

val add_arg : Stdlib.Arg.key -> Stdlib.Arg.spec -> doc:string -> unit
(** Add one argument to the command line *)

(** Error reported by linters *)
module Lint_error : sig
  type t

  val of_string : Location.t -> string -> t
end

module Cookies : sig
  type t

  val get : t -> string -> (expression, 'a -> 'a, 'b) Ast_pattern.t -> 'b option
  (** [get cookies name pattern] look for a cookie named [name] and parse it
      using [pattern]. Raises if the parsing fails. *)

  val get_res :
    t ->
    string ->
    (expression, 'a -> 'a, 'b) Ast_pattern.t ->
    ('b option, Location.Error.t NonEmptyList.t) result
  (** [get cookies name pattern] look for a cookie named [name] and parse it
      using [pattern], returning a [result] instead of raising when the parsing
      fails. *)

  val set : t -> string -> expression -> unit
  (** [set cookies name expr] set cookie [name] to [expr]. *)

  val add_handler : (t -> unit) -> unit
  (** Register a callback that is called before a rewriting. The handler is
      expected to lookup some cookies and set some global variables.

      This API is a temporary hack to allow to migrate from [add_arg] to the use
      of cookie, until ppxlib has been upgraded to pass cookies through. *)

  val add_simple_handler :
    string ->
    (expression, 'a -> 'a, 'b) Ast_pattern.t ->
    f:('b option -> unit) ->
    unit
  (** Shorthand for: [add_handler (fun t -> f (get t name pattern))] *)

  val add_post_handler : (t -> unit) -> unit
  (** Register a callback that is called after a rewriting. The handler is
      expected to set some cookies from some global variables. *)
end

module Instrument : sig
  type t
  type pos = Before | After

  val make : (Parsetree.structure -> Parsetree.structure) -> position:pos -> t
  (** [make transformation ~position] creates an instrumentation that can be
      passed to [Driver.register_transformation] to instrument an implementation
      file. [transformation] is the transformation that will be applied to the
      AST; [position] specifies if it should be applied before or after
      rewriters defined through [rules], [impl] or [intf] are applied.*)

  module V2 : sig
    val make :
      (Expansion_context.Base.t -> Parsetree.structure -> Parsetree.structure) ->
      position:pos ->
      t
    (** Same as [Instrument.make], but the transformation that will be applied
        to the AST has access to an expansion context. To be used together with
        [Driver.V2].*)
  end
end

val register_transformation :
  ?extensions:Extension.t list (* deprecated, use ~rules instead *) ->
  ?rules:Context_free.Rule.t list ->
  ?enclose_impl:(Location.t option -> structure * structure) ->
  ?enclose_intf:(Location.t option -> signature * signature) ->
  ?impl:(structure -> structure) ->
  ?intf:(signature -> signature) ->
  ?lint_impl:(structure -> Lint_error.t list) ->
  ?lint_intf:(signature -> Lint_error.t list) ->
  ?preprocess_impl:(structure -> structure) ->
  ?preprocess_intf:(signature -> signature) ->
  ?instrument:Instrument.t ->
  ?aliases:string list ->
  string ->
  unit
(** [register_transformation name] registers a code transformation.

    [name] is a logical name for the set of transformations (such as [sexp_conv]
    or [bin_prot]). It is mostly used for debugging purposes.

    [rules] is a list of context independent rewriting rules, such as extension
    point expanders. This is what most code transformation should use.

    [extensions] is a special cases of [rules] and is deprecated. It is only
    kept for backward compatibility.

    [enclose_impl] and [enclose_intf] produces a header and footer for
    implementation/interface files. They are a special case of [impl] and
    [intf]. The header is placed after any initial module-level attributes; the
    footer is placed after everything else. Both functions receive a location
    that denotes all of the items between header and footer, or [None] if the
    that list of items is empty.

    [impl] is an optional function that is applied on implementation files and
    [intf] is an optional function that is applied on interface files. These two
    functions are applied on the AST of the whole file. They should only be used
    when the other mechanism are not enough. For instance if the transformation
    expands extension points that depend on the context.

    [lint_impl] and [lint_intf] are applied to the unprocessed source. Errors
    they return will be reported to the user as preprocessor warnings.

    [instrument] can be used to instrument implementation files. Its
    transformation is applied to the AST of the whole file. The difference to
    [impl] is that you can specify if it should be applied before or after all
    rewriters defined through [rules], [impl] or [intf] are applied.

    More information on each phase, and their relative order, can be found in
    the {{!driver.driver_execution} manual}. *)

val register_transformation_using_ocaml_current_ast :
  ?impl:
    (Compiler_version.Ast.Parsetree.structure ->
    Compiler_version.Ast.Parsetree.structure) ->
  ?intf:
    (Compiler_version.Ast.Parsetree.signature ->
    Compiler_version.Ast.Parsetree.signature) ->
  ?aliases:string list ->
  string ->
  unit
(** Same as [register_transformation] except that it uses the same AST as the
    current ocaml compiler.

    This is not the intended way of using driver. This is only for ppx rewriters
    that are not written using ppxlib but want to export a driver compatible
    library. *)

val register_code_transformation :
  name:string ->
  ?aliases:string list ->
  impl:(structure -> structure) ->
  intf:(signature -> signature) ->
  unit
[@@deprecated "[since 2015-11] use register_transformation instead"]
(** Same as:

    {[
      register_transformation ~name ~impl ~intf ()
    ]} *)

val register_correction : loc:Location.t -> repl:string -> unit
(** Rewriters might call this function to suggest a correction to the code
    source. When they do this, the driver will generate a
    [file.ml.ppx-corrected] file with the suggested replacement. The build
    system will then show the diff to the user who is free to accept the
    correction or not. *)

val register_process_file_hook : (unit -> unit) -> unit
(** Hook called before processing a file *)

module V2 : sig
  val register_transformation :
    ?extensions:Extension.t list (* deprecated, use ~rules instead *) ->
    ?rules:Context_free.Rule.t list ->
    ?enclose_impl:
      (Expansion_context.Base.t -> Location.t option -> structure * structure) ->
    ?enclose_intf:
      (Expansion_context.Base.t -> Location.t option -> signature * signature) ->
    ?impl:(Expansion_context.Base.t -> structure -> structure) ->
    ?intf:(Expansion_context.Base.t -> signature -> signature) ->
    ?lint_impl:(Expansion_context.Base.t -> structure -> Lint_error.t list) ->
    ?lint_intf:(Expansion_context.Base.t -> signature -> Lint_error.t list) ->
    ?preprocess_impl:(Expansion_context.Base.t -> structure -> structure) ->
    ?preprocess_intf:(Expansion_context.Base.t -> signature -> signature) ->
    ?instrument:Instrument.t ->
    ?aliases:string list ->
    string ->
    unit
  (** Same as [Driver.register_transformation], but the callbacks have access to
      an expansion context. Their signatures coincide with the signatures of the
      respective methods in [Ast_traverse.map_with_expansion_context]. *)

  val register_transformation_using_ocaml_current_ast :
    ?impl:
      (Expansion_context.Base.t ->
      Compiler_version.Ast.Parsetree.structure ->
      Compiler_version.Ast.Parsetree.structure) ->
    ?intf:
      (Expansion_context.Base.t ->
      Compiler_version.Ast.Parsetree.signature ->
      Compiler_version.Ast.Parsetree.signature) ->
    ?aliases:string list ->
    string ->
    unit
  (** Same as [Driver.register_transformation_using_ocaml_current_ast], but the
      callbacks [?impl] and [?intf] have access to an expansion context. *)
end

(** Create a new file property.

    A file property represent a piece of information about a file that can be
    set during preprocessing. If the [-output-metadata FILE] command line option
    was passed to the driver, then it will output this information to the given
    file.

    This mechanism is used to pass information gathered while preprocessing the
    file to the build system. For instance, this is used by ppx_inline_test to
    tell whether a file contains tests or not.

    In the future we could also use this to directly compute the dependencies
    and pass them here, to avoid calling ocamldep separately. *)
module Create_file_property
    (Name : sig
      val name : string
    end)
    (T : Sexpable.S) : sig
  val set : T.t -> unit
end
[@@ocaml.warning "-67"]

val standalone : unit -> unit
(** Suitable for -pp and also usable as a standalone command line tool.

    If the first command line argument is [-as-ppx] then it will run as a ppx
    rewriter. *)

val run_as_ppx_rewriter : unit -> unit
(** Suitable for -ppx. Used only for the public release. *)

val pretty : unit -> bool
(** If [true], code transformations should avoid generating code that is not
    strictly necessary, such as extra type annotations. *)

(**/**)

val map_structure : structure -> structure
val map_signature : signature -> signature
val enable_checks : unit -> unit
val enable_location_check : unit -> unit
val disable_location_check : unit -> unit

val load_input :
  kind:Utils.Kind.t ->
  input_name:string ->
  relocate:bool ->
  string ->
  ( string * Utils.Ast_io.input_version * Utils.Intf_or_impl.t,
    Location.Error.t * Utils.Ast_io.input_version )
  result
