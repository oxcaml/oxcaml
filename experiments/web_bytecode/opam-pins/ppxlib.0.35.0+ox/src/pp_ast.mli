(** This module implements pretty printers for the OCaml AST's version used by
    ppxlib.

    Those pretty printers show the AST as its OCaml representation and do not
    pretty print the corresponding source code. For printing ASTs as source code
    use the {!Ppxlib.Pprintast} module instead.

    For example, calling [Pp_ast.expression Format.std_formatter [%expr x + 2]]
    will print:
    {v
   Pexp_apply
     ( Pexp_ident (Lident "+")
     , [ ( Nolabel, Pexp_ident (Lident "x"))
       ; ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
       ]
     )
    v}

    To keep the output easily readable, records with [_desc] fields such as
    {!Ppxlib.Ast.type-expression} or {!Ppxlib.Ast.type-pattern} are not printed
    as such and only the value of the corresponding [_desc] field is printed
    instead. This prevents AST nodes metadata, such as locations or attributes,
    from polluting the output, keeping it relatively concise and clean. The same
    goes for {!Location.type-loc} values which are printed as the value of their
    [txt] field.

    {!Location.t} and {!Ppxlib.Ast.attributes} are not displayed by default even
    outside of the records mentioned above.

    The {!Config} module below allows to override part or all of this behaviour.
    When configured to display locations or attributes, the entire record will
    be displayed, not only its [_desc] field. *)

open! Import

module Config : sig
  type t
  (** Type for AST pretty-printing config *)

  val make :
    ?show_attrs:bool ->
    ?show_locs:bool ->
    ?loc_mode:[ `Short | `Full ] ->
    unit ->
    t
  (** Create a custom pretty-printing config. Default values are the ones that
      are used when no configuration is passed to the pretty-printers defined in
      {!Pp_ast}.
      @param ?show_attrs
        controls whether attributes are shown or hidden. Defaults to [false].
      @param ?show_loc
        controls whether locations are shown or hidden. Defaults to [false].
      @param ?loc_mode
        controls how locations are shown if they are shown at all. Defaults to
        [`Short].
        - When set to [`Short], locations are displayed as ["l1c6..l2c2"] for
          multiline locations and as ["l1c6..12"] for single line locations.
          Ghost locations are suffixed with a ["(g)"].
        - When set to [`Full], locations are displayed as any other record would
          be. *)
end

type 'a pp = Format.formatter -> 'a -> unit
type 'a configurable = ?config:Config.t -> 'a pp
type 'a configured = 'a pp

module type S = sig
  type 'a printer

  val structure : structure printer
  val structure_item : structure_item printer
  val signature : signature printer
  val signature_item : signature_item printer
  val expression : expression printer
  val pattern : pattern printer
  val core_type : core_type printer
end

module type Conf = sig
  val config : Config.t
end

module type Configured = S with type 'a printer = 'a configured
module type Configurable = S with type 'a printer = 'a configurable

module Make (Conf : Conf) : Configured [@@ocaml.warning "-67"]

val make : Config.t -> (module Configured)

module Default : Configured
include Configurable
