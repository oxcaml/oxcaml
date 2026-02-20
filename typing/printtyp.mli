(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, INRIA Paris                        *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Printing functions *)


open Types

type namespace := Shape.Sig_component_kind.t option

val namespaced_ident: namespace -> Ident.t -> string
val string_of_path: Path.t -> string
val strings_of_paths: namespace -> Path.t list -> string list
(** Print a list of paths, using the same naming context to
    avoid name collisions *)

val raw_row_desc : Format.formatter -> row_desc -> unit
val raw_type_expr: Format.formatter -> type_expr -> unit
val raw_field : Format.formatter -> row_field -> unit

(* CR sspies: We seem to be using [Types.arg_label] whereas upstream uses
   [Asttypes.arg_label]. [string_of_label] moved to Asttypes upstream. *)
val string_of_label: Types.arg_label -> string

val expand_module_type: (Env.t -> module_type -> module_type) ref

val rewrite_double_underscore_longidents: Env.t -> Longident.t -> Longident.t

(** [printed_signature sourcefile ppf sg] print the signature [sg] of
        [sourcefile] with potential warnings for name collisions *)
val printed_signature: string -> Format.formatter -> signature -> unit

(* for [Translquote] *)
type typobject_repr = { fields : (string * type_expr) list; open_row : bool }
type typvariant_repr = {
  fields : (string * bool * type_expr list) list;
  name : (Path.t * type_expr list) option;
  closed : bool;
  present : (string * row_field) list;
  all_present : bool;
  tags : string list option
}
val tree_of_typobject_repr : type_expr -> typobject_repr
val tree_of_typvariant_repr : row_desc -> typvariant_repr

module type Printers := sig

    val wrap_printing_env: error:bool -> Env.t -> (unit -> 'a) -> 'a
    (** Call the function using the environment for type path shortening This
        affects all the printing functions below Also, if [~error:true], then
        disable the loading of cmis *)

    type 'a printer
    val longident: Longident.t printer
    val ident: Ident.t printer
    val path: Path.t printer
    val type_path: Path.t printer
    (** Print a type path taking account of [-short-paths].
        Calls should be within [wrap_printing_env]. *)


    (** Print out a type. This will pick names for type variables, and will not
        reuse names for common type variables shared across multiple type
        expressions. (It will also reset the printing state, which matters for
        other type formatters such as [prepared_type_expr].) If you want
        multiple types to use common names for type variables, see
        {!Out_type.prepare_for_printing} and {!Out_type.prepared_type_expr}. *)
    val type_expr: type_expr printer

    val type_scheme: type_expr printer

    val shared_type_scheme: type_expr printer
    (** [shared_type_scheme] is very similar to [type_scheme], but does not
        reset the printing context first. This is intended to be used in cases
        where the printing should have a particularly wide context, such as
        documentation generators; most use cases, such as error messages, have
        narrower contexts for which [type_scheme] is better suited. *)

    val type_expansion:
      Out_type.type_or_scheme -> Errortrace.expanded_type printer

    val label : label_declaration printer

    val constructor : constructor_declaration printer
    val constructor_arguments: constructor_arguments printer

    val extension_constructor:
      Ident.t -> extension_constructor printer
    (** Prints extension constructor with the type signature:
         type ('a, 'b) bar += A of float
    *)

    val extension_only_constructor:
      Ident.t -> extension_constructor printer
    (** Prints only extension constructor without type signature:
         A of float
    *)


    val value_description: Ident.t -> value_description printer
    val type_declaration: Ident.t -> type_declaration printer
    val modtype_declaration: Ident.t -> modtype_declaration printer
    val class_declaration: Ident.t -> class_declaration printer
    val cltype_declaration: Ident.t -> class_type_declaration printer


    val modtype: module_type printer
    val signature: signature printer
    val class_type: class_type printer

    (* CR sspies: The signature of [modality] was changed to use
       [unit printer] for [~id] (instead of [Format_doc.formatter -> unit])
       so it fits into [Printers]. The call sites in
       typecore.ml [report_block_index_error] and
       includecore.ml [report_modality_sub_error] were updated to pass
       [fun ppf () -> ...] instead of [fun ppf -> ...]. *)

    (** Prints a modality. If it is the identity modality, prints [id],
        which defaults to nothing. *)
    val modality :
      ?id:unit printer -> 'a Mode.Modality.Axis.t -> 'a printer

  end

module Doc : Printers with type 'a printer := 'a Format_doc.printer

(** For compatibility with Format printers *)
include Printers with type 'a printer := 'a Format_doc.format_printer
