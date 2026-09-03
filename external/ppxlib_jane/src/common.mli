open Ppxlib_ast

(** Similar to {!Ppxlib.get_type_param_name}, but also tells you the jkind of the
    variable. Raises a located error in case of failure. The type parameter should not be
    [_]. One way to ensure this is to get the type from {!Ppxlib.name_type_params_in_td}
    (or one of the similarly-named functions). *)
val get_type_param_name_and_jkind
  :  Parsetree.core_type * (Asttypes.variance * Asttypes.injectivity)
  -> string Location.loc * Shim.jkind_annotation option

val get_type_param_name_and_jkind_of_core_type
  :  Parsetree.core_type
  -> string Location.loc * Shim.jkind_annotation option

(** Check if the parsetree is of an unlabeled tuple (i.e. all labels are [None]), and
    return the components of the tuple dropping its [None] labels. *)
val as_unlabeled_tuple : (string option * 'a) list -> 'a list option

(** Modifies an [include_description] to append "__local" to the module type name. In
    particular, this is useful for modifying the result of {!Ppxlib.mk_named_sig}. For
    example, if the latter produces some [incl] representing
    [include Bin_prot.Binable.S1 with type 'a t := 'a t], calling
    [localize_include_sig incl] will turn this into
    [include Bin_prot.Binable.S1__local with type 'a t := 'a t]. *)
val localize_include_sig : Ast.include_description -> Ast.include_description

(** Like [localize_include_sig], but appends "__stack" rather than "__local". *)
val stackify_include_sig : Ast.include_description -> Ast.include_description

(** Like [localize_include_sig], but appends an arbitrary suffix.

    This is useful when the suffix for your signature is not a standard mangling suffix as
    produced by [ppx_template]. *)
val append_arbitrary_suffix_to_include_signature
  :  Ast.include_description
  -> suffix:string
  -> Ast.include_description
