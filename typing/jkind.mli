(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Chris Casinghino, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Allowance
module Jkind0 := Btype.Jkind0

(* This module is named Jkind, with a 'j', to distinguish jkinds
   as used here from type kinds (which might be abstract or record or variant,
   etc.). This is clearly far from ideal, but the current scheme has these
   positives:

   * It allows us to call jkinds "kinds" to users, connecting that word with
     a word that actually appears in the code.

   * It allows us to use "layout" to refer to the component of a jkind that
     describes the in-memory/in-register layout of a type. Using "layout"
     to refer to the whole jkind seems worse than the already-terrible "jkind".

   * We could imagine renaming the existing "kind" to something else ("shape"?),
     but that would introduce merge conflicts. Perhaps, with broad support from
     OCaml developers, we can make this switch someday.

   * It is very easy to search for and replace when we have a better name.
*)

(* The externality mode. This tracks whether or not an expression is external
   to the type checker; something external to the type checker can be skipped
   during garbage collection.

   This will eventually be incorporated into the mode
   solver, but it is defined here because we do not yet track externalities
   on expressions, just in jkinds. *)

module Sort : sig
  include
    Jkind_intf.Sort
      with type t = Jkind_types.Sort.t
       and type univar = Jkind_types.Sort.univar
       and type base = Jkind_types.Sort.base
       and type Const.t = Jkind_types.Sort.Const.t

  module Flat : sig
    (** A flat sort is returned from [get]. *)
    type t =
      | Var of Var.id (* [Var.id] is for debugging / printing only *)
      | Univar of univar
      | Base of base
  end
end

type sort = Sort.t

module Sub_failure_reason : sig
  type t = Jkind0.Violation.Sub_failure_reason.t =
    | Axis_disagreement of Jkind_axis.Axis.packed
    | Layout_disagreement
    | With_bounds_on_left
    | Constrain_ran_out_of_fuel
end

module Sub_result : sig
  type t =
    | Equal
    | Less
    | Not_le of Sub_failure_reason.t Misc.Nonempty_list.t

  val of_le_result :
    failure_reason:(unit -> Sub_failure_reason.t Misc.Nonempty_list.t) ->
    Misc.Le_result.t ->
    t

  val combine : t -> t -> t

  val require_le : t -> (unit, Sub_failure_reason.t Misc.Nonempty_list.t) result

  val is_le : t -> bool
end

module Scannable_axes : sig
  type t = Jkind_types.Scannable_axes.t

  (** Omits all axes that are max, for printing *)
  val to_string_list : t -> string list
end

(* The layout of a type describes its memory layout. A layout is either the
   indeterminate [Any] or a sort, which is a concrete memory layout. *)
module Layout : sig
  type 'sort t = 'sort Jkind_types.Layout.t =
    | Sort of 'sort * Scannable_axes.t
    | Product of 'sort t list
    | Any of Scannable_axes.t

  module Const : sig
    type t = Jkind_types.Layout.Const.t

    val get_sort : t -> Sort.Const.t option

    val of_sort_const : Sort.Const.t -> Scannable_axes.t -> t

    val to_string : t -> string
  end

  val sub : level:int -> Sort.t t -> Sort.t t -> Sub_result.t

  module Debug_printers : sig
    val t :
      (Format.formatter -> 'sort -> unit) -> Format.formatter -> 'sort t -> unit
  end
end

module Mod_bounds : sig
  val debug_print : Format.formatter -> Types.mod_bounds -> unit
end

module With_bounds : sig
  val debug_print_types : Format.formatter -> Types.with_bounds_types -> unit

  val debug_print : Format.formatter -> ('l * 'r) Types.with_bounds -> unit

  val map_type_expr :
    (Types.type_expr -> Types.type_expr) ->
    ('l * 'r) Types.with_bounds ->
    ('l * 'r) Types.with_bounds

  val format : Format_doc.formatter -> ('l * 'r) Types.with_bounds -> unit
end

module Base_and_axes : sig
  val jkind_desc_of_const : 'd Types.jkind_const_desc -> 'd Types.jkind_desc
end

(** A [jkind] is a full description of the runtime representation of values of a
    given type. It includes sorts, but also the abstract top jkind [Any] and
    subjkinds of other sorts, such as [Immediate].

    The type parameter gives information about whether the jkind can
    meaningfully appear to the left of a subjkind check (this is an l-jkind) or
    on the right of a subjkind check (this is an r-jkind).

    It may be convenient to use synonyms exported from [Types]:

    * [jkind_l]: This is the jkind of an actual type; it is returned from
    [estimate_type_jkind], for example. We can compute the joins (unions) of
    l-jkinds and check that an l-jkind is less than an r-jkind.

    * [jkind_r]: This is the jkind we want some type to have. Type variables
    have r-jkinds (because we will someday instantiate those variables). The
    type passed to [constrain_type_jkind] is an r-jkind. We can compute the
    meets (intersections) of r-jkinds and check that an l-jkind is less than an
    r-jkind. *)

include Allowance.Allow_disallow with type (_, _, 'd) sided = 'd Types.jkind

(** Try to treat this jkind as an r-jkind. *)
val try_allow_r : ('l * 'r) Types.jkind -> ('l * allowed) Types.jkind option

module History : sig
  include module type of struct
    include Jkind_intf.History
  end

  (* history *)

  val is_imported : 'd Types.jkind -> bool

  val update_reason : 'd Types.jkind -> creation_reason -> 'd Types.jkind

  (* Mark the jkind as having produced a compiler warning. *)
  val with_warning : 'd Types.jkind -> 'd Types.jkind

  (* Whether this jkind has produced a compiler warning. *)
  val has_warned : 'd Types.jkind -> bool
end

(******************************)
(* context *)

(** Context for jkind operations. *)
type jkind_context =
  { jkind_of_type : Types.type_expr -> Types.jkind_l option;
    is_abstract : Path.t -> bool
        (* Check if a type path refers to an abstract type *)
  }

(******************************)
(* errors *)

module Violation : sig
  type violation = Jkind0.Violation.violation =
    (* [Not_a_subjkind] allows l-jkinds on the right so that it can be used
       in [sub_jkind_l]. There is no downside to this, as the printing
       machinery works over l-jkinds. *)
    | Not_a_subjkind :
        (allowed * 'r1) Types.jkind
        * ('l * 'r2) Types.jkind
        * Sub_failure_reason.t list
        -> violation
    | No_intersection : 'd Types.jkind * ('l * allowed) Types.jkind -> violation

  type t = Jkind0.Violation.t

  (** Set [?missing_cmi] to mark [t] as having arisen from a missing cmi *)

  val of_ :
    context:jkind_context -> ?missing_cmi:Path.t -> Env.t -> violation -> t

  (** Is this error from a missing cmi? *)
  val is_missing_cmi : t -> bool

  (* CR layouts: The [offender] arguments below are always
     [Printtyp.type_expr], so we should either stash that in a ref (like with
     [set_printtyp_path] below) or just move all the printing machinery
     downstream of both [Jkinds] and [Printtyp]. *)

  (* CR layouts: Having these options for printing a violation was a choice
     made based on the needs of expedient debugging during development, but
     probably should be rethought at some point. *)

  (** Prints a violation and the thing that had an unexpected jkind ([offender],
      which you supply an arbitrary printer for). *)
  val report_with_offender :
    offender:(Format_doc.formatter -> unit) ->
    level:int ->
    Env.t ->
    Format_doc.formatter ->
    t ->
    unit

  (** Like [report_with_offender], but additionally prints that the issue is
      that a representable jkind was expected. *)
  val report_with_offender_sort :
    offender:(Format_doc.formatter -> unit) ->
    level:int ->
    Env.t ->
    Format_doc.formatter ->
    t ->
    unit

  (** Simpler version of [report_with_offender] for when the thing that had an
      unexpected jkind is available as a string. *)
  val report_with_name :
    name:string -> level:int -> Env.t -> Format_doc.formatter -> t -> unit
end

(******************************)
(* constants *)

module Const : sig
  (** Constant jkinds are used for user-written annotations. They are not
      actually constant, though: they might contain variables in [with]-types.
      The "constant" refers to the fact that there are no sort variables here.
      The existence of [with]-types means, though, that we still need the
      allowance machinery here. *)
  type 'd t = 'd Jkind0.Const.t constraint 'd = 'l * 'r

  include Allowance.Allow_disallow with type (_, _, 'd) sided = 'd t

  val to_out_jkind_const : Env.t -> 'd t -> Outcometree.out_jkind_const

  (** Creates an abstract jkind, with max with-bounds, from the path. *)
  val of_path : Path.t -> (allowed * allowed) t

  val equal : Env.t -> (allowed * allowed) t -> (allowed * allowed) t -> bool

  (* [use_abstract_jkinds] controls whether references to other kinds here count
     as uses of them for unused abstract kind warnings. *)
  val of_annotation :
    ?use_abstract_jkinds:bool ->
    context:('l * allowed) History.annotation_context ->
    Env.t ->
    Parsetree.jkind_annotation ->
    ('l * allowed) t

  val expand_once : Env.t -> (_, 'd) Types.base_and_axes -> 'd t option
end

module Builtin : sig
  (** This jkind is the top of the jkind lattice. All types have jkind [any].
      But we cannot compile run-time manipulations of values of types with jkind
      [any]. *)
  val any : why:History.any_creation_reason -> 'd Types.jkind

  (** Value of types of this jkind are not retained at all at runtime *)
  val void : why:History.void_creation_reason -> ('l * disallowed) Types.jkind

  val value_or_null :
    why:History.value_or_null_creation_reason -> 'd Types.jkind

  (** This is the jkind of normal ocaml values *)
  val value : why:History.value_creation_reason -> 'd Types.jkind

  (** This is suitable for records or variants without mutable fields. *)
  val immutable_data : why:History.value_creation_reason -> 'd Types.jkind

  (** This is suitable for records or variants with atomically mutable fields.
  *)
  val sync_data : why:History.value_creation_reason -> 'd Types.jkind

  (** This is suitable for records or variants with mutable fields. *)
  val mutable_data : why:History.value_creation_reason -> 'd Types.jkind

  (** We know for sure that values of types of this jkind are always immediate
  *)
  val immediate :
    why:History.immediate_creation_reason -> ('l * disallowed) Types.jkind

  (** Values of types of this jkind are either immediate or null pointers *)
  val immediate_or_null :
    why:History.immediate_or_null_creation_reason -> 'd Types.jkind

  (** Build a jkind of unboxed products, from a list of types with their
      layouts. Errors if zero inputs are given.

      Precondition: both input lists are the same length.

      This returns an [jkind_l] simply as a matter of convenience; it can be
      generalized if need be.

      The resulting jkind has quality [Best], because all the components of the
      product are represented in the with-bounds. *)
  val product :
    why:History.product_creation_reason ->
    (Types.type_expr * Mode.Modality.Const.t) list ->
    Sort.t Layout.t list ->
    Types.jkind_l

  (** Build a jkind of unboxed products, given only an arity. This jkind will
      not mode-cross (and has kind [Not_best] accordingly), even though unboxed
      products generally should. This is useful when creating an initial jkind
      in Typedecl. *)
  val product_of_sorts :
    why:History.product_creation_reason -> level:int -> int -> Types.jkind_l
end

(** Forcibly change the mod- and with-bounds of a [t] based on the mod- and
    with-bounds of [from]. *)
val unsafely_set_bounds :
  Env.t -> from:'d Types.jkind -> 'd Types.jkind -> 'd Types.jkind

(** Does this jkind have with-bounds? *)
val has_with_bounds : Types.jkind_l -> bool

(** Mark the given jkind as {i best}, meaning we can never learn any more
    information about it that will cause it to become lower in the preorder of
    kinds*)
val mark_best : ('l * 'r) Types.jkind -> ('l * disallowed) Types.jkind

(** Is the given kind best? *)
val is_best : ('l * disallowed) Types.jkind -> bool

(******************************)
(* construction *)

(** Create a fresh sort variable, packed into a jkind, returning both the
    resulting kind and the sort. *)
val of_new_sort_var :
  why:History.concrete_creation_reason -> level:int -> 'd Types.jkind * sort

(** Create a fresh sort variable, packed into a jkind. *)
val of_new_sort :
  why:History.concrete_creation_reason -> level:int -> 'd Types.jkind

(** Same as [of_new_sort], but the jkind is lowered to [Non_null] to mirror
    "legacy" OCaml values. Defaulting the sort variable produces exactly
    [value]. *)
val of_new_legacy_sort :
  why:History.concrete_legacy_creation_reason -> level:int -> 'd Types.jkind

(** Same as [of_new_sort_var], but the jkind is lowered to [Non_float].
    Defaulting the sort variable produces exactly the sort [value]. *)
val of_new_non_float_sort_var :
  why:History.concrete_creation_reason -> level:int -> 'd Types.jkind * sort

(** Create a jkind with a specific sort univar (for layout-polymorphic types).
*)
val of_sort_univar :
  why:History.concrete_creation_reason ->
  Jkind_types.Sort.univar ->
  'd Types.jkind

(* [use_abstract_jkinds] controls whether references to other kinds here count
   as uses of them for unused abstract kind warnings. *)
val of_annotation :
  ?use_abstract_jkinds:bool ->
  context:('l * allowed) History.annotation_context ->
  Env.t ->
  Parsetree.jkind_annotation ->
  ('l * allowed) Types.jkind

(* [use_abstract_jkinds] controls whether references to other kinds here count
   as uses of them for unused abstract kind warnings. *)
val of_annotation_option_default :
  ?use_abstract_jkinds:bool ->
  default:('l * allowed) Types.jkind ->
  context:('l * allowed) History.annotation_context ->
  Env.t ->
  Parsetree.jkind_annotation option ->
  ('l * allowed) Types.jkind

(** Find a jkind from a type declaration. Type declarations are special because
    the jkind may have been provided via [: jkind] syntax (which goes through
    Jane Syntax) or via the old-style [[@@immediate]] or [[@@immediate64]]
    attributes, and [of_type_decl] needs to look in two different places on the
    [type_declaration] to account for these two alternatives.

    Returns the jkind (at quality [Not_best]) and the user-written annotation.

    Raises if a disallowed or unknown jkind is present.

    [use_abstract_jkinds] controls whether references to other kinds here count
    as uses of them for unused abstract kind warnings. *)
val of_type_decl :
  ?use_abstract_jkinds:bool ->
  context:History.annotation_context_l ->
  transl_type:(Parsetree.core_type -> Types.type_expr) ->
  Env.t ->
  Parsetree.type_declaration ->
  (Types.jkind_l * Parsetree.jkind_annotation option) option

(** Find a jkind from a type declaration in the same way as [of_type_decl], but
    avoiding translating types in with-bounds. Overapproximates kinds containing
    with-bounds as [any].

    Returns the jkind (at quality [Not_best]). *)
val of_type_decl_overapproximate_unknown :
  context:History.annotation_context_l ->
  Env.t ->
  Parsetree.type_declaration ->
  Types.jkind_l option

(** Choose an appropriate jkind for a boxed record type *)
val for_boxed_record : Types.label_declaration list -> Types.jkind_l

(** Choose an appropriate jkind for an unboxed record type. *)
val for_unboxed_record :
  Types.label_declaration list -> sort Layout.t list -> Types.jkind_l

(** Choose an appropriate jkind for a boxed variant type.

    [decl_params] is the parameters in the head of the type declaration.
    [type_apply] should be [Ctype.apply] partially applied to an [env].

    [free_vars] is a function that, given a list of [Types.type_expr]s that are
    used in the boxed variant, returns all type variables that are free within
    the [Types.type_expr]s. [Ctype.free_variable_set_of_list] is a good
    candidate for implementing this function. *)
val for_boxed_variant :
  loc:Location.t ->
  decl_params:Types.type_expr list ->
  type_apply:
    (Types.type_expr list ->
    Types.type_expr ->
    Types.type_expr list ->
    Types.type_expr) ->
  free_vars:(Types.type_expr list -> Btype.TypeSet.t) ->
  Types.constructor_declaration list ->
  Types.jkind_l

(** Choose an appropriate jkind for a boxed tuple type. *)
val for_boxed_tuple : (string option * Types.type_expr) list -> Types.jkind_l

(** Choose an appropriate jkind for a row type. *)
val for_boxed_row : Types.row_desc -> Types.jkind_l

(** The jkind of an arrow type. *)
val for_arrow : Types.jkind_l

(** The jkind of an object type. *)
val for_object : Types.jkind_l

(** The jkind for values that are not floats. *)
val for_non_float : why:History.value_creation_reason -> 'd Types.jkind

(** The jkind for an abbreviation declaration. This implements the design in
    rule FIND_ABBREV in kind-inference.md, where we consider a definition

    {[
      type ... = rhs
    ]}

    to have the kind [<<layout of rhs>> mod everything with rhs]. This is
    important to allow code like this to type-check:

    {[
      module M : sig
        type 'a t : value mod portable with 'a
      end = struct
        type 'a t = 'a
      end
    ]} *)
val for_abbreviation :
  type_jkind_purely:(Types.type_expr -> Types.jkind_l) ->
  modality:Mode.Modality.Const.t ->
  Types.type_expr ->
  Types.jkind_l

(** The jkind for array elements, creating a new sort variable. *)
val for_array_element_sort : level:int -> Types.jkind_lr * sort

(******************************)
(* elimination and defaulting *)

module Desc : sig
  (** The description of a jkind, used as a return type from [get]. This
      description has no sort variables, but it might have [with]-types and thus
      needs the allowance machinery. *)
  type 'd t = (Sort.Flat.t Layout.t, 'd) Types.base_and_axes

  val get_const : 'd t -> 'd Const.t option

  val of_const : 'd Const.t -> 'd t

  val format : Env.t -> Format_doc.formatter -> 'd t -> unit
end

(** Get a description of a jkind. *)
val get : 'd Types.jkind -> 'd Desc.t

(** [get_layout_defaulting_to_scannable] extracts a constant layout, defaulting
    any sort variable to [scannable]. Returns [None] in the case of an abstract
    kind. *)
val get_layout_defaulting_to_scannable :
  Env.t -> 'd Types.jkind -> Layout.Const.t option

(** [default_to_scannable t] is [ignore (get_layout_defaulting_to_scannable t)]
*)
val default_to_scannable : 'd Types.jkind -> unit
(* CR layouts v5: When we have proper support for void, we'll want to change
   these three functions to default to void - it's the most efficient thing
   when we have a choice. *)

(** Returns the sort corresponding to the jkind. Call only on representable
    jkinds - raises on Any. *)
val sort_of_jkind : Env.t -> Types.jkind_l -> sort

(** Gets the layout of a jkind; returns [None] if the layout is still unknown,
    or the (fully expanded) kind is abstract. Never does mutation. *)
val get_layout : Env.t -> 'd Types.jkind -> Layout.Const.t option

(* CR reisenberg: do we need [extract_layout]? *)

(** Gets the layout of a jkind, without looking through sort variables. Returns
    [Error p] if the kind is abstract (and its layout is therefore unknown). *)
val extract_layout : Env.t -> 'd Types.jkind -> (Sort.t Layout.t, Path.t) result

(** Gets the mode crossing for types of this jkind. *)
val get_mode_crossing :
  context:jkind_context -> Env.t -> 'd Types.jkind -> Mode.Crossing.t

val to_unsafe_mode_crossing : Types.jkind_l -> Types.unsafe_mode_crossing

val get_externality_upper_bound :
  context:jkind_context -> Env.t -> 'd Types.jkind -> Jkind_axis.Externality.t

(** Computes a jkind that is the same as the input but with an updated maximum
    mode for the externality axis *)
val set_externality_upper_bound :
  Types.jkind_r -> Jkind_axis.Externality.t -> Types.jkind_r

(** Gets the nullability from a jkind. *)
val get_nullability : Types.jkind_l -> Jkind_axis.Nullability.t option

(** Computes a jkind that is the same as the input but with an updated maximum
    mode for the nullability axis *)
val set_root_nullability :
  Types.jkind_r -> Jkind_axis.Nullability.t -> Types.jkind_r

(** Computes a jkind that is the same as the input but with an updated maximum
    mode for the separability axis *)
val set_root_separability :
  Types.jkind_r -> Jkind_axis.Separability.t -> Types.jkind_r

(** Sets the layout in a jkind. *)
val set_layout : 'd Types.jkind -> Sort.t Layout.t -> 'd Types.jkind

(** Change a jkind to be appropriate for a type that appears under a modality.
    This means that the jkind will definitely cross the axes modified by the
    modality, by setting the mod-bounds appropriately and propagating the
    modality into any with-bounds. *)
val apply_modality_l :
  Mode.Modality.Const.t -> (allowed * 'r) Types.jkind -> Types.jkind_l

(** Change a jkind to be appropriate for an expectation of a type under a
    modality. This means that the jkind's axes affected by the modality will all
    be top. The with-bounds are left unchanged. *)
val apply_modality_r :
  Mode.Modality.Const.t -> ('l * allowed) Types.jkind -> Types.jkind_r

(** Change a jkind to be appropriate for ['a or_null] based on passed ['a].
    Adjusts nullability to be [Maybe_null], and separability to be
    [Maybe_separable] if it is already [Separable]. If the jkind is already
    [Maybe_null], fails. *)
val apply_or_null_l : Types.jkind_l -> (Types.jkind_l, unit) result

(** Change a jkind to be appropriate for an expectation of a type passed to the
    [or_null] constructor. Adjusts nullability to be [Non_null], and
    separability to be [Non_float] if it is demanded to be [Separable]. If the
    jkind is already [Non_null], fails. *)
val apply_or_null_r : Types.jkind_r -> (Types.jkind_r, unit) result

(** Extract out component jkinds from the product. Because there are no product
    jkinds, this is a bit of a lie: instead, this decomposes the layout but just
    reuses the non-layout parts of the original jkind. Never does any mutation.
    Because it just reuses the mode information, the resulting jkinds are higher
    in the jkind lattice than they might need to be. *)
val decompose_product : Env.t -> 'd Types.jkind -> 'd Types.jkind list option

(** Get an annotation (that a user might write) for this [t]. *)
val get_annotation : 'd Types.jkind -> Parsetree.jkind_annotation option

(*********************************)
(* normalization *)

type normalize_mode =
  | Require_best
      (** Normalize a jkind without losing any precision. That is, keep any
          with-bounds if the kind of the type is not best (a stronger kind may
          be found). *)
  | Ignore_best
      (** Normalize a left jkind, conservatively rounding up. That is, if the
          kind of a type is not best, use the not-best kind. The resulting jkind
          will have no with-bounds. *)

val normalize :
  mode:normalize_mode ->
  context:jkind_context ->
  Env.t ->
  Types.jkind_l ->
  Types.jkind_l

(*********************************)
(* pretty printing *)

(** Call these before trying to print. *)
val set_outcometrees_of_types :
  (Types.type_expr list -> Outcometree.out_type list) -> unit

val set_outcometree_of_modalities :
  (Types.mutability -> Mode.Modality.Const.t -> Outcometree.out_mode list) ->
  unit

(** Provides the [Printtyp.path] formatter back up the dependency chain to this
    module. *)
val set_printtyp_path : (Format_doc.formatter -> Path.t -> unit) -> unit

(** Provides the [type_expr] formatter back up the dependency chain to this
    module. *)
val set_print_type_expr : Types.type_expr Format_doc.printer -> unit

(** Provides the [raw_type_expr] formatter back up the dependency chain to this
    module. *)
val set_raw_type_expr : (Format.formatter -> Types.type_expr -> unit) -> unit

val format : Env.t -> Format_doc.formatter -> 'd Types.jkind -> unit

module Format_verbosity : sig
  type t =
    | Not_verbose  (** Normal, user-friendly printing *)
    | Expanded  (** Don't write the jkind in terms of any aliases. *)
    | Expanded_with_all_mod_bounds
        (** Like [Expanded], but also explicitly write all mod bounds, even top
            ones. *)
end

(** Similar to [format], but the kind is expanded as much as possible rather
    than written in terms of a kind abbreviation. This is used by Merlin. *)
val format_verbose :
  verbosity:Format_verbosity.t ->
  Env.t ->
  Format_doc.formatter ->
  'd Types.jkind ->
  unit

(** Format the history of this jkind: what interactions it has had and why it is
    the jkind that it is. Might be a no-op: see [display_histories] in the
    implementation of the [Jkind] module.

    The [intro] is something like "The jkind of t is". *)
val format_history :
  intro:(Format_doc.formatter -> unit) ->
  Env.t ->
  Format_doc.formatter ->
  'd Types.jkind ->
  unit

(******************************)
(* relations *)

(** This checks for equality, and sets any variables to make two jkinds equal,
    if possible. e.g. [equate] on a var and [value] will set the variable to be
    [value] *)
val equate : level:int -> Env.t -> Types.jkind_lr -> Types.jkind_lr -> bool

(** This checks for equality, but has the invariant that it can only be called
    when there is no need for unification; e.g. [equal] on a var and [value]
    will crash.

    CR layouts (v1.5): At the moment, this is actually the same as [equate]! *)
val equal : level:int -> Env.t -> Types.jkind_lr -> Types.jkind_lr -> bool

(** Checks whether two jkinds have a non-empty intersection. Might mutate sort
    variables. Works over any mix of l- and r-jkinds, because the only way not
    to have an intersection is by looking at the layout: all axes have a bottom
    element.

    When abstract kinds are involved and we cannot determine whether there is an
    intersection, this conservatively returns [true]. *)
val may_have_intersection :
  level:int -> Env.t -> 'd1 Types.jkind -> 'd2 Types.jkind -> bool

type 'd intersection_result =
  | Intersection of 'd Types.jkind
  | No_intersection of Violation.t
  | Unknown
      (** [Unknown] can happen in the case of computing an intersection with an
          abstract kind. In cases related to GADT matching, we need to
          distinguish it from the [No_intersection] result to avoid unsoundly
          believing match cases are unreachable. *)

val intersection :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:jkind_context ->
  reason:History.interact_reason ->
  level:int ->
  Env.t ->
  ('l1 * allowed) Types.jkind ->
  ('l2 * allowed) Types.jkind ->
  ('l1 * allowed) intersection_result

(** Finds the intersection of two jkinds, constraining sort variables to create
    one if needed, or returns a [Violation.t] if an intersection does not exist.
    Can update the jkinds. The returned jkind's history consists of the provided
    reason followed by the history of the first jkind argument. That is, due to
    histories, this function is asymmetric; it should be thought of as modifying
    the first jkind to be the intersection of the two, not something that
    modifies the second jkind.

    Note: When abstract kinds are involved and we cannot determine whether there
    is an intersection, this returns [Error] since we can't prove the
    intersection exists. Use [intersection_result] if you need to distinguish
    this case from definite non-intersection. *)
val intersection_or_error :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:jkind_context ->
  reason:History.interact_reason ->
  level:int ->
  Env.t ->
  ('l1 * allowed) Types.jkind ->
  ('l2 * allowed) Types.jkind ->
  (('l1 * allowed) Types.jkind, Violation.t) Result.t

(** [sub t1 t2] says whether [t1] is a subjkind of [t2]. Might update either
    [t1] or [t2] to make their layouts equal.*)
val sub :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:jkind_context ->
  level:int ->
  Env.t ->
  Types.jkind_l ->
  Types.jkind_r ->
  bool

type sub_or_intersect =
  | Sub  (** The first jkind is a subjkind of the second. *)
  | Disjoint of Sub_failure_reason.t Misc.Nonempty_list.t
      (** The two jkinds have no common ground. *)
  | May_have_intersection of Sub_failure_reason.t Misc.Nonempty_list.t
      (** The first jkind is not a subjkind of the second, but the two jkinds
          may have an intersection: try harder. *)

(** [sub_or_intersect t1 t2] does a subtype check, returning a
    [sub_or_intersect]; see comments there for more info. *)
val sub_or_intersect :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:jkind_context ->
  level:int ->
  Env.t ->
  (allowed * 'r) Types.jkind ->
  ('l * allowed) Types.jkind ->
  sub_or_intersect

(** [sub_or_error t1 t2] does a subtype check, returning an appropriate
    [Violation.t] upon failure. *)
val sub_or_error :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:jkind_context ->
  level:int ->
  Env.t ->
  (allowed * 'r) Types.jkind ->
  ('l * allowed) Types.jkind ->
  (unit, Violation.t) result

(** [sub_layout t1 t2] says whether [t1]'s layout is a sublayout of [t2]s. Might
    update either [t1] or [t2] to make their layouts equal. Does not check
    bounds at all. *)
val sub_layout_or_error :
  context:jkind_context ->
  level:int ->
  Env.t ->
  Types.jkind_l ->
  Types.jkind_l ->
  (unit, Violation.t) result

(** Like [sub], but compares a left jkind against another left jkind.
    Pre-condition: the super jkind must be fully settled; no variables which
    might be filled in later. *)
val sub_jkind_l :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  context:jkind_context ->
  level:int ->
  ?allow_any_crossing:bool ->
  Env.t ->
  Types.jkind_l ->
  Types.jkind_l ->
  (unit, Violation.t) result

(** "round up" a [jkind_l] to a [jkind_r] such that the input is less than the
    output. If the base is abstract, it may not be possible to eliminate the
    with bounds, in which case this returns [None]. *)
val round_up :
  context:jkind_context ->
  Env.t ->
  (allowed * 'r) Types.jkind ->
  ('l * allowed) Types.jkind option

(** Map a function over types in [upper_bounds] *)
val map_type_expr :
  (Types.type_expr -> Types.type_expr) ->
  (allowed * 'r) Types.jkind ->
  (allowed * 'r) Types.jkind

(** Checks to see whether a right-jkind is the maximum jkind. Never does any
    mutation. Is conservative and does not do any expansion. *)
val is_obviously_max : ('l * allowed) Types.jkind -> bool

(** Checks to see whether a jkind's mod bounds are max. Never does any mutation.
    Is conservative and does not do any expansion. *)
val mod_bounds_are_obviously_max : 'd Types.jkind -> bool

(** Fully expands the jkind's base - useful to avoid expanding twice for clients
    that both want to inspect the mod bounds and apply other functions to the
    jkind that would expand it. *)
val fully_expand_aliases : Env.t -> 'd Types.jkind -> 'd Types.jkind

(** Checks to see whether a jkind has layout any. Never does any mutation. *)
val has_layout_any :
  Env.t -> ('l * allowed) Types.jkind -> Jkind_types.Scannable_axes.t option

(** Checks whether a jkind is [value]. This really should require a [jkind_lr],
    but it works on any [jkind], because it's used in printing and is somewhat
    unprincipled. *)
val is_value_for_printing : ignore_null:bool -> Env.t -> 'd Types.jkind -> bool

(*********************************)
(* debugging *)

module Debug_printers : sig
  val t : Format.formatter -> 'd Types.jkind -> unit

  module Const : sig
    val t : Format.formatter -> 'd Const.t -> unit
  end
end
