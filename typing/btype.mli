(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Basic operations on core types *)

open Asttypes
open Types

(**** Sets, maps and hashtables of types ****)

module TypeSet : sig
  include Set.S with type elt = transient_expr
  val add: type_expr -> t -> t
  val mem: type_expr -> t -> bool
  val singleton: type_expr -> t
  val exists: (type_expr -> bool) -> t -> bool
  val elements: t -> type_expr list
  val debug_print : Format.formatter -> t -> unit
end
module TransientTypeMap : Map.S with type key = transient_expr
module TypeMap : sig
  include Map.S with type key = transient_expr
                     and type 'a t = 'a TransientTypeMap.t
  val add: type_expr -> 'a -> 'a t -> 'a t
  val find: type_expr -> 'a t -> 'a
  val singleton: type_expr -> 'a -> 'a t
  val fold: (type_expr -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end
module TypeHash : sig
  include Hashtbl.S with type key = transient_expr
  val mem: 'a t -> type_expr -> bool
  val add: 'a t -> type_expr -> 'a -> unit
  val replace: 'a t -> type_expr -> 'a -> unit
  val remove: 'a t -> type_expr -> unit
  val find: 'a t -> type_expr -> 'a
  val find_opt: 'a t -> type_expr -> 'a option
  val iter: (type_expr -> 'a -> unit) -> 'a t -> unit
end
module TypePairs : sig
  type t
  val create: int -> t
  val clear: t -> unit
  val add: t -> type_expr * type_expr -> unit
  val mem: t -> type_expr * type_expr -> bool
  val iter: (type_expr * type_expr -> unit) -> t -> unit
end

(**** Levels ****)

val generic_level: int
        (* level of polymorphic variables; = Ident.highest_scope *)
val lowest_level: int
        (* lowest level for type nodes; = Ident.lowest_scope *)

val newgenty: type_desc -> type_expr
        (* Create a generic type *)
val newgenvar: ?name:string -> jkind_lr -> type_expr
        (* Return a fresh generic variable *)
val newgenstub: scope:int -> jkind_lr -> type_expr
        (* Return a fresh generic node, to be instantiated
           by [Transient_expr.set_stub_desc] *)

(**** Types ****)

val is_Tvar: type_expr -> bool
val is_Tunivar: type_expr -> bool
val is_Tconstr: type_expr -> bool
val is_Tpoly: type_expr -> bool

val dummy_method: label
val type_kind_is_abstract: type_declaration -> bool
val type_origin : type_declaration -> type_origin

(**** polymorphic variants ****)

val is_fixed: row_desc -> bool
(* Return whether the row is directly marked as fixed or not *)

val has_fixed_explanation: row_desc -> bool
(* Return whether the row should be treated as fixed or not.
   In particular, [is_fixed row] implies [has_fixed_explanation row].
*)

val fixed_explanation: row_desc -> fixed_explanation option
(* Return the potential explanation for the fixed row *)

val merge_fixed_explanation:
  fixed_explanation option -> fixed_explanation option
  -> fixed_explanation option
(* Merge two explanations for a fixed row *)

val static_row: row_desc -> bool
        (* Return whether the row is static or not *)
val tvariant_not_immediate: row_desc -> bool
        (* Return whether the polymorphic variant is non-immediate
           (i.e., has arguments or is open) *)
val hash_variant: label -> int
        (* Hash function for variant tags *)

val proxy: type_expr -> type_expr
        (* Return the proxy representative of the type: either itself
           or a row variable *)

(* Poly types. *)

(* These three functions can only be called on [Tpoly] nodes. *)
val tpoly_is_mono : type_expr -> bool
val tpoly_get_mono : type_expr -> type_expr
val tpoly_get_poly : type_expr -> type_expr * type_expr list

(**** Utilities for private abbreviations with fixed rows ****)
val row_of_type: type_expr -> type_expr
val has_constr_row: type_expr -> bool
val is_row_name: string -> bool
val is_constr_row: allow_ident:bool -> type_expr -> bool

(* Set the polymorphic variant row_name field *)
val set_static_row_name: type_declaration -> Path.t -> unit

(**** Utilities for type traversal ****)

val iter_type_expr: (type_expr -> unit) -> type_expr -> unit
        (* Iteration on types *)
val fold_type_expr: ('a -> type_expr -> 'a) -> 'a -> type_expr -> 'a
val iter_row: (type_expr -> unit) -> row_desc -> unit
        (* Iteration on types in a row *)
val fold_row: ('a -> type_expr -> 'a) -> 'a -> row_desc -> 'a
val iter_abbrev: (type_expr -> unit) -> abbrev_memo -> unit
        (* Iteration on types in an abbreviation list *)
val iter_type_expr_kind: (type_expr -> unit) -> (type_decl_kind -> unit)

val iter_type_expr_cstr_args: (type_expr -> unit) ->
  (constructor_arguments -> unit)
val map_type_expr_cstr_args: (type_expr -> type_expr) ->
  (constructor_arguments -> constructor_arguments)

(**** Utilities for type marking ****)

val mark_type: type_mark -> type_expr -> unit
        (* Mark a type recursively *)
val mark_type_params: type_mark -> type_expr -> unit
        (* Mark the sons of a type node recursively *)

(**** (Object-oriented) iterator ****)

type 'a type_iterators =
  { it_signature: 'a type_iterators -> signature -> unit;
    it_signature_item: 'a type_iterators -> signature_item -> unit;
    it_value_description: 'a type_iterators -> value_description -> unit;
    it_type_declaration: 'a type_iterators -> type_declaration -> unit;
    it_extension_constructor:
        'a type_iterators -> extension_constructor -> unit;
    it_module_declaration: 'a type_iterators -> module_declaration -> unit;
    it_modtype_declaration: 'a type_iterators -> modtype_declaration -> unit;
    it_class_declaration: 'a type_iterators -> class_declaration -> unit;
    it_class_type_declaration:
        'a type_iterators -> class_type_declaration -> unit;
    it_functor_param: 'a type_iterators -> functor_parameter -> unit;
    it_module_type: 'a type_iterators -> module_type -> unit;
    it_class_type: 'a type_iterators -> class_type -> unit;
    it_type_kind: 'a type_iterators -> type_decl_kind -> unit;
    it_do_type_expr: 'a type_iterators -> 'a;
    it_type_expr: 'a type_iterators -> type_expr -> unit;
    it_path: Path.t -> unit; }

type type_iterators_full = (type_expr -> unit) type_iterators
type type_iterators_without_type_expr = (unit -> unit) type_iterators

val type_iterators: type_mark -> type_iterators_full
        (* Iteration on arbitrary type information, including [type_expr].
           [it_type_expr] calls [mark_node] to avoid loops. *)

val type_iterators_without_type_expr: type_iterators_without_type_expr
        (* Iteration on arbitrary type information.
           Cannot recurse on [type_expr]. *)

(**** Utilities for copying ****)

val copy_type_desc:
    ?keep_names:bool -> (type_expr -> type_expr) -> type_desc -> type_desc
        (* Copy on types *)
val copy_row:
    (type_expr -> type_expr) ->
    bool -> row_desc -> bool -> type_expr -> row_desc

val copy_commu : commutable -> commutable

module For_copy : sig

  type copy_scope
        (* The private state that the primitives below are mutating, it should
           remain scoped within a single [with_scope] call.

           While it is possible to circumvent that discipline in various
           ways, you should NOT do that. *)

  val redirect_desc: copy_scope -> type_expr -> type_desc -> unit
        (* Temporarily change a type description *)

  val with_scope: (copy_scope -> 'a) -> 'a
        (* [with_scope f] calls [f] and restores saved type descriptions
           before returning its result. *)
end

(**** Memorization of abbreviation expansion ****)

val find_expans: private_flag -> Path.t -> abbrev_memo -> type_expr option
        (* Look up a memorized abbreviation *)
val cleanup_abbrev: unit -> unit
        (* Flush the cache of abbreviation expansions.
           When some types are saved (using [output_value]), this
           function MUST be called just before. *)
val memorize_abbrev:
        abbrev_memo ref ->
        private_flag -> Path.t -> type_expr -> type_expr -> unit
        (* Add an expansion in the cache *)
val forget_abbrev:
        abbrev_memo ref -> Path.t -> unit
        (* Remove an abbreviation from the cache *)

(**** Backtracking ****)

val snapshot: unit -> snapshot
val backtrack: snapshot -> unit
        (* Backtrack to a given snapshot. Only possible if you have
           not already backtracked to a previous snapshot.
           Calls [cleanup_abbrev] internally *)

(**** Utilities for labels ****)

val is_optional_parsetree : Parsetree.arg_label -> bool
val is_optional : arg_label -> bool
val is_position : arg_label -> bool
val is_omittable : arg_label -> bool
val label_name : arg_label -> label

(* Returns the label name with first character '?' or '~' as appropriate. *)
val prefixed_label_name : arg_label -> label

val extract_label :
    label -> (arg_label * 'a) list ->
    (arg_label * 'a * bool * (arg_label * 'a) list) option
(* actual label,
   value,
   whether (label, value) was at the head of the list,
   list without the extracted (label, value) *)

(**** Utilities for class types ****)

(* Get the class signature within a class type *)
val signature_of_class_type : class_type -> class_signature

(* Get the body of a class type (i.e. without parameters) *)
val class_body : class_type -> class_type

(* Fully expand the head of a class type *)
val scrape_class_type : class_type -> class_type

(* Return the number of parameters of a class type *)
val class_type_arity : class_type -> int

(* Given a path and type parameters, add an abbreviation to a class type *)
val abbreviate_class_type :
  Path.t -> type_expr list -> class_type -> class_type

(* Get the self type of a class *)
val self_type : class_type -> type_expr

(* Get the row variable of the self type of a class *)
val self_type_row : class_type -> type_expr

(* Return the methods of a class signature *)
val methods : class_signature -> string list

(* Return the virtual methods of a class signature *)
val virtual_methods : class_signature -> string list

(* Return the concrete methods of a class signature *)
val concrete_methods : class_signature -> MethSet.t

(* Return the public methods of a class signature *)
val public_methods : class_signature -> string list

(* Return the instance variables of a class signature *)
val instance_vars : class_signature -> string list

(* Return the virtual instance variables of a class signature *)
val virtual_instance_vars : class_signature -> string list

(* Return the concrete instance variables of a class signature *)
val concrete_instance_vars : class_signature -> VarSet.t

(* Return the type of a method.
   @raises [Assert_failure] if the class has no such method. *)
val method_type : label -> class_signature -> type_expr

(* Return the type of an instance variable.
   @raises [Assert_failure] if the class has no such method. *)
val instance_variable_type : label -> class_signature -> type_expr

(**** Forward declarations ****)
val print_raw: (Format.formatter -> type_expr -> unit) ref

(**** Type information getter ****)

val cstr_type_path : constructor_description -> Path.t

(* These modules exists here to resolve a dependency cycle: [Subst], [Predef],
   [Datarepr], and [Env] must not depend on [Jkind].  The portions intended for
   use outside of those modules are re-exported as [Jkind.With_bounds] and
   documented in [jkind.mli]. *)
module Jkind0 : sig
  open Allowance

  module Mod_bounds : sig
    module Crossing = Mode.Crossing
    module Externality = Jkind_axis.Externality
    module Nullability = Jkind_axis.Nullability
    module Separability = Jkind_axis.Separability

    type t = mod_bounds

    val create :
      Crossing.t->
      externality:Externality.t ->
      nullability:Nullability.t ->
      separability:Separability.t ->
      t

    val crossing : t -> Crossing.t
    val externality : t -> Externality.t
    val nullability : t -> Nullability.t
    val separability : t -> Separability.t

    val set_crossing : Crossing.t -> t -> t
    val set_externality : Externality.t -> t -> t
    val set_nullability : Nullability.t -> t -> t
    val set_separability : Separability.t -> t -> t

    (** [set_max_in_set bounds axes] sets all the axes in [axes] to their [max]
        within [bounds] *)
    val set_max_in_set : t -> Jkind_axis.Axis_set.t -> t

    (** [set_min_in_set bounds axes] sets all the axes in [axes] to their [min]
        within [bounds] *)
    val set_min_in_set : t -> Jkind_axis.Axis_set.t -> t

    (** [is_max_within_set bounds axes] returns whether or not all the axes in
        [axes] are [max] within [bounds] *)
    val is_max_within_set : t -> Jkind_axis.Axis_set.t -> bool
    val is_max : t -> bool

    val min : t
    val max : t
    val for_arrow : t

    val equal : t -> t -> bool
    val join : t -> t -> t

    val relevant_axes_of_modality :
      relevant_for_shallow:[ `Irrelevant | `Relevant ] ->
      modality:Mode.Modality.Const.t -> Jkind_axis.Axis_set.t

    val debug_print : Format.formatter -> t -> unit
  end

  module With_bounds : sig
    type 'd t = 'd with_bounds constraint 'd = 'l * 'r

    include Allow_disallow with type (_, _, 'd) sided = 'd t

    val add_modality :
      relevant_for_shallow:[ `Irrelevant | `Relevant ] ->
      modality:Mode.Modality.Const.t ->
      type_expr:type_expr ->
      (allowed * disallowed) t ->
      (allowed * disallowed) t

    val add_bound :
      type_expr -> With_bounds_type_info.t -> with_bounds_types ->
      with_bounds_types

    val map_type_expr :
      (type_expr -> type_expr) -> ('l * 'r) with_bounds -> ('l * 'r) with_bounds
  end

  module Layout_and_axes : sig
    include Allowance.Allow_disallow
      with type (_, 'layout, 'd) sided = ('layout, 'd) layout_and_axes

    val map : ('a -> 'b) -> ('a, 'd) layout_and_axes -> ('b, 'd) layout_and_axes

    val map_option :
      ('a -> 'b option) -> ('a, 'd) layout_and_axes ->
      ('b, 'd) layout_and_axes option

    val try_allow_l :
      ('layout, 'l * 'r) layout_and_axes ->
      ('layout, allowed * 'r) layout_and_axes option

    val try_allow_r :
      ('layout, 'l * 'r) layout_and_axes ->
      ('layout, 'l * allowed) layout_and_axes option
  end

  module Const : sig
    type 'd t = (Jkind_types.Layout.Const.t, 'd) layout_and_axes

    val shallow_no_with_bounds_and_equal : 'd1 t -> 'd2 t -> bool

    include Allowance.Allow_disallow with type (_, _, 'd) sided = 'd t

    module Builtin : sig
      type nonrec t =
        { jkind : (allowed * allowed) t;
          name : string
        }

      val any : t
      val void : t
      val value_or_null : t
      val value_or_null_mod_everything : t
      val value : t
      val immutable_data : t
      val exn : t
      val sync_data : t
      val mutable_data : t
      val immediate64 : t
      val immediate64_or_null : t
      val immediate : t
      val immediate_or_null : t
      val float64 : t
      val kind_of_unboxed_float : t
      val float32 : t
      val kind_of_unboxed_float32 : t
      val word : t
      val kind_of_unboxed_nativeint : t
      val untagged_immediate : t
      val kind_of_untagged_immediate : t
      val bits8 : t
      val kind_of_unboxed_int8 : t
      val bits16 : t
      val kind_of_unboxed_int16 : t
      val bits32 : t
      val kind_of_unboxed_int32 : t
      val bits64 : t
      val kind_of_unboxed_int64 : t
      val kind_of_idx : t
      val vec128 : t
      val vec256 : t
      val vec512 : t
      val kind_of_unboxed_128bit_vectors : t
      val kind_of_unboxed_256bit_vectors : t
      val kind_of_unboxed_512bit_vectors : t
      val all : t list

      val of_attribute : Builtin_attributes.jkind_attribute -> t
    end
  end

  module Jkind : sig
    include Allowance.Allow_disallow with type (_, _, 'd) sided = 'd jkind

    val try_allow_r : ('l * 'r) jkind -> ('l * allowed) jkind option

    val of_const :
      annotation:Parsetree.jkind_annotation option ->
      why:Jkind_intf.History.creation_reason ->
      quality:'d jkind_quality ->
      ran_out_of_fuel_during_normalize:bool ->
      'd Const.t ->
      'd jkind

    val get_const : 'd jkind -> 'd Const.t option

    val fresh_jkind :
      (allowed * allowed) jkind_desc ->
      annotation:Parsetree.jkind_annotation option ->
      why:Jkind_intf.History.creation_reason ->
      ('a * 'b) jkind

    val fresh_jkind_poly :
      ('a * 'b) jkind_desc ->
      annotation:Parsetree.jkind_annotation option ->
      why:Jkind_intf.History.creation_reason ->
      ('a * 'b) jkind

    val mk_annot : string -> Parsetree.jkind_annotation option

    val mark_best : ('l * 'r) jkind -> ('l * disallowed) jkind

    val map_type_expr :
      (type_expr -> type_expr) -> (allowed * 'r) jkind -> (allowed * 'r) jkind

    val has_with_bounds : jkind_l -> bool

    module Builtin : sig
      val any : why:Jkind_intf.History.any_creation_reason -> 'd jkind
      val void :
        why:Jkind_intf.History.void_creation_reason -> ('l * disallowed) jkind
      val value_or_null :
        why:Jkind_intf.History.value_or_null_creation_reason -> 'd jkind
      val value : why:Jkind_intf.History.value_creation_reason -> 'd jkind
      val immutable_data :
        why:Jkind_intf.History.value_creation_reason -> 'd jkind
      val sync_data : why:Jkind_intf.History.value_creation_reason -> 'd jkind
      val mutable_data :
        why:Jkind_intf.History.value_creation_reason -> 'd jkind
      val immediate :
        why:Jkind_intf.History.immediate_creation_reason ->
        ('l * disallowed) jkind
      val immediate_or_null :
        why:Jkind_intf.History.immediate_or_null_creation_reason -> 'd jkind
      val product :
        why:Jkind_intf.History.product_creation_reason ->
        (type_expr * Mode.Modality.Const.t) list ->
        Jkind_types.Sort.t Jkind_types.Layout.t list ->
        jkind_l
      val product_of_sorts :
        why:Jkind_intf.History.product_creation_reason -> level:int -> int ->
        jkind_l
    end

    val add_with_bounds :
      modality:Mode.Modality.Const.t ->
      type_expr:type_expr ->
      jkind_l ->
      jkind_l

    val jkind_of_mutability :
      mutability -> why:Jkind_intf.History.value_creation_reason ->
      ('a * 'b) jkind

    val for_non_float : why:Jkind_intf.History.value_creation_reason -> 'd jkind
    val for_boxed_record : label_declaration list -> jkind_l
    val for_boxed_variant :
      loc:Location.t ->
      decl_params:Types.type_expr list ->
      type_apply:
        (Types.type_expr list ->
        Types.type_expr ->
        Types.type_expr list ->
        Types.type_expr) ->
      free_vars:(Types.type_expr list -> TypeSet.t) ->
      Types.constructor_declaration list ->
      Types.jkind_l
  end

  (** Memoize the built-in jkinds, either best or not-best. Primarily for use by
      [Subst], but placed here so that [Subst] need not depend on [Jkind]. *)
  module Builtins_memo : sig
    val find :
      quality:('l * 'r) jkind_quality ->
      ran_out_of_fuel_during_normalize:bool ->
      ('l * 'r) Const.t ->
      ('l * 'r) jkind option
  end

  include module type of Jkind
end
