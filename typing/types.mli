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

(** {0 Representation of types and declarations} *)

(** [Types] defines the representation of types and declarations (that is, the
    content of module signatures).

    CMI files are made of marshalled types.
*)

open Allowance

(** Asttypes exposes basic definitions shared both by Parsetree and Types. *)
open Asttypes

(** Whether or not a mutable field is atomic *)
type atomic =
  | Nonatomic
  | Atomic

(** Describes a mutable field/element. *)
type mutability =
  | Immutable
  | Mutable of
      { mode : Mode.Value.Comonadic.lr
        (** Mode of new field value in mutation. *)
      ; atomic : atomic
      }

(** Returns [true] is the [mutable_flag] is mutable or atomic. Should be called
    if not interested in the payload of [Mutable]. *)
val is_mutable : mutability -> bool

(** Returns [true] is the [mutable_flag] is atomic. Should be called
    if not interested in the payload of [Mutable]. *)
val is_atomic : mutability -> bool

(** Given the parameter [m0] on mutable, return the mode of future writes. *)
val mutable_mode : ('l * 'r) Mode.Value.Comonadic.t -> ('l * 'r) Mode.Value.t

(** Type expressions for the core language.

    The [type_desc] variant defines all the possible type expressions one can
    find in OCaml. [type_expr] wraps this with some annotations.

    The [level] field tracks the level of polymorphism associated to a type,
    guiding the generalization algorithm.
    Put shortly, when referring to a type in a given environment, both the type
    and the environment have a level. If the type has an higher level, then it
    can be considered fully polymorphic (type variables will be printed as
    ['a]), otherwise it'll be weakly polymorphic, or non generalized (type
    variables printed as ['_a]).
    See [http://okmij.org/ftp/ML/generalization.html] for more information.

    Note about [type_declaration]: one should not make the confusion between
    [type_expr] and [type_declaration].

    [type_declaration] refers specifically to the [type] construct in OCaml
    language, where you create and name a new type or type alias.

    [type_expr] is used when you refer to existing types, e.g. when annotating
    the expected type of a value.

    Also, as the type system of OCaml is generative, a [type_declaration] can
    have the side-effect of introducing a new type constructor, different from
    all other known types.
    Whereas [type_expr] is a pure construct which allows referring to existing
    types.

    Note on mutability: TBD.
 *)

(** The mod-bounds of a jkind *)
module Jkind_mod_bounds : sig
  module Locality = Mode.Locality.Const
  module Linearity = Mode.Linearity.Const
  module Uniqueness = Mode.Uniqueness.Const_op
  module Portability = Mode.Portability.Const
  module Contention = Mode.Contention.Const_op
  module Yielding = Mode.Yielding.Const
  module Statefulness = Mode.Statefulness.Const
  module Visibility = Mode.Visibility.Const_op
  module Externality = Jkind_axis.Externality
  module Nullability = Jkind_axis.Nullability
  module Separability = Jkind_axis.Separability

  type t

  val create :
    locality:Locality.t ->
    linearity:Linearity.t ->
    uniqueness:Uniqueness.t ->
    portability:Portability.t ->
    contention:Contention.t ->
    yielding:Yielding.t ->
    statefulness:Statefulness.t ->
    visibility:Visibility.t ->
    externality:Externality.t ->
    nullability:Nullability.t ->
    separability:Separability.t ->
    t

  val locality : t -> Locality.t
  val linearity : t -> Linearity.t
  val uniqueness : t -> Uniqueness.t
  val portability : t -> Portability.t
  val contention : t -> Contention.t
  val yielding : t -> Yielding.t
  val statefulness : t -> Statefulness.t
  val visibility : t -> Visibility.t
  val externality : t -> Externality.t
  val nullability : t -> Nullability.t
  val separability : t -> Separability.t

  val set_locality : Locality.t -> t -> t
  val set_linearity : Linearity.t -> t -> t
  val set_uniqueness : Uniqueness.t -> t -> t
  val set_portability : Portability.t -> t -> t
  val set_contention : Contention.t -> t -> t
  val set_yielding : Yielding.t -> t -> t
  val set_statefulness : Statefulness.t -> t -> t
  val set_visibility : Visibility.t -> t -> t
  val set_externality : Externality.t -> t -> t
  val set_nullability : Nullability.t -> t -> t
  val set_separability : Separability.t -> t -> t

  (** [set_max_in_set bounds axes] sets all the axes in [axes] to their [max] within
      [bounds] *)
  val set_max_in_set : t -> Jkind_axis.Axis_set.t -> t

  (** [set_min_in_set bounds axes] sets all the axes in [axes] to their [min] within
      [bounds] *)
  val set_min_in_set : t -> Jkind_axis.Axis_set.t -> t

  (** [is_max_within_set bounds axes] returns whether or not all the axes in [axes] are
      [max] within [bounds] *)
  val is_max_within_set : t -> Jkind_axis.Axis_set.t -> bool
  val is_max : t -> bool

  val debug_print : Format.formatter -> t -> unit
end


(** Information tracked about an individual type within the with-bounds for a jkind *)
module With_bounds_type_info : sig
  (** The axes that the with-bound applies to *)
  type t = { relevant_axes : Jkind_axis.Axis_set.t } [@@unboxed]
end

type type_expr
type row_desc
type row_field
type field_kind
type commutable

and type_desc =
  | Tvar of { name : string option; jkind : jkind_lr }
  (** [Tvar (Some "a")] ==> ['a] or ['_a]
      [Tvar None]       ==> [_] *)

  | Tarrow of arrow_desc * type_expr * type_expr * commutable
  (** [Tarrow (Nolabel,      e1, e2, c)] ==> [e1    -> e2]
      [Tarrow (Labelled "l", e1, e2, c)] ==> [l:e1  -> e2]
      [Tarrow (Optional "l", e1, e2, c)] ==> [?l:e1 -> e2]

      See [commutable] for the last argument. The argument
      type must be a [Tpoly] node *)

  | Ttuple of (string option * type_expr) list
  (** [Ttuple [None, t1; ...; None, tn]] ==> [t1 * ... * tn]
      [Ttuple [Some "l1", t1; ...; Some "ln", tn]] ==> [l1:t1 * ... * ln:tn]

      Any mix of labeled and unlabeled components also works:
      [Ttuple [Some "l1", t1; None, t2; Some "l3", t3]] ==> [l1:t1 * t2 * l3:t3]
  *)

  | Tunboxed_tuple of (string option * type_expr) list
  (** [Tunboxed_tuple [None, t1; ...; None, tn]] ==> [#(t1 * ... * tn)]
      [Tunboxed_tuple [Some "l1", t1; ...; Some "ln", tn]]
                  ==> [#(l1:t1 * ... * ln:tn)]

      Any mix of labeled and unlabeled components also works:
      [Tunboxed_tuple [Some "l1", t1; None, t2; Some "l3", t3]]
           ==> [#(l1:t1 * t2 * l3:t3)]
  *)

  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  (** [Tconstr (`A.B.t', [t1;...;tn], _)] ==> [(t1,...,tn) A.B.t]
      The last parameter keep tracks of known expansions, see [abbrev_memo]. *)

  | Tobject of type_expr * (Path.t * type_expr list) option ref
  (** [Tobject (`f1:t1;...;fn: tn', `None')] ==> [< f1: t1; ...; fn: tn >]
      f1, fn are represented as a linked list of types using Tfield and Tnil
      constructors.

      [Tobject (_, `Some (`A.ct', [t1;...;tn]')] ==> [(t1, ..., tn) A.ct].
      where A.ct is the type of some class.

      There are also special cases for so-called "class-types", cf. [Typeclass]
      and [Ctype.set_object_name]:

        [Tobject (Tfield(_,_,...(Tfield(_,_,rv)...),
                         Some(`A.#ct`, [rv;t1;...;tn])]
             ==> [(t1, ..., tn) #A.ct]
        [Tobject (_, Some(`A.#ct`, [Tnil;t1;...;tn])] ==> [(t1, ..., tn) A.ct]

      where [rv] is the hidden row variable.
  *)

  | Tfield of string * field_kind * type_expr * type_expr
  (** [Tfield ("foo", field_public, t, ts)] ==> [<...; foo : t; ts>] *)

  | Tnil
  (** [Tnil] ==> [<...; >] *)

  | Tlink of type_expr
  (** Indirection used by unification engine. *)

  | Tsubst of type_expr * type_expr option
  (** [Tsubst] is used temporarily to store information in low-level
      functions manipulating representation of types, such as
      instantiation or copy.
      The first argument contains a copy of the original node.
      The second is available only when the first is the row variable of
      a polymorphic variant.  It then contains a copy of the whole variant.
      This constructor should not appear outside of these cases. *)

  | Tvariant of row_desc
  (** Representation of polymorphic variants, see [row_desc]. *)

  | Tunivar of { name : string option; jkind : jkind_lr }
  (** Occurrence of a type variable introduced by a
      forall quantifier / [Tpoly]. *)

  | Tpoly of type_expr * type_expr list
  (** [Tpoly (ty,tyl)] ==> ['a1... 'an. ty],
      where 'a1 ... 'an are names given to types in tyl
      and occurrences of those types in ty. *)

  | Tpackage of Path.t * (Longident.t * type_expr) list
  (** Type of a first-class module (a.k.a package). *)

  | Tof_kind of jkind_lr
  (** [Tof_kind jkind] ==> [(type : jkind)]

      The "canonical" type of a particular kind.

      These types are uninhabited, and any appearing in translation will cause an error.
      They are only used to represent the kinds of existentially-quantified types
      mentioned in with-bounds. See test typing-jkind-bounds/gadt.ml *)

(** This is used in the Typedtree. It is distinct from
    {{!Asttypes.arg_label}[arg_label]} because Position argument labels are
    discovered through typechecking. *)
and arg_label =
  | Nolabel
  | Labelled of string (** [label:T -> ...] *)
  | Optional of string (** [?label:T -> ...] *)
  | Position of string (** [label:[%call_pos] -> ...] *)

and arrow_desc =
  arg_label * Mode.Alloc.lr * Mode.Alloc.lr



(** See also documentation for [row_more], which enumerates how these
    constructors arise. *)
and fixed_explanation =
  | Univar of type_expr (** The row type was bound to an univar *)
  | Fixed_private (** The row type is private *)
  | Reified of Path.t (** The row was reified *)
  | Rigid (** The row type was made rigid during constraint verification *)
  | Fixed_existential (** The row type is existential in a with-bound.
                      See Note [With-bounds for GADTs] in Jkind. *)

(** [abbrev_memo] allows one to keep track of different expansions of a type
    alias. This is done for performance purposes.

    For instance, when defining [type 'a pair = 'a * 'a], when one refers to an
    ['a pair], it is just a shortcut for the ['a * 'a] type.
    This expansion will be stored in the [abbrev_memo] of the corresponding
    [Tconstr] node.

    In practice, [abbrev_memo] behaves like list of expansions with a mutable
    tail.

    Note on marshalling: [abbrev_memo] must not appear in saved types.
    [Btype], with [cleanup_abbrev] and [memo], takes care of tracking and
    removing abbreviations.
*)
and abbrev_memo =
  | Mnil (** No known abbreviation *)

  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  (** Found one abbreviation.
      A valid abbreviation should be at least as visible and reachable by the
      same path.
      The first expression is the abbreviation and the second the expansion. *)

  | Mlink of abbrev_memo ref
  (** Abbreviations can be found after this indirection *)

(** [commutable] is a flag appended to every arrow type.

    When typing an application, if the type of the functional is
    known, its type is instantiated with [commu_ok] arrows, otherwise as
    [commu_var ()].

    When the type is not known, the application will be used to infer
    the actual type.  This is fragile in presence of labels where
    there is no principal type.

    Two incompatible applications must rely on [is_commu_ok] arrows,
    otherwise they will trigger an error.

    let f g =
      g ~a:() ~b:();
      g ~b:() ~a:();

    Error: This function is applied to arguments
    in an order different from other calls.
    This is only allowed when the real type is known.
*)


(**** Jkinds ****)

(** A history of conditions placed on a jkind.

   INVARIANT: at most one sort variable appears in this history.
   This is a natural consequence of producing this history by comparing
   jkinds.
*)
and jkind_history =
  | Interact of
      { reason : Jkind_intf.History.interact_reason;
        jkind1 : jkind_desc_packed;
        history1 : jkind_history;
        jkind2 : jkind_desc_packed;
        history2 : jkind_history
      }
  | Creation of Jkind_intf.History.creation_reason

(** The types within the with-bounds of a jkind *)
and with_bounds_types

and 'd with_bounds =
  | No_with_bounds : ('l * 'r) with_bounds
  | With_bounds
    : with_bounds_types -> ('l * Allowance.disallowed) with_bounds
    (** Invariant : there must always be at least one type in this set **)

and ('layout, 'd) layout_and_axes =
  { layout : 'layout;
    mod_bounds : Jkind_mod_bounds.t;
    with_bounds : 'd with_bounds
  }
  constraint 'd = 'l * 'r

and 'd jkind_desc = (Jkind_types.Sort.t Jkind_types.Layout.t, 'd) layout_and_axes
  constraint 'd = 'l * 'r

and jkind_desc_packed = Pack_jkind_desc : ('l * 'r) jkind_desc -> jkind_desc_packed

(** The "quality" of a jkind indicates whether we are able to learn more about the jkind
    later.

    We can never learn more about a [Best] jkind to make it "lower" (according to
    [Jkind.sub] / [Jkind.sub_jkind_l]). A [Not_best], jkind, however, might have more
    information provided about it later that makes it lower.

    Note that only left jkinds can be [Best] (meaning we can never compare less than or
    equal to a left jkind!)
*)
and 'd jkind_quality =
  | Best : ('l * disallowed) jkind_quality
  | Not_best : ('l * 'r) jkind_quality

and 'd jkind =
  { jkind : 'd jkind_desc;
    annotation : Parsetree.jkind_annotation option;
    history : jkind_history;
    has_warned : bool;
    ran_out_of_fuel_during_normalize : bool;
    quality : 'd jkind_quality;
  }
  constraint 'd = 'l * 'r

and jkind_l = (allowed * disallowed) jkind  (* the jkind of an actual type *)
and jkind_r = (disallowed * allowed) jkind  (* the jkind expected of a type *)
and jkind_lr = (allowed * allowed) jkind    (* the jkind of a variable *)
and jkind_packed = Pack_jkind : ('l * 'r) jkind -> jkind_packed

(* A map from [type_expr] to [With_bounds_type_info.t], specifically defined with a
   (best-effort) semantic comparison function on types to be used in the with-bounds of a
   jkind.

   This module is defined internally to be equal (via two uses of [Obj.magic]) to the
   abstract type [with_bound_types] to break the circular dependency between with-bounds
   and type_expr. The alternative to this approach would be mutually recursive modules,
   but this approach creates a smaller diff with upstream and makes rebasing easier.
*)
module With_bounds_types : sig
  (* Note that only the initially needed bits of [Stdlib.Map.S] are exposed here; feel
     free to expose more functions if you need them! *)
  type t = with_bounds_types
  type info := With_bounds_type_info.t

  val empty : t
  val is_empty : t -> bool
  val to_seq : t -> (type_expr * info) Seq.t
  val of_list : (type_expr * info) list -> t
  val of_seq : (type_expr * info) Seq.t -> t
  val singleton : type_expr -> info -> t
  val map : (info -> info) -> t -> t
  val merge
    : (type_expr -> info option -> info option -> info option) ->
    t -> t -> t
  val update : type_expr -> (info option -> info option) -> t -> t
  val find_opt : type_expr -> t -> info option
  val for_all : (type_expr -> info -> bool) -> t -> bool
  val map_with_key : (type_expr -> info -> type_expr * info) -> t -> t
end

val is_commu_ok: commutable -> bool
val commu_ok: commutable
val commu_var: unit -> commutable

(** [field_kind] indicates the accessibility of a method.

    An [Fprivate] field may become [Fpublic] or [Fabsent] during unification,
    but not the other way round.

    The same [field_kind] is kept shared when copying [Tfield] nodes
    so that the copies of the self-type of a class share the same accessibility
    (see also PR#10539).
 *)

type field_kind_view =
    Fprivate
  | Fpublic
  | Fabsent

val field_kind_repr: field_kind -> field_kind_view
val field_public: field_kind
val field_absent: field_kind
val field_private: unit -> field_kind
val field_kind_internal_repr: field_kind -> field_kind
        (* Removes indirections in [field_kind].
           Only needed for performance. *)

(** Getters for type_expr; calls repr before answering a value *)

val get_desc: type_expr -> type_desc
val get_level: type_expr -> int
val get_scope: type_expr -> int
val get_id: type_expr -> int

(** Transient [type_expr].
    Should only be used immediately after [Transient_expr.repr] *)
type transient_expr = private
      { mutable desc: type_desc;
        mutable level: int;
        mutable scope: int;
        id: int }

module Transient_expr : sig
  (** Operations on [transient_expr] *)

  val create: type_desc -> level: int -> scope: int -> id: int -> transient_expr
  val set_desc: transient_expr -> type_desc -> unit
  val set_level: transient_expr -> int -> unit
  val set_scope: transient_expr -> int -> unit
  val repr: type_expr -> transient_expr
  val type_expr: transient_expr -> type_expr
  val coerce: type_expr -> transient_expr
      (** Coerce without normalizing with [repr] *)

  val set_stub_desc: type_expr -> type_desc -> unit
      (** Instantiate a not yet instantiated stub.
          Fail if already instantiated. *)
end

val create_expr: type_desc -> level: int -> scope: int -> id: int -> type_expr

(** Functions and definitions moved from Btype *)

val newty3: level:int -> scope:int -> type_desc -> type_expr
        (** Create a type with a fresh id *)

val newty2: level:int -> type_desc -> type_expr
        (** Create a type with a fresh id and no scope *)

module TransientTypeOps : sig
  (** Comparisons for functors *)

  type t = transient_expr
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

(** Comparisons for [type_expr]; cannot be used for functors *)

val eq_type: type_expr -> type_expr -> bool
val compare_type: type_expr -> type_expr -> int

(** Constructor and accessors for [row_desc] *)

(** [  `X | `Y ]       (row_closed = true)
    [< `X | `Y ]       (row_closed = true)
    [> `X | `Y ]       (row_closed = false)
    [< `X | `Y > `X ]  (row_closed = true)

    type t = [> `X ] as 'a      (row_more = Tvar a)
    type t = private [> `X ]    (row_more = Tconstr ("t#row", [], ref Mnil))

    And for:

        let f = function `X -> `X -> | `Y -> `X

    the type of "f" will be a [Tarrow] whose lhs will (basically) be:

        Tvariant { row_fields = [("X", _)];
                   row_more   =
                     Tvariant { row_fields = [("Y", _)];
                                row_more   =
                                  Tvariant { row_fields = [];
                                             row_more   = _;
                                             _ };
                                _ };
                   _
                 }

*)

val create_row:
  fields:(label * row_field) list ->
  more:type_expr ->
  closed:bool ->
  fixed:fixed_explanation option ->
  name:(Path.t * type_expr list) option -> row_desc

val row_fields: row_desc -> (label * row_field) list

(** [row_more] returns a [type_expr] with one of the following [type_desc]s
    (also described with the return from [row_fixed], which varies similarly):

    * [Tvar]: This is a row variable; it would occur in e.g. [val f :
    [> `A | `B] -> int]. When/if we learn more about a polymorphic variant, this
    variable might get unified with one of the other [type_desc]s listed here,
    or a [Tvariant] that represents a new set of constructors to add to the row.

    During [constraint] checking (toward the end of checking a type declaration,
    in [Typedecl.check_constraints_rec]) we [Ctype.rigidify] a type to make it
    so that its unification variables will not unify. When a [Tvar] row variable
    is rigidified, its [fixed_explanation] will be [Rigid].

    * [Tunivar]: This is a universally quantified row variable; it would occur
    in e.g. [type t = { f : 'a. ([> `A | `B ] as 'a) -> int }]. A [Tunivar] has
    a [fixed_explanation] of [Univar].

    * [Tconstr]: There are two possible ways this can happen:

      1. This is an abstract [#row] type created by a [private] row type, as in
      [type t = private [> `A | `B]]. In this case, the [fixed_explanation] will
      be [Fixed_private].

      2. This is a locally abstract type created by [Ctype.reify], which happens
      when a row variable is free in the type of the scrutinee in a GADT pattern
      match. The [fixed_explanation] will be [Reified]. Note that any manifest
      of a reified row variable is actually ignored by [row_repr]; this causes
      some incompletness in type inference.

    * [Tnil]: Used to denote a static polymorphic variant (with no [>] or [<]).

    * [Tof_kind]: See Wrinkle BW2 in Note [With-bounds for GADTs] in Jkind.
    Briefly, [Tof_kind] can appear as a [row_more] when computing the kind
    of a GADT with an existentially-bound row variable. The [fixed_explanation]
    will be [Fixed_existential].

    ----------------------------------------

    It is an invariant that row variables are never shared between different
    types. That is, if [row_more row1 == row_more row2], then [row1] and [row2]
    come from structurally identical [Tvariant]s (but they might not be
    physically equal). When copying types, two types with the same [row_more]
    field are replaced by the same copy.
*)
val row_more: row_desc -> type_expr
val row_closed: row_desc -> bool

(** See documentation for [row_more]. *)
val row_fixed: row_desc -> fixed_explanation option

val row_name: row_desc -> (Path.t * type_expr list) option

val set_row_name: row_desc -> (Path.t * type_expr list) option -> row_desc

val get_row_field: label -> row_desc -> row_field

(** get all fields at once; different from the old [row_repr] *)
type row_desc_repr =
    Row of { fields: (label * row_field) list;
             more:   type_expr;
             closed: bool;
             fixed:  fixed_explanation option;
             name:   (Path.t * type_expr list) option }

val row_repr: row_desc -> row_desc_repr

(** Current contents of a row field *)
type row_field_view =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

val row_field_repr: row_field -> row_field_view
val rf_present: type_expr option -> row_field
val rf_absent: row_field
val rf_either:
    ?use_ext_of:row_field ->
    no_arg:bool -> type_expr list -> matched:bool -> row_field
val rf_either_of: type_expr option -> row_field

val eq_row_field_ext: row_field -> row_field -> bool
val changed_row_field_exts: row_field list -> (unit -> unit) -> bool

val match_row_field:
    present:(type_expr option -> 'a) ->
    absent:(unit -> 'a) ->
    either:(bool -> type_expr list -> bool -> row_field option ->'a) ->
    row_field -> 'a

(* *)

module Uid = Shape.Uid

(* Sets and maps of methods and instance variables *)

module MethSet : Set.S with type elt = string
module VarSet : Set.S with type elt = string

module Meths : Map.S with type key = string
module Vars  : Map.S with type key = string

(* Value descriptions *)

type value_kind =
    Val_reg                             (* Regular value *)
  | Val_mut of Mode.Value.Comonadic.lr * Jkind_types.Sort.t
                                        (* Mutable variable *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of class_signature * self_meths * Ident.t Vars.t * string
                                        (* Self *)
  | Val_anc of class_signature * Ident.t Meths.t * string
                                        (* Ancestor *)

and self_meths =
  | Self_concrete of Ident.t Meths.t
  | Self_virtual of Ident.t Meths.t ref

and class_signature =
  { csig_self: type_expr;
    mutable csig_self_row: type_expr;
    mutable csig_vars: (mutable_flag * virtual_flag * type_expr) Vars.t;
    mutable csig_meths: (method_privacy * virtual_flag * type_expr) Meths.t; }

and method_privacy =
  | Mpublic
  | Mprivate of field_kind
    (* The [field_kind] is always [Fabsent] in a complete class type. *)

(* Variance *)

module Variance : sig
  type t
  type f =
      May_pos                (* allow positive occurrences *)
    | May_neg                (* allow negative occurrences *)
    | May_weak               (* allow occurrences under a negative position *)
    | Inj                    (* type is injective in this parameter *)
    | Pos                    (* there is a positive occurrence *)
    | Neg                    (* there is a negative occurrence *)
    | Inv                    (* both negative and positive occurrences *)
  val null : t               (* no occurrence *)
  val full : t               (* strictly invariant (all flags) *)
  val covariant : t          (* strictly covariant (May_pos, Pos and Inj) *)
  val unknown : t            (* allow everything, guarantee nothing *)
  val union  : t -> t -> t
  val inter  : t -> t -> t
  val subset : t -> t -> bool
  val eq : t -> t -> bool
  val set : f -> t -> t
  val set_if : bool -> f -> t -> t
  val mem : f -> t -> bool
  val conjugate : t -> t                (* exchange positive and negative *)
  val compose : t -> t -> t
  val strengthen : t -> t                (* remove May_weak when possible *)
  val get_upper : t -> bool * bool                    (* may_pos, may_neg *)
  val get_lower : t -> bool * bool * bool                (* pos, neg, inj *)
  val unknown_signature : injective:bool -> arity:int -> t list
  (** The most pessimistic variance for a completely unknown type. *)
end

module Separability : sig
  (** see {!Typedecl_separability} for an explanation of separability
      and separability modes.*)

  type t = Ind | Sep | Deepsep
  val eq : t -> t -> bool
  val print : Format.formatter -> t -> unit

  val rank : t -> int
  (** Modes are ordered from the least to the most demanding:
      Ind < Sep < Deepsep.
      'rank' maps them to integers in an order-respecting way:
      m1 < m2  <=>  rank m1 < rank m2 *)

  val compare : t -> t -> int
  (** Compare two mode according to their mode ordering. *)

  val max : t -> t -> t
  (** [max_mode m1 m2] returns the most demanding mode. It is used to
      express the conjunction of two parameter mode constraints. *)

  type signature = t list
  (** The 'separability signature' of a type assigns a mode for
      each of its parameters. [('a, 'b) t] has mode [(m1, m2)] if
      [(t1, t2) t] is separable whenever [t1, t2] have mode [m1, m2]. *)

  val print_signature : Format.formatter -> signature -> unit

  val default_signature : arity:int -> signature
  (** The most pessimistic separability for a completely unknown type. *)
end

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_decl_kind;

    type_jkind: jkind_l;
    (* for an abstract decl kind or for [@@unboxed] types: this is the stored
       jkind for the type; expansion might find a type with a more precise
       jkind. See PR#10017 for motivating examples where subsitution or
       instantiation may refine the immediacy of a type.

       for other decl kinds: this is a cached jkind, computed from the
       decl kind. EXCEPTION: if a type's jkind is refined by a gadt equation,
       the jkind stored here might be a subjkind of the jkind that would
       be computed from the decl kind. This happens in
       Ctype.add_jkind_equation. *)

    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    (* covariant, contravariant, weakly contravariant, injective *)
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_unboxed_default: bool;
    (* true if the unboxed-ness of this type was chosen by a compiler flag *)
    type_uid: Uid.t;
    type_unboxed_version : type_declaration option;
    (* stores the unboxed version of that this type introduces: this is [Some]
       for predefined types with unboxed versions (e.g. [float]) and boxed
       records, but [None] for aliases of these types

       invariants:
       1. there are no "twice-unboxed" types: the [type_declaration] stored here
          itself has [type_unboxed_version = None].
       2. the Uid of the unboxed version is [Uid.unboxed_version <uid of boxed>]
    *)
  }

and type_decl_kind = (label_declaration, label_declaration, constructor_declaration) type_kind

and unsafe_mode_crossing =
  { unsafe_mod_bounds : Mode.Crossing.t
  ; unsafe_with_bounds : (allowed * disallowed) with_bounds
  }

and ('lbl, 'lbl_flat, 'cstr) type_kind =
    Type_abstract of type_origin
  | Type_record of 'lbl list * record_representation * unsafe_mode_crossing option
  | Type_record_unboxed_product of
      'lbl_flat list *
      record_unboxed_product_representation *
      unsafe_mode_crossing option
  | Type_variant of 'cstr list * variant_representation * unsafe_mode_crossing option
  | Type_open

(* CR layouts: after removing the void translation from lambda, we could get rid of
   this src_index / runtime_tag distinction.  But I am leaving it in because it
   may not be long before we need it again.

   In particular, lambda will need to do something about computing offsets for
   block projections when not everything is one word wide, whether that's
   because of void or because of other jkinds.  One option is to change these
   projections to be more abstract and pass the jkind information to other
   stages of the compiler, as is currently done for unboxed projection
   operations, but at the moment our plan is to do this math in lambda in the
   case of normal projections from boxes. *)
and tag = Ordinary of {src_index: int;  (* Unique name (per type) *)
                       runtime_tag: int}    (* The runtime tag *)
        | Extension of Path.t
        | Null (* Null pointer *)

(* A mixed product contains a possibly-empty prefix of values followed by a
   non-empty suffix of "flat" elements. Intuitively, a flat element is one that
   need not be scanned by the garbage collector. The front-end allows elements
   to appear in any order in a record, and later stages of the compiler
   re-arrange the block. *)
and mixed_block_element =
  | Value
  | Float_boxed
  (* A [Float_boxed] is a float that's stored flat but boxed upon projection. *)
  | Float64
  | Float32
  | Bits8
  | Bits16
  | Bits32
  | Bits64
  | Vec128
  | Vec256
  | Vec512
  | Word
  | Product of mixed_product_shape
  (* Invariant: the array has at least two things in it. *)
  | Void

and mixed_product_shape = mixed_block_element array

and type_origin =
    Definition
  | Rec_check_regularity       (* See Typedecl.transl_type_decl *)
  | Existential of string

and record_representation =
  | Record_unboxed
  | Record_inlined of tag * constructor_representation * variant_representation
  (* For an inlined record, we record the representation of the variant that
     contains it and the tag/representation of the relevant constructor of that
     variant. *)
  | Record_boxed of Jkind_types.Sort.Const.t array
  | Record_float (* All fields are floats *)
  | Record_ufloat
  (* All fields are [float#]s.  Same runtime representation as [Record_float],
     but operations on these (e.g., projection, update) work with unboxed floats
     rather than boxed floats. *)
  | Record_mixed of mixed_product_shape
  (* The record contains a mix of values and unboxed elements. The block
     is tagged such that polymorphic operations will not work.
  *)

and record_unboxed_product_representation =
  | Record_unboxed_product
  (* We give all unboxed records the same representation, as their layouts are
     encapsulated by their label's jkinds. We keep this variant for uniformity with boxed
     records, and to make it easier to support different representations in the future. *)

and variant_representation =
  | Variant_unboxed
  | Variant_boxed of (constructor_representation *
                      Jkind_types.Sort.Const.t array) array
  (* The outer array has an element for each constructor. Each inner array
     has a jkind for each argument of the corresponding constructor.

     A constructor with an inlined record argument has a length-1 inner array.
     Its single element is the jkind of the record itself. (It doesn't have a
     jkind for each field.) However, the constructor representation is about the
     fields of the record, not the record itself; that is, it will be
     [Constructor_mixed] if the inlined record has any unboxed fields.
  *)
  | Variant_extensible
  | Variant_with_null
  (* CR layouts v3.5: A custom variant representation for ['a or_null].
     Eventually, it should likely be merged into [Variant_unboxed], with
     [Variant_unboxed] allowing either one ordinary constructor, or one
     ordinary non-null and one [Null] constructor. *)

and constructor_representation =
  | Constructor_uniform_value
  (* A constant constructor or a constructor all of whose fields are values.
     This is named 'uniform_value' to distinguish from the 'Constructor_uniform'
     of [lambda.mli], which can also represent all-flat-float records.
  *)
  | Constructor_mixed of mixed_product_shape
  (* A constructor that has some non-value fields. *)

and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutability;
    ld_modalities: Mode.Modality.Value.Const.t;
    ld_type: type_expr;
    ld_sort: Jkind_types.Sort.Const.t;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
    ld_uid: Uid.t;
  }

and constructor_declaration =
  {
    cd_id: Ident.t;
    cd_args: constructor_arguments;
    cd_res: type_expr option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
    cd_uid: Uid.t;
  }

and constructor_argument =
  {
    ca_modalities: Mode.Modality.Value.Const.t;
    ca_type: type_expr;
    ca_sort: Jkind_types.Sort.Const.t;
    ca_loc: Location.t;
  }

and constructor_arguments =
  | Cstr_tuple of constructor_argument list
  | Cstr_record of label_declaration list

val tys_of_constr_args : constructor_arguments -> type_expr list

(* Returns the inner type and its modalities, if unboxed. *)
val find_unboxed_type : type_declaration -> (type_expr * Mode.Modality.Value.Const.t) option

type extension_constructor =
  {
    ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
    ext_shape: constructor_representation;
    ext_constant: bool;
    ext_ret_type: type_expr option;
    ext_private: private_flag;
    ext_loc: Location.t;
    ext_attributes: Parsetree.attributes;
    ext_uid: Uid.t;
  }

and type_transparence =
    Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)

(* Type expressions for the class language *)

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: Variance.t list;
    cty_loc: Location.t;
    cty_attributes: Parsetree.attributes;
    cty_uid: Uid.t;
  }

type class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_hash_type: type_declaration; (* object type with an open row *)
    clty_variance: Variance.t list;
    clty_loc: Location.t;
    clty_attributes: Parsetree.attributes;
    clty_uid: Uid.t;
  }

(* Type expressions for the module language *)

type visibility =
  | Exported
  | Hidden

type rec_status =
    Trec_not                   (* first in a nonrecursive group *)
  | Trec_first                 (* first in a recursive group *)
  | Trec_next                  (* not first in a recursive/nonrecursive group *)

type ext_status =
    Text_first                     (* first constructor in an extension *)
  | Text_next                      (* not first constructor in an extension *)
  | Text_exception

type module_presence =
  | Mp_present
  | Mp_absent

(* Module aliasability for strengthening *)
module Aliasability : sig
  type t = Not_aliasable | Aliasable

  val aliasable : bool -> t
  val is_aliasable : t -> bool
end

(* Wrap.t encapsulates bits of module types which can be lazy *)
module type Wrap = sig
  type 'a t
end

module type Wrapped = sig
  type 'a wrapped

  type value_description =
    { val_type: type_expr wrapped;                (* Type of the value *)
      val_modalities: Mode.Modality.Value.t;      (* Modalities on the value *)
      val_kind: value_kind;
      val_loc: Location.t;
      val_zero_alloc: Zero_alloc.t;
      val_attributes: Parsetree.attributes;
      val_uid: Uid.t;
    }

  type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t
  | Mty_strengthen of module_type * Path.t * Aliasability.t
      (* See comments about the aliasability of strengthening in mtype.ml *)

  and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type

  and signature = signature_item list wrapped

  and signature_item =
    Sig_value of Ident.t * value_description * visibility
  | Sig_type of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility

  and module_declaration =
  {
    md_type: module_type;
    md_modalities : Mode.Modality.Value.t;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
    md_uid: Uid.t;
  }

  and modtype_declaration =
  {
    mtd_type: module_type option;  (* None: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
    mtd_uid: Uid.t;
  }
end

module Make_wrapped(Wrap : Wrap) : Wrapped with type 'a wrapped = 'a Wrap.t

module Map_wrapped(From : Wrapped)(To : Wrapped) : sig
  type mapper =
    {
      map_signature: mapper -> From.signature -> To.signature;
      map_type_expr: mapper -> type_expr From.wrapped -> type_expr To.wrapped
    }

  val value_description :
    mapper -> From.value_description -> To.value_description
  val module_declaration :
    mapper -> From.module_declaration -> To.module_declaration
  val modtype_declaration :
    mapper -> From.modtype_declaration -> To.modtype_declaration
  val module_type : mapper -> From.module_type -> To.module_type
  val signature : mapper -> From.signature -> To.signature
  val signature_item : mapper -> From.signature_item -> To.signature_item
  val functor_parameter :
    mapper -> From.functor_parameter -> To.functor_parameter
end

include Wrapped with type 'a wrapped = 'a

val item_visibility : signature_item -> visibility

(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: constructor_argument list; (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: tag;                      (* Tag for heap blocks *)
    cstr_repr: variant_representation;  (* Repr of the outer variant *)
    cstr_shape: constructor_representation; (* Repr of the constructor itself *)
    cstr_constant: bool;                (* True if all args are void *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
      (* [Some decl] here iff the cstr has an inline record (which is decl) *)
    cstr_uid: Uid.t;
   }

(* Constructors are the same *)
val equal_tag :  tag -> tag -> bool

(* Comparison of tags to store them in sets. *)
val compare_tag :  tag -> tag -> int

(* Constructors may be the same, given potential rebinding *)
val may_equal_constr :
    constructor_description ->  constructor_description -> bool

(* Equality *)

val equal_record_representation :
  record_representation -> record_representation -> bool

val equal_record_unboxed_product_representation :
  record_unboxed_product_representation -> record_unboxed_product_representation -> bool

val equal_variant_representation :
  variant_representation -> variant_representation -> bool

type 'a gen_label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutability;                (* Is this a mutable field? *)
    lbl_modalities: Mode.Modality.Value.Const.t;
                                        (* Modalities on the field *)
    lbl_sort: Jkind_types.Sort.Const.t; (* Sort of the argument *)
    lbl_pos: int;                       (* Position in type *)
    lbl_all: 'a gen_label_description array;   (* All the labels in this type *)
    lbl_repres: 'a;  (* Representation for outer record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
  }

type label_description = record_representation gen_label_description

type unboxed_label_description = record_unboxed_product_representation gen_label_description

(** This type tracks the distinction between legacy records ([{ field }]) and unboxed
    records ([#{ field }]). Note that [Legacy] includes normal boxed records, as well as
    inlined and [[@@unboxed]] records.

    As a GADT, it also lets us avoid duplicating functions that handle both record forms,
    such as [Env.find_label_by_name], which has type
    ['rep record_form -> Longident.t -> Env.t -> 'rep gen_label_description].
*)
type _ record_form =
  | Legacy : record_representation record_form
  | Unboxed_product : record_unboxed_product_representation record_form

type record_form_packed =
  | P : _ record_form -> record_form_packed

val record_form_to_string : _ record_form -> string

(** Extracts the list of "value" identifiers bound by a signature.
    "Value" identifiers are identifiers for signature components that
    correspond to a run-time value: values, extensions, modules, classes.
    Note: manifest primitives do not correspond to a run-time value! *)
val bound_value_identifiers: signature -> Ident.t list

val signature_item_id : signature_item -> Ident.t

val equal_mixed_block_element :
  mixed_block_element -> mixed_block_element -> bool
val compare_mixed_block_element :
  mixed_block_element -> mixed_block_element -> int
val mixed_block_element_to_string : mixed_block_element -> string
val mixed_block_element_to_lowercase_string : mixed_block_element -> string

val equal_unsafe_mode_crossing :
  type_equal:(type_expr -> type_expr -> bool) ->
  unsafe_mode_crossing -> unsafe_mode_crossing -> bool

(**** Utilities for backtracking ****)

type snapshot
        (* A snapshot for backtracking *)
val snapshot: unit -> snapshot
        (* Make a snapshot for later backtracking. Costs nothing *)
val backtrack: cleanup_abbrev:(unit -> unit) -> snapshot -> unit
        (* Backtrack to a given snapshot. Only possible if you have
           not already backtracked to a previous snapshot.
           Calls [cleanup_abbrev] internally *)
val undo_first_change_after: snapshot -> unit
        (* Backtrack only the first change after a snapshot.
           Does not update the list of changes *)
val undo_compress: snapshot -> unit
        (* Backtrack only path compression. Only meaningful if you have
           not already backtracked to a previous snapshot.
           Does not call [cleanup_abbrev] *)

(** Functions to use when modifying a type (only Ctype?).
    The old values are logged and reverted on backtracking.
 *)

val link_type: type_expr -> type_expr -> unit
        (* Set the desc field of [t1] to [Tlink t2], logging the old value if
           there is an active snapshot.  Any jkind information in [t1]'s desc
           is thrown away without checking - calls to this in unification should
           first check that [t2]'s jkind is a subjkind of [t1]. *)
val set_type_desc: type_expr -> type_desc -> unit
        (* Set directly the desc field, without sharing *)
val set_level: type_expr -> int -> unit
val set_scope: type_expr -> int -> unit
val set_var_jkind: type_expr -> jkind_lr -> unit
        (* May only be called on Tvars *)
val set_name:
    (Path.t * type_expr list) option ref ->
    (Path.t * type_expr list) option -> unit
val link_row_field_ext: inside:row_field -> row_field -> unit
        (* Extract the extension variable of [inside] and set it to the
           second argument *)
val set_univar: type_expr option ref -> type_expr -> unit
val link_kind: inside:field_kind -> field_kind -> unit
val link_commu: inside:commutable -> commutable -> unit
val set_commu_ok: commutable -> unit

val functor_param_mode : Mode.Alloc.lr
val functor_res_mode : Mode.Alloc.lr
