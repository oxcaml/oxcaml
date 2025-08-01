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

(* Representation of types and declarations *)

open Allowance
open Asttypes

type atomic =
  | Nonatomic
  | Atomic

type mutability =
  | Immutable
  | Mutable of
      { mode : Mode.Value.Comonadic.lr
      ; atomic : atomic
      }

let is_mutable = function
  | Immutable -> false
  | Mutable _ -> true

let is_atomic = function
  | Immutable -> false
  | Mutable { atomic = Atomic; mode = _ } -> true
  | Mutable { atomic = Nonatomic; mode = _ } -> false

(** Takes [m0] which is the parameter of [let mutable], returns the
    mode of new values in future writes. *)
let mutable_mode m0 : _ Mode.Value.t =
  { comonadic = m0
  ; monadic = Mode.Value.Monadic.(min |> allow_left |> allow_right)
  }

(* Type expressions for the core language *)

module Jkind_mod_bounds = struct
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

  type t = {
    locality: Locality.t;
    linearity: Linearity.t;
    uniqueness: Uniqueness.t;
    portability: Portability.t;
    contention: Contention.t;
    yielding: Yielding.t;
    statefulness: Statefulness.t;
    visibility: Visibility.t;
    externality: Externality.t;
    nullability: Nullability.t;
    separability: Separability.t;
  }

  let[@inline] locality t = t.locality
  let[@inline] linearity t = t.linearity
  let[@inline] uniqueness t = t.uniqueness
  let[@inline] portability t = t.portability
  let[@inline] contention t = t.contention
  let[@inline] yielding t = t.yielding
  let[@inline] statefulness t = t.statefulness
  let[@inline] visibility t = t.visibility
  let[@inline] externality t = t.externality
  let[@inline] nullability t = t.nullability
  let[@inline] separability t = t.separability

  let[@inline] create
      ~locality
      ~linearity
      ~uniqueness
      ~portability
      ~contention
      ~yielding
      ~statefulness
      ~visibility
      ~externality
      ~nullability
      ~separability =
    {
      locality;
      linearity;
      uniqueness;
      portability;
      contention;
      yielding;
      statefulness;
      visibility;
      externality;
      nullability;
      separability;
    }

  let[@inline] set_locality locality t = { t with locality }
  let[@inline] set_linearity linearity t = { t with linearity }
  let[@inline] set_uniqueness uniqueness t = { t with uniqueness }
  let[@inline] set_portability portability t = { t with portability }
  let[@inline] set_contention contention t = { t with contention }
  let[@inline] set_yielding yielding t = { t with yielding }
  let[@inline] set_statefulness statefulness t = { t with statefulness }
  let[@inline] set_visibility visibility t = { t with visibility }
  let[@inline] set_externality externality t = { t with externality }
  let[@inline] set_nullability nullability t = { t with nullability }
  let[@inline] set_separability separability t = { t with separability }

  let[@inline] set_max_in_set t max_axes =
    let open Jkind_axis.Axis_set in
    (* a little optimization *)
    if is_empty max_axes then t else
    let locality =
      if mem max_axes (Modal (Comonadic Areality))
      then Locality.max
      else t.locality
    in
    let linearity =
      if mem max_axes (Modal (Comonadic Linearity))
      then Linearity.max
      else t.linearity
    in
    let uniqueness =
      if mem max_axes (Modal (Monadic Uniqueness))
      then Uniqueness.max
      else t.uniqueness
    in
    let portability =
      if mem max_axes (Modal (Comonadic Portability))
      then Portability.max
      else t.portability
    in
    let contention =
      if mem max_axes (Modal (Monadic Contention))
      then Contention.max
      else t.contention
    in
    let yielding =
      if mem max_axes (Modal (Comonadic Yielding))
      then Yielding.max
      else t.yielding
    in
    let statefulness =
      if mem max_axes (Modal (Comonadic Statefulness))
      then Statefulness.max
      else t.statefulness
    in
    let visibility =
      if mem max_axes (Modal (Monadic Visibility))
      then Visibility.max
      else t.visibility
    in
    let externality =
      if mem max_axes (Nonmodal Externality)
      then Externality.max
      else t.externality
    in
    let nullability =
      if mem max_axes (Nonmodal Nullability)
      then Nullability.max
      else t.nullability
    in
    let separability =
      if mem max_axes (Nonmodal Separability)
      then Separability.max
      else t.separability
    in
    {
      locality;
      linearity;
      uniqueness;
      portability;
      contention;
      yielding;
      statefulness;
      visibility;
      externality;
      nullability;
      separability;
    }

  let[@inline] set_min_in_set t min_axes =
    let open Jkind_axis.Axis_set in
    (* a little optimization *)
    if is_empty min_axes then t else
    let locality =
      if mem min_axes (Modal (Comonadic Areality))
      then Locality.min
      else t.locality
    in
    let linearity =
      if mem min_axes (Modal (Comonadic Linearity))
      then Linearity.min
      else t.linearity
    in
    let uniqueness =
      if mem min_axes (Modal (Monadic Uniqueness))
      then Uniqueness.min
      else t.uniqueness
    in
    let portability =
      if mem min_axes (Modal (Comonadic Portability))
      then Portability.min
      else t.portability
    in
    let contention =
      if mem min_axes (Modal (Monadic Contention))
      then Contention.min
      else t.contention
    in
    let yielding =
      if mem min_axes (Modal (Comonadic Yielding))
      then Yielding.min
      else t.yielding
    in
    let statefulness =
      if mem min_axes (Modal (Comonadic Statefulness))
      then Statefulness.min
      else t.statefulness
    in
    let visibility =
      if mem min_axes (Modal (Monadic Visibility))
      then Visibility.min
      else t.visibility
    in
    let externality =
      if mem min_axes (Nonmodal Externality)
      then Externality.min
      else t.externality
    in
    let nullability =
      if mem min_axes (Nonmodal Nullability)
      then Nullability.min
      else t.nullability
    in
    let separability =
      if mem min_axes (Nonmodal Separability)
      then Separability.min
      else t.separability
    in
    {
      locality;
      linearity;
      uniqueness;
      portability;
      contention;
      statefulness;
      visibility;
      yielding;
      externality;
      nullability;
      separability;
    }

  let[@inline] is_max_within_set t axes =
    let open Jkind_axis.Axis_set in
    (not (mem axes (Modal (Comonadic Areality))) ||
     Locality.(le max (locality t))) &&
    (not (mem axes (Modal (Comonadic Linearity))) ||
     Linearity.(le max (linearity t))) &&
    (not (mem axes (Modal (Monadic Uniqueness))) ||
     Uniqueness.(le max (uniqueness t))) &&
    (not (mem axes (Modal (Comonadic Portability))) ||
     Portability.(le max (portability t))) &&
    (not (mem axes (Modal (Monadic Contention))) ||
     Contention.(le max (contention t))) &&
    (not (mem axes (Modal (Comonadic Yielding))) ||
     Yielding.(le max (yielding t))) &&
    (not (mem axes (Modal (Comonadic Statefulness))) ||
     Statefulness.(le max (statefulness t))) &&
    (not (mem axes (Modal (Monadic Visibility))) ||
     Visibility.(le max (visibility t))) &&
    (not (mem axes (Nonmodal Externality)) ||
     Externality.(le max (externality t))) &&
    (not (mem axes (Nonmodal Nullability)) ||
     Nullability.(le max (nullability t))) &&
    (not (mem axes (Nonmodal Separability)) ||
     Separability.(le max (separability t)))

  let max =
      { locality = Locality.max;
        linearity = Linearity.max;
        uniqueness = Uniqueness.max;
        portability = Portability.max;
        contention = Contention.max;
        yielding = Yielding.max;
        statefulness = Statefulness.max;
        visibility = Visibility.max;
        externality = Externality.max;
        nullability = Nullability.max;
        separability = Separability.max}

  let[@inline] is_max m = m = max


  let debug_print ppf
        { locality;
          linearity;
          uniqueness;
          portability;
          contention;
          yielding;
          statefulness;
          visibility;
          externality;
          nullability;
          separability } =
    Format.fprintf ppf "@[{ locality = %a;@ linearity = %a;@ uniqueness = %a;@ \
      portability = %a;@ contention = %a;@ yielding = %a;@ statefulness = %a;@ \
      visibility = %a;@ externality = %a;@ \
      nullability = %a;@ separability = %a }@]"
      Locality.print locality
      Linearity.print linearity
      Uniqueness.print uniqueness
      Portability.print portability
      Contention.print contention
      Yielding.print yielding
      Statefulness.print statefulness
      Visibility.print visibility
      Externality.print externality
      Nullability.print nullability
      Separability.print separability
end


module With_bounds_type_info = struct
  type t = {relevant_axes : Jkind_axis.Axis_set.t } [@@unboxed]
end

type transient_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable scope: int;
    id: int }

and type_expr = transient_expr

and type_desc =
  | Tvar of { name : string option; jkind : jkind_lr }
  | Tarrow of arrow_desc * type_expr * type_expr * commutable
  | Ttuple of (string option * type_expr) list
  | Tunboxed_tuple of (string option * type_expr) list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr * type_expr option
  | Tvariant of row_desc
  | Tunivar of { name : string option; jkind : jkind_lr }
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * (Longident.t * type_expr) list
  | Tof_kind of jkind_lr

and arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string
  | Position of string

and arrow_desc =
  arg_label * Mode.Alloc.lr * Mode.Alloc.lr

and row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_closed: bool;
      row_fixed: fixed_explanation option;
      row_name: (Path.t * type_expr list) option }
and fixed_explanation =
  | Univar of type_expr
  | Fixed_private
  | Reified of Path.t
  | Rigid
  | Fixed_existential
and row_field = [`some] row_field_gen
and _ row_field_gen =
    RFpresent : type_expr option -> [> `some] row_field_gen
  | RFeither :
      { no_arg: bool;
        arg_type: type_expr list;
        matched: bool;
        ext: [`some | `none] row_field_gen ref} -> [> `some] row_field_gen
  | RFabsent : [> `some] row_field_gen
  | RFnone : [> `none] row_field_gen

and abbrev_memo =
    Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and any = [`some | `none | `var]
and field_kind = [`some|`var] field_kind_gen
and _ field_kind_gen =
    FKvar : {mutable field_kind: any field_kind_gen} -> [> `var] field_kind_gen
  | FKprivate : [> `none] field_kind_gen  (* private method; only under FKvar *)
  | FKpublic  : [> `some] field_kind_gen  (* public method *)
  | FKabsent  : [> `some] field_kind_gen  (* hidden private method *)

and commutable = [`some|`var] commutable_gen
and _ commutable_gen =
    Cok      : [> `some] commutable_gen
  | Cunknown : [> `none] commutable_gen
  | Cvar : {mutable commu: any commutable_gen} -> [> `var] commutable_gen

(* jkinds *)

and jkind_history =
  | Interact of
      { reason : Jkind_intf.History.interact_reason;
        jkind1 : jkind_desc_packed;
        history1 : jkind_history;
        jkind2 : jkind_desc_packed;
        history2 : jkind_history
      }
  | Creation of Jkind_intf.History.creation_reason

(* See [With_bounds_types] for more information on this abstract type. *)
and with_bounds_types

and 'd with_bounds =
  | No_with_bounds : ('l * 'r) with_bounds
  | With_bounds : with_bounds_types -> ('l * Allowance.disallowed) with_bounds

and ('layout, 'd) layout_and_axes =
  { layout : 'layout;
    mod_bounds : Jkind_mod_bounds.t;
    with_bounds : 'd with_bounds
  }
  constraint 'd = 'l * 'r

and 'd jkind_desc = (Jkind_types.Sort.t Jkind_types.Layout.t, 'd) layout_and_axes
  constraint 'd = 'l * 'r

and jkind_desc_packed = Pack_jkind_desc : ('l * 'r) jkind_desc -> jkind_desc_packed

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

and jkind_l = (allowed * disallowed) jkind
and jkind_r = (disallowed * allowed) jkind
and jkind_lr = (allowed * allowed) jkind
and jkind_packed = Pack_jkind : ('l * 'r) jkind -> jkind_packed

module TransientTypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

(* *)

module Uid = Shape.Uid

(* Maps of methods and instance variables *)

module MethSet = Misc.Stdlib.String.Set
module VarSet = Misc.Stdlib.String.Set

module Meths = Misc.Stdlib.String.Map
module Vars = Misc.Stdlib.String.Map


(* Value descriptions *)

type value_kind =
    Val_reg                             (* Regular value *)
  | Val_mut of Mode.Value.Comonadic.lr * Jkind_types.Sort.t
                                        (* Mutable value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of
      class_signature * self_meths * Ident.t Vars.t * string
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

(* Variance *)
(* Variance forms a product lattice of the following partial orders:
     0 <= may_pos <= pos
     0 <= may_weak <= may_neg <= neg
     0 <= inj
   Additionally, the following implications are valid
     pos => inj
     neg => inj
   Examples:
     type 'a t        : may_pos + may_neg + may_weak
     type 'a t = 'a   : pos
     type 'a t = 'a -> unit : neg
     type 'a t = ('a -> unit) -> unit : pos + may_weak
     type 'a t = A of (('a -> unit) -> unit) : pos
     type +'a p = ..  : may_pos + inj
     type +!'a t      : may_pos + inj
     type -!'a t      : may_neg + inj
     type 'a t = A    : inj
 *)

module Variance = struct
  type t = int
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  let single = function
    | May_pos -> 1
    | May_neg -> 2 + 4
    | May_weak -> 4
    | Inj -> 8
    | Pos -> 16 + 8 + 1
    | Neg -> 32 + 8 + 4 + 2
    | Inv -> 63
  let union v1 v2 = v1 lor v2
  let inter v1 v2 = v1 land v2
  let subset v1 v2 = (v1 land v2 = v1)
  let eq (v1 : t) v2 = (v1 = v2)
  let set x v = union v (single x)
  let set_if b x v = if b then set x v else v
  let mem x = subset (single x)
  let null = 0
  let unknown = 7
  let full = single Inv
  let covariant = single Pos
  let swap f1 f2 v v' =
    set_if (mem f2 v) f1 (set_if (mem f1 v) f2 v')
  let conjugate v =
    let v' = inter v (union (single Inj) (single May_weak)) in
    swap Pos Neg v (swap May_pos May_neg v v')
  let compose v1 v2 =
    if mem Inv v1 && mem Inj v2 then full else
    let mp =
      mem May_pos v1 && mem May_pos v2 || mem May_neg v1 && mem May_neg v2
    and mn =
      mem May_pos v1 && mem May_neg v2 || mem May_neg v1 && mem May_pos v2
    and mw = mem May_weak v1 && v2 <> null || v1 <> null && mem May_weak v2
    and inj = mem Inj v1 && mem Inj v2
    and pos = mem Pos v1 && mem Pos v2 || mem Neg v1 && mem Neg v2
    and neg = mem Pos v1 && mem Neg v2 || mem Neg v1 && mem Pos v2 in
    List.fold_left (fun v (b,f) -> set_if b f v) null
      [mp, May_pos; mn, May_neg; mw, May_weak; inj, Inj; pos, Pos; neg, Neg]
  let strengthen v =
    if mem May_neg v then v else v land (full - single May_weak)
  let get_upper v = (mem May_pos v, mem May_neg v)
  let get_lower v = (mem Pos v, mem Neg v, mem Inj v)
  let unknown_signature ~injective ~arity =
    let v = if injective then set Inj unknown else unknown in
    Misc.replicate_list v arity
end

module Separability = struct
  type t = Ind | Sep | Deepsep
  type signature = t list
  let eq (m1 : t) m2 = (m1 = m2)
  let rank = function
    | Ind -> 0
    | Sep -> 1
    | Deepsep -> 2
  let compare m1 m2 = compare (rank m1) (rank m2)
  let max m1 m2 = if rank m1 >= rank m2 then m1 else m2

  let print ppf = function
    | Ind -> Format.fprintf ppf "Ind"
    | Sep -> Format.fprintf ppf "Sep"
    | Deepsep -> Format.fprintf ppf "Deepsep"

  let print_signature ppf modes =
    let pp_sep ppf () = Format.fprintf ppf ",@," in
    Format.fprintf ppf "@[(%a)@]"
      (Format.pp_print_list ~pp_sep print) modes

  let default_signature ~arity =
    let default_mode = if Config.flat_float_array then Deepsep else Ind in
    Misc.replicate_list default_mode arity
end

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_decl_kind;
    type_jkind: jkind_l;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_unboxed_default: bool;
    type_uid: Uid.t;
    type_unboxed_version : type_declaration option;
 }

and type_decl_kind =
  (label_declaration, label_declaration, constructor_declaration) type_kind

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

and tag = Ordinary of {src_index: int;     (* Unique name (per type) *)
                       runtime_tag: int}   (* The runtime tag *)
        | Extension of Path.t
        | Null

and type_origin =
    Definition
  | Rec_check_regularity
  | Existential of string

and mixed_block_element =
  | Value
  | Float_boxed
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
  | Void

and mixed_product_shape = mixed_block_element array

and record_representation =
  | Record_unboxed
  | Record_inlined of tag * constructor_representation * variant_representation
  | Record_boxed of Jkind_types.Sort.Const.t array
  | Record_float
  | Record_ufloat
  | Record_mixed of mixed_product_shape

and record_unboxed_product_representation =
  | Record_unboxed_product

and variant_representation =
  | Variant_unboxed
  | Variant_boxed of (constructor_representation *
                      Jkind_types.Sort.Const.t array) array
  | Variant_extensible
  | Variant_with_null

and constructor_representation =
  | Constructor_uniform_value
  | Constructor_mixed of mixed_product_shape

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

type extension_constructor =
  { ext_type_path: Path.t;
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

let tys_of_constr_args = function
  | Cstr_tuple tl -> List.map (fun ca -> ca.ca_type) tl
  | Cstr_record lbls -> List.map (fun l -> l.ld_type) lbls

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
    clty_hash_type: type_declaration;
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
  Text_first                     (* first constructor of an extension *)
| Text_next                      (* not first constructor of an extension *)
| Text_exception                 (* an exception *)

type module_presence =
  | Mp_present
  | Mp_absent

module Aliasability = struct
  type t = Not_aliasable | Aliasable

  let aliasable b = if b then Aliasable else Not_aliasable

  let is_aliasable = function
    | Aliasable -> true
    | Not_aliasable -> false
end

module type Wrap = sig
  type 'a t
end

module type Wrapped = sig
  type 'a wrapped

  type value_description =
    { val_type: type_expr wrapped;                (* Type of the value *)
      val_modalities : Mode.Modality.Value.t;     (* Modalities on the value *)
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
    md_modalities: Mode.Modality.Value.t;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
    md_uid: Uid.t;
  }

  and modtype_declaration =
  {
    mtd_type: module_type option;  (* Note: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
    mtd_uid: Uid.t;
  }
end

module Make_wrapped(Wrap : Wrap) = struct
  (* Avoid repeating everything in Wrapped *)
  module rec M : Wrapped with type 'a wrapped = 'a Wrap.t = M
  include M
end

module Map_wrapped(From : Wrapped)(To : Wrapped) = struct
  open From
  type mapper =
    {
      map_signature: mapper -> signature -> To.signature;
      map_type_expr: mapper -> type_expr wrapped -> type_expr To.wrapped
    }

  let signature m = m.map_signature m

  let rec module_type m = function
    | Mty_ident p -> To.Mty_ident p
    | Mty_alias p -> To.Mty_alias p
    | Mty_functor (parm,mty) ->
        To.Mty_functor (functor_parameter m parm, module_type m mty)
    | Mty_signature sg -> To.Mty_signature (signature m sg)
    | Mty_strengthen (mty,p,aliasable) ->
        To.Mty_strengthen (module_type m mty, p, aliasable)

  and functor_parameter m = function
      | Unit -> To.Unit
      | Named (id,mty) -> To.Named (id, module_type m mty)

  let value_description m {val_type; val_modalities; val_kind; val_zero_alloc;
                           val_attributes; val_loc; val_uid} =
    To.{
      val_type = m.map_type_expr m val_type;
      val_modalities;
      val_kind;
      val_zero_alloc;
      val_attributes;
      val_loc;
      val_uid
    }

  let module_declaration m {md_type; md_modalities; md_attributes;
    md_loc; md_uid} =
    To.{
      md_type = module_type m md_type;
      md_modalities;
      md_attributes;
      md_loc;
      md_uid;
    }

  let modtype_declaration m {mtd_type; mtd_attributes; mtd_loc; mtd_uid} =
    To.{
      mtd_type = Option.map (module_type m) mtd_type;
      mtd_attributes;
      mtd_loc;
      mtd_uid;
    }

  let signature_item m = function
    | Sig_value (id,vd,vis) ->
        To.Sig_value (id, value_description m vd, vis)
    | Sig_type (id,td,rs,vis) ->
        To.Sig_type (id,td,rs,vis)
    | Sig_module (id,pres,md,rs,vis) ->
        To.Sig_module (id, pres, module_declaration m md, rs, vis)
    | Sig_modtype (id,mtd,vis) ->
        To.Sig_modtype (id, modtype_declaration m mtd, vis)
    | Sig_typext (id,ec,es,vis) ->
        To.Sig_typext (id,ec,es,vis)
    | Sig_class (id,cd,rs,vis) ->
        To.Sig_class (id,cd,rs,vis)
    | Sig_class_type (id,ctd,rs,vis) ->
        To.Sig_class_type (id,ctd,rs,vis)
end

include Make_wrapped(struct type 'a t = 'a end)

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
    cstr_uid: Uid.t;
   }

let equal_tag t1 t2 =
  match (t1, t2) with
  | Ordinary {src_index=i1}, Ordinary {src_index=i2} ->
    i2 = i1 (* If i1 = i2, the runtime_tags will also be equal *)
  | Extension path1, Extension path2 -> Path.same path1 path2
  | Null, Null -> true
  | (Ordinary _ | Extension _ | Null), _ -> false

let compare_tag t1 t2 =
  match (t1, t2) with
  | Ordinary {src_index=i1}, Ordinary {src_index=i2} ->
    Int.compare i1 i2
  | Extension path1, Extension path2 -> Path.compare path1 path2
  | Null, Null -> 0
  | Ordinary _, (Extension _ | Null) -> -1
  | (Extension _ | Null), Ordinary _ -> 1
  | Extension _, Null -> -1
  | Null, Extension _ -> 1

let rec equal_mixed_block_element e1 e2 =
  match e1, e2 with
  | Value, Value | Float64, Float64 | Float32, Float32 | Float_boxed, Float_boxed
  | Word, Word | Bits8, Bits8 | Bits16, Bits16 | Bits32, Bits32 | Bits64, Bits64
  | Vec128, Vec128 | Vec256, Vec256 | Vec512, Vec512
  | Void, Void
    -> true
  | Product es1, Product es2
    -> Misc.Stdlib.Array.equal equal_mixed_block_element es1 es2
  | ( Value | Float64 | Float32 | Float_boxed | Word | Bits8 | Bits16 | Bits32
    | Bits64 | Vec128 | Vec256 | Vec512 | Product _ | Void ), _
    -> false

let rec compare_mixed_block_element e1 e2 =
  match e1, e2 with
  | Value, Value | Float_boxed, Float_boxed
  | Float64, Float64 | Float32, Float32
  | Word, Word | Bits8, Bits8 | Bits16, Bits16 | Bits32, Bits32 | Bits64, Bits64
  | Vec128, Vec128 | Vec256, Vec256 | Vec512, Vec512
  | Void, Void
    -> 0
  | Product es1, Product es2
    -> Misc.Stdlib.Array.compare compare_mixed_block_element es1 es2
  | Value, _ -> -1
  | _, Value -> 1
  | Float_boxed, _ -> -1
  | _, Float_boxed -> 1
  | Float64, _ -> -1
  | _, Float64 -> 1
  | Float32, _ -> -1
  | _, Float32 -> 1
  | Word, _ -> -1
  | _, Word -> 1
  | Bits8, _ -> -1
  | _, Bits8 -> 1
  | Bits16, _ -> -1
  | _, Bits16 -> 1
  | Bits32, _ -> -1
  | _, Bits32 -> 1
  | Bits64, _ -> -1
  | _, Bits64 -> 1
  | Vec128, _ -> -1
  | _, Vec128 -> 1
  | Vec256, _ -> -1
  | _, Vec256 -> 1
  | Vec512, _ -> -1
  | _, Vec512 -> 1
  | Void, _ -> -1
  | _, Void -> 1

let equal_mixed_product_shape r1 r2 = r1 == r2 ||
  Misc.Stdlib.Array.equal equal_mixed_block_element r1 r2

let equal_constructor_representation r1 r2 = r1 == r2 || match r1, r2 with
  | Constructor_uniform_value, Constructor_uniform_value -> true
  | Constructor_mixed mx1, Constructor_mixed mx2 ->
      equal_mixed_product_shape mx1 mx2
  | (Constructor_mixed _ | Constructor_uniform_value), _ -> false

let equal_variant_representation r1 r2 = r1 == r2 || match r1, r2 with
  | Variant_unboxed, Variant_unboxed ->
      true
  | Variant_boxed cstrs_and_sorts1, Variant_boxed cstrs_and_sorts2 ->
      Misc.Stdlib.Array.equal (fun (cstr1, sorts1) (cstr2, sorts2) ->
          equal_constructor_representation cstr1 cstr2
          && Misc.Stdlib.Array.equal Jkind_types.Sort.Const.equal
               sorts1 sorts2)
        cstrs_and_sorts1
        cstrs_and_sorts2
  | Variant_extensible, Variant_extensible ->
      true
  | Variant_with_null, Variant_with_null -> true
  | (Variant_unboxed | Variant_boxed _ | Variant_extensible | Variant_with_null), _ ->
      false

let equal_record_representation r1 r2 = match r1, r2 with
  | Record_unboxed, Record_unboxed ->
      true
  | Record_inlined (tag1, cr1, vr1), Record_inlined (tag2, cr2, vr2) ->
      (* Equality of tag and variant representation imply equality of
         constructor representation. *)
      ignore (cr1 : constructor_representation);
      ignore (cr2 : constructor_representation);
      equal_tag tag1 tag2 && equal_variant_representation vr1 vr2
  | Record_boxed sorts1, Record_boxed sorts2 ->
      Misc.Stdlib.Array.equal Jkind_types.Sort.Const.equal sorts1 sorts2
  | Record_float, Record_float ->
      true
  | Record_ufloat, Record_ufloat ->
      true
  | Record_mixed mx1, Record_mixed mx2 -> equal_mixed_product_shape mx1 mx2
  | (Record_unboxed | Record_inlined _ | Record_boxed _ | Record_float
    | Record_ufloat | Record_mixed _), _ ->
      false

let equal_record_unboxed_product_representation r1 r2 = match r1, r2 with
  | Record_unboxed_product, Record_unboxed_product -> true

let may_equal_constr c1 c2 =
  c1.cstr_arity = c2.cstr_arity
  && (match c1.cstr_tag,c2.cstr_tag with
     | Extension _, Extension _ ->
         (* extension constructors may be rebindings of each other *)
         true
     | tag1, tag2 ->
         equal_tag tag1 tag2)

type 'a gen_label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutability;                (* Is this a mutable field? *)
    lbl_modalities: Mode.Modality.Value.Const.t;(* Modalities on the field *)
    lbl_sort: Jkind_types.Sort.Const.t; (* Sort of the argument *)
    lbl_pos: int;                       (* Position in type *)
    lbl_all: 'a gen_label_description array;   (* All the labels in this type *)
    lbl_repres: 'a;                     (* Representation for outer record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
  }

type label_description = record_representation gen_label_description

type unboxed_label_description =
  record_unboxed_product_representation gen_label_description

type _ record_form =
  | Legacy : record_representation record_form
  | Unboxed_product : record_unboxed_product_representation record_form

type record_form_packed =
  | P : _ record_form -> record_form_packed

let record_form_to_string (type rep) (record_form : rep record_form) =
  match record_form with
  | Legacy -> "record"
  | Unboxed_product -> "unboxed record"

let find_unboxed_type decl =
  match decl.type_kind with
    Type_record ([{ld_type = arg; ld_modalities = ms; _}], Record_unboxed, _)
  | Type_record
      ([{ld_type = arg; ld_modalities = ms; _ }], Record_inlined (_, _, Variant_unboxed), _)
  | Type_record_unboxed_product
      ([{ld_type = arg; ld_modalities = ms; _ }], Record_unboxed_product, _)
  | Type_variant ([{cd_args = Cstr_tuple [{ca_type = arg; ca_modalities = ms; _}]; _}], Variant_unboxed, _)
  | Type_variant ([{cd_args = Cstr_record [{ld_type = arg; ld_modalities = ms; _}]; _}], Variant_unboxed, _) ->
    Some (arg, ms)
  | Type_record (_, ( Record_inlined _ | Record_unboxed
                    | Record_boxed _ | Record_float | Record_ufloat
                    | Record_mixed _), _)
  | Type_record_unboxed_product (_, Record_unboxed_product, _)
  | Type_variant (_, ( Variant_boxed _ | Variant_unboxed
                     | Variant_extensible | Variant_with_null), _)
  | Type_abstract _ | Type_open ->
    None

let item_visibility = function
  | Sig_value (_, _, vis)
  | Sig_type (_, _, _, vis)
  | Sig_typext (_, _, _, vis)
  | Sig_module (_, _, _, _, vis)
  | Sig_modtype (_, _, vis)
  | Sig_class (_, _, _, vis)
  | Sig_class_type (_, _, _, vis) -> vis

let rec bound_value_identifiers = function
    [] -> []
  | Sig_value(id, {val_kind = Val_reg}, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_typext(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_module(id, Mp_present, _, _, _) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_class(id, _, _, _) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem

let signature_item_id = function
  | Sig_value (id, _, _)
  | Sig_type (id, _, _, _)
  | Sig_typext (id, _, _, _)
  | Sig_module (id, _, _, _, _)
  | Sig_modtype (id, _, _)
  | Sig_class (id, _, _, _)
  | Sig_class_type (id, _, _, _)
    -> id

let rec mixed_block_element_to_string = function
  | Value -> "Value"
  | Float_boxed -> "Float_boxed"
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | Bits8 -> "Bits8"
  | Bits16 -> "Bits16"
  | Bits32 -> "Bits32"
  | Bits64 -> "Bits64"
  | Vec128 -> "Vec128"
  | Vec256 -> "Vec256"
  | Vec512 -> "Vec512"
  | Word -> "Word"
  | Product es ->
    "Product ["
    ^ (String.concat ", "
         (Array.to_list (Array.map mixed_block_element_to_string es)))
    ^ "]"
  | Void -> "Void"

let mixed_block_element_to_lowercase_string = function
  | Value -> "value"
  | Float_boxed -> "float"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | Bits8 -> "bits8"
  | Bits16 -> "bits16"
  | Bits32 -> "bits32"
  | Bits64 -> "bits64"
  | Vec128 -> "vec128"
  | Vec256 -> "vec256"
  | Vec512 -> "vec512"
  | Word -> "word"
  | Product es ->
    "product ["
    ^ (String.concat ", "
         (Array.to_list (Array.map mixed_block_element_to_string es)))
    ^ "]"
  | Void -> "void"

(**** Definitions for backtracking ****)

type change =
    Ctype : type_expr * type_desc -> change
  | Ccompress : type_expr * type_desc * type_desc -> change
  | Clevel : type_expr * int -> change
  | Cscope : type_expr * int -> change
  | Cname :
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option -> change
  | Crow : [`none|`some] row_field_gen ref -> change
  | Ckind : [`var] field_kind_gen -> change
  | Ccommu : [`var] commutable_gen -> change
  | Cuniv : type_expr option ref * type_expr option -> change
  | Cmodes : Mode.changes -> change
  | Csort : Jkind_types.Sort.change -> change
  | Czero_alloc : Zero_alloc.change -> change

type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid

let trail = Local_store.s_table ref Unchanged

let log_change ch =
  let r' = ref Unchanged in
  !trail := Change (ch, r');
  trail := r'

let () =
  Mode.set_append_changes (fun changes -> log_change (Cmodes !changes));
  Jkind_types.Sort.set_change_log (fun change -> log_change (Csort change));
  Zero_alloc.set_change_log (fun change -> log_change (Czero_alloc change))

(* constructor and accessors for [field_kind] *)

type field_kind_view =
    Fprivate
  | Fpublic
  | Fabsent

let rec field_kind_internal_repr : field_kind -> field_kind = function
  | FKvar {field_kind = FKvar _ | FKpublic | FKabsent as fk} ->
      field_kind_internal_repr fk
  | kind -> kind

let field_kind_repr fk =
  match field_kind_internal_repr fk with
  | FKvar _ -> Fprivate
  | FKpublic -> Fpublic
  | FKabsent -> Fabsent

let field_public = FKpublic
let field_absent = FKabsent
let field_private () = FKvar {field_kind=FKprivate}

(* Constructor and accessors for [commutable] *)

let rec is_commu_ok : type a. a commutable_gen -> bool = function
  | Cvar {commu} -> is_commu_ok commu
  | Cunknown -> false
  | Cok -> true

let commu_ok = Cok
let commu_var () = Cvar {commu=Cunknown}

(**** Representative of a type ****)

let rec repr_link (t : type_expr) d : type_expr -> type_expr =
 function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'}
   when field_kind_internal_repr k = FKabsent ->
     repr_link t d' t'
 | t' ->
     log_change (Ccompress (t, t.desc, d));
     t.desc <- d;
     t'

let repr_link1 t = function
   {desc = Tlink t' as d'} ->
     repr_link t d' t'
 | {desc = Tfield (_, k, _, t') as d'}
   when field_kind_internal_repr k = FKabsent ->
     repr_link t d' t'
 | t' -> t'

let repr t =
  match t.desc with
   Tlink t' ->
     repr_link1 t t'
 | Tfield (_, k, _, t') when field_kind_internal_repr k = FKabsent ->
     repr_link1 t t'
 | _ -> t

(* getters for type_expr *)

let get_desc t = (repr t).desc
let get_level t = (repr t).level
let get_scope t = (repr t).scope
let get_id t = (repr t).id

(* transient type_expr *)

module Transient_expr = struct
  let create desc ~level ~scope ~id = {desc; level; scope; id}
  let set_desc ty d = ty.desc <- d
  let set_stub_desc ty d =
    (match ty.desc with
    | Tvar {name = None; _} -> ()
    | _ -> assert false);
    ty.desc <- d
  let set_level ty lv = ty.level <- lv
  let set_scope ty sc = ty.scope <- sc
  let set_var_jkind ty jkind' =
    match ty.desc with
    | Tvar { name; _ } ->
      set_desc ty (Tvar { name; jkind = jkind' })
    | _ -> Misc.fatal_error "set_var_jkind called on non-var"
  let coerce ty = ty
  let repr = repr
  let type_expr ty = ty
end

(* Comparison for [type_expr]; cannot be used for functors *)

let eq_type t1 t2 = t1 == t2 || repr t1 == repr t2
let compare_type t1 t2 = compare (get_id t1) (get_id t2)

(* with-bounds *)

(* Compare types roughly semantically, to allow best-effort deduplication of the types
   inside of with-bounds.

   This function might compare two types as inequal that are actually equal, but should
   /never/ compare two types as equal that are not semantically equal. It may go without
   saying but it also needs to expose a total order.

   Someday, it's probably desirable to merge this, and make it compatible, with
   [Ctype.eqtype], though that seems quite hard.
*)
(* CR layouts v2.8: this will likely loop infinitely on rectypes *)
(* CR layouts v2.8: this whole approach is probably /quite/ wrong, since type_expr is
   fundamentally mutable, and using mutable things in the keys of maps is a recipe for
   disaster. We haven't found a way that this can break /yet/, but it is likely that one
   exists. We should rethink this whole approach soon. *)
let best_effort_compare_type_expr te1 te2 =
  let max_depth = 50 in
  let rank_by_id ty =
    (* This negation is important! We want all these types to compare strictly /less/
       than the structural ones - the easiest way to make that happen is to make the
       id negative, and ensure the ranks of all the other variants are positive *)
    -ty.id
  in
  let rec aux depth te1 te2 =
    if te1 == te2 || repr te1 == repr te2 then 0
    else if depth >= max_depth
    then (rank_by_id te1) - (rank_by_id te2)
    else
      let rank ty =
        match get_desc ty with
        (* Types which must be compared by id *)
        | Tvar _
        | Tunivar _
        | Tobject (_, _)
        | Tfield (_, _, _, _)
        | Tnil
        | Tvariant _
        | Tpackage (_, _)
        | Tarrow (_, _, _, _)
        (* CR layouts v2.8: we can actually see Tsubst here in certain cases, eg during
           [Ctype.copy] when copying the types inside of with_bounds. We also can't
           compare Tsubst structurally, because the Tsubsts that are created in
           Ctype.copy are cyclic (?). So the best we can do here is compare by id.
           this is almost definitely wrong, primarily because of the mutability - we
           should fix that. *)
        | Tsubst (_, _)
          -> rank_by_id ty
        (* Types which we know how to compare structurally*)
        | Ttuple _ -> 2
        | Tunboxed_tuple _ -> 3
        | Tconstr (_, _, _) -> 5
        | Tpoly (_, _) -> 6
        | Tof_kind _ -> 7
        (* Types we should never see *)
        | Tlink _ -> Misc.fatal_error "Tlink encountered in With_bounds_types"
      in
      match get_desc te1, get_desc te2 with
      | Ttuple elts1, Ttuple elts2
      | Tunboxed_tuple elts1, Tunboxed_tuple elts2 ->
        List.compare
          (fun (l1, te1) (l2, te2) ->
             let l = Option.compare String.compare l1 l2 in
             if l = 0 then aux (depth + 1) te1 te2 else l
          )
          elts1
          elts2
      | Tconstr (p1, args1, _), Tconstr (p2, args2, _) ->
        let p = Path.compare p1 p2 in
        if p = 0
        then List.compare (aux (depth + 1)) args1 args2
        else p
      | Tpoly (t1, ts1), Tpoly (t2, ts2) ->
        (* NOTE: this is mostly broken according to the semantics of type_expr, but probably
           fine for the particular "best-effort" comparison we want. *)
        List.compare (aux (depth + 1)) (t1 :: ts1) (t2 :: ts2)
      | _, _ -> rank te1 - rank te2
  in
  aux 0 te1 te2

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
  val map_with_key : (type_expr -> info -> type_expr * info) -> t -> t
  val merge
    : (type_expr -> info option -> info option -> info option) ->
    t -> t -> t
  val update : type_expr -> (info option -> info option) -> t -> t
  val find_opt : type_expr -> t -> info option
  val for_all : (type_expr -> info -> bool) -> t -> bool
  val exists : (type_expr -> info -> bool) -> t -> bool
end = struct
  module M = Map.Make(struct
      (* CR layouts v2.8: A [Map] with mutable values (of which [type_expr] is one) as
         keys is deeply problematic. And in fact we never actually use this map structure
         for anything other than deduplication (indeed we can't, because of its
         best-effort nature). Instead of this structure, we should store the types inside
         of with-bounds as a (morally immutable) array, and write a [deduplicate]
         function, private to [Jkind], which uses this map structure to deduplicate the
         with-bounds, but only during construction and after normalization. *)
      type t = type_expr

      let compare = best_effort_compare_type_expr
    end)
  include M

  type map = With_bounds_type_info.t M.t
  type t = with_bounds_types

  let of_map : map -> with_bounds_types = Obj.magic
  let to_map : with_bounds_types -> map = Obj.magic

  let empty = empty |> of_map
  let is_empty t = t |> to_map |> is_empty
  let to_seq t = t |> to_map |> to_seq
  let of_seq s = of_seq s |> of_map
  let of_list l = l |> List.to_seq |> of_seq
  let singleton ty i = add ty i (to_map empty) |> of_map
  let map f t = map f (to_map t) |> of_map
  let merge f t1 t2 = merge f (to_map t1) (to_map t2) |> of_map
  let update te f t = update te f (to_map t) |> of_map
  let find_opt te t = find_opt te (to_map t)
  let for_all f t = for_all f (to_map t)
  let exists f t = exists f (to_map t)
  let map_with_key f t =
    fold (fun key value acc ->
      let key, value = f key value in
      M.add key value acc) (to_map t) M.empty |> of_map
end

let equal_unsafe_mode_crossing
      ~type_equal
      { unsafe_mod_bounds = mc1; unsafe_with_bounds = wb2 }
      umc2 =
  Misc.Le_result.equal ~le:Mode.Crossing.le mc1 umc2.unsafe_mod_bounds
  && (match wb2, umc2.unsafe_with_bounds with
    | No_with_bounds, No_with_bounds -> true
    | No_with_bounds, With_bounds _ | With_bounds _, No_with_bounds -> false
    | With_bounds wb1, With_bounds wb2 ->
      (* It's tough (impossible?) to do better than a double subset check here because of
         the fact that these maps are best-effort. But in practice these will usually not
         be huge, and the attribute triggering this check is (hopefully) rare. *)
      With_bounds_types.for_all
        (fun ty1 _info ->
           With_bounds_types.exists
             (fun ty2 _info -> type_equal ty1 ty2)
             wb2)
        wb1
      && With_bounds_types.for_all
        (fun ty2 _info ->
           With_bounds_types.exists
             (fun ty1 _info -> type_equal ty1 ty2)
             wb1)
        wb2)

(* Constructor and accessors for [row_desc] *)

let create_row ~fields ~more ~closed ~fixed ~name =
  { row_fields=fields; row_more=more;
    row_closed=closed; row_fixed=fixed; row_name=name }

(* [row_fields] subsumes the original [row_repr] *)
let rec row_fields row =
  match get_desc row.row_more with
  | Tvariant row' ->
    row.row_fields @ row_fields row'
  | _ ->
    row.row_fields

let rec row_repr_no_fields row =
  match get_desc row.row_more with
  | Tvariant row' -> row_repr_no_fields row'
  | _ -> row

let row_more row = (row_repr_no_fields row).row_more
let row_closed row = (row_repr_no_fields row).row_closed
let row_fixed row = (row_repr_no_fields row).row_fixed
let row_name row = (row_repr_no_fields row).row_name

let rec get_row_field tag row =
  let rec find = function
    | (tag',f) :: fields ->
        if tag = tag' then f else find fields
    | [] ->
        match get_desc row.row_more with
        | Tvariant row' -> get_row_field tag row'
        | _ -> RFabsent
  in find row.row_fields

let set_row_name row row_name =
  let row_fields = row_fields row in
  let row = row_repr_no_fields row in
  {row with row_fields; row_name}

type row_desc_repr =
    Row of { fields: (label * row_field) list;
             more:type_expr;
             closed:bool;
             fixed:fixed_explanation option;
             name:(Path.t * type_expr list) option }

let row_repr row =
  let fields = row_fields row in
  let row = row_repr_no_fields row in
  Row { fields;
        more = row.row_more;
        closed = row.row_closed;
        fixed = row.row_fixed;
        name = row.row_name }

type row_field_view =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

let rec row_field_repr_aux tl : row_field -> row_field = function
  | RFeither ({ext = {contents = RFnone}} as r) ->
      RFeither {r with arg_type = tl@r.arg_type}
  | RFeither {arg_type;
              ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      row_field_repr_aux (tl@arg_type) rf
  | RFpresent (Some _) when tl <> [] ->
      RFpresent (Some (List.hd tl))
  | RFpresent _ as rf -> rf
  | RFabsent -> RFabsent

let row_field_repr fi =
  match row_field_repr_aux [] fi with
  | RFeither {no_arg; arg_type; matched} -> Reither (no_arg, arg_type, matched)
  | RFpresent t -> Rpresent t
  | RFabsent -> Rabsent

let rec row_field_ext (fi : row_field) =
  match fi with
  | RFeither {ext = {contents = RFnone} as ext} -> ext
  | RFeither {ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      row_field_ext rf
  | _ -> Misc.fatal_error "Types.row_field_ext "

let rf_present oty = RFpresent oty
let rf_absent = RFabsent
let rf_either ?use_ext_of ~no_arg arg_type ~matched =
  let ext =
    match use_ext_of with
      Some rf -> row_field_ext rf
    | None -> ref RFnone
  in
  RFeither {no_arg; arg_type; matched; ext}

let rf_either_of = function
  | None ->
      RFeither {no_arg=true; arg_type=[]; matched=false; ext=ref RFnone}
  | Some ty ->
      RFeither {no_arg=false; arg_type=[ty]; matched=false; ext=ref RFnone}

let eq_row_field_ext rf1 rf2 =
  row_field_ext rf1 == row_field_ext rf2

let changed_row_field_exts l f =
  let exts = List.map row_field_ext l in
  f ();
  List.exists (fun r -> !r <> RFnone) exts

let match_row_field ~present ~absent ~either (f : row_field) =
  match f with
  | RFabsent -> absent ()
  | RFpresent t -> present t
  | RFeither {no_arg; arg_type; matched; ext} ->
      let e : row_field option =
        match !ext with
        | RFnone -> None
        | RFeither _ | RFpresent _ | RFabsent as e -> Some e
      in
      either no_arg arg_type matched e


(**** Some type creators ****)

let new_id = Local_store.s_ref (-1)

let create_expr = Transient_expr.create

let newty3 ~level ~scope desc  =
  incr new_id;
  create_expr desc ~level ~scope ~id:!new_id

let newty2 ~level desc =
  newty3 ~level ~scope:Ident.lowest_scope desc

                  (**********************************)
                  (*  Utilities for backtracking    *)
                  (**********************************)

let undo_change = function
    Ctype  (ty, desc) -> Transient_expr.set_desc ty desc
  | Ccompress (ty, desc, _) -> Transient_expr.set_desc ty desc
  | Clevel (ty, level) -> Transient_expr.set_level ty level
  | Cscope (ty, scope) -> Transient_expr.set_scope ty scope
  | Cname  (r, v)    -> r := v
  | Crow   r         -> r := RFnone
  | Ckind  (FKvar r) -> r.field_kind <- FKprivate
  | Ccommu (Cvar r)  -> r.commu <- Cunknown
  | Cuniv  (r, v)    -> r := v
  | Cmodes c          -> Mode.undo_changes c
  | Csort change -> Jkind_types.Sort.undo_change change
  | Czero_alloc c -> Zero_alloc.undo_change c

type snapshot = changes ref * int
let last_snapshot = Local_store.s_ref 0

let log_type ty =
  if ty.id <= !last_snapshot then log_change (Ctype (ty, ty.desc))
let link_type ty ty' =
  let ty = repr ty in
  let ty' = repr ty' in
  if ty == ty' then () else begin
  log_type ty;
  let desc = ty.desc in
  Transient_expr.set_desc ty (Tlink ty');
  (* Name is a user-supplied name for this unification variable (obtained
   * through a type annotation for instance). *)
  match desc, ty'.desc with
    Tvar { name }, Tvar { name = name'; jkind = jkind' } ->
      begin match name, name' with
      | Some _, None ->
        log_type ty';
        Transient_expr.set_desc ty' (Tvar { name; jkind = jkind' })
      | None, Some _ -> ()
      | Some _, Some _ ->
        if ty.level < ty'.level then begin
          log_type ty';
          Transient_expr.set_desc ty' (Tvar { name; jkind = jkind' })
        end
      | None, None   -> ()
      end
  | _ -> ()
  end
  (* ; assert (check_memorized_abbrevs ()) *)
  (*  ; check_expans [] ty' *)
(* TODO: consider eliminating set_type_desc, replacing it with link types *)
let set_type_desc ty td =
  let ty = repr ty in
  if td != ty.desc then begin
    log_type ty;
    Transient_expr.set_desc ty td
  end
(* TODO: separate set_level into two specific functions: *)
(*  set_lower_level and set_generic_level *)
let set_level ty level =
  let ty = repr ty in
  if level <> ty.level then begin
    if ty.id <= !last_snapshot then log_change (Clevel (ty, ty.level));
    Transient_expr.set_level ty level
  end
(* TODO: introduce a guard and rename it to set_higher_scope? *)
let set_scope ty scope =
  let ty = repr ty in
  if scope <> ty.scope then begin
    if ty.id <= !last_snapshot then log_change (Cscope (ty, ty.scope));
    Transient_expr.set_scope ty scope
  end
let set_var_jkind ty jkind =
  let ty = repr ty in
  log_type ty;
  Transient_expr.set_var_jkind ty jkind
let set_univar rty ty =
  log_change (Cuniv (rty, !rty)); rty := Some ty
let set_name nm v =
  log_change (Cname (nm, !nm)); nm := v

let rec link_row_field_ext ~(inside : row_field) (v : row_field) =
  match inside with
  | RFeither {ext = {contents = RFnone} as e} ->
      let RFeither _ | RFpresent _ | RFabsent as v = v in
      log_change (Crow e); e := v
  | RFeither {ext = {contents = RFeither _ | RFpresent _ | RFabsent as rf}} ->
      link_row_field_ext ~inside:rf v
  | _ -> invalid_arg "Types.link_row_field_ext"

let rec link_kind ~(inside : field_kind) (k : field_kind) =
  match inside with
  | FKvar ({field_kind = FKprivate} as rk) as inside ->
      (* prevent a loop by normalizing k and comparing it with inside *)
      let FKvar _ | FKpublic | FKabsent as k = field_kind_internal_repr k in
      if k != inside then begin
        log_change (Ckind inside);
        rk.field_kind <- k
      end
  | FKvar {field_kind = FKvar _ | FKpublic | FKabsent as inside} ->
      link_kind ~inside k
  | _ -> invalid_arg "Types.link_kind"

let rec commu_repr : commutable -> commutable = function
  | Cvar {commu = Cvar _ | Cok as commu} -> commu_repr commu
  | c -> c

let rec link_commu ~(inside : commutable) (c : commutable) =
  match inside with
  | Cvar ({commu = Cunknown} as rc) as inside ->
      (* prevent a loop by normalizing c and comparing it with inside *)
      let Cvar _ | Cok as c = commu_repr c in
      if c != inside then begin
        log_change (Ccommu inside);
        rc.commu <- c
      end
  | Cvar {commu = Cvar _ | Cok as inside} ->
      link_commu ~inside c
  | _ -> invalid_arg "Types.link_commu"

let set_commu_ok c = link_commu ~inside:c Cok

let snapshot () =
  let old = !last_snapshot in
  last_snapshot := !new_id;
  (!trail, old)

let rec rev_log accu = function
    Unchanged -> accu
  | Invalid -> assert false
  | Change (ch, next) ->
      let d = !next in
      next := Invalid;
      rev_log (ch::accu) d

let backtrack ~cleanup_abbrev (changes, old) =
  match !changes with
    Unchanged -> last_snapshot := old
  | Invalid -> failwith "Types.backtrack"
  | Change _ as change ->
      cleanup_abbrev ();
      let backlog = rev_log [] change in
      List.iter undo_change backlog;
      changes := Unchanged;
      last_snapshot := old;
      trail := changes

let undo_first_change_after (changes, _) =
  match !changes with
  | Change (ch, _) ->
      undo_change ch
  | _ -> ()

let rec rev_compress_log log r =
  match !r with
    Unchanged | Invalid ->
      log
  | Change (Ccompress _, next) ->
      rev_compress_log (r::log) next
  | Change (_, next) ->
      rev_compress_log log next

let undo_compress (changes, _old) =
  match !changes with
    Unchanged
  | Invalid -> ()
  | Change _ ->
      let log = rev_compress_log [] changes in
      List.iter
        (fun r -> match !r with
          Change (Ccompress (ty, desc, d), next) when ty.desc == d ->
            Transient_expr.set_desc ty desc; r := !next
        | _ -> ())
        log

let functor_param_mode = Mode.Alloc.legacy
let functor_res_mode = Mode.Alloc.legacy
