module Jkind = Btype.Jkind0

(* CR-soon liam923: The [modes] and [modalities] types should be defined in
   [Typedtree], but can't be due to a dependency cycle. *)

type 'a modes =
  { mode_modes : 'a;
    mode_desc : Mode.Alloc.atom Location.loc list
  }

type modalities =
  { moda_modalities : Mode.Modality.Const.t;
    moda_desc : Mode.Modality.atom Location.loc list
  }

(** Interpret mode syntax as mode annotation, where axes can be left unspecified
*)
val transl_mode_annots : Parsetree.modes -> Mode.Alloc.Const.Option.t modes

val untransl_mode : _ modes -> Parsetree.modes

(** Interpret mode syntax as alloc mode (on arrow types), where axes are set to
    legacy if unspecified *)
val transl_alloc_mode : Parsetree.modes -> Mode.Alloc.Const.t modes

(** Interpret mode syntax as modalities. Modalities occuring at different places
    requires different levels of maturity. Also takes the mutability and
    attributes on the field and insert mutable-implied modalities accordingly.
*)
val transl_modalities :
  maturity:Language_extension.maturity ->
  Types.mutability ->
  Parsetree.modalities ->
  modalities

val least_modalities_implying :
  Types.mutability -> Mode.Modality.Const.t -> Mode.Modality.atom list

val sort_dedup_modalities : Mode.Modality.atom list -> Mode.Modality.atom list

(** Get the default modalities implied by the mutability of a field. *)
val mutable_modalities : Types.mutability -> Mode.Modality.Const.t

(** Similar to [transl_modalities] but takes an explicit [default] modality
    instead of computing it from mutability. Used when merging explicit
    modalities with existing signature default modalities. *)
val transl_modalities_with_default :
  maturity:Language_extension.maturity ->
  default:Mode.Modality.Const.t ->
  Parsetree.modalities ->
  modalities

val let_mutable_modalities : Mode.Modality.Const.t

(** The (default) modalities for an atomic mutable field *)
val atomic_mutable_modalities : Mode.Modality.Const.t

val untransl_modality :
  Mode.Modality.atom Location.loc -> Parsetree.modality Location.loc

val untransl_modalities : modalities -> Parsetree.modalities

(** Interpret a mod-bounds. *)
val transl_mod_bounds : Parsetree.modes -> Jkind.Mod_bounds.t

(** Translate an algebraic representation of mod bounds into user syntax. If
    [verbose] is true, redundant annotations are included. *)
val untransl_mod_bounds : ?verbose:bool -> Jkind.Mod_bounds.t -> Parsetree.modes

val idx_expected_modalities : mut:bool -> Mode.Modality.Const.t
