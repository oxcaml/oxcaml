module Jkind = Btype.Jkind0

type mode_annot = Mode.Alloc.atom Location.loc

type modes_annot = mode_annot list

type modality_annot = Mode.Modality.atom Location.loc

type modalities_annot = modality_annot list

(** Interpret mode syntax as mode annotation, where axes can be left unspecified
*)
val transl_mode_annots :
  Parsetree.modes -> Mode.Alloc.Const.Option.t * modes_annot

val untransl_mode : Mode.Alloc.Const.Option.t -> Parsetree.modes

val untransl_mode_annots : modes_annot -> Parsetree.modes

(** Interpret mode syntax as alloc mode (on arrow types), where axes are set to
    legacy if unspecified *)
val transl_alloc_mode : Parsetree.modes -> Mode.Alloc.Const.t * modes_annot

(** Interpret mode syntax as modalities. Modalities occuring at different places
    requires different levels of maturity. Also takes the mutability and
    attributes on the field and insert mutable-implied modalities accordingly.
*)
val transl_modalities :
  maturity:Language_extension.maturity ->
  Types.mutability ->
  Parsetree.modalities ->
  Mode.Modality.Const.t * modalities_annot

val let_mutable_modalities : Mode.Modality.Const.t

(** The (default) modalities for an atomic mutable field *)
val atomic_mutable_modalities : Mode.Modality.Const.t

val untransl_modality : Mode.Modality.atom -> Parsetree.modality Location.loc

val untransl_modality_annot : modality_annot -> Parsetree.modality Location.loc

(** Un-interpret modalities back to parsetree. Takes the mutability and
    attributes on the field and remove mutable-implied modalities accordingly.
*)
val untransl_modalities :
  Types.mutability -> Mode.Modality.Const.t -> Parsetree.modalities

val untransl_modalities_annot : modalities_annot -> Parsetree.modalities

(** Interpret a mod-bounds. *)
val transl_mod_bounds : Parsetree.modes -> Jkind.Mod_bounds.t

val untransl_mod_bounds : Jkind.Mod_bounds.t -> Parsetree.modes

val idx_expected_modalities : mut:bool -> Mode.Modality.Const.t

val mode_annot_to_modality_annot : mode_annot -> modality_annot
