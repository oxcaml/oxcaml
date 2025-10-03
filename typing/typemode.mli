(* CR zeisbach: with the addition of crossings in modes, maybe it is cleaner if
   this and the other (un)transl were to take in core_modes/modalities *)
(** Interpret mode syntax as mode annotation, where axes can be left unspecified *)
val transl_mode_annots : Parsetree.modes -> Mode.Alloc.Const.Option.t

val untransl_mode_annots : Mode.Alloc.Const.Option.t -> Parsetree.modes

(** Interpret mode syntax as alloc mode (on arrow types), where axes are set to
    legacy if unspecified *)
val transl_alloc_mode : Parsetree.modes -> Mode.Alloc.Const.t

(** Interpret mode syntax as modalities. Modalities occuring at different places
    requires different levels of maturity. Also takes the mutability and
    attributes on the field and insert mutable-implied modalities accordingly.
    *)
val transl_modalities :
  maturity:Language_extension.maturity ->
  Types.mutability ->
  Parsetree.modalities ->
  Mode.Modality.Const.t

val let_mutable_modalities : Mode.Modality.Const.t

(** The (default) modalities for an atomic mutable field *)
val atomic_mutable_modalities : Mode.Modality.Const.t

val untransl_modality : Mode.Modality.atom -> Parsetree.modality Location.loc

(** Un-interpret modalities back to parsetree. Takes the mutability and
    attributes on the field and remove mutable-implied modalities accordingly.
    *)
val untransl_modalities :
  Types.mutability -> Mode.Modality.Const.t -> Parsetree.modalities

(** Interpret a mod-bounds. *)
val transl_mod_bounds : Parsetree.crossings -> Types.Jkind_mod_bounds.t

val idx_expected_modalities : mut:bool -> Mode.Modality.Const.t
