val remove_migration_attributes_from_str :
  Parsetree.structure -> Parsetree.structure
(** [remove_migration_attributes_from_str str] removes any left-over metadata
    that be in the parsetree after a migration. Most users will not have to
    worry about applying this function.*)

val remove_migration_attributes_from_sig :
  Parsetree.signature -> Parsetree.signature
(** The same as {! remove_migration_attributes_from_str} except for
    {! Parsetree.signature}. *)
