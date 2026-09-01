(** Clean the AST.

    This module provides functions to help clean the AST *)

val remove_migration_attributes : Ast_traverse.map
(** A map that removes any leftover migration metadata in the AST. *)
