[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(** Post-dominance on a finished SSA graph. *)

(* CR-someday xclerc for xclerc: consider whether post-dominance should be part
   of the graph metadata. *)

open Ssa.Export

type t

val compute : finished Ssa.graph -> t

(** [post_dominates t a b] is [true] iff [a] post-dominates [b]. Reflexive. *)
val post_dominates : t -> finished Block.t -> finished Block.t -> bool
