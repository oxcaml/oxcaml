[@@@ocaml.warning "+a-40-41-42"]

(** Run the peephole optimizer over a buffered function body, walking forward
    through the list of assembly lines. *)
val optimize : Peephole_utils.asm_line Doubly_linked_list.t -> unit
