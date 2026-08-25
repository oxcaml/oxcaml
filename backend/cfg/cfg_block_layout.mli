[@@@ocaml.warning "+a-40-41-42"]

(* Static block-layout pass: permutes the layout (and only the layout) so that
   cold blocks are moved to the end of the function and likely successors are
   placed immediately after their predecessors, turning branches into
   fallthroughs at linearization. Runs right before linearization; enabled by
   [-cfg-block-layout]. *)

val run : Cfg_with_layout.t -> unit
