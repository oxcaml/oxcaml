[@@@ocaml.warning "+a-40-41-42"]

(** The ext-TSP block layout (Newell and Pupyrev, as in BOLT and LLVM's
    CodeLayout): [layout ~blocks ~edges] orders the blocks, given for each its
    label, estimated size in bytes and execution count, the entry block first,
    and the weighted edges between them. See the implementation for the
    algorithm and its parameters. *)
val layout :
  blocks:(Label.t * int * int64) array ->
  edges:(Label.t * Label.t * int64) list ->
  Label.t list
