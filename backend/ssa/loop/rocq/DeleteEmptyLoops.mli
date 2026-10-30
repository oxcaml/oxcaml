open DeadInductionVar
open InductionVar
open LoopTermination
open NaturalLoop
open SsaModel

val graph_supported : block list -> bool

val body_supported : natural_loop -> bool

val single_exit : natural_loop -> block -> bool

val loop_terminates : op_def -> biv list -> bool

val find_deletions : op_def -> block list -> (block * block) list
