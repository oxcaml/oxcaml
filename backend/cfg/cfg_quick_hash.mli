[@@@ocaml.warning "+a-30-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

(* CR-soon xclerc for xclerc: some work may be needed on the hash functions, as
   the performance of some passes (e.g. `Cfg_merg_blocks`) seems to be quite
   sensitive to it. *)

(* Note: the functions below are hashing a number of mutable elements; the user
   have to be sure mutations only happen when the hash value is no longer
   used. *)

(* Note: the functions below are probably "low quality" as they have to ignore
   nested values that some passes may want to substitute. For instance, labels
   and function names are ignored so that the functions can be used to get the
   same hash value for elements equivalent up to a substitution. *)

val operation : Operation.t -> int

val basic : Cfg.basic -> int

val terminator : Cfg.terminator -> int

val basic_instruction_list : Cfg.basic Cfg.instruction DLL.t -> int

val basic_block : Cfg.basic_block -> int
