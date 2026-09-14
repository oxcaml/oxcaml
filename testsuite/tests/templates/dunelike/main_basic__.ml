(* Parameters: (none) *)

module Basic_int = Basic(P)(P_int) [@jane.non_erasable.instances]
module Basic_string = Basic(P)(P_string) [@jane.non_erasable.instances]

module Main_basic = Main_basic
