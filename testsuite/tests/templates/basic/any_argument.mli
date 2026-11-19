(* Not allowed to use as an argument for [Unrepresentable_param]: we need to be
   able to decide this module's representation in order to compile the argument
   block. *)
type t : any

val x : t
val y : int
