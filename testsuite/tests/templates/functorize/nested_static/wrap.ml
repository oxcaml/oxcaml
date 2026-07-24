(* Parameters: Q *)

module Bar_of_p_int = Bar(P)(P_int) [@jane.non_erasable.instances]

let foo_count () = Bar_of_p_int.foo_count ()
let foo_bump () = Bar_of_p_int.foo_bump ()
