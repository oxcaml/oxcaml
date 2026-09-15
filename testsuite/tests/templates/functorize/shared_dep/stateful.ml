(* Parameters: P *)

let counter = ref 0
let inc_count () = incr counter
let get_count () = !counter
