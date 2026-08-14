(* Refined declarations that cross a .cmi boundary: written here, read back
   and printed by the test. *)

type nat = int{ _ >= 0 }

type dep = x:int{ x > 0 } -> int{ _ >= x }

val sub : s:string -> int{ _ < String.length s } -> char

val labelled : ~x:int{ x > 0 } -> unit

type wf = { size : int{ _ >= 0 } }

type pos = Pos of int{ _ > 0 }
