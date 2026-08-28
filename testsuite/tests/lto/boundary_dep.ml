exception Custom of int

let[@inline never] raise_custom n = raise (Custom n)

(* A pair of closures projected out and called by the main module. *)
let make_adder n x = x + n

let adder_pair = make_adder 10, make_adder 20

(* A dead export whose module block field should be poisoned while the block
   itself stays alive as a GC root. *)
let unused_strings = List.init 1000 string_of_int

let used_list = [1; 2; 3]
