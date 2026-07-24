let[@inline] triple x = x * 3

(* Returns a closure capturing [n]; exercises value-slot export across the
   pack. *)
let[@inline] make_adder n = (fun x -> x + n) [@inline]
