(* Static ([f], [c]) and dynamic ([n], [r]) fields, used whole by
   [whole_block.ml]. *)

let f x = x * 3
let c = "constant"
let n = Sys.opaque_identity 14
let r = ref 1
let () = incr r
