type t = string

(* Treats its argument as a heap-allocated string (reads the block). *)
let use (x : t) = print_string x
