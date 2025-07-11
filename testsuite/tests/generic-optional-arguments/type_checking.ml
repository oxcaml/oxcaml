(* TEST
 flags = "-extension-universe alpha";
*)

(* Test case to trigger Generic_optional code path in filter_arrow *)
let test_fn Stdlib.Or_null.?'(value : int = 42) () = value

let () = Printf.printf "%d\n" (test_fn ())
