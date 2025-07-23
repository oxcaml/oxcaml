(* TEST
 flags = "-extension-universe alpha";

 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Function signature mismatch - different generic optional modules *)
module M = struct
  let f Stdlib.Option.?'(x : int = 42) () = x
end

(* This should fail - trying to call with Stdlib.Or_null instead of
   Stdlib.Option *)
let g x = M.f Stdlib.Or_null.?'x ()
