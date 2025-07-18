(* TEST
 flags = "-extension-universe alpha";

 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Stdlib.Or_null.?' is incompatible with vanilla ? *)
module M = struct
  let f ?x () = match x with None -> 0 | Some v -> v
end

(* This should fail - trying to call vanilla ? function with
   Stdlib.Or_null.?' *)
let g x = M.f Stdlib.Or_null.?'x ()
