(* TEST
 modules = "encode_identity_stubs.c";
 native;
*)

(* Check that the compiler and the runtime produce bit-identical
   reserved header fields for mixed blocks (the runtime's ephemeron
   copy and [caml_update_dummy] assert this equality). *)

external check : 'a -> int -> bool = "encode_identity_check"

type r0 = { f : float# }

type r1 = { a : int; f : float# }

type r3 = { a : int; b : int; c : int; f : float#; g : float# }

type r15 =
  { a1 : int; a2 : int; a3 : int; a4 : int; a5 : int;
    a6 : int; a7 : int; a8 : int; a9 : int; a10 : int;
    a11 : int; a12 : int; a13 : int; a14 : int; a15 : int;
    f : float#; g : float#; h : float# }

let[@inline never] mk_r0 f = { f }
let[@inline never] mk_r1 a f = { a; f }
let[@inline never] mk_r3 a f = { a; b = a + 1; c = a + 2; f; g = f }
let[@inline never] mk_r15 a f =
  { a1 = a; a2 = a; a3 = a; a4 = a; a5 = a;
    a6 = a; a7 = a; a8 = a; a9 = a; a10 = a;
    a11 = a; a12 = a; a13 = a; a14 = a; a15 = a;
    f; g = f; h = f }

let () =
  let a = Sys.opaque_identity 42 in
  let f = Sys.opaque_identity #1.5 in
  assert (check (mk_r0 f) 0);
  assert (check (mk_r1 a f) 1);
  assert (check (mk_r3 a f) 3);
  assert (check (mk_r15 a f) 15);
  print_endline "OK"
