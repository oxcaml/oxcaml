(* TEST_BELOW *)

(* standard atomics *)

let standard_atomic_get (r : 'a Atomic.t) =
  Atomic.get r

let standard_atomic_get (r : 'a Atomic.t) v =
  Atomic.set r v

(* atomic record fields *)

type 'a atomic = { filler : unit; mutable x : 'a [@atomic] }

let get (r : 'a atomic) : 'a =
  r.x

let set (r : 'a atomic) v =
  r.x <- v

(* TEST
   arch_amd64;
   flambda;
   no-tsan;

   setup-ocamlopt.byte-build-env;
   flags = "-c -dcmm -dno-locations -dno-unique-ids";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
*)
