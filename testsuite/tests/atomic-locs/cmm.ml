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

(* check immediates too *)

let get_imm (r : int atomic) : int =
  r.x

let set_imm (r : int atomic) v =
  r.x <- v

(* TEST
   arch_amd64;
   flambda;
   no-tsan;
   (* frame_pointers causes different, unstable CMM output, so we skip this test
      when it's enabled *)
   no-frame_pointers;

   flags = "-c -dcmm -dno-locations -dno-unique-ids";

   {
    setup-ocamlopt.byte-build-env;
    ocamlopt.byte;
    check-ocamlopt.byte-output;
   }
   {
    setup-ocamlopt.byte-build-env;
    flags += " -O3";
    ocamlopt.byte;
    check-ocamlopt.byte-output;
   }
*)
