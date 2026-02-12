(* TEST_BELOW *)

<<<<<<< oxcaml
(* CR-someday mslater: this should also work on arm once atomics are builtins *)

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
||||||| upstream-base
=======
(* standard atomics *)

let standard_atomic_get (r : 'a Atomic.t) =
  Atomic.get r

let standard_atomic_cas (r : 'a Atomic.t) oldv newv =
  Atomic.compare_and_set r oldv newv


(* atomic record fields *)

type 'a atomic = { filler : unit; mutable x : 'a [@atomic] }

let get (r : 'a atomic) : 'a =
  r.x

let set (r : 'a atomic) v =
  r.x <- v

let cas (r : 'a atomic) oldv newv =
  Atomic.Loc.compare_and_set [%atomic.loc r.x] oldv newv

(* TEST

  (* we restrict this test to a single configuration,
       amd64+linux no-tsan no-flambda
     to avoid dealing with differences in cmm output across systems
     (the check is known to fail under MSCV, which uses a different
     symbol generator.)
   *)
   arch_amd64;
   linux;
   no-flambda; (* the output will be slightly different under Flambda *)
   no-tsan; (* TSan modifies the generated code *)

   setup-ocamlopt.byte-build-env;
   flags = "-c -dcmm -dno-locations -dno-unique-ids";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
>>>>>>> upstream-incoming
*)
