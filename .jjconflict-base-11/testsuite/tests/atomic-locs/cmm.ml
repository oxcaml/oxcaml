(* TEST_BELOW *)

open Stdlib_stable

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

let cas (r : 'a atomic) oldv newv =
  Atomic.Loc.compare_and_set [%atomic.loc r.x] oldv newv

(* atomic block-index operations *)

let idx_get (r : int atomic) (idx : (int atomic, int) idx_atomic) : int =
  Idx_atomic.get r idx

let idx_set (r : int atomic) (idx : (int atomic, int) idx_atomic) value =
  Idx_atomic.set r idx value

let idx_exchange (r : int atomic) (idx : (int atomic, int) idx_atomic) value =
  Idx_atomic.exchange r idx value

let idx_compare_and_set (r : int atomic)
    (idx : (int atomic, int) idx_atomic) old_value new_value =
  Idx_atomic.compare_and_set r idx old_value new_value

let idx_compare_exchange (r : int atomic)
    (idx : (int atomic, int) idx_atomic) old_value new_value =
  Idx_atomic.compare_exchange r idx old_value new_value

let idx_fetch_and_add (r : int atomic)
    (idx : (int atomic, int) idx_atomic) value =
  Idx_atomic.fetch_and_add r idx value

(* TEST
   include stdlib_stable;
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
