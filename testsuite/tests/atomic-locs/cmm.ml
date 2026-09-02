(* TEST_BELOW *)

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

(* acquire loads and release stores: plain loads and stores on amd64, with
   [caml_modify] providing the write barrier for pointers *)

external get_acquire : 'a Atomic.t -> 'a = "%atomic_load_acquire"
external set_release : 'a Atomic.t -> 'a -> unit = "%atomic_store_release"
external loc_get_acquire : 'a Atomic.Loc.t @ local -> 'a
  = "%atomic_load_acquire_loc"
external loc_set_release : 'a Atomic.Loc.t @ local -> 'a -> unit
  = "%atomic_store_release_loc"

let standard_atomic_get_acquire (r : 'a Atomic.t) =
  get_acquire r

let standard_atomic_set_release (r : 'a Atomic.t) v =
  set_release r v

let standard_atomic_set_release_imm (r : int Atomic.t) v =
  set_release r v

let get_acquire (r : 'a atomic) : 'a =
  loc_get_acquire [%atomic.loc r.x]

let set_release (r : 'a atomic) v =
  loc_set_release [%atomic.loc r.x] v

let get_acquire_imm (r : int atomic) : int =
  loc_get_acquire [%atomic.loc r.x]

let set_release_imm (r : int atomic) v =
  loc_set_release [%atomic.loc r.x] v

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
