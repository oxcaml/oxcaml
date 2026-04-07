(* TEST
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

type t = { flt : float#; t : t }

(* Run Gc.full_major while constructing [t] so that it is promoted
   to the major heap. This ensures that the [caml_modify] run as part
   of [caml_update_dummy] actually does some interesting work.
*)

let rec t =
  { flt = (Gc.full_major (); #0.);
    t;
  };;

let (_ : t) = Sys.opaque_identity t

type v = { void : unit#; i : int }

let rec v =
  { void = (Gc.full_major (); #());
    i = 0xdeadbeef }

let (_ : int) = Sys.opaque_identity v.i;
;;
