(* TEST
 modules = "classic_mode_dep.ml";
 flambda2;
 {
   native;
 }{
   jsir;
 }{
   ocamlopt_flags = "-Oclassic";
   jsir;
 }
*)

(* The JSIR translation does not support classic mode, so [-Oclassic] and
   [[@@@flambda_oclassic]] (see [classic_mode_dep.ml]) are ignored when
   targeting JSIR. Checks that units asking for classic mode compile and run,
   in particular the constructs that classic mode treats differently: direct
   calls to the functions of other units, the unit results of primitives, and
   functions using toplevel values that are computed at runtime. *)

exception Local

let triple = Sys.opaque_identity 1, 2, 3

let[@inline never] set_field r v = r := v

let[@inline never] set_element a i v = a.(i) <- v

let[@inline never] raise_local () = raise Local

let[@inline never] first () =
  let x, _, _ = triple in
  x

let () =
  Classic_mode_dep.incr_counter ();
  assert (!Classic_mode_dep.counter = 1);
  assert (Classic_mode_dep.add 1 2 = 3);
  assert (List.length [1; 2] = 2)

let () =
  let r = ref 0 in
  assert (Sys.opaque_identity (set_field r 1) = ());
  assert (!r = 1);
  let a = [| 0 |] in
  assert (Sys.opaque_identity (set_element a 0 1) = ());
  assert (a.(0) = 1)

let () =
  assert (first () = 1);
  assert (Classic_mode_dep.sum_pair () = 3);
  (match raise_local () with exception Local -> () | () -> assert false);
  match (Classic_mode_dep.raise_error 4 : unit) with
  | exception Classic_mode_dep.Error 4 -> ()
  | exception _ | () -> assert false
