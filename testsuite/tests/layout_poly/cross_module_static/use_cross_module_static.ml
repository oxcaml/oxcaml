(* TEST
 readonly_files = "cross_module_static_lib.mli cross_module_static_lib.ml \
                   cross_module_static_relay.mli cross_module_static_relay.ml";
 setup-ocamlopt.byte-build-env;
 (* [-nocwd] is needed because [-Ix .] doesn't override the implicit [-I .]. *)
 flags = "-extension layout_poly_alpha -nocwd -Ix .";
 module = "cross_module_static_lib.mli";
 ocamlopt.byte;
 module = "cross_module_static_lib.ml";
 ocamlopt.byte;
 module = "cross_module_static_relay.mli";
 ocamlopt.byte;
 module = "cross_module_static_relay.ml";
 ocamlopt.byte;
 module = "use_cross_module_static.ml";
 ocamlopt.byte;
 unset module;
 {
  program = "${test_build_directory}/use_cross_module_static.exe";
  all_modules = "cross_module_static_lib.cmx cross_module_static_relay.cmx \
                 use_cross_module_static.cmx";
  ocamlopt.byte;
  run;
  check-program-output;
 }{
  program = "-no-code -no-approx cross_module_static_lib.cmx";
  output = "cross_module_static_lib.objinfo.output";
  reference = "${test_source_directory}/cross_module_static_lib.objinfo.reference";
  ocamlobjinfo;
  check-program-output;
 }{
  program = "-no-code -no-approx cross_module_static_relay.cmx";
  output = "cross_module_static_relay.objinfo.output";
  reference = "${test_source_directory}/cross_module_static_relay.objinfo.reference";
  ocamlobjinfo;
  check-program-output;
 }
*)

(* Instantiating a layout-polymorphic value defined in another compilation unit
   requires reading that unit's static data out of its cmx. *)

external to_float : float# -> float = "%box_float"
external to_int64 : int64_u -> int64 = "%box_int64"
external to_nativeint : nativeint_u -> nativeint = "%box_nativeint"

module Lib = Cross_module_static_lib

(* Instantiation at [value] and at several unboxed layouts. *)
let () =
  Printf.printf "id: %d %s %.1f %Ld %nd\n" (Lib.id 42) (Lib.id "forty-two")
    (to_float (Lib.id #42.0))
    (to_int64 (Lib.id #42L))
    (to_nativeint (Lib.id #42n))

(* Instantiating the same value repeatedly, both at a repeated layout and at a
   fresh one: the unit's static data is read once and reused. *)
let () =
  Printf.printf "id again: %d %d %.1f\n" (Lib.id 1) (Lib.id 2)
    (to_float (Lib.id #3.0))

(* Instantiation with two independent layout variables, returning an unboxed
   product. *)
let () =
  let #(a, b) = Lib.pair "left" #7L in
  let #(c, d) = Lib.pair #8.0 #9n in
  Printf.printf "pair: %s %Ld %.1f %nd\n" a (to_int64 b) (to_float c)
    (to_nativeint d)

(* The instantiations share the single [ref] allocated by [Lib], rather than
   each getting a copy of it. *)
let () =
  let _ = Lib.counted_id 1 in
  let _ = Lib.counted_id #2.0 in
  let _ = Lib.counted_id #3L in
  Printf.printf "calls: %d\n" !Lib.calls

(* Chained: [relay_id]'s static data itself refers to another unit. *)
let () =
  Printf.printf "relay: %d %.1f\n"
    (Cross_module_static_relay.relay_id 5)
    (to_float (Cross_module_static_relay.relay_id #6.0))

(* An imported static functor returns a template capturing both its dynamic
   argument and a value computed by the functor body. The all-dynamic arguments
   share a static specialization, not their runtime values or effects. *)
let () =
  let module A = struct
    let offset = Sys.opaque_identity 10
    let state = ref 0
  end in
  let module B = struct
    let offset = Sys.opaque_identity 20
    let state = ref 100
  end in
  let module R1 = Lib.Capture (A) in
  let module R2 = Lib.Capture (B) in
  let #(a, i) = R1.capture 7 in
  let #(b, j) = R2.capture 8 in
  let #(c, f) = R1.capture #3.5 in
  let #(d, g) = R2.capture #4.5 in
  Printf.printf "capture: %d/%d %d/%d %d/%.1f %d/%.1f\n"
    a i b j c (to_float f) d (to_float g);
  Printf.printf "capture effects: %d %d %d\n" !Lib.calls !A.state !B.state
