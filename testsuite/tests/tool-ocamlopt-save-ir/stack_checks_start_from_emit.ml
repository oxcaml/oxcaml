(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-no-cfg-stack-checks -save-ir-after linearization -S";
 module = "stack_checks_start_from_emit.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "mv stack_checks_start_from_emit.${asmext} from_source.${asmext}";
 script;
 script = "touch empty.ml";
 script;
 flags = "-no-cfg-stack-checks -S stack_checks_start_from_emit.cmir-linear";
 module = "empty.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "sh ${test_source_directory}/stack_checks_start_from_emit.sh";
 script;
*)

(* With [-no-cfg-stack-checks], stack checks (or, when they are disabled, the
   probes of large frames) are inserted in the Linear IR. The IR saved after
   linearization must contain them, since compiling it only emits it: the
   script compares the instructions emitted from this file with those
   emitted from its saved IR. [f] has a large frame: its callee returns most
   of a large unboxed product on the stack. *)

type t2 = #(float# * float#)
type t4 = #(t2 * t2)
type t8 = #(t4 * t4)
type t16 = #(t8 * t8)
type t32 = #(t16 * t16)
type t64 = #(t32 * t32)
type t128 = #(t64 * t64)
type t256 = #(t128 * t128)
type t512 = #(t256 * t256)
type t1025 = #(float# * #(t512 * t512))

let[@inline never] g (x : float#) : t1025 =
  let p2 = #(x, x) in
  let p4 = #(p2, p2) in
  let p8 = #(p4, p4) in
  let p16 = #(p8, p8) in
  let p32 = #(p16, p16) in
  let p64 = #(p32, p32) in
  let p128 = #(p64, p64) in
  let p256 = #(p128, p128) in
  let p512 = #(p256, p256) in
  #(x, #(p512, p512))

let[@inline never] f (x : float#) : float# =
  let #(y, _) = g x in
  y
