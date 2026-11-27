(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-save-ir-before register_allocation -save-ir-after linearization";
 compile_only = "true";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 src = "save_ir_before_regalloc_start_from_emit.cmir-cfg-regalloc";
 dst = "expected.cmir-cfg-regalloc";
 copy;
 script = "touch empty.ml";
 script;
 flags = "-save-ir-before register_allocation \
          save_ir_before_regalloc_start_from_emit.cmir-linear";
 module = "empty.ml";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "cmp save_ir_before_regalloc_start_from_emit.cmir-cfg-regalloc \
           expected.cmir-cfg-regalloc";
 script;
*)

(* Compiling the saved Linear IR of a unit runs no register allocation and
   must leave alone the IR saved before register allocation. *)

let[@inline never] unit_function x = x + 1
