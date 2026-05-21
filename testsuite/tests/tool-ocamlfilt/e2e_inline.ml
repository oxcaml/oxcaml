(* TEST
 native-compiler;
 readonly_files = "e2e_inline_lib.ml e2e_inline_mid.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-name-mangling-scheme structured -O3 -c";
 module = "e2e_inline_lib.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 module = "e2e_inline_mid.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 module = "";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 output = "e2e_inline.table";
 script = "sh ${test_source_directory}/e2e_table.sh \
           ${test_build_directory}/e2e_inline.o \
           'camlStdlib|camlCamlinternal'";
 script;
 reference = "${test_source_directory}/e2e_inline.reference";
 check-program-output;
*)

(* Exercise -O3 cross-module inlining. Every callee in
   [E2e_inline_lib] is [@inline always], so the wrapper bodies
   collapse into the caller. The anonymous closures *inside* the
   wrappers (the [List.map] / [List.fold_left] arguments,
   [mk_adder]'s returned closure, every layer of [chain4]) are
   [@inline never] so they must survive as their own symbols
   post-inlining. Those surviving closures are what produces the
   structured-mangling [I] (inlining) path items here, e.g.
   [E2e_inline.<inlining>.E2e_inline_lib.chain4.fn_N_N_code]. *)

let inline_squares x y =
  E2e_inline_lib.square x + E2e_inline_lib.square y

let chain_three a b c =
  E2e_inline_lib.add (E2e_inline_lib.add a b) c

let composed x =
  E2e_inline_lib.compose
    E2e_inline_lib.square E2e_inline_lib.square x

let use_mk_adder x =
  let f = E2e_inline_lib.mk_adder x in
  f 10 + f 20

let use_thrice x =
  E2e_inline_lib.apply_thrice (fun[@inline never] y -> y + 1) x

(* Cross-module inlining through a real dependency chain:
   [Deeper.sum_squares] -> [add_pair] -> [triple] -> List.map
   closure. Each link is [@inline always], so -O3 has to compose
   all three sites plus the stdlib HOFs into one body. *)
let use_inner xs =
  E2e_inline_lib.Inner.scale 3 (List.hd xs)
  + E2e_inline_lib.Inner.add_pair xs
  + E2e_inline_lib.Inner.Deeper.sum_squares xs

(* Library-style wrapper with a recursive helper inside. The
   wrapper inlines away; the inner [rec loop] cannot be inlined
   and surfaces as its own symbol. *)
let use_iterate x =
  E2e_inline_lib.iterate_n (fun[@inline never] a -> a + 1) x 10

(* Walk the curried-closure chain layer by layer. Whether each
   layer survives as a distinct symbol depends on -O3 inlining
   decisions; either outcome is informative for the reference. *)
let use_chain a b c d =
  let f = E2e_inline_lib.chain4 a in
  let g = f b in
  let h = g c in
  h d

(* Intra-module inlining for comparison. *)
let local_inline x =
  let[@inline always] bump y = y * y + 1 in
  bump x + bump (x + 1)

(* Functor-defined function inlined cross-module: the surviving
   [scale_all] closure mangles with both the functor path and the
   [<inlining>] marker, as
   [E2e_inline.<inlining>.E2e_inline_lib.Scaler.scale_all.fn]. *)
module Scaler3 = E2e_inline_lib.Scaler (struct
  let factor = 3
end)

let use_functor xs = Scaler3.scale_all xs

(* First-class module produced by an [@inline always] lib function:
   the module body inlines in and its [@inline never] method survives
   as [E2e_inline.<inlining>.E2e_inline_lib.pack_op.apply]. *)
let use_first_class x =
  let (module Op) = E2e_inline_lib.pack_op 5 in
  Op.apply x

(* Transitive inlining through [E2e_inline_mid]: the lib closure is
   attributed straight to [E2e_inline_lib]
   ([E2e_inline.<inlining>.E2e_inline_lib.relayed.fn]); the
   [E2e_inline_mid] hop never appears, so an inlining chain of length
   two is indistinguishable from a direct one. *)
let use_transitive x = E2e_inline_mid.relay x 100

let () =
  ignore (inline_squares 3 4);
  ignore (chain_three 1 2 3);
  ignore (composed 5);
  ignore (use_mk_adder 6);
  ignore (use_thrice 7);
  ignore (use_inner [8; 9; 10]);
  ignore (use_iterate 11);
  ignore (use_chain 1 2 3 4);
  ignore (local_inline 12);
  ignore (use_functor [1; 2; 3]);
  ignore (use_first_class 9);
  ignore (use_transitive 4)
