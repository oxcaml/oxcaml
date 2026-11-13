(* TEST
 flambda2;
 readonly_files = "env_types_lib.mli env_types_lib.ml";
 setup-ocamlopt.byte-build-env;
 (* [-nocwd] is needed because [-Ix .] doesn't override the implicit [-I .]. *)
 flags = "-extension layout_poly_alpha -nocwd -Ix .";
 ocamlopt_flags = "-O3";
 module = "env_types_lib.mli";
 ocamlopt.byte;
 module = "env_types_lib.ml";
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
 module = "env_types_use.ml";
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
 unset module;
 program = "${test_build_directory}/env_types_use.exe";
 all_modules = "env_types_lib.cmx env_types_use.cmx";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* The code of an instantiation of a template is simplified using the type
   that the environment of the template has at its definition. In
   [env_types_lib.ml], the code of [F]'s instantiation is simplified just after
   [F]'s definition, and calls [f] directly; the code of [h]'s instantiation in
   it is simplified just after [h]'s definition, inside [F]'s instantiation.
   Here, the code of the instantiations of [Env_types_lib]'s templates uses the
   types that [Env_types_lib] exports: in [top], [f] is called directly without
   reading it from the environment, and in [h], the code of [g] is called
   directly although each application of [F] has its own [g] closure. The code
   of [h] is shared by [M1] and [M2]. *)

let[@inline never] use_top x = Env_types_lib.top x

let[@inline never] use_h1 x = Env_types_lib.M1.h x

let[@inline never] use_h2 x = Env_types_lib.M2.h x

let[@inline never] print_ints ints =
  print_endline (String.concat " " (List.map Int.to_string ints))

let () =
  let #(a, b) = use_top 5 in
  print_ints [a; b];
  let #(a, b, c) = use_h1 7 in
  print_ints [a; b; c];
  let #(a, b, c) = use_h2 8 in
  print_ints [a; b; c]
