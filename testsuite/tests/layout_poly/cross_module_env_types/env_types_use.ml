(* TEST
 flambda2;
 readonly_files = "env_types_lib.mli env_types_lib.ml";
 setup-ocamlopt.byte-build-env;
 (* The runs choose their own optimisation levels and reaper settings. *)
 unset OCAMLPARAM;
 (* [-nocwd] is needed because [-Ix .] doesn't override the implicit [-I .]. *)
 flags = "-extension layout_poly_alpha -nocwd -Ix .";
 module = "env_types_lib.mli";
 ocamlopt.byte;
 program = "${test_build_directory}/env_types_use.exe";
 all_modules = "env_types_lib.cmx env_types_use.cmx";
 {
   ocamlopt_flags = "-O3";
   module = "env_types_lib.ml";
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
   module = "env_types_use.ml";
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
   unset module;
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   ocamlopt_flags = "-O3 -flambda2-reaper";
   module = "env_types_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-O3";
   module = "env_types_use.ml";
   ocamlopt.byte;
   unset module;
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   ocamlopt_flags = "-O3";
   module = "env_types_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-O3 -flambda2-reaper";
   module = "env_types_use.ml";
   ocamlopt.byte;
   unset module;
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   ocamlopt_flags = "-Oclassic";
   module = "env_types_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-O3";
   module = "env_types_use.ml";
   ocamlopt.byte;
   unset module;
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   ocamlopt_flags = "-O3";
   module = "env_types_lib.ml";
   ocamlopt.byte;
   ocamlopt_flags = "-Oclassic";
   module = "env_types_use.ml";
   ocamlopt.byte;
   unset module;
   ocamlopt.byte;
   run;
   check-program-output;
 }{
   ocamlopt_flags = "-Oclassic";
   module = "env_types_lib.ml";
   ocamlopt.byte with dump-raw;
   check-fexpr-dump;
   module = "env_types_use.ml";
   ocamlopt.byte with dump-raw;
   check-fexpr-dump;
   unset module;
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* [Env_types_lib] defines templates whose environments contain values known at
   their definitions: [top] and [F] capture [f], and [h], which is defined in
   [F], captures [f], [F]'s argument, a closure [g] and [r]. Each application of
   [F] has its own [g] closure and its own environment for [h]. [Env_types_lib]
   instantiates [top] and [h] in [top_in_lib] and [h_in_lib], and this unit
   instantiates them in [use_top], [use_h1] and [use_h2]. The dumps show how the
   code of these instantiations reads the templates' environments, in the first
   run with [-O3] and in the last in classic mode.

   The code of an instantiation of a template is simplified using the type that
   the template's environment has at its definition. In [Env_types_lib], the
   code of [top]'s instantiation calls [f] directly rather than reading it from
   the environment, and that of [h]'s calls [g]'s code directly, although each
   application of [F] has its own [g] closure. [Env_types_lib] exports these
   types, so this unit's instantiations do the same, and the code of [h]'s
   instantiation is shared by [use_h1] and [use_h2]. In classic mode, closure
   conversion does the same using the approximations of the environments.

   The other runs compile the two units at different optimisation levels, with
   and without the reaper. *)

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
  print_ints [a; b; c];
  let #(a, b) = Env_types_lib.top_in_lib 6 in
  print_ints [a; b];
  let #(a, b, c) = Env_types_lib.h_in_lib 9 in
  print_ints [a; b; c]
