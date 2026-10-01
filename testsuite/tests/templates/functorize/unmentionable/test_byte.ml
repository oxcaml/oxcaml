(* TEST
 (* [Unmentionable] transitive deps of a [-functorize] bundle, in the
    dune-library-style layout of [../dunelike]: only the wrapper [Foo] is
    passed to [-functorize]; [Foo__], [Foo__A], [Foo__B] and [Foo__C] are
    pulled in transitively and bundled as [Unmentionable].  Only [Foo] can
    be named through the instance.  Naming a dep, whether by a dotted path
    ([main_dep_via_dot.ml]), after [open]ing the instance
    ([main_open_dep.ml]), by [open]ing the dep itself
    ([main_open_dep_directly.ml]) or by ascribing a signature that exports
    it ([main_ascribe_dep.ml]), fails to compile. *)

 readonly_files = "\
   main_dep_via_dot.ml bad_dep_via_dot.reference \
   main_open_dep.ml bad_open_dep.reference \
   main_open_dep_directly.ml bad_open_dep_directly.reference \
   main_ascribe_dep.ml bad_ascribe_dep.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int foo bundle_foo_lib";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../../dunelike/p_int.mli \
        ${test_source_directory}/../../dunelike/p_int.ml \
        ${test_source_directory}/../../dunelike/p_int__.ml";
 dst = "p_int/";
 copy;

 src = "${test_source_directory}/../dunelike/foo__.ml \
        ${test_source_directory}/../dunelike/a.ml \
        ${test_source_directory}/../dunelike/b.ml \
        ${test_source_directory}/../dunelike/c.ml \
        ${test_source_directory}/../dunelike/foo.ml";
 dst = "foo/";
 copy;

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* Parameter [P] and argument [P_int]. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H p -open-cmi p/p__.cmi";
 module = "p/p.mli";
 ocamlc.byte;

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -H p_int -open-cmi p_int/p_int__.cmi";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* Library [Foo]: renaming module [Foo__], [Foo__A]/[Foo__B]/[Foo__C] and
    the wrapper. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "foo/foo__.ml";
 ocamlc.byte;

 set flg_lib = "$flg -parameter P -I p -H foo -open-cmi foo/foo__.cmi";

 flags = "$flg_lib -o foo/foo__A.cmo";
 module = "foo/a.ml";
 ocamlc.byte;

 flags = "$flg_lib -o foo/foo__B.cmo";
 module = "foo/b.ml";
 ocamlc.byte;

 flags = "$flg_lib -o foo/foo__C.cmo";
 module = "foo/c.ml";
 ocamlc.byte;

 flags = "$flg_lib";
 module = "foo/foo.ml";
 ocamlc.byte;

 (* Bundle only [Foo]; the [Foo__*] modules are pulled in transitively. *)

 flags = "$flg -functorize -I p -I foo Foo";
 module = "";
 program = "bundle_foo_lib/bundle_foo_lib.cmo";
 all_modules = "";
 ocamlc.byte;

 set flg_main = "$flg -I bundle_foo_lib -I p -I p_int -I foo";

 (* Naming a dep via a dotted path. *)

 flags = "$flg_main";
 module = "main_dep_via_dot.ml";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_dep_via_dot.output";
 ocamlc.byte;

 compiler_reference = "bad_dep_via_dot.reference";
 check-ocamlc.byte-output;

 (* Naming a dep after [open]ing the instance: opening a functor
    application goes through [Env.add_signature], which must preserve
    visibility. *)

 flags = "$flg_main";
 module = "main_open_dep.ml";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_open_dep.output";
 ocamlc.byte;

 compiler_reference = "bad_open_dep.reference";
 check-ocamlc.byte-output;

 (* [open]ing a dep directly is rejected like any other mention of it. *)

 flags = "$flg_main";
 module = "main_open_dep_directly.ml";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_open_dep_directly.output";
 ocamlc.byte;

 compiler_reference = "bad_open_dep_directly.reference";
 check-ocamlc.byte-output;

 (* Ascribing a signature that exports a dep is rejected by [Includemod]. *)

 flags = "$flg_main";
 module = "main_ascribe_dep.ml";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_ascribe_dep.output";
 ocamlc.byte;

 compiler_reference = "bad_ascribe_dep.reference";
 check-ocamlc.byte-output;
*)
