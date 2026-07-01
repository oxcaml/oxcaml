(* TEST
 (* Dune-library-style layout: source files [a.ml], [b.ml] and [c.ml] are
    compiled with [-o] to give them the mangled compilation-unit names
    [Foo__A], [Foo__B] and [Foo__C].  The renaming module [foo__.ml]
    aliases [module A = Foo__A], [module B = Foo__B],
    [module C = Foo__C].  [B] depends on [A] concretely and [C] depends
    on both [A] and [B] (through the [-open Foo__] alias).  The wrapper
    [foo.ml] re-exposes only [B] via [module B = struct include B end];
    it does not mention [A] or [C].

    Only the wrapper [Foo] is passed to [-functorize].  The [include]
    forces [Foo__B] into [foo.cmi]'s bound_globals with [Exact] precision
    even under [-no-alias-deps], and [Foo__B.cmi] in turn lists [Foo__A]
    as [Exact], so both are pulled into the bundle transitively.

    [Foo__C] is only referenced as an [Approximate] alias in [foo__.cmi]
    (never [Exact] anywhere), so the functorizer prunes it to
    [Pruned_Foo__C].  Accessing [Inst.DEP__Foo__.C] fails to compile.

    [Foo__A] and [Foo__B] on the other hand are also [Approximate] in
    [foo__.cmi] but [Exact] elsewhere, so [Inst.DEP__Foo__.A] and
    [Inst.DEP__Foo__.B] are accessible (transitive deps get a [DEP__]
    prefix in the bundle). *)

 readonly_files = "\
   foo__.ml a.ml b.ml c.ml foo.ml \
   main_foo_lib.ml test_functorize_foo_lib.reference \
   main_pruned_c.ml bad_pruned_c.reference \
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

 src = "foo__.ml a.ml b.ml c.ml foo.ml";
 dst = "foo/";
 copy;

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Parameter P. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlc.byte;

 (* [P_int] argument for P. *)

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* Renaming module [Foo__]. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "foo/foo__.ml";
 ocamlc.byte;

 set flg_lib = "$flg -parameter P -I p -I foo -open Foo__";

 (* [a.ml] compiled as [Foo__A]. *)

 flags = "$flg_lib -o foo/foo__A.cmo";
 module = "foo/a.ml";
 ocamlc.byte;

 (* [b.ml] compiled as [Foo__B].  Uses [A] via the [Foo__] alias. *)

 flags = "$flg_lib -o foo/foo__B.cmo";
 module = "foo/b.ml";
 ocamlc.byte;

 (* [c.ml] compiled as [Foo__C].  Uses both [A] and [B]; not referenced
    by the wrapper [Foo]. *)

 flags = "$flg_lib -o foo/foo__C.cmo";
 module = "foo/c.ml";
 ocamlc.byte;

 (* Wrapper [Foo] — body-level uses of [A] and [B] force [Exact] deps. *)

 flags = "$flg_lib";
 module = "foo/foo.ml";
 ocamlc.byte;

 (* Bundle only [Foo].  [Foo__A] and [Foo__B] get pulled in transitively
    via [foo.cmi]'s [Exact]-precision bound_globals. *)

 flags = "$flg -functorize -I p -I foo Foo";
 module = "";
 program = "bundle_foo_lib/bundle_foo_lib.cmo";
 all_modules = "";
 ocamlc.byte;

 flags = "$flg -I bundle_foo_lib -I p -I p_int -I foo";
 module = "main_foo_lib.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_foo_lib.bc";
 all_modules = "\
   foo/foo__.cmo \
   foo/foo__A.cmo \
   foo/foo__B.cmo \
   foo/foo.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   bundle_foo_lib/bundle_foo_lib.cmo \
   main_foo_lib.cmo \
 ";
 ocamlc.byte;

 stdout = "test_functorize_foo_lib.output";
 stderr = "test_functorize_foo_lib.output";
 output = "test_functorize_foo_lib.output";
 run;

 reference = "test_functorize_foo_lib.reference";
 check-program-output;

 (* [Foo__C] is pruned to [Pruned_Foo__C] in the bundle.  Accessing it
    through [Inst.DEP__Foo__.C] fails to compile. *)

 flags = "$flg -I bundle_foo_lib -I p -I p_int -I foo";
 module = "main_pruned_c.ml";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_pruned_c.output";
 ocamlc.byte;

 compiler_reference = "bad_pruned_c.reference";
 check-ocamlc.byte-output;
*)
