(* TEST
 (* Bundle a compound reference [Foo_r(R)(R_impl)] where [R_impl] is
    parameterised (by [P]) and its primary block has an extra [filler]
    field before [greeting].  When the bundle runs, [R_impl(P)]'s main
    block must be projected through [mod_field arg_block_idx main_repr]
    to yield [R_impl]'s arg block before being passed to [Foo_r]'s
    functor — otherwise [Foo_r] reads offset 0 (= [filler]) as
    [R.greeting] and calling [filler] as a function segfaults. *)

 readonly_files = "\
   r.mli r__.ml \
   r_impl.mli r_impl.ml r_impl__.ml \
   r_int.mli r_int.ml \
   foo_r.mli foo_r.ml foo_r__.ml \
   nested_r.mli nested_r.ml nested_r__.ml \
   main_nested_r.ml test_functorize_nested_r.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int r r_int r_impl foo_r nested_r bundle";
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

 src = "r.mli r__.ml";
 dst = "r/";
 copy;

 src = "r_int.mli r_int.ml";
 dst = "r_int/";
 copy;

 src = "r_impl.mli r_impl.ml r_impl__.ml";
 dst = "r_impl/";
 copy;

 src = "foo_r.mli foo_r.ml foo_r__.ml";
 dst = "foo_r/";
 copy;

 src = "nested_r.mli nested_r.ml nested_r__.ml";
 dst = "nested_r/";
 copy;

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Parameter P and argument P_int. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlc.byte;

 flags = "$flg_int_iface";
 module = "p_int/p_int__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for P -I p -I p_int -open P_int__";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlc.byte;

 (* Parameter R and argument R_int. *)

 flags = "$flg_int_iface";
 module = "r/r__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -I r -open R__";
 module = "r/r.mli";
 ocamlc.byte;

 flags = "$flg -as-argument-for R -I r -I r_int";
 module = "r_int/r_int.mli r_int/r_int.ml";
 ocamlc.byte;

 (* R_impl: -as-argument-for R -parameter P.  A parameterised argument
    with an extra [filler] field before [greeting]. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "r_impl/r_impl__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for R -parameter P -I p -I r -I r_impl \
   -open R_impl__";
 module = "r_impl/r_impl.mli r_impl/r_impl.ml";
 ocamlc.byte;

 (* Foo_r: parameterised by R only; calls [R.greeting ()]. *)

 flags = "$flg_int_iface -parameter R -I r";
 module = "foo_r/foo_r__.ml";
 ocamlc.byte;

 flags = "$flg -parameter R -I p -I r -I foo_r -open Foo_r__";
 module = "foo_r/foo_r.mli foo_r/foo_r.ml";
 ocamlc.byte;

 (* Nested_r: parameterised by P and R; uses [Foo_r(R)(R_impl)]. *)

 flags = "$flg_int_iface -parameter P -parameter R -I p -I r -I r_impl \
   -I foo_r";
 module = "nested_r/nested_r__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -parameter R -I p -I r -I r_impl -I foo_r \
   -I nested_r -open Nested_r__";
 module = "nested_r/nested_r.mli nested_r/nested_r.ml";
 ocamlc.byte;

 (* Bundle [Nested_r]. *)

 flags = "$flg -functorize -I p -I r -I r_impl -I foo_r -I nested_r \
   Nested_r";
 module = "";
 program = "bundle/bundle.cmo";
 all_modules = "";
 ocamlc.byte;

 flags = "$flg -I bundle -I p -I p_int -I r -I r_int -I r_impl -I foo_r \
   -I nested_r";
 module = "main_nested_r.ml";
 ocamlc.byte;

 flags = "";
 module = "";
 program = "$test_build_directory/test_functorize_nested_r.bc";
 all_modules = "\
   foo_r/foo_r__.cmo \
   foo_r/foo_r.cmo \
   r_impl/r_impl__.cmo \
   r_impl/r_impl.cmo \
   nested_r/nested_r__.cmo \
   nested_r/nested_r.cmo \
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   r_int/r_int.cmo \
   bundle/bundle.cmo \
   main_nested_r.cmo \
 ";
 ocamlc.byte;

 stdout = "test_functorize_nested_r.output";
 stderr = "test_functorize_nested_r.output";
 output = "test_functorize_nested_r.output";
 run;

 reference = "test_functorize_nested_r.reference";
 check-program-output;
*)
