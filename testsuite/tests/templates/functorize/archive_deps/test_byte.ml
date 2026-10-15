(* TEST
 (* A bundle input that is a PARTIAL instance turns the pass-through
    parameter of one of its dependencies into a complete instance that
    only the bundle names.  Quasi-OCaml, writing parameterised units as
    functors:

    {[
      module Q_impl : Q = struct end                  (* plain argument *)
      module R_impl (P : P) : R = struct ... end      (* parameterised *)
      module Foo_q (Q : Q) = struct let name = "Foo_q" end
      module Pair_qr (Q : Q) (R : R) = struct
        let name () = Foo_q (Q).name ^ " + " ^ R.greeting ()
      end
      module Partial_qr (P : P) = struct
        module Pair_qr_inst = Pair_qr (Q_impl) (R_impl (P))
        let describe () = "Partial_qr > " ^ Pair_qr_inst.name ()
      end
    ]}

    Functorizing [Partial_qr] re-instantiates [Pair_qr[Q:Q_impl]
    [R:R_impl{P}]] inside the bundle, and its dependency [Foo_q{Q}]
    becomes the complete instance [Foo_q[Q:Q_impl]]: not bundled, but
    referenced as a global.  Nothing but the bundle references it.

    The libraries are linked from archives, as a build system links
    them.  The linker only includes (and the startup code only
    initialises) an archive member that some linked unit imports.  If
    the bundle's compilation unit does not record [Foo_q[Q:Q_impl]] as
    an import, the native linker still pulls its object code out of
    the archive to resolve the bundle's symbol references, but its
    initialiser never runs: the bundle reads [Val_unit] out of its
    module block and the program crashes.

    Native code is compiled with [-Oclassic], as our build system does:
    in classic mode module blocks are filled at run time (so reading an
    uninitialised one is fatal), and it is in classic mode that the
    bundle records no import for the complete instance. *)

 readonly_files = "\
   pair_qr.mli pair_qr.ml pair_qr__.ml \
   partial_qr.mli partial_qr.ml partial_qr__.ml \
   main_archive_deps.ml run_and_report.sh \
   test_archive_deps_byte.reference \
 ";

 setup-ocamlc.byte-build-env;

 set OCAMLPARAM = "";
 ocamlopt_flags = "-Oclassic";

 script = "mkdir p p_int q q_impl r r_impl foo_q instances pair_qr \
                 partial_qr lib bundle";
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

 src = "${test_source_directory}/../../dunelike/q.mli \
        ${test_source_directory}/../../dunelike/q__.ml";
 dst = "q/";
 copy;

 src = "${test_source_directory}/../../dunelike/q_impl.ml \
        ${test_source_directory}/../../dunelike/q_impl__.ml";
 dst = "q_impl/";
 copy;

 src = "${test_source_directory}/../partial_deps/r.mli \
        ${test_source_directory}/../partial_deps/r__.ml";
 dst = "r/";
 copy;

 src = "${test_source_directory}/../partial_deps/r_impl.mli \
        ${test_source_directory}/../partial_deps/r_impl.ml \
        ${test_source_directory}/../partial_deps/r_impl__.ml";
 dst = "r_impl/";
 copy;

 src = "${test_source_directory}/../partial_deps/foo_q.mli \
        ${test_source_directory}/../partial_deps/foo_q.ml \
        ${test_source_directory}/../partial_deps/foo_q__.ml";
 dst = "foo_q/";
 copy;

 src = "pair_qr.mli pair_qr.ml pair_qr__.ml";
 dst = "pair_qr/";
 copy;

 src = "partial_qr.mli partial_qr.ml partial_qr__.ml";
 dst = "partial_qr/";
 copy;

 set flg_base = "-w -53";
 set flg = "$flg_base -no-alias-deps -nocwd";
 set flg_int_iface = "$flg -w -49";

 (* dune does not pass [-nocwd] to link *)
 set flg_link = "$flg_base -no-alias-deps";

 (* Step 1: parameters [P], [Q] and [R]; arguments [P_int], [Q_impl]
    (plain) and [R_impl] (parameterised by [P]). *)

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

 flags = "$flg_int_iface";
 module = "q/q__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H q -open-cmi q/q__.cmi";
 module = "q/q.mli";
 ocamlc.byte;

 flags = "$flg_int_iface";
 module = "q_impl/q_impl__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for Q -I q -H q_impl \
   -open-cmi q_impl/q_impl__.cmi";
 module = "q_impl/q_impl.ml";
 ocamlc.byte;

 flags = "$flg_int_iface";
 module = "r/r__.ml";
 ocamlc.byte;

 flags = "$flg -as-parameter -H r -open-cmi r/r__.cmi";
 module = "r/r.mli";
 ocamlc.byte;

 flags = "$flg_int_iface -parameter P -I p";
 module = "r_impl/r_impl__.ml";
 ocamlc.byte;

 flags = "$flg -as-argument-for R -parameter P -I p -I r -H r_impl \
   -open-cmi r_impl/r_impl__.cmi";
 module = "r_impl/r_impl.mli r_impl/r_impl.ml";
 ocamlc.byte;

 (* Step 2: [Foo_q], parameterised by [Q], and its complete instance
    [Foo_q[Q:Q_impl]] in [instances/]. *)

 flags = "$flg_int_iface -parameter Q -I q";
 module = "foo_q/foo_q__.ml";
 ocamlc.byte;

 flags = "$flg -parameter Q -I q -H foo_q -open-cmi foo_q/foo_q__.cmi";
 module = "foo_q/foo_q.mli foo_q/foo_q.ml";
 ocamlc.byte;

 flags = "$flg -I q -I q_impl -I foo_q -instantiate";
 module = "";
 program = "instances/foo_q__-Q_impl.cmo";
 all_modules = "foo_q/foo_q__.cmo q_impl/q_impl.cmo";
 ocamlc.byte;

 flags = "$flg -I q -I q_impl -I foo_q -instantiate";
 module = "";
 program = "instances/foo_q-Q_impl.cmo";
 all_modules = "foo_q/foo_q.cmo q_impl/q_impl.cmo";
 ocamlc.byte;

 (* Step 3: [Pair_qr], parameterised by [Q] and [R], passing [Q] through
    to [Foo_q]. *)

 flags = "$flg_int_iface -parameter Q -parameter R -I q -I r -I foo_q";
 module = "pair_qr/pair_qr__.ml";
 ocamlc.byte;

 flags = "$flg -parameter Q -parameter R -I q -I r -I foo_q -H pair_qr \
   -open-cmi pair_qr/pair_qr__.cmi";
 module = "pair_qr/pair_qr.mli pair_qr/pair_qr.ml";
 ocamlc.byte;

 (* Step 4: [Partial_qr], parameterised by [P] only, whose prelude
    references [Pair_qr[Q:Q_impl][R:R_impl{P}]]. *)

 flags = "$flg_int_iface -parameter P -I p -I q -I q_impl -I r -I r_impl \
   -I foo_q -I instances -I pair_qr";
 module = "partial_qr/partial_qr__.ml";
 ocamlc.byte;

 flags = "$flg -parameter P -I p -I q -I q_impl -I r -I r_impl -I foo_q \
   -I instances -I pair_qr -H partial_qr \
   -open-cmi partial_qr/partial_qr__.cmi";
 module = "partial_qr/partial_qr.mli partial_qr/partial_qr.ml";
 ocamlc.byte;

 (* Step 5: archive the libraries. *)

 flags = "$flg_link -a";
 module = "";
 program = "lib/deps.cma";
 all_modules = "\
   q_impl/q_impl__.cmo \
   q_impl/q_impl.cmo \
   foo_q/foo_q__.cmo \
   foo_q/foo_q.cmo \
   instances/foo_q__-Q_impl.cmo \
   instances/foo_q-Q_impl.cmo \
 ";
 ocamlc.byte;

 flags = "$flg_link -a";
 module = "";
 program = "lib/r_impl.cma";
 all_modules = "r_impl/r_impl__.cmo r_impl/r_impl.cmo";
 ocamlc.byte;

 flags = "$flg_link -a";
 module = "";
 program = "lib/pair_qr.cma";
 all_modules = "pair_qr/pair_qr__.cmo pair_qr/pair_qr.cmo";
 ocamlc.byte;

 flags = "$flg_link -a";
 module = "";
 program = "lib/partial_qr.cma";
 all_modules = "partial_qr/partial_qr__.cmo partial_qr/partial_qr.cmo";
 ocamlc.byte;

 (* Step 6: functorize [Partial_qr].  [Pair_qr[Q:Q_impl][R:R_impl{P}]]
    and [R_impl{P}] are re-instantiated inside the bundle;
    [Foo_q[Q:Q_impl]] is left as a global reference. *)

 flags = "$flg -functorize -I p -I q -I q_impl -I r -I r_impl -I foo_q \
   -I instances -I pair_qr -I partial_qr Partial_qr";
 module = "";
 program = "bundle/bundle.cmo";
 all_modules = "";
 ocamlc.byte;

 (* Step 7: consume the bundle, linking the libraries from their
    archives. *)

 flags = "$flg -I bundle -I p -I p_int -I q -I q_impl -I r -I r_impl \
   -I foo_q -I instances -I pair_qr -I partial_qr";
 module = "main_archive_deps.ml";
 ocamlc.byte;

 flags = "$flg_link";
 module = "";
 program = "$test_build_directory/test_archive_deps_byte.bc";
 all_modules = "\
   p_int/p_int__.cmo \
   p_int/p_int.cmo \
   lib/deps.cma \
   lib/r_impl.cma \
   lib/pair_qr.cma \
   lib/partial_qr.cma \
   bundle/bundle.cmo \
   main_archive_deps.cmo \
 ";
 ocamlc.byte;

 (* Run through a wrapper that reports a crash in the output. *)

 script = "sh ${test_source_directory}/run_and_report.sh \
   ${test_build_directory}/test_archive_deps_byte.bc";
 stdout = "test_archive_deps_byte.output";
 stderr = "test_archive_deps_byte.output";
 output = "test_archive_deps_byte.output";
 script;

 reference = "test_archive_deps_byte.reference";
 check-program-output;
*)
