(* TEST (* DO NOT EDIT. Instead edit output_modes/test_byte.ml and run gen-native.sh. *)
 (* Output-mode tests: what files does [-functorize] produce for each
    combination of target extension and bin-annot flags?

    - [.cmi] target alone: produces only [.cmi].
    - [.cmx] target alone: produces [.cmx] and [.cmi].
    - [.cmi] target with [-bin-annot]: additionally produces [.cmti].
    - [.cmx] target with [-bin-annot]: additionally produces [.cmt].
    - [.cmi] target with [-bin-annot-cms]: additionally produces [.cmsi].
    - [.cmx] target with [-bin-annot-cms]: additionally produces [.cms].
    Bin-annot artifacts are interface-vs-implementation specific: [.cmti] /
    [.cmsi] only on [.cmi] targets, [.cmt] / [.cms] only on [.cmx] targets. *)

 readonly_files = "\
   bundle_phases.cmi.objinfo.reference \
   bundle_cms.cms.objinfo_native.reference \
   bundle_cms.cms.objinfo_native.reference \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p basic util \
                 bundle_phases bundle_onestep \
                 bundle_cmt bundle_cmti \
                 bundle_cms bundle_cmsi";
 script;

 src = "${test_source_directory}/../p.mli \
        ${test_source_directory}/../../dunelike/p__.ml";
 dst = "p/";
 copy;

 src = "${test_source_directory}/../../dunelike/basic.mli \
        ${test_source_directory}/../../dunelike/basic.ml \
        ${test_source_directory}/../../dunelike/basic__.ml";
 dst = "basic/";
 copy;

 src = "${test_source_directory}/../../dunelike/util.mli \
        ${test_source_directory}/../../dunelike/util.ml \
        ${test_source_directory}/../../dunelike/util__.ml";
 dst = "util/";
 copy;

 set flg = "-no-alias-deps -w -53";
 set flg_int_iface = "$flg -w -49";

 (* Parameter P. *)

 flags = "$flg_int_iface";
 module = "p/p__.ml";
 ocamlopt.byte;

 flags = "$flg -as-parameter -I p -open P__";
 module = "p/p.mli";
 ocamlopt.byte;

 (* [Basic] and [Util]. *)

 flags = "$flg_int_iface -parameter P -I p";
 module = "basic/basic__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I basic -open Basic__";
 module = "basic/basic.mli basic/basic.ml";
 ocamlopt.byte;

 flags = "$flg_int_iface -parameter P -I p";
 module = "util/util__.ml";
 ocamlopt.byte;

 flags = "$flg -parameter P -I p -I util -open Util__";
 module = "util/util.mli util/util.ml";
 ocamlopt.byte;

 (* (1) [.cmi] target → only [.cmi].  Verify cmi signature via objinfo. *)

 flags = "$flg -functorize -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle_phases/bundle.cmi";
 all_modules = "";
 ocamlopt.byte;

 script = "sh -c 'test -f bundle_phases/bundle.cmi && \
                  ! test -f bundle_phases/bundle.cmx && \
                  ! test -f bundle_phases/bundle.cmti && \
                  ! test -f bundle_phases/bundle.cmsi'";
 script;

 program = "-no-approx -no-code bundle_phases/bundle.cmi";
 output = "bundle_phases.cmi.objinfo.output";
 ocamlobjinfo;

 reference = "bundle_phases.cmi.objinfo.reference";
 check-program-output;

 (* (2) [.cmx] target → both [.cmx] and [.cmi]. *)

 flags = "$flg -functorize -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle_onestep/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 script = "sh -c 'test -f bundle_onestep/bundle.cmx && \
                  test -f bundle_onestep/bundle.cmi && \
                  ! test -f bundle_onestep/bundle.cmt && \
                  ! test -f bundle_onestep/bundle.cms'";
 script;

 (* (3) [.cmi] target with [-bin-annot] → also [.cmti]. *)

 flags = "$flg -functorize -bin-annot -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle_cmti/bundle.cmi";
 all_modules = "";
 ocamlopt.byte;

 script = "sh -c 'test -f bundle_cmti/bundle.cmti && \
                  ! test -f bundle_cmti/bundle.cmt'";
 script;

 (* (4) [.cmx] target with [-bin-annot] → also [.cmt]. *)

 flags = "$flg -functorize -bin-annot -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle_cmt/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 script = "sh -c 'test -f bundle_cmt/bundle.cmt && \
                  ! test -f bundle_cmt/bundle.cmti'";
 script;

 (* (5) [.cmi] target with [-bin-annot-cms] → also [.cmsi]. *)

 flags = "$flg -functorize -bin-annot-cms -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle_cmsi/bundle.cmi";
 all_modules = "";
 ocamlopt.byte;

 script = "sh -c 'test -f bundle_cmsi/bundle.cmsi && \
                  ! test -f bundle_cmsi/bundle.cms'";
 script;

 (* (6) [.cmx] target with [-bin-annot-cms] → also [.cms]. *)

 flags = "$flg -functorize -bin-annot-cms -I p -I basic -I util Basic Util";
 module = "";
 program = "bundle_cms/bundle.cmx";
 all_modules = "";
 ocamlopt.byte;

 script = "sh -c 'test -f bundle_cms/bundle.cms && \
                  ! test -f bundle_cms/bundle.cmsi'";
 script;

 program = "bundle_cms/bundle.cms";
 output = "bundle_cms.cms.objinfo.output";
 ocamlobjinfo;

 reference = "bundle_cms.cms.objinfo_native.reference";
 check-program-output;
*)
