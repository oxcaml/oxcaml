(* TEST (* DO NOT EDIT. Instead edit test_byte.ml and run gen-native.sh. *)

 (* Regression test for resolving an [Approximate] global whose phantom
    hidden argument was revealed into a visible argument by a substitution.

    [mid.ml] aliases [Clk] without [clk.cmi] on its load path, so [mid.cmi]
    records the over-approximated global [Clk{P}].  [outer.ml] instantiates
    [Mid] with [P := P_int]; the substitution reveals the phantom hidden
    argument, so [outer.cmi] records [module C = Clk[P:P_int]] even though
    [Clk] takes no parameters.

    A consumer with [clk.cmi] but not [p_int.cmi] on its load path must
    still be able to resolve [Outer.C]: the excess [P:P_int] argument
    contributes nothing to the elaborated global, so its value must not be
    demanded.  This used to fail with:

      Unbound module P_int in instance Clk[P:P_int]
 *)

 readonly_files = "\
   clk.ml clk.mli consumer.ml mid.ml outer.ml \
   p.mli p_int.ml p_int.mli \
 ";

 setup-ocamlopt.byte-build-env;

 set OCAMLPARAM = "";

 script = "mkdir p p_int clk mid outer";
 script;

 src = "p.mli";
 dst = "p/";
 copy;

 src = "p_int.mli p_int.ml";
 dst = "p_int/";
 copy;

 src = "clk.mli clk.ml";
 dst = "clk/";
 copy;

 src = "mid.ml";
 dst = "mid/";
 copy;

 src = "outer.ml";
 dst = "outer/";
 copy;

 (* warning 49: Absent cmi file when looking up module alias. *)
 (* warning 53: Misplaced attribute (from "@jane.non_erasable.instances"). *)
 (* warning 58: No cmx file (native only). *)
 (* warning 70: No mli file. *)
 set flg = "-no-alias-deps -w -49-53-58-70";

 flags = "$flg -as-parameter -I p";
 module = "p/p.mli";
 ocamlopt.byte;

 flags = "$flg -as-argument-for P -I p -I p_int";
 module = "p_int/p_int.mli p_int/p_int.ml";
 ocamlopt.byte;

 flags = "$flg -I clk";
 module = "clk/clk.mli clk/clk.ml";
 ocamlopt.byte;

 (* [clk] is deliberately not on the load path: the alias in [mid.ml] is
    recorded as the over-approximated global [Clk{P}]. *)
 flags = "$flg -parameter P -I p -I mid";
 module = "mid/mid.ml";
 ocamlopt.byte;

 flags = "$flg -I p -I p_int -I clk -I mid -I outer";
 module = "outer/outer.ml";
 ocamlopt.byte;

 (* The consumer has [clk.cmi] but neither [p_int.cmi] nor [mid.cmi] on its
    load path; resolving [Outer.C] must drop the revealed excess argument
    [P:P_int] rather than demand [p_int.cmi]. *)
 flags = "$flg -I outer -I clk";
 module = "consumer.ml";
 ocamlopt.byte;
*)
