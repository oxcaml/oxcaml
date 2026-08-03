(* TEST
 readonly_files = "foo.ml baz.ml bar.mli";
 include ocamlcommon;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/parsing \
          -I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/file_formats";
 setup-ocamlc.byte-build-env;
 module = "foo.ml";
 ocamlc.byte;
 module = "baz.ml";
 ocamlc.byte;
 module = "bar.mli";
 ocamlc.byte;
 src = "bar.cmi";
 dst = "bar_orig.cmi";
 copy;
 flags = "-open Baz";
 module = "bar.mli";
 ocamlc.byte;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/parsing \
          -I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/file_formats";
 expect;
*)

(* [Bar] refers to [Foo.t], yet its bound identifiers are stamped 1, 2, ...
   regardless of how many declarations [Foo] has: ident stamps are normalized
   when the signature is written to the [.cmi], so they no longer leak the
   global [Ident.currentstamp] counter (which is perturbed by loading [Foo]).

   [bar.mli] is compiled twice: the first [.cmi] is preserved as
   [bar_orig.cmi], then the same source is recompiled with [-open Baz], which
   loads an extra dependency ([baz.cmi]) that the first compilation never
   loads, perturbing the stamp counter differently. Without normalization the
   two [.cmi]s would carry different stamps; with it they are identical.

   (Both [.cmi]s were built by the preceding steps into the [ocamlc.byte]
   subdirectory of the [expect] working directory.) *)

let bound_ident_stamps cmi_file =
  let cmi = Cmi_format.read_cmi cmi_file in
  let sg, _ = cmi.Cmi_format.cmi_sign in
  List.map
    (fun (item : Types.signature_item) ->
      let id =
        match item with
        | Sig_value (id, _, _) -> id
        | Sig_type (id, _, _, _) -> id
        | Sig_typext (id, _, _, _) -> id
        | Sig_module (id, _, _, _, _) -> id
        | Sig_modtype (id, _, _) -> id
        | Sig_class (id, _, _, _) -> id
        | Sig_class_type (id, _, _, _) -> id
        | Sig_jkind (id, _, _) -> id
      in
      Ident.unique_name id)
    sg
;;

bound_ident_stamps "ocamlc.byte/bar_orig.cmi"
;;
[%%expect {|
val bound_ident_stamps : string -> string list = <fun>
- : string list = ["t_1"; "r_2"; "v_3"; "y_4"]
|}]

;;
bound_ident_stamps "ocamlc.byte/bar.cmi"
;;
[%%expect {|
- : string list = ["t_1"; "r_2"; "v_3"; "y_4"]
|}]
