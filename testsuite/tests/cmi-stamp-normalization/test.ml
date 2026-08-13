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

let label_ids (lbls : Types.label_declaration list) =
  List.map (fun (l : Types.label_declaration) -> l.ld_id) lbls

let decl_ids (decl : Types.type_declaration) =
  match decl.type_kind with
  | Type_record (lbls, _, _) | Type_record_unboxed_product (lbls, _, _) ->
      label_ids lbls
  | Type_variant (cstrs, _, _) ->
      List.concat_map
        (fun (c : Types.constructor_declaration) ->
          c.cd_id ::
          (match c.cd_args with
           | Cstr_record lbls -> label_ids lbls
           | Cstr_tuple _ -> []))
        cstrs
  | Type_abstract _ | Type_open -> []

let bound_ident_stamps cmi_file =
  let cmi = Cmi_format.read_cmi cmi_file in
  let sg, _ = cmi.Cmi_format.cmi_sign in
  List.concat_map
    (fun (item : Types.signature_item) ->
      let ids =
        match item with
        | Sig_value (id, _, _) -> [id]
        | Sig_type (id, decl, _, _) -> id :: decl_ids decl
        | Sig_typext (id, _, _, _) -> [id]
        | Sig_module (id, _, _, _, _) -> [id]
        | Sig_modtype (id, _, _) -> [id]
        | Sig_class (id, _, _, _) -> [id]
        | Sig_class_type (id, _, _, _) -> [id]
        | Sig_jkind (id, _, _) -> [id]
      in
      List.map Ident.unique_name ids)
    sg
;;

bound_ident_stamps "ocamlc.byte/bar_orig.cmi"
;;
[%%expect {|
val label_ids : Types.label_declaration list -> Ident.t list = <fun>
val decl_ids : Types.type_declaration -> Ident.t list = <fun>
val bound_ident_stamps : string -> string list = <fun>
- : string list = ["t_1"; "r_2"; "lbl_8"; "v_3"; "A_5"; "B_6"; "y_4"]
|}]

;;
bound_ident_stamps "ocamlc.byte/bar.cmi"
;;
[%%expect {|
- : string list = ["t_1"; "r_2"; "lbl_8"; "v_3"; "A_5"; "B_6"; "y_4"]
|}]
