(* TEST
 readonly_files = "foo.ml bar.mli";
 include ocamlcommon;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/parsing \
          -I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/file_formats";
 setup-ocamlc.byte-build-env;
 module = "foo.ml";
 ocamlc.byte;
 module = "bar.mli";
 ocamlc.byte;
 expect;
*)

(* [Bar] refers to [Foo.t], yet its bound identifiers are stamped 1, 2, ...
   regardless of how many declarations [Foo] has: ident stamps are normalized
   when the signature is written to the [.cmi], so they no longer leak the
   global [Ident.currentstamp] counter (which is perturbed by loading [Foo]).
   ([bar.cmi] was built by the preceding steps into the [ocamlc.byte]
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

bound_ident_stamps "ocamlc.byte/bar.cmi"
;;
[%%expect {|
val bound_ident_stamps : string -> string list = <fun>
- : string list = ["t_288"; "y_289"]
|}]
