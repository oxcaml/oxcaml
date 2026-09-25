(* TEST
 readonly_files = "definitions.mli";
 include ocamlcommon;
 flags = "-I ${ocamlsrcdir}/utils -I ${ocamlsrcdir}/parsing \
          -I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/file_formats";
 setup-ocamlc.byte-build-env;
 module = "definitions.mli";
 ocamlc.byte;
 expect;
*)

let () =
  let cmi = Cmi_format.read_cmi "ocamlc.byte/definitions.cmi" in
  let signature, _ = cmi.Cmi_format.cmi_sign in
  let rec is_compact = function
    | Types.Mty_ident _ -> true
    | Types.Mty_with (body, _, _, _) -> is_compact body
    | _ -> false
  in
  List.iter (fun name ->
    let declaration =
      List.find_map (function
        | Types.Sig_modtype (id, decl, _) when Ident.name id = name ->
            decl.Types.mtd_type
        | _ -> None) signature
    in
    match declaration with
    | Some (Types.Mty_with _ as body) when is_compact body -> ()
    | _ -> failwith ("Expanded constraint in CMI: " ^ name))
    ["T"; "U"; "Int_record"; "Refined_record"; "Recursive_alias"; "Chain"]
;;
[%%expect {|
|}]
