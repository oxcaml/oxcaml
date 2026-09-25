open Odoc_module

let contains text part =
  match Str.search_forward (Str.regexp_string part) text 0 with
  | _ -> true
  | exception Not_found -> false

let check_text name text part =
  if not (contains text part) then
    failwith (Printf.sprintf "%s: expected %S in %S" name part text)

let check_expanded name mty =
  let text = Odoc_print.string_of_module_type ~complete:true mty in
  if contains text "..." then
    failwith (Printf.sprintf "%s: unexpanded signature: %s" name text);
  check_text name text "type t = int"

let modtype name elements =
  List.find (fun mt -> Odoc_name.simple mt.mt_name = name) (mod_types elements)

let module_named name elements =
  List.find (fun m -> Odoc_name.simple m.m_name = name) (modules elements)

let check_modtype name elements =
  let mt = modtype name elements in
  match mt.mt_type with
  | Some mty -> check_expanded mt.mt_name mty
  | None -> failwith (mt.mt_name ^ ": missing module type")

let check_parameter name elements =
  let m = module_named name elements in
  match module_parameters m with
  | [({ mp_type = Some mty; _ }, _)] -> check_expanded m.m_name mty
  | _ -> failwith (m.m_name ^ ": missing functor parameter")

let analyse name file =
  (* As when documenting Stdlib itself, do not implicitly open another module.
     The fixtures only need predefined types. *)
  Odoc_global.initially_opened_module := name;
  match Odoc_analyse.analyse_files [file] with
  | [m] when !Odoc_global.errors = 0 -> module_elements m
  | _ -> failwith (name ^ ": Ocamldoc analysis failed")

let () =
  let definitions =
    analyse "Definitions" (Odoc_global.Intf_file "definitions.mli") in
  List.iter (fun name -> check_modtype name definitions)
    ["Inline"; "Named"; "Nested"; "Payload"];
  let inline = modtype "Inline" definitions in
  let get = List.find (fun v -> Odoc_name.simple v.Odoc_value.val_name = "get")
      (module_type_values inline) in
  check_text "qualified value type"
    (Odoc_print.string_of_type_expr get.Odoc_value.val_type)
    "Definitions.Inline.t";
  check_modtype "Result" (module_elements (module_named "F" definitions));
  check_parameter "G" definitions;
  let implementations =
    analyse "Implementations" (Odoc_global.Impl_file "implementations.ml") in
  check_modtype "Inline" implementations;
  check_expanded "Implementations.M" (module_named "M" implementations).m_type;
  check_parameter "F" implementations;
  check_modtype "Result" (module_elements (module_named "G" implementations))
