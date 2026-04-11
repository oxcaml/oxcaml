module To_before_502 =
  Ppxlib_ast.Convert (Ppxlib_ast.Js) (Ppxlib_ast__.Versions.OCaml_501)

module From_before_502 =
  Ppxlib_ast.Convert (Ppxlib_ast__.Versions.OCaml_501) (Ppxlib_ast.Js)

module Before_502_to_ocaml =
  Ppxlib_ast.Convert
    (Ppxlib_ast__.Versions.OCaml_501)
    (Ppxlib_ast.Compiler_version)

module OCaml_501 = Ppxlib_ast__.Versions.OCaml_501.Ast

let rec unfold_list_lit x next =
  let open OCaml_501.Parsetree in
  let open Astlib.Longident in
  match next.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> [ x ]
  | Pexp_construct
      ( { txt = Lident "::"; _ },
        Some { pexp_desc = Pexp_tuple [ elm; rest ]; _ } ) ->
      x :: unfold_list_lit elm rest
  | _ -> invalid_arg "list_lit"

(* Only deals with the basic blocks needed for ocaml.ppx.context *)
let rec basic_expr_to_string expr =
  let open OCaml_501.Parsetree in
  let open Astlib.Longident in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (s, _, None)) -> Printf.sprintf "%S" s
  | Pexp_ident { txt = Lident name; _ } -> name
  | Pexp_tuple l ->
      let strs = List.map basic_expr_to_string l in
      "(" ^ String.concat ", " strs ^ ")"
  | Pexp_construct ({ txt = Lident s; _ }, None) -> s
  | Pexp_construct
      ( { txt = Lident "::"; _ },
        Some { pexp_desc = Pexp_tuple [ elm; rest ]; _ } ) ->
      let exprs = unfold_list_lit elm rest in
      let strs = List.map basic_expr_to_string exprs in
      "[" ^ String.concat "; " strs ^ "]"
  | _ -> invalid_arg "basic_expr_to_string"

let print_field (lident_loc, expr) =
  match lident_loc with
  | { OCaml_501.Asttypes.txt = Astlib.Longident.Lident name; _ } ->
      Printf.printf "    %s: %s;\n" name (basic_expr_to_string expr)
  | _ -> ()

let print_ocaml_ppx_context stri =
  let open OCaml_501.Parsetree in
  match stri.pstr_desc with
  | Pstr_attribute
      {
        attr_payload =
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval ({ pexp_desc = Pexp_record (fields, None); _ }, _);
                _;
              };
            ];
        _;
      } ->
      Printf.printf "[@@@ocaml.ppx.context\n";
      Printf.printf "  {\n";
      List.iter print_field fields;
      Printf.printf "  }\n";
      Printf.printf "]\n"
  | _ -> ()

let is_ppx_context stri =
  let open OCaml_501.Parsetree in
  match stri.pstr_desc with
  | Pstr_attribute
      { attr_name = { OCaml_501.Asttypes.txt = "ocaml.ppx.context"; _ }; _ } ->
      true
  | _ -> false

let impl _ctxt str =
  let before_502_ast = To_before_502.copy_structure str in
  let ppx_context = List.find is_ppx_context before_502_ast in
  Printf.printf "ocaml.ppx.context before 5.02:\n";
  print_ocaml_ppx_context ppx_context;
  let round_trip = Before_502_to_ocaml.copy_structure_item ppx_context in
  Printf.printf "ocaml.ppx.context round tripped:\n";
  Ocaml_common.Pprintast.structure_item Format.std_formatter round_trip;
  str

let () = Ppxlib.Driver.V2.register_transformation ~impl "ocaml.ppx.context-test"
let () = Ppxlib.Driver.standalone ()
