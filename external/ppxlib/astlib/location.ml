include Ocaml_common.Location

let set_input_name name = input_name := name
let set_input_lexbuf lexbuf_opt = input_lexbuf := lexbuf_opt

module Error = struct
  [@@@warning "-37"]

  type old_t (*IF_NOT_AT_LEAST 408 = Ocaml_common.Location.error *) = {
    loc: t;
    msg: string;
    sub: old_t list;
    if_highlight: string;
  }

  type location_report_kind = Ocaml_common.Location.report_kind =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string

  type location_msg =
    (*IF_AT_LEAST 503 Ocaml_common.Format_doc.t loc *)
    (*IF_NOT_AT_LEAST 503 (Format.formatter -> unit) loc *)

  type location_report = Ocaml_common.Location.report = {
    kind : location_report_kind;
    main : location_msg;
    sub : location_msg list;
    (*IF_AT_LEAST 503 footnote: Format_doc.t option; *)
  }

  type t = Ocaml_common.Location.error

  let is_well_formed error =
    match error with
    | { kind = Report_error; _ } -> true
    | _ -> false

  let string_of_location_msg (msg : location_msg) =
     (*IF_AT_LEAST 503 Format.asprintf "%a" Ocaml_common.Format_doc.Doc.format msg.txt *)
     (*IF_NOT_AT_LEAST 503 Format.asprintf "%t" msg.txt *)

  let main_msg { main; _ } =
    { txt = string_of_location_msg main; loc = main.loc }

  let sub_msgs { sub; _ } =
    List.map
      (fun err -> { txt = string_of_location_msg err; loc = err.loc })
      sub

  let of_exn exn =
    match error_of_exn exn with
    | Some (`Ok e) -> Some e
    | None | Some `Already_displayed -> None

  let set_main_msg error msg =
    (*IF_AT_LEAST 503 let txt = Ocaml_common.Format_doc.Doc.msg "%s" msg in *)
    (*IF_NOT_AT_LEAST 503 let txt ppf = Format.pp_print_string ppf msg in *)
    let main = { error.main with txt } in
    { error with main }

  let make ~sub { loc; txt } =
    (*IF_AT_LEAST 503 let mk_txt x = Ocaml_common.Format_doc.Doc.msg "%s" x in *)
    (*IF_NOT_AT_LEAST 503 let mk_txt x ppf = Format.pp_print_string ppf x in *)
    let mk loc x = { loc; txt = mk_txt x } in
    {
      kind = Report_error;
      main = mk loc txt;
      sub = List.map (fun { loc; txt } -> mk loc txt) sub;
      (*IF_AT_LEAST 503 footnote = None; *)
    }

  let set_main_loc error loc =
    let main = { error.main with loc } in
    { error with main }
end

let raise_errorf ?loc msg =
  (* Update from [kasprintf] to [kdprintf] + [Format_doc.deprecated_printer]
     when ocaml lower bound is 4.08+ *)
  (*IF_AT_LEAST 503 Format.kdprintf (fun pr -> raise_errorf ?loc "%t" (Format_doc.deprecated_printer pr)) msg *)
  (*IF_NOT_AT_LEAST 503 raise_errorf ?loc msg *)
