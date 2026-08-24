module Diagnostic = Structured_diagnostic

let flag = "-structured-diagnostics"
let converter_attempts = 5

let string_to_json value =
  let escaped = Buffer.create (String.length value + 2) in
  let width = String.length value in
  let rec add index =
    if index < width then
      match String.get value index with
      | '"' ->
          Buffer.add_string escaped "\\\"";
          add (index + 1)
      | '\\' ->
          Buffer.add_string escaped "\\\\";
          add (index + 1)
      | '\b' ->
          Buffer.add_string escaped "\\b";
          add (index + 1)
      | '\012' ->
          Buffer.add_string escaped "\\f";
          add (index + 1)
      | '\n' ->
          Buffer.add_string escaped "\\n";
          add (index + 1)
      | '\r' ->
          Buffer.add_string escaped "\\r";
          add (index + 1)
      | '\t' ->
          Buffer.add_string escaped "\\t";
          add (index + 1)
      | '\000' .. '\031' as control ->
          Buffer.add_string escaped
            (Printf.sprintf "\\u%04x" (Char.code control));
          add (index + 1)
      | ' ' .. '\127' as ascii ->
          Buffer.add_char escaped ascii;
          add (index + 1)
      | _ ->
          let decoded = String.get_utf_8_uchar value index in
          let bytes = Uchar.utf_decode_length decoded in
          if Uchar.utf_decode_is_valid decoded then
            Buffer.add_substring escaped value index bytes
          else Buffer.add_utf_8_uchar escaped Uchar.rep;
          add (index + bytes)
  in
  Buffer.add_char escaped '"';
  add 0;
  Buffer.add_char escaped '"';
  Buffer.contents escaped

let kind_field kind = Misc.Json.field "kind" (string_to_json kind)

let position_to_json (position : Lexing.position) =
  Misc.Json.object_
    [
      Misc.Json.field "line" (Misc.Json.int position.pos_lnum);
      Misc.Json.field "col"
        (Misc.Json.int (position.pos_cnum - position.pos_bol));
    ]

let location_to_json (loc : Location.t) =
  Misc.Json.object_
    [
      Misc.Json.field "file" (string_to_json loc.loc_start.pos_fname);
      Misc.Json.field "start" (position_to_json loc.loc_start);
      Misc.Json.field "end" (position_to_json loc.loc_end);
    ]

let form_to_string (form : Diagnostic.Form.t) =
  match form with Name -> "name" | Pronoun -> "pronoun"

let kind_to_string (kind : Diagnostic.Kind.t) =
  match kind with
  | Explanation -> "explanation"
  | Background -> "background"
  | Suggestion -> "suggestion"

let relation_to_string (relation : Diagnostic.Relation.t) =
  match relation with Claim -> "claim" | Elaboration -> "elaboration"

let annotation_to_json (annotation : Diagnostic.Annotation.t) =
  match annotation with
  | Code -> Misc.Json.object_ [ kind_field "code" ]
  | Source loc ->
      Misc.Json.object_
        [ kind_field "source"; Misc.Json.field "loc" (location_to_json loc) ]
  | Mention { entity; form } ->
      Misc.Json.object_
        [
          kind_field "mention";
          Misc.Json.field "entity"
            (Misc.Json.int (Diagnostic.Entities.Id.to_int entity));
          Misc.Json.field "form" (string_to_json (form_to_string form));
        ]
  | Term term ->
      Misc.Json.object_
        [
          kind_field "term";
          Misc.Json.field "term"
            (Misc.Json.int (Diagnostic.Glossary.Id.to_int term));
        ]

let rec inline_to_json (inline : Diagnostic.Inline.t) =
  match inline with
  | Text text ->
      Misc.Json.object_
        [ kind_field "text"; Misc.Json.field "text" (string_to_json text) ]
  | Annotated { annotation; content } ->
      Misc.Json.object_
        [
          kind_field "annotated";
          Misc.Json.field "annotation" (annotation_to_json annotation);
          Misc.Json.field "content" (inlines_to_json content);
        ]

and inlines_to_json content = Misc.Json.array (List.map inline_to_json content)

let rec block_to_json (block : Diagnostic.Block.t) =
  Misc.Json.object_
    [
      Misc.Json.field "kind" (string_to_json (kind_to_string block.kind));
      Misc.Json.field "content" (inlines_to_json block.content);
      Misc.Json.field "children"
        (Misc.Json.array (List.map child_to_json block.children));
    ]

and child_to_json
    ((relation, block) : Diagnostic.Relation.t * Diagnostic.Block.t) =
  Misc.Json.object_
    [
      Misc.Json.field "relation" (string_to_json (relation_to_string relation));
      Misc.Json.field "block" (block_to_json block);
    ]

let entity_to_json ((id, loc) : Diagnostic.Entities.Id.t * Location.t) =
  Misc.Json.object_
    [
      Misc.Json.field "id" (Misc.Json.int (Diagnostic.Entities.Id.to_int id));
      Misc.Json.field "loc" (location_to_json loc);
    ]

let glossary_entry_to_json
    ((id, entry) : Diagnostic.Glossary.Id.t * Diagnostic.Glossary.Entry.t) =
  let url =
    match entry.url with
    | None -> []
    | Some url -> [ Misc.Json.field "url" (string_to_json url) ]
  in
  Misc.Json.object_
    ([
       Misc.Json.field "id" (Misc.Json.int (Diagnostic.Glossary.Id.to_int id));
       Misc.Json.field "term" (string_to_json entry.term);
       Misc.Json.field "category" (string_to_json entry.category);
       Misc.Json.field "description" (string_to_json entry.description);
     ]
    @ url)

let diagnostic_to_json (diagnostic : Diagnostic.t) =
  Misc.Json.object_
    [
      Misc.Json.field "loc" (location_to_json diagnostic.loc);
      Misc.Json.field "title" (string_to_json diagnostic.title);
      Misc.Json.field "entities"
        (Misc.Json.array
           (List.map entity_to_json
              (Diagnostic.Entities.to_list diagnostic.entities)));
      Misc.Json.field "glossary"
        (Misc.Json.array
           (List.map glossary_entry_to_json
              (Diagnostic.Glossary.to_list diagnostic.glossary)));
      Misc.Json.field "body"
        (Misc.Json.array (List.map block_to_json diagnostic.body));
    ]

let documentation_unavailable : Mode_diagnostics.Documentation.lookup =
  { of_mode = (fun _ -> None); of_modality = (fun _ -> None) }

let typedtree_unavailable : Mode_diagnostics.context =
  {
    inclusion_site_at = (fun _ -> None);
    declared_modalities_at = (fun _ ~argument:_ -> None);
    constructor_arguments_at = (fun _ _ -> None);
    documentation = documentation_unavailable;
  }

let without_structural_newlines json =
  String.concat "" (String.split_on_char '\n' json)

let emit ppf diagnostic =
  Format.fprintf ppf "%s@."
    (without_structural_newlines (diagnostic_to_json diagnostic))

let emit_diagnostic ppf diagnostic = emit ppf diagnostic

let rendered_by (printer : Location.report_printer) report =
  Format.asprintf "%a" (fun ppf report -> printer.pp printer ppf report) report

let rendered_kind_of (printer : Location.report_printer) report =
  Format.asprintf "%a"
    (fun ppf kind -> printer.pp_report_kind printer report ppf kind)
    report.Location.kind

let diagnostic_of_text ~loc ~title text : Structured_diagnostic.t =
  {
    loc;
    title;
    entities = Structured_diagnostic.Entities.empty;
    glossary = Structured_diagnostic.Glossary.empty;
    body =
      [
        {
          kind = Structured_diagnostic.Kind.Explanation;
          content = [ Structured_diagnostic.Inline.Text text ];
          children = [];
        };
      ];
  }

let generic_diagnostic report =
  let printer = Location.batch_mode_printer in
  diagnostic_of_text ~loc:report.Location.main.loc
    ~title:(rendered_kind_of printer report)
    (String.trim (rendered_by printer report))

let source_of report =
  let file = report.Location.main.loc.loc_start.pos_fname in
  match In_channel.with_open_bin file In_channel.input_all with
  | text -> Some (Mode_diagnostics.Source.create ~file ~text)
  | exception Sys_error _ -> None

let within_type_snapshot diagnose =
  let snapshot = Btype.snapshot () in
  Fun.protect ~finally:(fun () -> Btype.backtrack snapshot) diagnose

let mode_diagnostic report exn =
  match source_of report with
  | None -> None
  | Some source ->
      within_type_snapshot (fun () ->
          match
            Mode_diagnostics.error ~source ~context:typedtree_unavailable
              ~pronouns:Use_pronouns ~loc:report.Location.main.loc exn
          with
          | diagnostic -> diagnostic
          | exception ((Out_of_memory | Stack_overflow) as unrecoverable) ->
              raise unrecoverable
          | exception _ -> None)

let rec converted attempts_left exn =
  match Location.error_of_exn exn with
  | Some (`Ok report) -> Some (report, exn)
  | None | Some `Already_displayed -> None
  | exception raised when attempts_left > 0 ->
      converted (attempts_left - 1) raised
  | exception _ -> None

let specialized_diagnostic exn =
  match converted converter_attempts exn with
  | None -> None
  | Some (report, exn) -> mode_diagnostic report exn

let json_report_printer : Location.report_printer =
  {
    Location.batch_mode_printer with
    pp =
      (fun _self ppf report -> emit_diagnostic ppf (generic_diagnostic report));
  }

let report_exception_as_json ppf exn =
  match specialized_diagnostic exn with
  | Some diagnostic -> emit_diagnostic ppf diagnostic
  | None -> Location.report_exception ppf exn

let report_message_as_prose ~usage message =
  prerr_endline message;
  Option.iter (fun print_usage -> print_usage ()) usage

let report_message_as_json ~usage:_ message =
  emit_diagnostic Format.err_formatter
    (diagnostic_of_text ~loc:Location.none ~title:"Error" message)

let exception_reporter = ref Location.report_exception
let message_reporter = ref report_message_as_prose

let setup argv =
  if Array.exists (String.equal flag) argv then
    Clflags.structured_diagnostics := true;
  if !Clflags.structured_diagnostics then begin
    (Location.report_printer := fun () -> json_report_printer);
    exception_reporter := report_exception_as_json;
    message_reporter := report_message_as_json
  end

let report_exception ppf exn = !exception_reporter ppf exn
let report_message ?usage message = !message_reporter ~usage message
