module Protocol = Structured_diagnostic_protocol

let flag = "-structured-diagnostics"
let converter_attempts = 5

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

let emit ppf (diagnostic : Protocol.Raw.diagnostic) =
  Format.fprintf ppf "%s@."
    (without_structural_newlines (Protocol.Raw.diagnostic_to_json diagnostic))

let emit_diagnostic ppf diagnostic =
  emit ppf (Structured_diagnostic.to_raw_diagnostic diagnostic)

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
