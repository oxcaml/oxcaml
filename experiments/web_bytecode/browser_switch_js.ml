type js_string
type js_callback
type js_object
external js_string : string -> js_string = "caml_jsstring_of_string"
external string_of_js_string : js_string -> string = "caml_string_of_jsstring"
external wrap_callback : 'a -> js_callback = "caml_js_wrap_callback"
external set_channel_output : out_channel -> js_callback -> unit
  = "caml_ml_set_channel_output"
external pure_js_expr : string -> 'a = "caml_pure_js_expr"
external js_set : 'a -> js_string -> 'b -> unit = "caml_js_set"

let output_buffer : Buffer.t option ref = ref None

let append_output text =
  let current = !output_buffer in
  match current with
  | None -> ()
  | Some buffer -> Buffer.add_string buffer (string_of_js_string text)

let ends_with ~suffix text =
  let text_length = String.length text in
  let suffix_length = String.length suffix in
  text_length >= suffix_length
  && String.sub text (text_length - suffix_length) suffix_length = suffix

let normalize_captured_filenames ~filename text =
  let basename = Filename.basename filename in
  let marker = "_" ^ basename in
  let buffer = Buffer.create (String.length text) in
  let rec loop start =
    match String.index_from_opt text start '"' with
    | None ->
        Buffer.add_substring buffer text start (String.length text - start)
    | Some quote_start -> (
        Buffer.add_substring buffer text start (quote_start - start);
        match String.index_from_opt text (quote_start + 1) '"' with
        | None ->
            Buffer.add_substring buffer text quote_start
              (String.length text - quote_start)
        | Some quote_end ->
            let quoted =
              String.sub text (quote_start + 1) (quote_end - quote_start - 1)
            in
            Buffer.add_char buffer '"';
            if ends_with ~suffix:marker quoted
            then Buffer.add_string buffer filename
            else Buffer.add_string buffer quoted;
            Buffer.add_char buffer '"';
            loop (quote_end + 1))
  in
  loop 0;
  Buffer.contents buffer

let () =
  Printexc.record_backtrace true;
  set_channel_output stdout (wrap_callback append_output);
  set_channel_output stderr (wrap_callback append_output)

let with_output_capture f =
  let buffer = Buffer.create 256 in
  let previous = !output_buffer in
  output_buffer := Some buffer;
  let result =
    Fun.protect f ~finally:(fun () ->
        flush_all ();
        output_buffer := previous)
  in
  Buffer.contents buffer, result

let make_result kind =
  let obj : js_object = pure_js_expr "({})" in
  js_set obj (js_string "kind") (js_string kind);
  obj

let ok_result output =
  let obj = make_result "ok" in
  js_set obj (js_string "output") (js_string output);
  obj

let missing_cmi_result filename =
  let obj = make_result "missing_cmi" in
  js_set obj (js_string "filename") (js_string filename);
  obj

let check_string filename source =
  try
    let filename = string_of_js_string filename in
    let output, diagnostics =
      with_output_capture (fun () ->
          Browser_switch_check.check_string
            ~browser:true
            ~filename
            ~source:(string_of_js_string source))
    in
    let output = normalize_captured_filenames ~filename output in
    if String.equal diagnostics ""
    then ok_result output
    else ok_result (output ^ diagnostics)
  with
  | Browser_switch_common.Missing_cmi filename -> missing_cmi_result filename

let interface_string filename source =
  try
    let filename = string_of_js_string filename in
    let output, diagnostics =
      with_output_capture (fun () ->
          Browser_switch_interface.interface_string
            ~browser:true
            ~filename
            ~source:(string_of_js_string source))
    in
    let output = normalize_captured_filenames ~filename output in
    if String.equal diagnostics ""
    then ok_result output
    else ok_result (output ^ diagnostics)
  with
  | Browser_switch_common.Missing_cmi filename -> missing_cmi_result filename

let run_string filename source =
  try
    let filename = string_of_js_string filename in
    let output, diagnostics =
      with_output_capture (fun () ->
          Browser_switch_run.run_string
            ~browser:true
            ~filename
            ~source:(string_of_js_string source))
    in
    let output = normalize_captured_filenames ~filename output in
    if String.equal diagnostics ""
    then ok_result output
    else ok_result (output ^ diagnostics)
  with
  | Browser_switch_common.Missing_cmi filename -> missing_cmi_result filename

let utop_string filename source =
  try
    let filename = string_of_js_string filename in
    let output, diagnostics =
      with_output_capture (fun () ->
          Browser_switch_run.utop_string
            ~browser:true
            ~filename
            ~source:(string_of_js_string source))
    in
    let output = normalize_captured_filenames ~filename output in
    if String.equal diagnostics ""
    then ok_result output
    else ok_result (output ^ diagnostics)
  with
  | Browser_switch_common.Missing_cmi filename -> missing_cmi_result filename

let export name value =
  let global = pure_js_expr "globalThis" in
  js_set global (js_string name) value

let () =
  let obj : js_object = pure_js_expr "({})" in
  js_set obj (js_string "checkString") (wrap_callback check_string);
  js_set obj (js_string "interfaceString") (wrap_callback interface_string);
  js_set obj (js_string "runString") (wrap_callback run_string);
  js_set obj (js_string "utopString") (wrap_callback utop_string);
  export "WebBytecodeJs" obj
