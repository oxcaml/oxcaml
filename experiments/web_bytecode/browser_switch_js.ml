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

let check_string filename source =
  Browser_switch_check.check_string
    ~browser:true
    ~filename:(string_of_js_string filename)
    ~source:(string_of_js_string source)
  |> js_string

let run_string filename source =
  let output, diagnostics =
    with_output_capture (fun () ->
        Browser_switch_run.run_string
          ~browser:true
          ~filename:(string_of_js_string filename)
          ~source:(string_of_js_string source))
  in
  if String.equal diagnostics ""
  then js_string output
  else js_string (output ^ diagnostics)

let export name value =
  let global = pure_js_expr "globalThis" in
  js_set global (js_string name) value

let () =
  let obj : js_object = pure_js_expr "({})" in
  js_set obj (js_string "checkString") (wrap_callback check_string);
  js_set obj (js_string "runString") (wrap_callback run_string);
  export "WebBytecodeJs" obj
