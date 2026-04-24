open Js_of_ocaml

let output_buffer : Buffer.t option ref = ref None

let append_output text =
  match !output_buffer with
  | None -> ()
  | Some buffer -> Buffer.add_string buffer text

let () =
  Sys_js.set_channel_flusher stdout append_output;
  Sys_js.set_channel_flusher stderr append_output

let ensure_toplevel_initialized =
  let initialized = ref false in
  fun () ->
    if not !initialized
    then (
      Js_of_ocaml_toplevel.JsooTop.initialize ();
      initialized := true)

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
  let output, diagnostics =
    with_output_capture (fun () ->
        Web_bytecode_check.check_string
          ~browser:true
          ~filename:(Js.to_string filename)
          ~source:(Js.to_string source))
  in
  if String.equal diagnostics ""
  then Js.string output
  else Js.string (output ^ diagnostics)

let interface_string filename source =
  let output, diagnostics =
    with_output_capture (fun () ->
        Web_bytecode_interface.interface_string
          ~browser:true
          ~filename:(Js.to_string filename)
          ~source:(Js.to_string source))
  in
  if String.equal diagnostics ""
  then Js.string output
  else Js.string (output ^ diagnostics)

let run_string filename source =
  let output, result =
    with_output_capture (fun () ->
        try
          ensure_toplevel_initialized ();
          `Diagnostics
            (Web_bytecode_run.run_string
               ~browser:true
               ~filename:(Js.to_string filename)
               ~source:(Js.to_string source))
        with exn -> `Exception exn)
  in
  match result with
  | `Diagnostics diagnostics ->
      if String.equal diagnostics ""
      then Js.string output
      else Js.string (output ^ diagnostics)
  | `Exception exn -> Js.string (output ^ Printexc.to_string exn)

let utop_string filename source =
  let output, result =
    with_output_capture (fun () ->
        try
          ensure_toplevel_initialized ();
          `Diagnostics
            (Web_bytecode_run.utop_string
               ~browser:true
               ~filename:(Js.to_string filename)
               ~source:(Js.to_string source))
        with exn -> `Exception exn)
  in
  match result with
  | `Diagnostics diagnostics ->
      if String.equal diagnostics ""
      then Js.string output
      else Js.string (output ^ diagnostics)
  | `Exception exn -> Js.string (output ^ Printexc.to_string exn)

let () =
  Js.export
    "WebBytecodeJs"
    (Js.Unsafe.obj
       [| "checkString",
          Js.Unsafe.inject (Js.wrap_callback check_string);
          "interfaceString",
          Js.Unsafe.inject (Js.wrap_callback interface_string);
          "runString",
          Js.Unsafe.inject (Js.wrap_callback run_string);
          "utopString",
          Js.Unsafe.inject (Js.wrap_callback utop_string) |])
