type outputs = Emit.outputs =
  { ml : string;
    mli : string
  }

let report_warnings (model : Model.t) =
  List.iter
    (fun (warning : Model.warning) ->
      prerr_endline
        (Printf.sprintf
           "warning: %s"
           (Error.to_string (warning.loc, warning.message))))
    model.warnings

let root_module_of_path path =
  let base = Filename.basename path in
  let stem =
    try Filename.chop_extension base with
    | Invalid_argument _ -> base
  in
  let module_name = String.capitalize_ascii stem in
  if not (Name.is_module_name module_name)
  then invalid_arg ("invalid OCaml module basename for generated output: " ^ path);
  module_name

let render_string ?root_module ~input_name source =
  let ast = Parse.from_string ~input_name source in
  let model = Model.resolve ast in
  let root_module =
    match root_module with
    | Some root_module -> root_module
    | None -> root_module_of_path input_name
  in
  Emit.render ~root_module model

let resolve_and_render_string ?root_module ~input_name source =
  let ast = Parse.from_string ~input_name source in
  let model = Model.resolve ast in
  let root_module =
    match root_module with
    | Some root_module -> root_module
    | None -> root_module_of_path input_name
  in
  model, Emit.render ~root_module model

let render_file ?root_module input_path =
  let source = In_channel.with_open_bin input_path In_channel.input_all in
  render_string ?root_module ~input_name:input_path source

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let generate_to_files ~input_path ~ml_path ~mli_path () =
  let root_module = root_module_of_path ml_path in
  let source = In_channel.with_open_bin input_path In_channel.input_all in
  let model, outputs =
    resolve_and_render_string ~root_module ~input_name:input_path source
  in
  report_warnings model;
  write_file ml_path outputs.ml;
  write_file mli_path outputs.mli
