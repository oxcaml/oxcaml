open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Wasm_of_ocaml_compiler

let binary_exn ~context ~profile =
  Fs.with_intermediate_file (Filename.temp_file "partition" ".wasm")
  @@ fun input_file ->
  Fs.with_intermediate_file (Filename.temp_file "partition-opt" ".wasm")
  @@ fun output_file ->
  Out_channel.with_open_bin input_file (fun channel ->
      Generate.wasm_output channel ~opt_source_map_file:None ~context);
  Binaryen.optimize
    ~profile
    ~opt_input_sourcemap:None
    ~input_file
    ~opt_output_sourcemap:None
    ~output_file
    ();
  Fs.read_file output_file

let fragments_module fragments ~unit_name =
  let open Javascript in
  let arguments =
    Link.build_runtime_arguments
      ~link_spec:[]
      ~separate_compilation:true
      ~missing_primitives:[]
      ~wasm_dir:"."
      ~generated_js:[ Some unit_name, fragments ]
      ~embedded_files:[]
      ()
  in
  (* Reuse the compiler's bindings for globalThis, joo_global_object, and
     jsoo_exports. *)
  let generated =
    match arguments with
    | EObj properties ->
        List.find_map properties ~f:(function
          | Property (PNS name, expression)
            when Utf8_string.equal name (Utf8_string.of_string_exn "generated") ->
              Some expression
          | _ -> None)
    | _ -> None
  in
  let generated =
    match generated with
    | Some generated -> generated
    | None -> failwith "Missing generated fragments in the runtime arguments"
  in
  let program =
    [ Export (ExportDefaultExpression generated, Parse_info.zero), N ]
    |> Driver.simplify_js
    |> Driver.name_variables
  in
  let output = Buffer.create 1024 in
  let formatter = Pretty_print.to_buffer output in
  Driver.configure formatter;
  let (_ : Source_map.info) = Js_output.program formatter program in
  Buffer.contents output

let compile code ~unit_name ~profile =
  let optimized, global_flow_data =
    Driver.optimize_for_wasm ~shapes:false ~profile code
  in
  let context = Generate.start () in
  let _toplevel_name, fragments =
    Generate.f
      ~context
      ~unit_name:(Some unit_name)
      ~live_vars:optimized.variable_uses
      ~in_cps:optimized.in_cps
      ~deadcode_sentinel:optimized.deadcode_sentinel
      ~global_flow_data
      optimized.program
  in
  let wasm = binary_exn ~context ~profile in
  wasm, fragments_module fragments ~unit_name

let startup ~profile =
  let context = Generate.start () in
  Generate.add_init_function ~context ~to_link:[ "prelude"; "partition_main" ];
  binary_exn ~context ~profile

let info =
  Info.make
    ~name:"split"
    ~doc:"Split a program into eager code and deferred roots"
    ~description:
      "wasm_of_ocaml-split compiles OCaml bytecode units into eager code and deferred \
       roots, which can each be loaded later, at most once, after the eager code. It \
       writes $(i,main.wasm), $(i,startup.wasm), one $(i,root-UNIT.wasm) per root, an ES \
       module $(i,*.fragments.mjs) per Wasm module with its fragment namespace map, and \
       $(i,report.json), which describes the outputs, the configuration and the \
       partition of the units. The runtime is built separately, with $(b,wasm_of_ocaml \
       build-runtime)."

let command =
  Jsoo_static_partition.Split_command.command
    ~info
    ~backend:(Wasm { init = Generate.init; compile; startup })
    ~effects_backends:
      (List.map Build_info.effects_backends_wasm ~f:(fun (name, effects) ->
           name, (effects :> Config.effects_backend)))
    ~default_effects:`Jspi
