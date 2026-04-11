open Js_of_ocaml

let unsupported_run = "__web_bytecode_wasm_run_unsupported__"

let check_string filename source =
  Browser_switch_check.check_string
    ~browser:true
    ~filename:(Js.to_string filename)
    ~source:(Js.to_string source)
  |> Js.string

let run_string _filename _source = Js.string unsupported_run

let () =
  Js.export
    "WebBytecodeWasm"
    (Js.Unsafe.obj
       [| "checkString",
          Js.Unsafe.inject (Js.wrap_callback check_string);
          "runString",
          Js.Unsafe.inject (Js.wrap_callback run_string) |])
