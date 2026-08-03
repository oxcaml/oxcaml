type js_string

external js_string : string -> js_string = "caml_jsstring_of_string"
external string_of_js_string : js_string -> string = "caml_string_of_jsstring"
external reset_trace_primitive : unit -> unit = "doxResetTrace"
external read_trace_primitive : unit -> js_string = "doxReadTrace"
external set_env_primitive : js_string -> js_string -> unit = "doxSetEnv"

let reset_trace () = reset_trace_primitive ()
let read_trace () = read_trace_primitive () |> string_of_js_string
let set_env name value = set_env_primitive (js_string name) (js_string value)
