open Js_of_ocaml

(* Built with --no-cmis: the cmis are in main.cmis.js. *)
let () =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "require")
    [| Js.Unsafe.inject (Js.string "./main.cmis.js") |]

let () = Js_of_ocaml_toplevel.Direct.initialize ()

let execute code =
  Js_of_ocaml_toplevel.Direct.execute true Format.std_formatter code

let () =
  execute "let x = 1 + 1;;";
  execute "print_endline (string_of_int (x * 21));;";
  execute "List.map (fun i -> i * i) [ 1; 2; 3 ];;";
  Format.pp_print_flush Format.std_formatter ()
