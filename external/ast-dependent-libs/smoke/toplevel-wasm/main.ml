(* There is no wasm counterpart of the .cmis.js bundle: the toplevel reads the
   compiler's stdlib cmis from disk. *)
let () = Js_of_ocaml_toplevel.Direct.initialize ()

let () =
  Load_path.reset ();
  Topdirs.dir_directory (Sys.getenv "TOPLEVEL_CMIS")

let execute code =
  Js_of_ocaml_toplevel.Direct.execute true Format.std_formatter code

let () =
  execute "let x = 1 + 1;;";
  execute "print_endline (string_of_int (x * 21));;";
  execute "List.map (fun i -> i * i) [ 1; 2; 3 ];;";
  Format.pp_print_flush Format.std_formatter ()
