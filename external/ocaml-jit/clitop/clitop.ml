type input = Stdin | In_file of string

let named f arg_term = Cmdliner.Term.(const f $ arg_term)

let input =
  let doc =
    "The path to the file containing the toplevel phrase to evaluate, pass \
     $(b,-) to read from stdin instead"
  in
  let docv = "TOPLEVEL_PHRASE" in
  let input =
    let parse = function "-" -> `Ok Stdin | s -> `Ok (In_file s) in
    let print fmt = function
      | Stdin -> Format.fprintf fmt "stdin"
      | In_file s -> Format.fprintf fmt "%s" s
    in
    (parse, print)
  in
  named
    (fun x -> `Input x)
    Cmdliner.Arg.(value & pos 0 (some input) None & info ~doc ~docv [])

let keep_asm_files =
  let doc =
    "Keep temporary files such as the assembler, binary assembler and dll.\n\
    \ Only meaningful for native toplevel using the external toolchain."
  in
  named
    (fun x -> `Keep_asm_files x)
    Cmdliner.Arg.(value & flag & info ~doc [ "k"; "keep-asm-files" ])

let eval_script ~eval_phrase input =
  let lexbuf =
    match input with
    | Stdin -> Lexing.from_channel stdin
    | In_file file ->
        let ic = open_in file in
        Lexing.from_channel ic
  in
  let rec aux () =
    try
      let phrase = Parse.toplevel_phrase lexbuf in
      let _ = eval_phrase true Format.std_formatter phrase in
      aux ()
    with
    | End_of_file -> ()
    | x -> Location.report_exception Format.std_formatter x
  in
  aux ()

let run ~eval_phrase ~loop (`Keep_asm_files keep_asm_files) (`Input input) =
  Clflags.keep_asm_file := keep_asm_files;
  match input with
  | None -> loop Format.std_formatter
  | Some input -> eval_script ~eval_phrase input

let term ~eval_phrase ~loop =
  Cmdliner.Term.(const (run ~eval_phrase ~loop) $ keep_asm_files $ input)

let info ~name =
  let doc = "Run the given OCaml toplevel phrase" in
  Cmdliner.Term.info ~doc name

let main ~name ~eval_phrase ~loop () =
  let term = term ~eval_phrase ~loop in
  let info = info ~name in
  let ret = Cmdliner.Term.eval (term, info) in
  Cmdliner.Term.exit ret
