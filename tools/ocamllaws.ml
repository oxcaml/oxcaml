(******************************************************************************
 *                                  OxCaml                                    *
 *                          Simon Spies, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* Command-line access to the laws that compiled units record. *)

let bad ~prog ~spec ~usage msg =
  let usage = Arg.usage_string spec usage in
  raise (Arg.Bad (Printf.sprintf "%s: %s\n%s" prog msg usage))

type format = Laws_file

let formats = ["laws-file", Laws_file]

type object_file =
  | Cmo
  | Cmx

let object_file_of_filename file =
  if Filename.check_suffix file ".cmo"
  then Some Cmo
  else if Filename.check_suffix file ".cmx"
  then Some Cmx
  else None

let laws_of_object_file object_file file =
  match object_file with
  | Cmo -> (Cmo_io.read_cmo file).cu_laws
  | Cmx ->
    let ui, _crc = Compilenv.read_unit_info file in
    Option.map (File_sections.get ui.ui_file_sections) ui.ui_laws

let extract_law_file ~output object_file file =
  match laws_of_object_file object_file file with
  | None ->
    Location.raise_errorf ~loc:(Location.in_file file)
      "%a records no laws file" Location.Doc.filename file
  | Some laws -> (
    let contents = Laws_data.Serializable.contents laws in
    match output with
    | None -> print_string contents
    | Some output ->
      Out_channel.with_open_bin output (fun oc ->
          Out_channel.output_string oc contents;
          Out_channel.close oc))

let extract ~prog argv =
  let format = ref None in
  let output = ref None in
  let files = ref [] in
  let usage =
    Printf.sprintf
      "Usage: %s -format <format> [-o <file>] <unit>.cmo|<unit>.cmx\n\
       Extract the laws of a compiled unit.\n\
       Options are:"
      prog
  in
  let spec =
    Arg.align
      [ ( "-format",
          Arg.Symbol
            ( List.map fst formats,
              fun s -> format := Some (List.assoc s formats) ),
          " Which laws to extract" );
        ( "-o",
          Arg.String (fun file -> output := Some file),
          "<file> Write to <file> instead of standard output" ) ]
  in
  let fail msg = bad ~prog ~spec ~usage msg in
  Arg.parse_argv ~current:(ref 0) argv spec
    (fun file -> files := file :: !files)
    usage;
  match !format, !files with
  | None, _ -> fail "expected -format"
  | Some Laws_file, [file] -> (
    match object_file_of_filename file with
    | Some object_file -> extract_law_file ~output:!output object_file file
    | None -> fail (Printf.sprintf "%s is not a .cmo or .cmx file" file))
  | Some Laws_file, _ -> fail "expected exactly one object file"

let commands = ["extract", (extract, "Extract the laws of a compiled unit")]

let usage =
  "Usage: ocamllaws <command> [<options>]\nCommands are:\n"
  ^ String.concat ""
      (List.map
         (fun (name, (_, doc)) -> Printf.sprintf "  %s  %s\n" name doc)
         commands)
  ^ "Run 'ocamllaws <command> -help' for the options of a command.\n"

let main () =
  match Array.to_list Sys.argv with
  | _ :: ("-help" | "--help") :: _ -> raise (Arg.Help usage)
  | _ :: name :: _ -> (
    match List.assoc_opt name commands with
    | Some (run, _) ->
      let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
      let prog = "ocamllaws " ^ name in
      argv.(0) <- prog;
      run ~prog argv
    | None ->
      let msg = Printf.sprintf "ocamllaws: unknown command %s\n%s" name usage in
      raise (Arg.Bad msg))
  | _ -> raise (Arg.Bad ("ocamllaws: expected a command\n" ^ usage))

let () =
  try main () with
  | Arg.Help msg ->
    print_string msg;
    exit 0
  | Arg.Bad msg ->
    prerr_string msg;
    exit 2
  | exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
