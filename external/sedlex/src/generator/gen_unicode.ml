(* This file generates unicode data from
 * the files exported at https://www.unicode.org/Public/<unicode version>
 * and stored at src/generator/data. *)
open Sedlex_utils
module SSet = Set.Make (String)

let target = Sys.argv.(1)
let categories = Hashtbl.create 1024
let labels = Hashtbl.create 1024

(* Drop comments and split semi-column separated fields *)
let parse_line l =
  let l =
    match String.index_opt l '#' with None -> l | Some i -> String.sub l 0 i
  in
  String.split_on_char ';' l |> List.map String.trim

let parse_code s =
  try int_of_string (Printf.sprintf "0x%s" s)
  with _ -> failwith (Printf.sprintf "invalid code %s" s)

let parse_category x = String.lowercase_ascii (String.trim x)
let parse_prop x = String.lowercase_ascii (String.trim x)

let parse_interval s =
  match String.split_on_char '.' (String.trim s) with
    | [] -> assert false
    | [x] ->
        let x = parse_code x in
        Cset.singleton x
    | [x; ""; y] ->
        let x = parse_code x and y = parse_code y in
        Cset.interval x y
    | _ -> failwith (Printf.sprintf "invalid interval %s" s)

let print_elements ch hashtbl cats =
  let cats_set = SSet.of_list cats in
  let all_keys = SSet.of_seq (Hashtbl.to_seq_keys hashtbl) in
  let missing = SSet.diff cats_set all_keys in
  let ignoring = SSet.diff all_keys cats_set in
  let len = List.length cats in
  List.iter
    (fun c ->
      let entries =
        List.map
          (fun (b, e) -> Printf.sprintf "0x%x, 0x%x" b e)
          (Cset.union_list (Hashtbl.find_all hashtbl c) :> (int * int) list)
      in
      Printf.fprintf ch "  let %s = Sedlex_cset.of_list\n    [" c;
      List.iteri
        (fun i x ->
          if i > 0 then
            if i mod 5 = 0 then Printf.fprintf ch ";\n     "
            else Printf.fprintf ch "; ";
          Printf.fprintf ch "%s" x)
        entries;
      Printf.fprintf ch "]\n\n")
    cats;
  Printf.fprintf ch "  let list = [\n";
  List.iteri
    (fun pos c ->
      Printf.fprintf ch "    (%S, %s)%s\n" c c
        (if pos == len - 1 then "" else ";"))
    cats;
  Printf.fprintf ch "  ]\n\n";
  if not (SSet.is_empty ignoring) then (
    Printf.fprintf ch "(* ignoring:\n";
    SSet.iter (fun s -> Printf.fprintf ch "  - %s\n" s) ignoring;
    Printf.fprintf ch "*)\n");
  if not (SSet.is_empty missing) then (
    Printf.fprintf ch "(* missing:\n";
    SSet.iter (fun s -> Printf.fprintf ch "  - %s\n" s) missing;
    Printf.fprintf ch "*)\n")

let files =
  [
    ( "PropList.txt",
      fun s ->
        match parse_line s with
          | [""] -> ()
          | [interval; prop] ->
              let interval = parse_interval interval in
              let prop = parse_prop prop in
              Hashtbl.add labels prop interval
          | _ -> assert false );
    ( "DerivedCoreProperties.txt",
      fun s ->
        match parse_line s with
          | [""] -> ()
          | [interval; prop] ->
              let interval = parse_interval interval in
              let prop = parse_prop prop in
              Hashtbl.add labels prop interval
          | [_interval; "InCB"; ("Extend" | "Consonant" | "Linker")] ->
              (* TODO: support non-binary properties? *)
              ()
          | _ -> assert false );
    ( "DerivedGeneralCategory.txt",
      fun s ->
        match parse_line s with
          | [""] -> ()
          | [interval; cat] ->
              let interval = parse_interval interval in
              let cat = parse_category cat in
              Hashtbl.add categories cat interval
          | _ -> assert false );
    ( "UnicodeData.txt",
      fun s ->
        match parse_line s with
          | [""] -> ()
          | interval :: _ :: cat :: _ ->
              let interval = parse_interval interval in
              let cat = parse_category cat in
              Hashtbl.add categories cat interval
          | _ -> assert false );
  ]

let read_version fname =
  let version_rex =
    Str.regexp "^# PropList-\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\\.txt"
  in
  let ch = open_in_bin fname in
  let s = input_line ch in
  close_in ch;
  ignore (Str.string_match version_rex s 0);
  Str.matched_group 1 s

let exported_categories =
  [
    "cc";
    "cf";
    "cn";
    "co";
    "cs";
    "ll";
    "lm";
    "lo";
    "lt";
    "lu";
    "mc";
    "me";
    "mn";
    "nd";
    "nl";
    "no";
    "pc";
    "pd";
    "pe";
    "pf";
    "pi";
    "po";
    "ps";
    "sc";
    "sk";
    "sm";
    "so";
    "zl";
    "zp";
    "zs";
  ]

let exported_properties =
  [
    "alphabetic";
    "ascii_hex_digit";
    "hex_digit";
    "id_continue";
    "id_start";
    "lowercase";
    "math";
    "other_alphabetic";
    "other_lowercase";
    "other_math";
    "other_uppercase";
    "uppercase";
    "white_space";
    "xid_continue";
    "xid_start";
  ]

let () =
  let base_dir =
    Filename.concat (Filename.dirname Sys.executable_name) "data"
  in
  let version = read_version (Filename.concat base_dir "PropList.txt") in
  List.iter
    (fun (fname, fn) ->
      let ch = open_in_bin (Filename.concat base_dir fname) in
      try
        while true do
          let ret = input_line ch in
          fn ret
        done
      with End_of_file -> close_in ch)
    files;
  let ch = open_out_bin target in
  Printf.fprintf ch {|[@@@ocamlformat "disable"]|};
  Printf.fprintf ch "\n\n";
  Printf.fprintf ch
    "(* This file was automatically generated, do not edit. *)\n";
  Printf.fprintf ch "(* Edit gen_unicode.ml.inc instead. *)\n\n";
  Printf.fprintf ch "\n\nlet version = %S\n\n" version;
  Printf.fprintf ch "module Categories = struct\n\n";
  print_elements ch categories exported_categories;
  Printf.fprintf ch "end\n\n";
  Printf.fprintf ch "module Properties = struct\n\n";
  print_elements ch labels exported_properties;
  Printf.fprintf ch "end\n";
  close_out ch
