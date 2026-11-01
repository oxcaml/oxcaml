(* The parts of the output of [ocamlobjinfo -no-approx -no-code] on .cmx and
   .cmxa files that describe how a library was built: its units in link
   order, what each imports, and whether it was archived with -linkall. *)

type unit_ =
  { name : string;  (* the compilation unit, e.g. Stdlib__List *)
    interfaces_imported : string list;
    implementations_imported : string list;
    force_link : bool
  }

let starts_with ~prefix s = String.starts_with ~prefix s

let suffix ~prefix s =
  String.sub s (String.length prefix) (String.length s - String.length prefix)

(* "\t<crc>\t<name>", where <crc> may be "-" *)
let imported_name line =
  match List.filter (( <> ) "") (String.split_on_char '\t' line) with
  | [] -> failwith ("ocamlobjinfo: malformed import line: " ^ line)
  | fields -> List.nth fields (List.length fields - 1)

let parse_lines lines =
  let units = ref [] and current = ref None and section = ref `Other in
  let finish () =
    Option.iter (fun u -> units := u :: !units) !current;
    current := None
  in
  let update f =
    match !current with
    | None -> failwith "ocamlobjinfo: field before the first Name:"
    | Some u -> current := Some (f u)
  in
  List.iter
    (fun line ->
      if starts_with ~prefix:"Name: " line then begin
        finish ();
        current :=
          Some
            { name = suffix ~prefix:"Name: " line;
              interfaces_imported = [];
              implementations_imported = [];
              force_link = false
            };
        section := `Other
      end
      else if line = "Interfaces imported:" then section := `Interfaces
      else if line = "Implementations imported:" then
        section := `Implementations
      else if starts_with ~prefix:"Force link: " line then begin
        update (fun u ->
          { u with force_link = suffix ~prefix:"Force link: " line = "YES" });
        section := `Other
      end
      else if line <> "" && line.[0] = '\t' then begin
        match !section with
        | `Interfaces ->
          update (fun u ->
            { u with
              interfaces_imported = imported_name line :: u.interfaces_imported
            })
        | `Implementations ->
          update (fun u ->
            { u with
              implementations_imported =
                imported_name line :: u.implementations_imported
            })
        | `Other -> ()
      end
      else section := `Other)
    lines;
  finish ();
  List.rev !units

let parse_file filename =
  In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n' |> parse_lines
