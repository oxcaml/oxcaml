(* Extracts the FDO annotations from fexpr dumps (as written with [-fdo-counters
   -dfexpr-annot-after raw]), so that tests can check the counters without
   depending on the rest of the dump.

   - [list <dump>] prints every annotation of the dump, in order. -
   [compare-entries <dump a> <dump b>] prints each dump's function entry
   annotations, then those in only one of them. *)

(* CR yusumez: This is done to not dump the entire fexpr. I still find this a
   bit sad. *)

module Kind = struct
  type t =
    | Entry
    | Callsite
    | Branch

  let all = [Entry; Callsite; Branch]

  (* What precedes the parenthesised contents of an annotation of this kind. *)
  let keyword = function
    | Entry -> "fdo_entry_annotation"
    | Callsite -> "callsite_counter"
    | Branch -> "fdo_annotation"

  let to_string = function
    | Entry -> "entry"
    | Callsite -> "call"
    | Branch -> "branch"
end

module Annotation = struct
  type t =
    { kind : Kind.t;
      contents : string
    }

  let to_string { kind; contents } = Kind.to_string kind ^ ": " ^ contents
end

(* The index just after the string literal starting at [i]. *)
let end_of_string dump i =
  let rec go i =
    match dump.[i] with '\\' -> go (i + 2) | '"' -> i + 1 | _ -> go (i + 1)
  in
  go (i + 1)

(* The index of the parenthesis closing the one just before [i]. Contents can
   contain parentheses (e.g. in specializations and module-level calls) and
   string literals (names). *)
let closing_paren dump i =
  let rec go i ~depth =
    match dump.[i] with
    | '"' -> go (end_of_string dump i) ~depth
    | '(' -> go (i + 1) ~depth:(depth + 1)
    | ')' -> if depth = 0 then i else go (i + 1) ~depth:(depth - 1)
    | _ -> go (i + 1) ~depth
  in
  go i ~depth:0

(* The dump is printed with line breaks and indentation, which can fall inside
   an annotation. *)
let normalize_spaces s =
  String.split_on_char '\n' s
  |> List.map String.trim
  |> List.filter (fun s -> not (String.equal s ""))
  |> String.concat " "

(* The annotation starting at [i], if any, with the index just after it. *)
let annotation_at dump i =
  List.find_map
    (fun kind ->
      let prefix = Kind.keyword kind ^ "(" in
      let start = i + String.length prefix in
      if
        start <= String.length dump
        && String.equal (String.sub dump i (String.length prefix)) prefix
      then
        let close = closing_paren dump start in
        let contents =
          normalize_spaces (String.sub dump start (close - start))
        in
        Some ({ Annotation.kind; contents }, close + 1)
      else None)
    Kind.all

let annotations file =
  let dump = In_channel.with_open_text file In_channel.input_all in
  let rec go i acc =
    if i >= String.length dump
    then List.rev acc
    else if Char.equal dump.[i] '"'
    then go (end_of_string dump i) acc
    else
      match annotation_at dump i with
      | Some (annotation, next) -> go next (annotation :: acc)
      | None -> go (i + 1) acc
  in
  go 0 []

let print_lines title lines =
  print_endline title;
  List.iter (fun line -> print_endline ("  " ^ line)) lines

let list file =
  annotations file |> List.map Annotation.to_string |> print_lines (file ^ ":")

let entries file =
  annotations file
  |> List.filter_map (fun ({ kind; contents } : Annotation.t) ->
      match kind with Entry -> Some contents | Callsite | Branch -> None)

let compare_entries a b =
  let la = entries a and lb = entries b in
  let only_in l1 l2 = List.filter (fun l -> not (List.mem l l2)) l1 in
  print_lines (a ^ ":") la;
  print_lines (b ^ ":") lb;
  print_lines ("only in " ^ a ^ ":") (only_in la lb);
  print_lines ("only in " ^ b ^ ":") (only_in lb la)

let () =
  match Sys.argv with
  | [| _; "list"; file |] -> list file
  | [| _; "compare-entries"; a; b |] -> compare_entries a b
  | _ ->
    failwith
      "usage: fdo_annotations (list <dump> | compare-entries <dump a> <dump b>)"
