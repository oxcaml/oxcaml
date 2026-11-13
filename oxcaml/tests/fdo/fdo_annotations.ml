(* Extracts the FDO annotations from fexpr dumps (as written with [-fdo-counters
   -dfexpr-annot-after raw]), so that tests can check the counters without
   depending on the rest of the dump.

   - [list <dump>] prints every annotation of the dump, in order.

   - [compare-entries <dump a> <dump b>] prints each dump's function entry
   annotations, then those in only one of them.

   - [compare-counters <dump a> <dump b>] checks that the functions with the
   same body hash in both dumps have the same interior counters, in order.

   - [compare-sets <dump a> <dump b>] prints the counters in the annotations of
   only one of the dumps, which can also be Cmm dumps (as written with
   [-fdo-counters -dcmm]). *)

(* CR yusumez: This is done to not dump the entire fexpr. I still find this a
   bit sad. *)
(* CR yusumez: The string-wrangling here can be turned into proper parsers in
   their respective modules. Alternatively, we can use an easier-to-parse
   representation (like a sexp) *)

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

let rec skip_spaces dump i =
  if
    i < String.length dump
    && (Char.equal dump.[i] ' ' || Char.equal dump.[i] '\n')
  then skip_spaces dump (i + 1)
  else i

(* The annotation starting at [i], if any, with the index just after it. A line
   break can separate the keyword from the parenthesis. *)
let annotation_at dump i =
  List.find_map
    (fun kind ->
      let keyword = Kind.keyword kind in
      let after_keyword = i + String.length keyword in
      let paren = skip_spaces dump after_keyword in
      if
        paren < String.length dump
        && String.equal (String.sub dump i (String.length keyword)) keyword
        && Char.equal dump.[paren] '('
      then
        let start = paren + 1 in
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

let suffix s i = String.sub s i (String.length s - i)

(* The index just after the function an annotation starts with (see
   [Fdo_counter_identity.Function.to_string]): its name, discriminator and
   specializations. *)
let end_of_function contents =
  let rec digits i =
    match contents.[i] with '0' .. '9' -> digits (i + 1) | _ -> i
  in
  let rec specializations i =
    if String.starts_with ~prefix:"@(" (suffix contents i)
    then specializations (closing_paren contents (i + 2) + 1)
    else i
  in
  let name_end = end_of_string contents 0 in
  specializations (digits (name_end + 1))

(* The interior counters of the functions of a dump, by function and body hash,
   in order. Module-level calls aren't guarded by body hashes (see
   [Fdo_counter_identity]), so they are left out. *)
module Interior_counters = struct
  type t =
    { function_ : string;
      positions : string list
    }

  (* The function and body hash of an annotation, and the position of its
     counter in the body if it is an interior counter. *)
  let of_annotation ({ kind; contents } : Annotation.t) =
    let function_end = end_of_function contents in
    let function_ = String.sub contents 0 function_end in
    let rest = suffix contents function_end in
    match kind with
    | Entry ->
      let prefix = " body_hash:" in
      if String.starts_with ~prefix rest
      then Some (function_ ^ ":" ^ suffix rest (String.length prefix), None)
      else None
    | Callsite | Branch ->
      let edge = suffix rest 1 in
      if String.starts_with ~prefix:"module(" edge
      then None
      else
        let hash_end = String.index edge ':' in
        Some
          ( function_ ^ ":" ^ String.sub edge 0 hash_end,
            Some (suffix edge (hash_end + 1)) )

  let of_dump file =
    let annotations = annotations file |> List.filter_map of_annotation in
    let positions function_ =
      List.filter_map
        (fun (f, position) ->
          if String.equal f function_ then position else None)
        annotations
    in
    annotations |> List.map fst
    |> List.fold_left
         (fun functions f ->
           if List.mem f functions then functions else f :: functions)
         []
    |> List.rev_map (fun function_ ->
        { function_; positions = positions function_ })
end

let compare_counters a b =
  let counters_b = Interior_counters.of_dump b in
  let find (counters : Interior_counters.t) =
    List.find_opt
      (fun (c : Interior_counters.t) ->
        String.equal c.function_ counters.function_)
      counters_b
    |> Option.map (fun c -> counters, c)
  in
  let same, different =
    Interior_counters.of_dump a
    |> List.filter_map find
    |> List.partition
         (fun ((ca : Interior_counters.t), (cb : Interior_counters.t)) ->
           List.equal String.equal ca.positions cb.positions)
  in
  let positions file (c : Interior_counters.t) =
    "  " ^ file ^ ": " ^ String.concat " " c.positions
  in
  print_lines "same body hash, same counters:"
    (List.map (fun ((ca : Interior_counters.t), _) -> ca.function_) same);
  print_lines "same body hash, different counters:"
    (List.concat_map
       (fun ((ca : Interior_counters.t), cb) ->
         [ca.function_; positions a ca; positions b cb])
       different)

(* The counters of an annotation's contents, which are separated by spaces
   outside of names and specializations. A counter in inlined code is the
   positions of its inlining stack separated by [" <- "]. *)
let counters_of_contents contents =
  let length = String.length contents in
  let rec words i ~start ~depth acc =
    let word () = String.sub contents start (i - start) :: acc in
    if i >= length
    then List.rev (word ())
    else
      match contents.[i] with
      | '"' -> words (end_of_string contents i) ~start ~depth acc
      | '(' -> words (i + 1) ~start ~depth:(depth + 1) acc
      | ')' -> words (i + 1) ~start ~depth:(depth - 1) acc
      | ' ' when depth = 0 -> words (i + 1) ~start:(i + 1) ~depth (word ())
      | _ -> words (i + 1) ~start ~depth acc
  in
  let rec join = function
    | counter :: "<-" :: position :: rest ->
      join ((counter ^ " <- " ^ position) :: rest)
    | counter :: rest -> counter :: join rest
    | [] -> []
  in
  words 0 ~start:0 ~depth:0 []
  |> List.filter (fun word ->
      not (String.equal word "" || String.starts_with ~prefix:"body_hash:" word))
  |> join

let counters file =
  annotations file
  |> List.concat_map (fun ({ contents; _ } : Annotation.t) ->
      counters_of_contents contents)
  |> List.sort_uniq String.compare

let compare_sets a b =
  let ca = counters a and cb = counters b in
  let only_in l1 l2 = List.filter (fun c -> not (List.mem c l2)) l1 in
  print_lines ("only in " ^ a ^ ":") (only_in ca cb);
  print_lines ("only in " ^ b ^ ":") (only_in cb ca)

let () =
  match Sys.argv with
  | [| _; "list"; file |] -> list file
  | [| _; "compare-entries"; a; b |] -> compare_entries a b
  | [| _; "compare-counters"; a; b |] -> compare_counters a b
  | [| _; "compare-sets"; a; b |] -> compare_sets a b
  | _ ->
    failwith
      "usage: fdo_annotations (list <dump> | (compare-entries | \
       compare-counters | compare-sets) <dump a> <dump b>)"
