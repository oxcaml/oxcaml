(* Compares the function entry annotations of two compilations of the same
   module. Extracts the annoations via the raw fexpr dumps obtained via
   `-drawfexpr`. Prints each dump's entries, then those in only one of them. *)

let prefix = "fdo_entry_annotation("

(* Each entry is [prefix], a quoted name (with escapes), and the rest of the
   counter up to the closing parenthesis. *)
(* CR yusumez: This is done to not dump the entire fexpr. I still find this a
   bit sad. *)
let entries file =
  let dump = In_channel.with_open_text file In_channel.input_all in
  (* The index just after the string starting at the quote at [i]. *)
  let eat_string i =
    let rec go i =
      match dump.[i] with '\\' -> go (i + 2) | '"' -> i + 1 | _ -> go (i + 1)
    in
    go (i + 1)
  in
  let rec go i acc =
    match Misc.search_substring prefix dump i with
    | i ->
      let start = i + String.length prefix in
      let string_end = eat_string start in
      let close = String.index_from dump string_end ')' in
      go (i + 1) (String.sub dump start (close - start) :: acc)
    | exception Not_found -> List.rev acc
  in
  go 0 []

let () =
  match Sys.argv with
  | [| _; a; b |] ->
    let la = entries a and lb = entries b in
    let print title ls =
      print_endline title;
      List.iter (fun l -> print_endline ("  " ^ l)) ls
    in
    let only_in l1 l2 = List.filter (fun l -> not (List.mem l l2)) l1 in
    print (a ^ ":") la;
    print (b ^ ":") lb;
    print ("only in " ^ a ^ ":") (only_in la lb);
    print ("only in " ^ b ^ ":") (only_in lb la)
  | _ -> failwith "usage: fdo_entries <dump a> <dump b>"
