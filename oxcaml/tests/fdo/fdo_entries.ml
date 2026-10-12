(* Compare the function entry counters in the -drawflambda dumps of two
   compilations of the same module: print each dump's entries, then those in
   only one of them. *)

let entries file =
  let dump = In_channel.with_open_text file In_channel.input_all in
  (* The printer breaks lines freely: collapse whitespace. *)
  let dump =
    String.split_on_char '\n' dump
    |> List.concat_map (String.split_on_char ' ')
    |> List.filter (fun s -> s <> "")
    |> String.concat " "
  in
  let find_from prefix pos =
    let rec go i =
      if i + String.length prefix > String.length dump
      then None
      else if String.sub dump i (String.length prefix) = prefix
      then Some (i + String.length prefix)
      else go (i + 1)
    in
    go pos
  in
  (* The text up to the parenthesis closing the one before [pos]; names are
     quoted strings and may contain parentheses. *)
  let until_close pos =
    let rec go i depth in_string =
      match dump.[i] with
      | '\\' when in_string -> go (i + 2) depth in_string
      | '"' -> go (i + 1) depth (not in_string)
      | _ when in_string -> go (i + 1) depth in_string
      | '(' -> go (i + 1) (depth + 1) in_string
      | ')' when depth = 0 -> i
      | ')' -> go (i + 1) (depth - 1) in_string
      | _ -> go (i + 1) depth in_string
    in
    let close = go pos 0 false in
    String.sub dump pos (close - pos), close
  in
  let rec go pos acc =
    match find_from "(fdo_entry_annotation (" pos with
    | None -> List.rev acc
    | Some pos ->
      let counters, pos = until_close pos in
      go pos (counters :: acc)
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
