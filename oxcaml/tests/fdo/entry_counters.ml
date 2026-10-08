(* Compare the function entry counters in the -dfdo dumps of two compilations of
   the same module: print each dump's counters, then those in only one of
   them. *)
let counters file =
  In_channel.with_open_text file In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      let prefix = "  entry: [" in
      if String.starts_with ~prefix line
      then
        let s =
          String.sub line (String.length prefix)
            (String.length line - String.length prefix)
        in
        Some (String.sub s 0 (String.length s - 1))
      else None)

let () =
  match Sys.argv with
  | [| _; a; b |] ->
    let la = counters a and lb = counters b in
    let print title ls =
      print_endline title;
      List.iter (fun l -> print_endline ("  " ^ l)) ls
    in
    print (a ^ ":") la;
    print (b ^ ":") lb;
    print ("only in " ^ a ^ ":") (List.filter (fun l -> not (List.mem l lb)) la);
    print ("only in " ^ b ^ ":") (List.filter (fun l -> not (List.mem l la)) lb)
  | _ -> failwith "usage: entry_counters <dump a> <dump b>"
