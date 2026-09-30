(* Filters a -dcfg -dfdo dump down to what the FDO tests compare: the -dfdo
   lines (per function: its entry counter, the block frequencies, the edges with
   their counters and weights, and the call graph edges) and the block order of
   each "After cfg_fdo_layout" section. *)

let () =
  let file =
    match Sys.argv with
    | [| _; file |] -> file
    | _ -> failwith "usage: filter_fdo_dump <dump>"
  in
  let starts prefix line = String.starts_with ~prefix line in
  let section = ref `Other in
  In_channel.with_open_text file (fun ic ->
      let rec loop () =
        match In_channel.input_line ic with
        | None -> ()
        | Some line ->
          (if starts "*** FDO block frequencies" line
           then (
             section := `Fdo;
             print_endline line)
           else if starts "*** " line
           then
             section
               := if String.equal line "*** After cfg_fdo_layout"
                  then `Layout
                  else `Other
           else
             match !section with
             | `Fdo -> print_endline line
             | `Layout -> (
               if starts "cfg for " line
               then print_endline line
               else if starts "block " line
               then
                 match String.split_on_char ' ' line with
                 | "block" :: block :: _ -> Printf.printf "  block %s\n" block
                 | _ -> failwith ("cannot parse block line: " ^ line))
             | `Other -> ());
          loop ()
      in
      loop ())
