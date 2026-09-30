(* Filters a -dfdo dump down to what the FDO tests compare: per function, its
   entry counter, the block frequencies and the edges with their counters and
   weights. *)

let () =
  let file =
    match Sys.argv with
    | [| _; file |] -> file
    | _ -> failwith "usage: filter_fdo_dump <dump>"
  in
  let starts prefix line = String.starts_with ~prefix line in
  let fdo = ref false in
  In_channel.with_open_text file (fun ic ->
      let rec loop () =
        match In_channel.input_line ic with
        | None -> ()
        | Some line ->
          if starts "*** FDO block frequencies" line
          then (
            fdo := true;
            print_endline line)
          else if starts "*** " line
          then fdo := false
          else if !fdo
          then print_endline line;
          loop ()
      in
      loop ())
