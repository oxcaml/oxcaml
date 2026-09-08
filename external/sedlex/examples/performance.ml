let rec token buf =
  match%sedlex buf with any -> token buf | eof -> () | _ -> assert false

let time f x =
  let rec acc f x = function
    | 0 -> f x
    | n ->
        f x |> ignore;
        acc f x (n - 1)
  in
  let t = Sys.time () in
  let fx = acc f x 10 in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  fx

let () =
  let long_str = String.make 1000000 '\n' in
  let token_from _ =
    let lexbuf = Sedlexing.Latin1.from_string long_str in
    (* let () = Sedlexing.set_curr_p lexbuf Lexing.dummy_pos in *)
    token lexbuf
  in
  time token_from long_str
