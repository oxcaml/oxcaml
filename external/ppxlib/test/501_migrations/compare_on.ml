let run () =
  let example_fn, ppx =
    let args = Sys.argv in
    if not (Array.length args = 3) then failwith "expected exactly two args"
    else (Array.get args 1, Array.get args 2)
  in
  let direct = "without_migrations" in
  let migrations = "with_migrations" in
  let direct_ec =
    Sys.command ("ocamlc -dparsetree " ^ example_fn ^ " 2> " ^ direct)
  in
  if direct_ec > 0 then (
    print_endline "compile error even without migrations";
    let _ = Sys.command ("cat " ^ direct) in
    ())
  else
    let migrations_ec =
      Sys.command
        ("ocamlc -dparsetree -ppx '" ^ ppx ^ " -as-ppx' " ^ example_fn ^ " 2> "
       ^ migrations)
    in
    if migrations_ec > 0 then (
      print_endline "compile error after migrations";
      let _ = Sys.command ("cat " ^ migrations) in
      ())
    else
      let _ = Sys.command ("diff -U 0 " ^ direct ^ " " ^ migrations) in
      ()

let () = run ()
