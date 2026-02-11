(* Module that raises an exception during initialization *)
let () = Printf.printf "Raises: initializing\n%!"
let () = failwith "Raises: deliberate failure during init"
