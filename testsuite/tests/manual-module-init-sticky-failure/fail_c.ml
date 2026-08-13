let () = Printf.printf "Fail_c: initializing\n%!"
let () =
  match !(Fail_config.state) with
  | Broken _ | Not_ready as reason ->
    raise (Fail_config.Invalid_config reason)
  | Ready -> ()

let initialized = true
