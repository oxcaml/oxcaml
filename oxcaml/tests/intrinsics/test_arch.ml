external amd64 : unit -> bool @@ portable = "%arch_is_amd64"
external arm64 : unit -> bool @@ portable = "%arch_is_arm64"


let amd64 = amd64 ()
let arm64 = arm64 ()

let () =
  if amd64 then print_endline "amd64";
  if arm64 then print_endline "arm64";
  match Sys.arch with
  | Amd64 -> assert amd64
  | Arm64 -> assert arm64
