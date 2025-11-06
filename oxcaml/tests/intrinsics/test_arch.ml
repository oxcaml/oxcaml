external amd64 : unit -> bool @@ portable = "%arch_amd64"
external arm64 : unit -> bool @@ portable = "%arch_arm64"


let amd64 = amd64 ()
let arm64 = arm64 ()

let () =
  if amd64 then print_endline "amd64";
  if arm64 then print_endline "arm64";
  ()
