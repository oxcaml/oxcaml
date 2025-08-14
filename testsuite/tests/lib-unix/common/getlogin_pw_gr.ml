(* TEST
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

let () =
  let login =
    try Unix.getlogin () with
    | Unix.Unix_error ((ENOTTY | ENOENT), _, _) -> "root"
  in
  try
    let pw_for_login = Unix.getpwnam login in
    let pw_for_uid = Unix.getpwuid pw_for_login.pw_uid in
    let gr_for_gid = Unix.getgrgid pw_for_uid.pw_gid in
    let _gr_for_name = Unix.getgrnam gr_for_gid.gr_name in
    print_endline "OK"
  with Not_found ->
    print_endline "OK"
;;
