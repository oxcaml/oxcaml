external process_and_close_fd : int -> string -> unit = "caml_process_fd"

let () =
  if Sys.win32 then
    (* Ensure the ancestor process has definitely terminated (and therefore
       closed its handles to tmp.txt) *)
    let wait_until file =
      if Sys.file_exists file then
        let fd = Unix.openfile file [O_RDWR] 0o600 in
        Unix.lockf fd Unix.F_LOCK 0;
        Unix.close fd;
        Sys.remove file
    in
    wait_until "lock.txt"

let () =
  for i = 2 to (Array.length Sys.argv) -1
  do
    process_and_close_fd (i - 1) Sys.argv.(i);
  done;
  (* For the execv version of the test, clean-up tmp.txt - for the
     Unix.create_process version, this is done by cloexec.ml *)
  if Sys.argv.(1) = "execv" then
    Sys.remove "tmp.txt"
