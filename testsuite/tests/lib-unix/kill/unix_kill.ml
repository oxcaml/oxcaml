(* TEST
 include unix;
<<<<<<< HEAD
 flags = "-alert -unsafe_multidomain";
 libunix;
||||||| 23e84b8c4d
 libunix;
=======
 hasunix;
 not-target-windows;
 (*
   Disabled on MacOS amd64 with TSan due to a
   possible infinite signal loop with TSan under MacOS
   see https://github.com/llvm/llvm-project/issues/63824
 *)
 not_macos_amd64_tsan;
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
 {
   bytecode;
 }{
   native;
 }
*)

let () =
  let r = ref false in
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> r := true));
  Unix.kill (Unix.getpid ()) Sys.sigint;
  let x = !r in
  Printf.printf "%b " x;
  Printf.printf "%b\n" !r

let () =
  let r = ref false in
  let _ = Unix.sigprocmask SIG_BLOCK [Sys.sigint] in
  Sys.set_signal Sys.sigint (Signal_handle (fun _ -> r := true));
  Unix.kill (Unix.getpid ()) Sys.sigint;
  Gc.full_major ();
  let a = !r in
  let _ = Unix.sigprocmask SIG_UNBLOCK [Sys.sigint] in
  let b = !r in
  Printf.printf "%b %b " a b;
  Printf.printf "%b\n" !r
