(* TEST
   flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
   include systhreads;
   runtime5;
   hassysthreads;
   multidomain;
   { bytecode; }
   { native; }
*)

(*
   This test checks that signals are masked in all threads that are not part of the main
   domain
*)

let get_sigmask () = Thread.sigmask SIG_BLOCK []
let string_of_sigmask () =
  match get_sigmask () with
  | [] -> "<empty>"
  | sigmask ->
    sigmask
    |> List.sort (Int.compare)
    |> List.map string_of_int
    |> String.concat ", "
let print_sigmask prefix =
  Printf.printf "%s %s\n" prefix (string_of_sigmask ())

let () = print_sigmask "sigmask from toplevel:"

let () =
  Thread.create
    (fun () -> print_sigmask "sigmask from thread in main domain:")
    ()
  |> Thread.join

let () =
  Domain.spawn
    (fun () ->
       print_sigmask "sigmask from main thread in child domain:";
       Thread.create
         (fun () -> print_sigmask "sigmask from other thread in child domain:")
         ()
       |> Thread.join
    )
  |> Domain.join

(* Make sure we properly cleared the mask after spawning the thread *)

let () = print_sigmask "sigmask from toplevel again:"

let () =
  Thread.create
    (fun () -> print_sigmask "sigmask from thread in main domain again:")
    ()
  |> Thread.join
