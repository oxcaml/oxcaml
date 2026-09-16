(* TEST
   modules = "thread_name_stubs.c";
   include systhreads;
   hassysthreads;
   native;
*)

external get_current_thread_name : unit -> string =
  "thread_get_current_thread_name"

let () =
  Thread.create (fun () ->
    Thread.set_current_thread_name "thread name";
    print_endline (get_current_thread_name ())
  ) ()
  |> Thread.join
