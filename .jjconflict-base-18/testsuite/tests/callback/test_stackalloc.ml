(* TEST
 modules = "test_stackalloc_.c";
 stack-allocation;
 native;
*)

external local_stack_offset : unit -> int = "caml_local_stack_offset"

external apply10 : (int ref @ local -> unit) -> unit = "apply10"

let to_words n = n / (Sys.word_size / 8)

let () =
  apply10 (fun r ->
    Printf.printf "%d %d\n" !r (local_stack_offset () |> to_words))
