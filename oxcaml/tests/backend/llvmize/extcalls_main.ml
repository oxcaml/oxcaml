let () =
  let print_and_add_res = Extcalls.call_print_and_add () in
  Out_channel.flush stdout;
  Format.printf "print_and_add res: %d\n" print_and_add_res;
  Out_channel.flush stdout;
  let too_many_res = Extcalls.call_too_many () in
  Out_channel.flush stdout;
  Format.printf "too_many res: %d\n" too_many_res;
  Out_channel.flush stdout
(* CR yusumez: Add float constants for this to work *)
(* Extcalls.call_int_and_float () *)
