let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

(* Function that takes a mutable reference as parameter and mutates it *)
let[@inline never] [@local never] f_step_through_mutations (counter : int ref) =
  (* First increment *)
  counter := !counter + 1;
  (* Second increment *)
  counter := !counter + 1;
  (* Third increment *)
  counter := !counter + 1;
  (* Fourth increment *)
  counter := !counter + 1;
  (* Return final value *)
  !counter

let _ =
  let my_counter = ref 0 in
  let final_value = f_step_through_mutations my_counter in
  Printf.printf "Final value: %d\n" final_value
