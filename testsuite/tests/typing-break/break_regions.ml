(* TEST
 {
   stack-allocation;
   reference = "${test_source_directory}/break_regions.stack.reference";
   native;
 }{
   no-stack-allocation;
   reference = "${test_source_directory}/break_regions.heap.reference";
   native;
 }
*)

external local_stack_offset : unit -> int = "caml_local_stack_offset"
let local_stack_offset () = local_stack_offset () / (Sys.word_size / 8)
external opaque_local : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

let print_offsets (name,allocs) =
  let p xs = String.concat "; " (List.map Int.to_string xs) in
  Printf.printf "%25s: [%s]\n%!"
    name (p allocs)

let break_for () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  for _ = 0 to 10 do
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = opaque_local z in
    offset_loop := local_stack_offset ();
    if Sys.opaque_identity true then break_
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

let break_while () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  while true do
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = opaque_local z in
    offset_loop := local_stack_offset ();
    if Sys.opaque_identity true then break_
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

let break_after_iterations () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  for i = 0 to 10 do
    let z = local_ (Some (Sys.opaque_identity 42)) in
    let _ = opaque_local z in
    offset_loop := local_stack_offset ();
    if i > 3 then break_
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

let break_out_of_try () =
  let offset_loop = ref (-1) in
  let offset1 = local_stack_offset () in
  for _ = 0 to 10 do
    try
      let z = local_ (Some (Sys.opaque_identity 42)) in
      let _ = opaque_local z in
      offset_loop := local_stack_offset ();
      if Sys.opaque_identity true then break_
    with Not_found -> ()
  done;
  let offset2 = local_stack_offset () in
  [offset1; !offset_loop; offset2]

let () =
  List.iter print_offsets [
    "break for",              break_for ();
    "break while",            break_while ();
    "break after iterations", break_after_iterations ();
    "break out of try",       break_out_of_try ();
  ]
