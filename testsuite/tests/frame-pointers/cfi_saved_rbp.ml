(* TEST
 modules = "cfi_saved_rbp_.c";
 frame_pointers;
 linux;
 native;
*)

(* The unwind information (CFI) of a function must say where its prologue
   saved the caller's frame pointer, so that an unwinder that follows CFI
   finds the same %rbp for each frame as one that follows frame pointers.
   [frames_agree n] checks this for the [n] frames above it. *)
external frames_agree : int -> bool = "cfi_saved_rbp_frames_agree" [@@noalloc]

(* Enough to go through the CFI of the function that makes the call and of
   its caller. *)
let frames = 3

let[@inline never] plain () = frames_agree frames

(* One of the two calls is laid out after the epilogue of the other. *)
let[@inline never] after_epilogue b =
  if b then frames_agree frames else not (frames_agree frames)

let[@inline never] maybe_raise b = if b then raise Exit

let[@inline never] in_try () =
  try
    let ok = frames_agree frames in
    maybe_raise (Sys.opaque_identity false);
    ok
  with Exit -> false

let[@inline never] in_handler () =
  try
    maybe_raise (Sys.opaque_identity true);
    false
  with Exit -> frames_agree frames

let[@inline never] check name f =
  Printf.printf "%s: %s\n%!" name (if f () then "ok" else "FAILED")

let () =
  check "plain" plain;
  check "after epilogue (1)" (fun () -> after_epilogue true);
  check "after epilogue (2)" (fun () -> not (after_epilogue false));
  check "in try" in_try;
  check "in handler" in_handler
