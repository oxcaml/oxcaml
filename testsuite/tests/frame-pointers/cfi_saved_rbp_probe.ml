(* TEST
 modules = "cfi_saved_rbp_.c";
 flags = "-probes-optimized";
 frame_pointers;
 linux;
 probes;
 native;
*)

(* As [cfi_saved_rbp.ml], for the wrapper through which an enabled probe calls
   its handler: the frames above [frames_agree] are the handler, the wrapper,
   [probed] and its caller. *)
external frames_agree : int -> bool = "cfi_saved_rbp_frames_agree" [@@noalloc]

let[@inline never] probed () =
  let ok = ref false in
  [%probe "cfi_saved_rbp" ~enabled_at_init:true (ok := frames_agree 4)];
  !ok

let () = print_endline (if probed () then "ok" else "FAILED")
