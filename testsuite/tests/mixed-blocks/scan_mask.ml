(* TEST
 modules = "scan_mask_stubs.c";
*)

(* Check the runtime's block-scannability iterator (caml/scan_mask.h)
   against an independent reference, for all four header formats. The
   compiler only emits a few of the possible layouts, so the test
   builds headers, layout words and layout blocks by hand in C. *)

external scan_mask_test : unit -> unit = "scan_mask_test"

let () =
  scan_mask_test ();
  print_endline "OK"
