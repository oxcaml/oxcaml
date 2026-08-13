(* This test exercises the DWARF variable range computation for inlined call
   frames. It observed a bug while working on supporting function sections with
   the following error message:

   error: test_tailrec_dwarf.exe : adding range
   [0x000000000000006e-0x0000000000000081) which has a base that is less than
   the function's low PC 0x0000000000464190. Please file a bug and attach the
   file at the start of this error message*)
let[@inline never] [@local never] [@loop never] rec loop x =
  print_int x;
  print_newline ();
  if x = 0 then 0 else loop (x - 1)

let () = loop (Sys.opaque_identity 3) |> ignore
