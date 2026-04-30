(* bundle_crc.cmo was compiled against bundle_crc.cmi v1 (Basic+Util).
   bundle_crc.cmi has been replaced with v2 (Basic only).
   Linking should fail with a CRC mismatch for Bundle_crc. *)
module R = Bundle_crc.Func(P_int)()

let () =
  let b = R.Basic.create 42 in
  print_endline (R.Basic.to_string b)
