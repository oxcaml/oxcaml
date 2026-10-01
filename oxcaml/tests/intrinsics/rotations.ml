(* Exercise the rotate instructions, in particular their encodings in the
   internal assembler: immediate and variable-count forms at both widths. *)

let opaque = Sys.opaque_identity

let () =
  (* Immediate rotation counts *)
  assert (Int64.rotate_left (opaque 0x0123456789ABCDEFL) 8 = 0x23456789ABCDEF01L);
  assert (
    Int64.rotate_right (opaque 0x0123456789ABCDEFL) 8 = 0xEF0123456789ABCDL);
  assert (Int32.rotate_left (opaque 0x01234567l) 8 = 0x23456701l);
  assert (Int32.rotate_right (opaque 0x01234567l) 8 = 0x67012345l);
  (* Variable rotation counts *)
  let n = opaque 12 in
  assert (Int64.rotate_left (opaque 0x0123456789ABCDEFL) n = 0x3456789ABCDEF012L);
  assert (
    Int64.rotate_right (opaque 0x0123456789ABCDEFL) n = 0xDEF0123456789ABCL);
  assert (Int32.rotate_left (opaque 0x01234567l) n = 0x34567012l);
  assert (Int32.rotate_right (opaque 0x01234567l) n = 0x56701234l)
