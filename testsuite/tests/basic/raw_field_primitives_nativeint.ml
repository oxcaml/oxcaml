(* TEST
 native;
*)

(* Test %raw_field and %set_raw_field with nativeint# fields - native code only *)

external raw_field : 'a -> int -> nativeint# = "%raw_field"
external set_raw_field : 'a -> int -> nativeint# -> unit = "%set_raw_field"

external box_nativeint : nativeint# -> nativeint = "%box_nativeint"

let print_nativeint_unboxed (n : nativeint#) =
  print_int (Nativeint.to_int (box_nativeint n));
  print_newline ()

(* Test with records containing nativeint# fields *)
type nativeint_record = {
  n1 : nativeint#;
  mutable n2 : nativeint#;
  n3 : nativeint#;
}

let test_nativeint_records () =
  print_endline "=== Testing records with nativeint# fields ===";
  let r = {
    n1 = #0x1234n;
    n2 = #0x5678n;
    n3 = #0x9ABCn;
  } in
  print_endline "Record with nativeint# fields:";
  print_nativeint_unboxed (raw_field r 0);
  print_nativeint_unboxed (raw_field r 1);
  print_nativeint_unboxed (raw_field r 2);

  print_endline "Setting nativeint# field to 0xDEF0:";
  set_raw_field r 1 #0xDEF0n;
  print_nativeint_unboxed (raw_field r 1);
  ()

let () =
  test_nativeint_records ()