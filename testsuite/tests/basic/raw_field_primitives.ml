(* TEST *)

(* Test %raw_field and %set_raw_field primitives *)

external raw_field : 'a -> int -> nativeint# = "%raw_field"
external set_raw_field : 'a -> int -> nativeint# -> unit = "%set_raw_field"

(* Helper to print nativeint# values *)
let print_nativeint_unboxed (n : nativeint#) =
  Printf.printf "%nd\n" (Nativeint_u.to_nativeint n)

(* Test with tuples *)
let test_tuples () =
  print_endline "=== Testing tuples ===";
  let t2 = (42, 100) in
  print_endline "Tuple (42, 100):";
  print_nativeint_unboxed (raw_field t2 0);
  print_nativeint_unboxed (raw_field t2 1);
  
  let t3 = (1, 2, 3) in
  print_endline "Tuple (1, 2, 3):";
  print_nativeint_unboxed (raw_field t3 0);
  print_nativeint_unboxed (raw_field t3 1);
  print_nativeint_unboxed (raw_field t3 2);
  ()

(* Test with variants/data constructors *)
type variant = 
  | A 
  | B of int
  | C of int * int
  | D of int * int * int

let test_variants () =
  print_endline "=== Testing variants ===";
  
  let b = B 42 in
  print_endline "B 42:";
  print_nativeint_unboxed (raw_field b 0);
  
  let c = C (10, 20) in
  print_endline "C (10, 20):";
  print_nativeint_unboxed (raw_field c 0);
  print_nativeint_unboxed (raw_field c 1);
  
  let d = D (100, 200, 300) in
  print_endline "D (100, 200, 300):";
  print_nativeint_unboxed (raw_field d 0);
  print_nativeint_unboxed (raw_field d 1);
  print_nativeint_unboxed (raw_field d 2);
  ()

(* Test with immutable records *)
type immut_record = {
  x : int;
  y : int;
  z : int;
}

let test_immutable_records () =
  print_endline "=== Testing immutable records ===";
  let r = { x = 11; y = 22; z = 33 } in
  print_endline "Record { x = 11; y = 22; z = 33 }:";
  print_nativeint_unboxed (raw_field r 0);
  print_nativeint_unboxed (raw_field r 1);
  print_nativeint_unboxed (raw_field r 2);
  ()

(* Test with mutable records *)
type mut_record = {
  mutable a : int;
  mutable b : int;
  c : int;
  mutable d : int;
}

let test_mutable_records () =
  print_endline "=== Testing mutable records ===";
  let r = { a = 111; b = 222; c = 333; d = 444 } in
  print_endline "Initial record { a = 111; b = 222; c = 333; d = 444 }:";
  print_nativeint_unboxed (raw_field r 0);
  print_nativeint_unboxed (raw_field r 1);
  print_nativeint_unboxed (raw_field r 2);
  print_nativeint_unboxed (raw_field r 3);
  
  print_endline "Setting field 0 to 999:";
  set_raw_field r 0 #999n;
  print_nativeint_unboxed (raw_field r 0);
  Printf.printf "r.a = %d\n" r.a;
  
  print_endline "Setting field 1 to 888:";
  set_raw_field r 1 #888n;
  print_nativeint_unboxed (raw_field r 1);
  Printf.printf "r.b = %d\n" r.b;
  
  print_endline "Setting field 3 to 777:";
  set_raw_field r 3 #777n;
  print_nativeint_unboxed (raw_field r 3);
  Printf.printf "r.d = %d\n" r.d;
  ()

(* Test with records containing nativeint# fields *)
type nativeint_record = {
  n1 : nativeint#;
  mutable n2 : nativeint#;
  n3 : int;
}

let test_nativeint_records () =
  print_endline "=== Testing records with nativeint# fields ===";
  let r = { 
    n1 = #0x1234n;
    n2 = #0x5678n;
    n3 = 42;
  } in
  print_endline "Record with nativeint# fields:";
  print_nativeint_unboxed (raw_field r 0);
  print_nativeint_unboxed (raw_field r 1);
  print_nativeint_unboxed (raw_field r 2);
  
  print_endline "Setting nativeint# field to 0xABCD:";
  set_raw_field r 1 #0xABCDn;
  print_nativeint_unboxed (raw_field r 1);
  print_nativeint_unboxed r.n2;
  ()

let () =
  test_tuples ();
  print_newline ();
  test_variants ();
  print_newline ();
  test_immutable_records ();
  print_newline ();
  test_mutable_records ();
  print_newline ();
  test_nativeint_records ()