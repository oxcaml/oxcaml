(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flags = "-extension small_numbers -dlambda";
 (* Soon this file is expected to compile and run. At that point the remainder
    of this test spec can simply be deleted. *)
 {
   setup-ocamlc.byte-build-env;
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)

module Int8_u = Stdlib_stable.Int8_u
module Int16_u = Stdlib_stable.Int16_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u

module Float32_u = Stdlib_stable.Float32_u
module Float_u = Stdlib_upstream_compatible.Float_u

let pr = print_endline

let print_i8 ?(i : int8# option) () =
  let i = match i with Some i -> i | None -> #1s in
  pr (Int8_u.to_string i)
;;

let () =
  pr "--- Printing i8 ---";
  print_i8 ();
  print_i8 () ?i:None;
  print_i8 () ?i:(Some #10s);
  print_i8 () ~i:#11s
;;

let print_i16 ?(i = #2S) () =
  pr (Int16_u.to_string i)
;;

let () =
  pr "\n--- Printing i16 ---";
  print_i16 ();
  print_i16 () ?i:None;
  print_i16 () ?i:(Some #20S);
  print_i16 () ~i:#21S
;;

let print_unboxed_pair ?(p = #(#3l, "three")) () =
  let #(i, s) = p in
  pr ("#(" ^ (Int32_u.to_string i) ^ ", \"" ^ s ^ "\")")
;;

let () =
  pr "\n--- Printing unboxed pair ---";
  print_unboxed_pair ();
  print_unboxed_pair () ?p:None;
  print_unboxed_pair () ?p:(Some #(#30l, "thirty"));
  print_unboxed_pair () ~p:#(#31l, "thirty-one")
;;

let f32_of_i64 i64 =
  Float32_u.of_int (Int64_u.to_int i64)
;;

let print_multiple
      ?(i64 = #4L) ?(f32 = f32_of_i64 i64) ?(f64 : float# option) ?u () =
  let f64 = match f64 with Some f -> f | None -> #5.0 in
  let u_passed = match u with Some #() -> true | None -> false in
  pr ("("
      ^ Int64_u.to_string i64
      ^ ", "
      ^ Float32_u.to_string f32
      ^ ", "
      ^ Float_u.to_string f64
      ^ ", "
      ^ Bool.to_string u_passed
      ^ ")")
;;

let () =
  pr "\n--- Printing multiple ---";
  print_multiple ();
  print_multiple () ?i64:None ?f32:None ?f64:None ?u:None;
  print_multiple () ?i64:(Some #40L) ?f32:None ?f64:None ?u:(Some #());
  print_multiple () ?f32:None ?f64:None ?u:(Some #()) ?i64:(Some #40L);
  print_multiple () ?i64:None ?f32:(Some #41.0s) ?f64:(Some #50.0) ?u:None;
  print_multiple ()
    ?i64:(Some #40L) ?f32:(Some #41.0s) ?f64:(Some #50.0) ?u:(Some #());
  print_multiple () ~i64:#40L ~u:#();
  print_multiple () ~u:#() ~i64:#40L;
  print_multiple () ~f32:#41.0s ~f64:#50.0;
  print_multiple () ~i64:#40L ~f32:#41.0s ~f64:#50.0 ~u:#()
;;
