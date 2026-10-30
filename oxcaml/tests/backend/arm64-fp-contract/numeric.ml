(* ACTUAL COMPILER: arm64 implicit-contraction policy regression. *)

let[@inline never] add_right a b c = c +. (a *. b)

let[@inline never] add_left a b c = (a *. b) +. c

let[@inline never] sub_right a b c = c -. (a *. b)

let[@inline never] sub_left a b c = (a *. b) -. c

let[@inline never] neg_mul a b = -.(a *. b)

let[@inline never] explicit_fma a b c = Float.fma a b c

external add32 : float32 -> float32 -> float32 = "%addfloat32"

external mul32 : float32 -> float32 -> float32 = "%mulfloat32"

external of_float32 : float -> float32 = "%float32offloat"

external to_float32 : float32 -> float = "%floatoffloat32"

external fma32 : float32 -> float32 -> float32 -> float32
  = "caml_fma_float32_bytecode" "fmaf"
[@@unboxed] [@@noalloc]

let[@inline never] implicit32 a b c = add32 c (mul32 a b)

let[@inline never] explicit32 a b c = fma32 a b c

let check64 name expected value =
  let actual = Int64.bits_of_float value in
  if actual <> expected
  then
    failwith (Printf.sprintf "%s: expected %Lx, got %Lx" name expected actual)

let check32 name expected value =
  let actual = Int32.bits_of_float (to_float32 value) in
  if actual <> expected
  then
    failwith (Printf.sprintf "%s: expected %lx, got %lx" name expected actual)

let () =
  if Array.length Sys.argv <> 6 then invalid_arg "MODE A B A32 B32";
  let fused =
    match Sys.argv.(1) with
    | "on" -> true
    | "off" -> false
    | _ -> invalid_arg "MODE"
  in
  let a = float_of_string Sys.argv.(2)
  and b = float_of_string Sys.argv.(3)
  and a32 = of_float32 (float_of_string Sys.argv.(4))
  and b32 = of_float32 (float_of_string Sys.argv.(5)) in
  let neg = 0xbc90000000000000L and pos = 0x3c90000000000000L in
  let expected_neg = if fused then neg else 0L in
  let expected_pos = if fused then pos else 0L in
  check64 "add-right" expected_neg (add_right a b (-1.));
  check64 "add-left" expected_neg (add_left a b (-1.));
  check64 "sub-right" expected_pos (sub_right a b 1.);
  check64 "sub-left" expected_neg (sub_left a b 1.);
  check64 "negate-product" 0xbff0000000000000L (neg_mul a b);
  check64 "explicit-fma" neg (explicit_fma a b (-1.));
  let minus_one32 = of_float32 (-1.) in
  check32 "implicit32" 0l (implicit32 a32 b32 minus_one32);
  check32 "explicit32" 0xb2800000l (explicit32 a32 b32 minus_one32);
  print_endline "numeric checks passed"
