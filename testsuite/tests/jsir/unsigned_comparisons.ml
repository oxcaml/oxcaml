(* TEST
 flambda2;
 {
   native;
 }{
   jsir;
 }
*)

external int_ult : int -> int -> bool = "%int_unsigned_lessthan"
external int_ule : int -> int -> bool = "%int_unsigned_lessequal"
external int_ugt : int -> int -> bool = "%int_unsigned_greaterthan"
external int_uge : int -> int -> bool = "%int_unsigned_greaterequal"
external int32_ult : int32 -> int32 -> bool = "%int32_unsigned_lessthan"
external int32_ule : int32 -> int32 -> bool = "%int32_unsigned_lessequal"
external int32_ugt : int32 -> int32 -> bool = "%int32_unsigned_greaterthan"
external int32_uge : int32 -> int32 -> bool = "%int32_unsigned_greaterequal"
external int64_ult : int64 -> int64 -> bool = "%int64_unsigned_lessthan"
external int64_ule : int64 -> int64 -> bool = "%int64_unsigned_lessequal"
external int64_ugt : int64 -> int64 -> bool = "%int64_unsigned_greaterthan"
external int64_uge : int64 -> int64 -> bool = "%int64_unsigned_greaterequal"
external nativeint_ult : nativeint -> nativeint -> bool
  = "%nativeint_unsigned_lessthan"
external nativeint_ule : nativeint -> nativeint -> bool
  = "%nativeint_unsigned_lessequal"
external nativeint_ugt : nativeint -> nativeint -> bool
  = "%nativeint_unsigned_greaterthan"
external nativeint_uge : nativeint -> nativeint -> bool
  = "%nativeint_unsigned_greaterequal"

let[@inline never] check name (b : bool) =
  let i : int = Obj.magic b in
  let is_bool = i = 0 || i = 1 in
  Printf.printf "%s: %b %d %b\n" name b i is_bool

let[@inline never] test name ult ule ugt uge three minus_one =
  let three = Sys.opaque_identity three in
  let minus_one = Sys.opaque_identity minus_one in
  check (name ^ " 3 u< -1") (ult three minus_one);
  check (name ^ " -1 u< 3") (ult minus_one three);
  check (name ^ " 3 u<= 3") (ule three three);
  check (name ^ " -1 u<= 3") (ule minus_one three);
  check (name ^ " -1 u> 3") (ugt minus_one three);
  check (name ^ " 3 u> -1") (ugt three minus_one);
  check (name ^ " 3 u>= -1") (uge three minus_one);
  check (name ^ " -1 u>= -1") (uge minus_one minus_one)

let () =
  test "int" int_ult int_ule int_ugt int_uge 3 (-1);
  test "int32" int32_ult int32_ule int32_ugt int32_uge 3l (-1l);
  test "int64" int64_ult int64_ule int64_ugt int64_uge 3L (-1L);
  test "nativeint" nativeint_ult nativeint_ule nativeint_ugt nativeint_uge 3n
    (-1n)
