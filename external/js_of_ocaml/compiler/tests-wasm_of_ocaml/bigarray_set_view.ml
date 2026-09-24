open Bigarray

type bytes = (int, int8_unsigned_elt, c_layout) Array1.t

external replace_backing : bytes -> bytes -> unit = "test_bigarray_replace_backing"

external get16 : bytes -> int -> int = "test_bigarray_get16"

let () =
  let original = Array1.of_array int8_unsigned c_layout [| 1; 2; 3; 4 |] in
  let replacement = Array1.of_array int8_unsigned c_layout [| 5; 6; 7; 8 |] in
  replace_backing original replacement;
  assert (original.{0} = 5);
  assert (get16 original 0 = 0x0605);
  replacement.{1} <- 9;
  assert (original.{1} = 9);
  assert (get16 original 0 = 0x0905)
