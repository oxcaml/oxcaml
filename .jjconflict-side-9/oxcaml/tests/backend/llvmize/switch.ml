let[@inline never] [@local never] next x =
  match[@warning "-8"] x with 1 -> 2 | 3 -> 4 | 5 -> 6 | 7 -> 8 | 9 -> 10

let test_next x =
  let res = try next x |> Int.to_string with _ -> "not found" in
  Format.printf "next %d = %s\n" x res

let () =
  test_next 1;
  test_next 3;
  test_next 5;
  test_next 7;
  test_next 9;
  test_next 2;
  test_next 0;
  test_next 100
