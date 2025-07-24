external int_array_get_int64x2 : int array -> int -> int64x2
  = "%caml_int_array_get128"

let () = int_array_get_int64x2 [| 1; 2 |] 0 |> ignore
