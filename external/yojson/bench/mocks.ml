(* chosen by fair dice roll, guaranteed to be large *)
let large = 10_000

let large_int_assoc =
  let ints = List.init large (fun n -> (string_of_int n, `Int n)) in
  `Assoc ints

let data =
  In_channel.with_open_text "bench.json" @@ fun ic -> In_channel.input_all ic

let yojson_data = Yojson.Safe.from_string data

let large_int_list =
  let ints = List.init large (fun n -> `Int n) in
  `List ints

let large_string_list =
  let strings = List.init large (fun n -> `String (string_of_int n)) in
  `List strings

let streamable_string =
  let buf = Buffer.create (large * 100) in
  for i = 1 to large do
    Printf.bprintf buf "%d\n" i
  done;
  Buffer.contents buf
