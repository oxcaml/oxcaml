(* Verifies that reads sign-extends the byte, following native behavior *)

external get : 'base -> Obj.t -> int = "caml_get_idx_bytecode"

external set : 'base -> Obj.t -> int -> unit = "caml_set_idx_bytecode"

let idx tag i =
  let b = Obj.new_block tag 1 in
  Obj.set_field b 0 (Obj.repr i);
  b

let s = "\x00\x01\x7f\x80\x81\xfe\xff"

let test f =
  List.init (String.length s) f
  |> List.map string_of_int
  |> String.concat " "
  |> print_endline

(** Byte [s.i] as [i8] *)
let expected i =
  let c = Char.code s.[i] in
  if c < 0x80 then c else c - 0x100

let bigstring_of_string s =
  Bigarray.Array1.init Bigarray.char Bigarray.c_layout (String.length s) (String.get s)

let%expect_test ("reads sign-extend" [@when not native]) =
  (* Raw *)
  test (fun i -> Char.code s.[i]);
  [%expect {| 0 1 127 128 129 254 255 |}];
  (* Expected *)
  test expected;
  [%expect {| 0 1 127 -128 -127 -2 -1 |}];
  (* String *)
  test (fun i -> get s (idx 2 i));
  [%expect {| 0 1 127 -128 -127 -2 -1 |}];
  (* Bytes *)
  let b = Bytes.of_string s in
  test (fun i -> get b (idx 3 i));
  [%expect {| 0 1 127 -128 -127 -2 -1 |}];
  (* Bigstring *)
  let bs = bigstring_of_string s in
  test (fun i -> get bs (idx 4 i));
  [%expect {| 0 1 127 -128 -127 -2 -1 |}]

let%expect_test ("writes keep the low byte" [@when not native]) =
  (* Bytes *)
  let b = Bytes.make (String.length s) '\000' in
  for i = 0 to String.length s - 1 do
    set b (idx 3 i) (expected i)
  done;
  test (fun i -> Char.code (Bytes.get b i));
  [%expect {| 0 1 127 128 129 254 255 |}];
  test (fun i -> get b (idx 3 i));
  [%expect {| 0 1 127 -128 -127 -2 -1 |}];
  (* Bigstring *)
  let bs = bigstring_of_string (String.make (String.length s) '\000') in
  for i = 0 to String.length s - 1 do
    set bs (idx 4 i) (expected i)
  done;
  test (fun i -> Char.code bs.{i});
  [%expect {| 0 1 127 128 129 254 255 |}];
  test (fun i -> get bs (idx 4 i));
  [%expect {| 0 1 127 -128 -127 -2 -1 |}]

let print_result f =
  match f () with
  | v -> print_endline (string_of_int v)
  | exception e -> print_endline (Printexc.to_string e)

let%expect_test ("out-of-bounds reads raise" [@when not native]) =
  let len = String.length s in
  let b = Bytes.of_string s in
  let bs = bigstring_of_string s in
  List.iter
    (fun i ->
      print_result (fun () -> get s (idx 2 i));
      print_result (fun () -> get b (idx 3 i));
      print_result (fun () -> get bs (idx 4 i)))
    [ -1; len ];
  [%expect
    {|
    Invalid_argument("index out of bounds")
    Invalid_argument("index out of bounds")
    Invalid_argument("index out of bounds")
    Invalid_argument("index out of bounds")
    Invalid_argument("index out of bounds")
    Invalid_argument("index out of bounds")
    |}]
