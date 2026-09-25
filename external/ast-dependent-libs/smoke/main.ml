open Js_of_ocaml

let () =
  let point =
    object%js
      val x = 3
      val y = 4
    end
  in
  assert (point##.x + point##.y = 7);
  print_endline (Js.to_string (Js.string "js_of_ocaml from the compiler package"))
