(* Cursor is inside of an array literal *)

let x () = [| 1; 2; 3 |]
              (* ^ *)

let x () = exclave_ [| 1; 2; 3 |]
                       (* ^ *)
