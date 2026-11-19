let loc = Ppxlib.Location.none

let () =
  match [%type: a:int * b:string] with
  | [%type: int * string] | [%type: b:string * a:int] ->
      print_endline "Matched incorrectly."
  | [%type: a:int * b:string] -> print_endline "Correct!"
  | _ -> ()

let () =
  match [%type: int * string] with
  | [%type: a:int * b:string] -> print_endline "Matched incorrectly."
  | [%type: int * string] -> print_endline "Correct!"
  | _ -> ()
