(* TEST *)

let test0 =
  Printf.printf "\nTest 0\n";
  let eval : <[int]> expr -> unit -> int = [%eval: int] in
  Printf.printf "Compiling...\n";
  let compiled = eval <[ 42 ]> in
  Printf.printf "Running...\n";
  let output = compiled () in
  Printf.printf "Output: %d\n" output;
;;

let test1 =
  Printf.printf "\nTest 1\n";
  let eval = [%eval: int -> int list] in
  Printf.printf "Compiling...\n";
  let compiled = eval <[ fun x -> [ x ; x + 1 ] ]> in
  Printf.printf "Running...\n";
  let output = (compiled ()) 42 in
  Printf.printf
    "Output: [%s]\n"
    (String.concat "; " (List.map string_of_int output))
;;

let test2 =
  Printf.printf "\nTest 2\n";
  let eval = [%eval: Buffer.t] in
  Printf.printf "Compiling...\n";
  let compiled : unit -> Buffer.t = eval
    <[ let b = Buffer.create 42 in Buffer.add_string b "Hello world!" ; b ]>
  in
  Printf.printf "Running...\n";
  let output = compiled () in
  Printf.printf "Output: %s\n" (Buffer.contents output);
;;

let test3 =
  Printf.printf "\nTest 3\n";
  let eval = [%eval: string] in
  Printf.printf "Compiling...\n";
  (* This quote passes type-checking but fail during compilation, this lets us test exactly
     when compilation of the quote happens (and what happens when it fails). *)
  let quote = <[
    let ignore (_ @ local) = () in
    let[@tail_mod_cons] rec foo x = exclave_
      if x = 0 then [] else x :: foo (x - 1)
    in
    ignore (foo 5);
    "This quote should fail to compile. If it's started working then you need\n\
     to come up with a new way for this test to pass type-checking but fail\n\
     to compile."
  ]> in
  try
    let compiled = eval quote in
    Printf.printf "Running...\n";
    let output = compiled () in
    Printf.printf "Output: %s\n" output;
  with Failure error -> Printf.printf "Error: %s\n" error
;;
