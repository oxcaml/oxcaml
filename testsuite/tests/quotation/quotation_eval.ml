(* TEST *)

let test_simple_eval =
  Printf.printf "\nTest simple eval\n";
  let eval : <[int]> expr -> unit -> int = [%eval: int] in
  Printf.printf "Compiling...\n";
  let compiled = eval <[ 42 ]> in
  Printf.printf "Running...\n";
  let output = compiled () in
  Printf.printf "Output: %d\n" output;
;;

let test_complex_return_type =
  Printf.printf "\nTest complex return type\n";
  let eval = [%eval: int -> int list] in
  Printf.printf "Compiling...\n";
  let compiled = eval <[ fun x -> [ x ; x + 1 ] ]> in
  Printf.printf "Running...\n";
  let output = (compiled ()) 42 in
  Printf.printf
    "Output: [%s]\n"
    (String.concat "; " (List.map string_of_int output))
;;

let test_reference_to_global =
  Printf.printf "\nTest reference to global\n";
  let eval = [%eval: Buffer.t] in
  Printf.printf "Compiling...\n";
  let compiled : unit -> Buffer.t = eval
    <[ let b = Buffer.create 42 in Buffer.add_string b "Hello world!" ; b ]>
  in
  Printf.printf "Running...\n";
  let output = compiled () in
  Printf.printf "Output: %s\n" (Buffer.contents output);
;;

let test_late_compilation_error =
  (* Eventually we should run quotes through transl so that we spot these errors
     (and don't emit warning 53 for the attributes). *)
  Printf.printf "\nTest late compilation error\n";
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
    "This quote should fail to compile. If it's started working but you\n\
     aren't running quotes through transl then you may need to find a new way\n\
     to trigger this."
  ]> in
  try
    let compiled = eval quote in
    Printf.printf "Running...\n";
    let output = compiled () in
    Printf.printf "Output: %s\n" output;
  with Failure error -> Printf.printf "Error: %s\n" error
;;
