(* TEST *)

let test0 =
  Printf.printf "\nTest 0\n";
  let eval : <[int]> expr -> int = [%eval: int] in
  let output = eval <[ 42 ]> in
  Printf.printf "Output: %d\n" output;
;;

let test1 =
  Printf.printf "\nTest 1\n";
  let compiled = [%eval: int -> int list] <[ fun x -> [ x ; x + 1 ] ]> in
  let output = compiled 42 in
  Printf.printf
    "Output: [%s]\n"
    (String.concat "; " (List.map string_of_int output))
;;

let test2 =
  Printf.printf "\nTest 2\n";
  let eval = [%eval: Buffer.t] in
  let output : Buffer.t = eval
    <[ let b = Buffer.create 42 in Buffer.add_string b "Hello world!" ; b ]>
  in
  Printf.printf "Output: %s\n" (Buffer.contents output);
;;

let test3 =
  Printf.printf "\nTest 3\n";
  (* This quote passes type-checking but fail during compilation. *)
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
    let output = [%eval: string] quote in
    Printf.printf "Output: %s\n" output;
  with Failure error -> Printf.printf "Error: %s\n" error
;;
