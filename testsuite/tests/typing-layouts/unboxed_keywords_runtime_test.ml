(* TEST
 flags = "-extension comprehensions -strict-sequence";
 reference = "${test_source_directory}/unboxed_keywords_runtime_test.reference";
 native;
 flambda2;
 {
   native;
 }{
   flags = "-extension comprehensions -strict-sequence -Oclassic";
   native;
 }{
   flags = "-extension comprehensions -strict-sequence -O3";
   native;
 }{
   bytecode;
 }
*)

(try #assert #false with
 | Assert_failure _ ->
   print_endline "false"; 
   #())#;

#if #true
then (
  print_endline "true";
  #assert #true)#;

#if #false
then #assert #false
else (
  print_endline "not false";
  #())#;

#while #false do #if #true then #assert #false done#;
print_endline "while never";

let open struct type ref = { mutable contents : bool# } end in
let ref = { contents = #true } in
#while ref.contents do
  print_endline "while once";
  ref.contents #<- #false
done#;

let mutable continue = #true in
#while continue do
  print_endline "while twice";
  continue #<- #false
done#;

let obj = object
  val mutable var = true
  method get #() = if var then #true else #false
  method set b = var #<- #if b then true else false
end in
#while obj#get #() do
  print_endline "while thrice";
  obj#set #false
done#;

#for _ = 0 to 0 do print_endline "for once"; #() done#;

(match #true with
 | x #when x ->
   print_endline "when true";
   #assert x
 | _ -> #assert #false)#;

(match #false with
 | x #when x -> #assert x
 | _ ->
   print_endline "when false";
   #assert #true)#;

(match [: i for i = 0 to 2 #when #true :] with
 | [: 0; 1; 2 :] -> 
   print_endline "three";
   #()
 | _ -> #assert #false)#;

(match [: i for i = 0 to 2 #when #false :] with
 | [::] ->
   print_endline "zero";
   #()
 | _ -> #assert #false)#;

()

