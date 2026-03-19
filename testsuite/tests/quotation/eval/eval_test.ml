(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming -gno-upstream-dwarf -g";
  native;
*)

(* We add extra debugging flags above in case of failure, when this test
   can be hard to debug *)


#syntax quotations on

let test_simple_eval =
  Printf.printf "\nTest simple eval\n";
  let output : int = Eval.eval <[ 42 ]> in
  Printf.printf "Output: %d\n" output;
;;

let test_complex_return_type =
  Printf.printf "\nTest complex return type\n";
  let compiled : int -> int list =
    Eval.eval <[ fun x -> [ x ; x + 1 ] ]>
  in
  let output = compiled 42 in
  Printf.printf
  "Output: [%s]\n"
  (String.concat "; " (List.map string_of_int output))
;;

let test_side_effects =
  Printf.printf "\nTest side effects\n";
  Printf.printf "Compiling...\n";
  let compiled : unit -> unit =
    Eval.eval
    <[ print_endline "Outside";
       fun () -> print_endline "Inside" ]>
  in
  Printf.printf "Running...\n";
  let () = compiled () in
  Printf.printf "Done\n"

(* CR jrickard: This test highlights a bug in eval: The cmx that gets built
   from this claims it depends on [Stdlib] because it depends on [Stdlib.Buffer]
   which is correct for the cmi lookup but breaks for cmx lookup because
   [Stdlib] doesn't depend on the implementation of [Stdlib__Buffer]. Maybe I
   need to split the quotation depends into intf and impls? (and somehow do a
   lookup that resolves to Stdlib__Buffer). *)
let test_reference_to_global =
  Printf.printf "\nTest reference to global\n";
  let output : Buffer.t =
    Eval.eval
    <[ let b = Buffer.create 42 in Buffer.add_string b "Hello world!" ; b ]>
  in
  Printf.printf "Output: %s\n" (Buffer.contents output);
;;

let test_late_compilation_error =
  Printf.printf "\nTest late compilation error\n";
  (* This quote passes type-checking but fails during compilation (or parsing,
     since the quotation printer may produce syntax that doesn't round-trip
     through the parser). *)
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
  (try
    let output : string = Eval.eval quote in
    Printf.printf "Output: %s\n" output;
  with _ -> Printf.printf "Error during eval (expected)\n")
;;

let test_warning =
  Printf.printf "\nTest warnings emitted during eval\n";
  (* Unused variable *)
  let (_ : unit) =
    Eval.eval
      <[ let a = () in $( if false then <[ a ]> else <[ () ]>) ]>
  in
  Printf.printf "Done\n"

(* Checks that Simplify is being used rather than classic mode *)

let test_simplify_being_used =
  [%eval: Int64.t]
  <[
    let[@zero_alloc][@inline never][@local never] check_simplify () =
      (* This relies on simplification of inlined bodies, variant unboxing,
         and simplification of addition - none of which are done in
         classic mode *)
      let[@inline] f () =
        match
          if (Sys.opaque_identity 4) = 0
          then None
          else Some (Int64.add 1L 2L)
        with
        | None -> 0L
        | Some x -> Int64.add x 1L
      in
      f ()
    in
    check_simplify ()
  ]>

