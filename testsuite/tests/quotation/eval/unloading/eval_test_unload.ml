(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

(* Variant of eval_test.ml that wraps each Eval.eval phrase in a function so
   the JIT'd unit doesn't escape into a top-level binding, then forces
   Gc.compact between phrases and prints registered/unloaded counters.
   Run with [OCAML_UNLOADABLE_DEBUG=1] to also see the per-event traces
   printed by [runtime/unloadable.c].

   Each phrase is in its own [run_*] function (rather than a top-level
   [let], as in [eval_test.ml]) so that closures bound by the phrase don't
   escape into module-level bindings — that would keep the unit live
   across [Gc.compact] and defeat the test. *)

#syntax quotations on

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "[%s] registered=%d unloaded=%d live=%d\n" label r u (r - u)

let[@inline never] run_simple_eval () =
  Printf.printf "\nTest simple eval\n";
  let output : int = Eval.eval <[ 42 ]> in
  Printf.printf "Output: %d\n" output

let[@inline never] run_complex_return_type () =
  Printf.printf "\nTest complex return type\n";
  let compiled : int -> int list =
    Eval.eval <[ fun x -> [ x ; x + 1 ] ]>
  in
  let output = compiled 42 in
  Printf.printf "Output: [%s]\n"
    (String.concat "; " (List.map string_of_int output))

let[@inline never] run_side_effects () =
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

let[@inline never] run_reference_to_global () =
  Printf.printf "\nTest reference to global\n";
  let output : Buffer.t =
    Eval.eval
      <[ let b = Buffer.create 42 in Buffer.add_string b "Hello world!" ; b ]>
  in
  Printf.printf "Output: %s\n" (Buffer.contents output)

let[@inline never] run_heap_value_returned_directly () =
  Printf.printf "\nTest heap value returned directly\n";
  let r : int ref = Eval.eval <[ ref 0 ]> in
  r := 17;
  r := !r + 1;
  Printf.printf "Output: %d\n" !r

let[@inline never] run_late_compilation_error () =
  Printf.printf "\nTest late compilation error\n";
  let quote = <[
    let ignore (_ @ local) = () in
    let[@tail_mod_cons] rec foo x = exclave_
      if x = 0 then [] else x :: foo (x - 1)
    in
    ignore (foo 5);
    "This quote should fail to compile."
  ]> in
  (try
    let output : string = Eval.eval quote in
    Printf.printf "Output: %s\n" output
  with _ -> Printf.printf "Error during eval (expected)\n")

let[@inline never] run_warning () =
  Printf.printf "\nTest warnings emitted during eval\n";
  let (_ : unit) =
    Eval.eval
      <[ let a = () in $( if false then <[ a ]> else <[ () ]>) ]>
  in
  Printf.printf "Done\n"

let[@inline never] run_simplify () =
  Printf.printf "\nTest simplify\n";
  let _ : int64 =
    Eval.eval
      <[
        let[@zero_alloc][@inline never][@local never] check_simplify () =
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
  in
  Printf.printf "Done\n"

let () =
  report "start";
  run_simple_eval ();
  Gc.compact ();
  report "after simple_eval + compact";

  run_complex_return_type ();
  Gc.compact ();
  report "after complex_return_type + compact";

  run_side_effects ();
  Gc.compact ();
  report "after side_effects + compact";

  run_reference_to_global ();
  Gc.compact ();
  report "after reference_to_global + compact";

  run_heap_value_returned_directly ();
  Gc.compact ();
  report "after heap_value_returned_directly + compact";

  run_late_compilation_error ();
  Gc.compact ();
  report "after late_compilation_error + compact";

  run_warning ();
  Gc.compact ();
  report "after warning + compact";

  run_simplify ();
  Gc.compact ();
  report "after simplify + compact"
