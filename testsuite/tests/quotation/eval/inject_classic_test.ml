(* TEST
 include eval;
 flags = "-extension runtime_metaprogramming -Oclassic";
 native;
*)

(* As inject_test.ml, but compiled with -Oclassic so that [%reify_approx]
   collects real approximations, exercising the reconstruction of cmx export
   information for the injector's compilation unit (including recovering
   code through symbols recorded in the approximations). *)

#syntax quotations on

let top_level_double x = x * 2

let test_inject_toplevel_function () =
  let n : int =
    Eval.eval_with_injector (fun injector ->
        <[ $(Eval.inject injector top_level_double) 21 ]>)
  in
  Printf.printf "toplevel function: %d\n" n

let top_level_sum2 (x : int) (y : int) = x + y

let test_inject_stdlib_function () =
  (* Polymorphic values must be monomorphised by an annotation: the type of
     an injected value has to be ground for ['a eval] to be inferred. *)
  let l : int list =
    Eval.eval_with_injector (fun injector ->
        <[ $(Eval.inject injector (List.rev : int list -> int list))
             [1; 2; 3] ]>)
  in
  Printf.printf "stdlib function: [%s]\n"
    (String.concat "; " (List.map string_of_int l))

let test_inject_second_toplevel_function () =
  let n : int =
    Eval.eval_with_injector (fun injector ->
        <[ $(Eval.inject injector top_level_sum2) 40 2 ]>)
  in
  Printf.printf "two-argument function: %d\n" n

let test_inject_local_closure () =
  let base = Sys.opaque_identity 40 in
  let add x = base + x in
  let n : int =
    Eval.eval_with_injector (fun injector ->
        <[ $(Eval.inject injector add) 2 ]>)
  in
  Printf.printf "local closure: %d\n" n

let test_inject_ref () =
  let r = ref 41 in
  let incr_r : unit -> unit =
    Eval.eval_with_injector (fun injector ->
        <[ fun () -> incr $(Eval.inject injector r) ]>)
  in
  incr_r ();
  Printf.printf "ref after eval'd incr: %d\n" !r

let () =
  test_inject_toplevel_function ();
  test_inject_second_toplevel_function ();
  test_inject_stdlib_function ();
  test_inject_local_closure ();
  test_inject_ref ()
