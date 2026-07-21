(* TEST
 include eval;
 flags = "-extension runtime_metaprogramming";
 native;
*)

(* Tests of [Eval.inject]: injecting values with identity (refs, closures)
   into quotes evaluated in the context of the current program execution. *)

#syntax quotations on

let test_inject_ref () =
  let r = ref 41 in
  let incr_r : unit -> unit =
    Eval.eval_with_injector (fun injector ->
        <[ fun () -> incr $(Eval.inject injector r) ]>)
  in
  incr_r ();
  Printf.printf "ref after eval'd incr: %d\n" !r

let test_inject_closure () =
  let double x = x * 2 in
  let doubled : int =
    Eval.eval_with_injector (fun injector ->
        <[ $(Eval.inject injector double) 21 ]>)
  in
  Printf.printf "doubled: %d\n" doubled

let test_two_values () =
  let a = ref 1 in
  let b = ref 2 in
  let sum : int =
    Eval.eval_with_injector (fun injector ->
        <[ !($(Eval.inject injector a)) + !($(Eval.inject injector b)) ]>)
  in
  Printf.printf "sum: %d\n" sum

let test_gc () =
  (* After [eval_with_injector] returns, this string is reachable only
     through the injected module block, which must keep it alive across
     collections (including compaction). *)
  let s = String.concat "-" ["keep"; "me"; "alive"] in
  let get : unit -> string =
    Eval.eval_with_injector (fun injector ->
        <[ fun () -> $(Eval.inject injector s) ]>)
  in
  Gc.full_major ();
  Gc.compact ();
  Printf.printf "string: %s\n" (get ())

let test_no_injections () =
  (* [eval_with_injector] with an unused injector behaves like [eval]. *)
  let n : int = Eval.eval_with_injector (fun _injector -> <[ 6 * 7 ]>) in
  Printf.printf "no injections: %d\n" n

let () =
  test_inject_ref ();
  test_inject_closure ();
  test_two_values ();
  test_gc ();
  test_no_injections ()
