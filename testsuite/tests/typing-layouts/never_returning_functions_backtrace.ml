(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 reference =
   "${test_source_directory}/never_returning_functions_backtrace.reference";
 flambda2;
 {
   native;
 }{
   flags = "-g -O3";
   native;
 }{
   flags = "-g -Oclassic";
   native;
 }{
   bytecode;
 }
*)

(* A call to a function that never returns, whose result types are not known
   to the caller, stays a call: the caller keeps its frame in backtraces. See
   [simplify_direct_full_application] in [simplify_apply_expr.ml]. *)

let[@opaque] fail s = raise (Failure s)

let[@inline never] caller x =
  if x > 0 then ignore (fail "caller" : int);
  x + 1

let () =
  match caller 1 with
  | _ -> print_endline "returned"
  | exception Failure s ->
    print_endline s;
    Printexc.print_backtrace stdout

(* Raise primitives at a result of a non-value layout keep or restore the
   backtrace *)

external reraise_any : ('a : any). exn -> 'a = "%reraise"

external raise_with_backtrace_any : ('a : any).
  exn -> Printexc.raw_backtrace -> 'a = "%raise_with_backtrace"

let[@inline never] origin () : int =
  raise (Failure (Sys.opaque_identity "origin"))

let[@inline never] reraised () : #(int * float#) =
  match origin () with
  | _ -> #(0, #0.0)
  | exception exn -> reraise_any exn

let[@inline never] restored () : #(int * float#) =
  match origin () with
  | _ -> #(0, #0.0)
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    (try raise Not_found with Not_found -> ());
    raise_with_backtrace_any exn bt

let[@inline never] check name f =
  match f () with
  | #(_, _) -> print_endline "returned"
  | exception Failure s ->
    Printf.printf "%s: %s\n" name s;
    Printexc.print_backtrace stdout

let () =
  check "reraised" reraised;
  check "restored" restored
