(* TEST
 flambda2;
 flags += " -O3 -dcfg-invariants";
 setup-ocamlopt.opt-build-env;
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
*)

(* A call that Simplify finds never returns, last in a [try] so that it is not
   a tail call, where the caller returns something else than the callee. *)

exception Fail of string

let[@inline never] fail_product s : #(int * float#) =
  match raise (Fail s) with (n : int) -> #(n, #0.0)

let[@inline never] fail_void s : unit# =
  match raise (Fail s) with (_ : int) -> #()

let[@inline never] use (#(n, _) : #(int * float#)) = print_int n

let[@inline never] in_function s : int =
  try
    let #(n, _) = fail_product s in
    n
  with Fail s -> String.length s

let[@inline never] void_in_function s : int =
  try
    let #() = fail_void s in
    0
  with Fail s -> String.length s

(* The exception continuation of the call has an extra argument, the mutable
   variable [r]. *)
let[@inline never] with_extra_args may_raise s x : int =
  let r = ref 0 in
  try
    may_raise ();
    r := x;
    let #(n, _) = fail_product s in
    n
  with _ -> !r

let () =
  Printf.printf "%d\n" (in_function "abc");
  Printf.printf "%d\n" (void_in_function "abcd");
  Printf.printf "%d\n" (with_extra_args (fun () -> ()) "f" 5);
  Printf.printf "%d\n" (with_extra_args (fun () -> raise Exit) "g" 6)

let () =
  try use (if Sys.opaque_identity true then fail_product "e" else #(3, #3.5))
  with Fail s -> print_endline s
