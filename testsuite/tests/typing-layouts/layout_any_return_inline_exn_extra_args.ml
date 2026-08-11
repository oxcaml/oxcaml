(* TEST
 flambda2;
 flags = "-extension layouts_beta";
 {
   ocamlopt_flags = "-Oclassic";
   compiler_directory_suffix = ".Oclassic";
   native;
 }{
   ocamlopt_flags = "-O3";
   compiler_directory_suffix = ".O3";
   native;
 }{
   native;
 }
*)

(* Inlining an unknown-result callee at a call site whose exception
   continuation carries extra arguments (the mutable [r] kept live across
   [try]) wraps the inlined body in a fresh return continuation.  The
   forwarding apply inside the body must still be upgraded to the call site's
   concrete result arity, at both value and float# layouts. *)

external box_float : float# -> float = "%box_float"
external unbox_float : float -> float# = "%unbox_float"

let[@inline] u : type (a : any). (int -> a) -> int -> a = fun h x -> h x

let f (h : int -> float#) : float# =
  let r = ref 0 in
  try
    r := 1;
    if !r > 5 then raise Exit;
    u h 5
  with Exit -> if !r = 1 then #1.0 else #2.0

let g (h : int -> int) : int =
  let r = ref 0 in
  try
    r := 1;
    if !r > 5 then raise Exit;
    u h 5
  with Exit -> !r

let () =
  Printf.printf "%.1f %d\n"
    (box_float (f (fun x -> unbox_float (float_of_int x))))
    (g (fun x -> x + 1))
