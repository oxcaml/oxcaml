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

(* Partially applying a forwarder makes the simplifier build a wrapper for
   the remaining arguments whose code has an unknown result arity -- the only
   place the middle end creates such code itself.  The direct full
   application of [fwd], whose closure is unused, additionally exercises the
   classic-mode erased-callee path for unknown-result callees. *)

let[@inline never] f : type (a : any). int -> (unit -> a) -> a =
 fun _ g -> g ()

let[@inline never] fwd : type (a : any). (unit -> a) -> a = fun g -> g ()

let h () = 42

let () =
  let p = f 1 in
  Printf.printf "partial: %d\n" (p h)

let () = Printf.printf "direct: %d\n" (fwd h)

(* The same wrappers instantiated at a non-value layout: the forwarded result
   travels through the unknown-result convention in a float register. *)

external box_float : float# -> float = "%box_float"

let hf () = #2.5

let () =
  let p = f 1 in
  Printf.printf "partial float#: %.1f\n" (box_float (p hf))

let () = Printf.printf "direct float#: %.1f\n" (box_float (fwd hf))
