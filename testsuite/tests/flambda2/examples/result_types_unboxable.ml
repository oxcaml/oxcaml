(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-result-types-all-functions";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* A function with an unboxed calling convention returns through a wrapper
   continuation, so its return continuation has a single inlinable use. The
   computation of its result types used to fail on that use ("No extra args
   for rewrite Id"), whatever the result. *)

let[@unboxable] g : unit -> float = fun () -> 2.0

let[@unboxable] h : float -> float = fun x -> x +. 1.0

let k () = int_of_float (h (g ()))
