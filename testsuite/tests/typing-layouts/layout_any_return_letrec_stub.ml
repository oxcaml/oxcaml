(* TEST
 flambda2;
 exit_status = "2";
 {
   ocamlopt_flags = "-Oclassic";
   compiler_directory_suffix = ".Oclassic";
   native;
 }{
   ocamlopt_flags = "-O3";
   compiler_directory_suffix = ".O3";
   native;
 }{
   ocamlopt_flags = "-O3 -flambda2-reaper";
   compiler_directory_suffix = ".reaper";
   native;
 }{
   native;
 }
*)

(* Letrec eta-expansion stub -> [Pbottom]-result apply, which records no
   return continuation, so the code after it dies as unreachable at every
   optimisation level; the reaper block pins the reaper's result-arity path. *)

exception E

let rec x =
  let g = fun a b c -> ignore y; ignore (a + b + c); raise E in
  (g : int -> int -> int -> int)

and y = 0

let r = ref 0

let () = r := x 1 2 3

let () = print_string "unreachable\n"
