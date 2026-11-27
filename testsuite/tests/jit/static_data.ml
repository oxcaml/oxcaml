(* TEST
 modules = "run_phrases.ml";
 flags = "-I +compiler-libs -I +unix -I +dynlink -I +ocaml-jit -linkall -w -58";
 libraries = "unix dynlink ocamlcommon ocamlfrontend ocamloptcomp";
 libraries += " ocamlopttoplevel jit";
 native;
*)

(* Phrases whose static data points to other static data of the same phrase.
   The blocks of the formats and of the list of strings are only used by the
   code of their phrase, and so have no global symbol; [nested], [even] and
   [odd] are exported by theirs.

   Warning 58 is disabled because the [.cmx] files of some modules of the
   libraries are not installed. *)

let () =
  Run_phrases.run
    {|
let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);;
let () = Printf.printf "fib 20 = %d\n%!" (fib 20);;
let () =
  print_endline (String.concat ", " ["static"; "list"; "of"; "strings"]);;
let nested = [Some (1, "one"); None; Some (2, "two")];;
let () =
  List.iter
    (function
      | None -> print_endline "none"
      | Some (n, s) -> Printf.printf "%d is %s\n%!" n s)
    nested;;
let rec even n = n = 0 || odd (n - 1)
and odd n = n <> 0 && even (n - 1);;
let () = Printf.printf "even 10 = %b, odd 10 = %b\n%!" (even 10) (odd 10);;
|}
