(* TEST
 modules = "link_units_a.ml link_units_b.ml";
 flambda2;
 {
   native;
 }{
   jsir;
 }{
   ocamlopt_flags = "-link-jsir";
   jsir;
 }{
   ocamlopt_flags = "-link-jsir -g";
   jsir;
 }{
   ocamlopt_flags = "-link-jsir -linkall";
   jsir;
 }
*)

(* With -link-jsir, the JSIR of the units of an executable is linked
   into one program before js_of_ocaml compiles it, and references to the
   symbols of other units are resolved to their definitions (see Jsir_linker).
   Checks that the units are initialised in order, and that values, closures,
   exceptions, module blocks and cyclic values of other units are the same
   across units, as when they are compiled separately. *)

open Link_units_a

let () = print_endline "init main"

let () =
  let second = incr_counter () in
  Printf.printf "counter: %d %d %d\n" Link_units_b.first second !counter;
  Printf.printf "add: %d %d\n" (add 1 2) (Link_units_b.add3 4);
  Printf.printf "constant: %s %d\n" constant.name constant.value;
  Printf.printf "alias: %b\n" (Link_units_b.alias == constant);
  Printf.printf "branchy: %s %d\n" branchy.name branchy.value;
  Printf.printf "even/odd: %b %b\n" (even 10) (odd 10);
  Printf.printf "cycle: %d %d %b\n" (List.hd cycle)
    (List.hd (List.tl cycle))
    (List.tl (List.tl cycle) == cycle);
  Printf.printf "inner: %d\n" (Inner.f 21);
  Printf.printf "apply_twice: %d\n" (Link_units_b.apply_twice Inner.f 5);
  (match raise_error "boom" with
   | exception Error s -> Printf.printf "exception: %s\n" s
   | () -> print_endline "no exception");
  (* The exception of another unit is the same one. *)
  (match Link_units_b.apply_twice (fun _ -> raise (Error "twice")) 0 with
   | exception Error s -> Printf.printf "exception: %s\n" s
   | _ -> print_endline "no exception");
  Printf.printf "module: %d\n"
    (Sys.opaque_identity Link_units_a.Inner.f (Sys.opaque_identity 100))
