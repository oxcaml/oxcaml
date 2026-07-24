(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Eval'd code that references constants likely lifted to the eval'd
   unit's static data section: [Some 42], list literals, string
   literals, tuple literals. These blocks live at fixed addresses inside
   the JIT'd .data section and must be tracked as the unit's
   [data_blocks]. While the closure is alive the unit must stay; once
   dropped it must unload. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let some_const = ref (fun () -> None)
let list_const = ref (fun () -> [])
let str_const = ref (fun () -> "")
let tuple_const = ref (fun () -> 0, "x", 0)
let nested_const = ref (fun () -> [None])

let[@inline never] populate () =
  some_const   := Eval.eval <[ fun () -> Some 42 ]>;
  list_const   := Eval.eval <[ fun () -> [1; 2; 3; 4; 5] ]>;
  str_const    := Eval.eval <[ fun () -> "static-string-literal" ]>;
  tuple_const  := Eval.eval <[ fun () -> (7, "tag", 99) ]>;
  nested_const := Eval.eval <[ fun () -> [Some 1; Some 2; None; Some 3] ]>

let print_results () =
  (match !some_const () with
    | Some v -> Printf.printf "some_const: Some %d\n" v
    | None -> Printf.printf "some_const: None\n");
  Printf.printf "list_const: %d items, sum=%d\n"
    (List.length (!list_const ()))
    (List.fold_left (+) 0 (!list_const ()));
  Printf.printf "str_const: %s\n" (!str_const ());
  let (a, b, c) = !tuple_const () in
  Printf.printf "tuple_const: (%d, %s, %d)\n" a b c;
  Printf.printf "nested_const: %d items\n"
    (List.length (!nested_const ()))

let () =
  report "start";
  populate ();
  print_results ();
  report "after populate";

  (* Force several major cycles while the eval'd closures (and thus
     their static data) are reachable. Nothing should unload yet. *)
  Gc.compact ();
  Gc.compact ();
  report "after 2x Gc.compact (all held)";
  print_results ();

  (* Drop one at a time and verify the others stay, then drop all. *)
  some_const := (fun () -> None);
  Gc.compact ();
  report "after release some_const";
  Printf.printf "list_const still: %d items\n"
    (List.length (!list_const ()));

  list_const := (fun () -> []);
  str_const := (fun () -> "");
  tuple_const := (fun () -> 0, "x", 0);
  nested_const := (fun () -> []);
  Gc.compact ();
  report "after release all"
