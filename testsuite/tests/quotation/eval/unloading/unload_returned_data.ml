(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Eval'd code returns various heap-allocated data structures (list,
   array, ref, tuple, record). Once the eval'd closure is dropped, only
   the returned data should keep its OWN unit live (if it does at all);
   most heap blocks allocated *during* the call live in the regular
   minor/major heap and don't keep the unit alive. After dropping the
   data, the unit unloads. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

(* List of length up to 3 — printed without keeping the producer alive. *)
let[@inline never] eval_list () =
  let mk = Eval.eval <[ fun n -> [n; n + 1; n + 2] ]> in
  let xs = mk 10 in
  Printf.printf "list: [%s]\n"
    (String.concat ";" (List.map string_of_int xs))

let[@inline never] eval_array () =
  let mk = Eval.eval <[ fun n -> [| n; n * 2; n * 3; n * 4 |] ]> in
  let a = mk 5 in
  Printf.printf "array: [|%d; %d; %d; %d|]\n" a.(0) a.(1) a.(2) a.(3)

let[@inline never] eval_record () =
  (* Returning a tuple — exercises a multi-field block with mixed
     scannable fields. *)
  let mk =
    Eval.eval <[ fun n -> (n, "name-" ^ string_of_int n, [| n; n * 2 |]) ]>
  in
  let (i, s, a) = mk 42 in
  Printf.printf "tuple: (%d, %s, [|%d; %d|])\n" i s a.(0) a.(1)

let[@inline never] eval_ref () =
  (* Reference returned by eval'd code is heap-allocated AND its
     contents (an int) are immediate. *)
  let mk = Eval.eval <[ fun () -> ref 0 ]> in
  let r = mk () in
  r := 11;
  r := !r + 7;
  Printf.printf "ref: %d\n" !r

let () =
  report "start";

  eval_list ();
  Gc.compact ();
  report "after eval_list + compact";

  eval_array ();
  Gc.compact ();
  report "after eval_array + compact";

  eval_record ();
  Gc.compact ();
  report "after eval_record + compact";

  eval_ref ();
  Gc.compact ();
  report "after eval_ref + compact"
