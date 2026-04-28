(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Closure with a mix of scannable (refs, strings, arrays) and
   non-scannable (ints) captures. The closure block layout is
     [code; closinfo; <non-scannable env>; <scannable env>]
   with [startenv] pointing at the first scannable word. The runtime
   closure-scan must classify the function slot as size-2 from the
   closinfo arity (arity 1) without being misled by the non-scannable
   env words sitting between the slot and the scannable env. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let () = Random.init 17

let kept = ref (fun n -> n)

let[@inline never] populate () =
  kept := Eval.eval <[
    let i = Random.int 100 in              (* non-scannable *)
    let j = Random.int 100 in              (* non-scannable *)
    let r = ref (Random.int 100) in        (* scannable *)
    let s = "tag-" ^ string_of_int i in    (* scannable *)
    let a = Array.make 4 j in              (* scannable *)
    fun n ->
      r := !r + n;
      a.(n mod 4) <- a.(n mod 4) + n;
      !r + i + j + String.length s
        + Array.fold_left (+) 0 a
  ]>

let () =
  report "start";
  populate ();
  let v0 = !kept 1 in
  let v1 = !kept 2 in
  let v2 = !kept 3 in
  Printf.printf "calls: %d %d %d\n" v0 v1 v2;
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  report "after 2x Gc.compact (held)";
  let v3 = !kept 4 in
  let v4 = !kept 5 in
  Printf.printf "after compact: %d %d\n" v3 v4;

  kept := (fun n -> n);
  Gc.compact ();
  report "after release"
