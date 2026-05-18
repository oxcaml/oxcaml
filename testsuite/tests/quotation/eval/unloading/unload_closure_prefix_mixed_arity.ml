(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  runtime5;
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Stress the closure-prefix scanner over a mutually-recursive group with
   mixed arities (slot sizes 2 and 3) and an immediate captured env value. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let dummy : (int -> int) = fun x -> x

let kept : (int -> int) ref = ref dummy

let[@inline never] populate () =
  kept :=
    Eval.eval
      <[
        let k = 7 in
        let rec f x =
          if x <= 0 then k else g (x - 1) 0 + k
        and g x y =
          if x = 0 then y else g (x - 1) (y + 1)
        and h (x, y) =
          if x < 0 then y else x * y + k
        in
        fun n -> f n + g n 0 + h (n, 2)
      ]>

let () =
  report "start";
  populate ();
  Printf.printf "kept 5 = %d\n" (!kept 5);
  Gc.compact ();
  report "after populate + compact (held)";
  Printf.printf "kept 6 = %d\n" (!kept 6);
  kept := dummy;
  Gc.compact ();
  report "after release + compact"

