(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Three-way mutual recursion: the closure block has THREE function
   slots with TWO infix headers between them. Layout:
     [F_0] [infix] [F_1] [infix] [F_2] (env)
   The closure-scan must iterate past both infix headers, identifying
   each slot and darkening each [Code_block] back-pointer. We exercise
   mixed slot sizes (a 1-arg and a 2-arg), and we deliberately hold
   the LAST function (F_2), whose closure value is an Infix_tag pointer
   into the middle of the block — the GC must follow [Infix_offset_hd]
   back to F_0 to find the real header. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

(* Hold the LAST of three (an Infix_tag pointer). *)
let last_only = ref (fun _ -> 0)

(* Hold the MIDDLE of three (also an Infix_tag pointer). *)
let middle_only = ref (fun _ -> 0)

let[@inline never] populate_last_only () =
  last_only := Eval.eval <[
    let rec a n = if n <= 0 then 1 else b (n - 1) + 1
    and b n = if n <= 0 then 2 else c (n - 1) + 2
    and c n = if n <= 0 then 3 else a (n - 1) + 3
    in c
  ]>

let[@inline never] populate_middle_only () =
  middle_only := Eval.eval <[
    (* Mixed slot sizes: a is arity 1 (size 2), b is arity 2 (size 3),
       c is arity 1 (size 2). The infix header offsets vary. *)
    let rec a n = if n <= 0 then 100 else b (n - 1) 7
    and b x y = if x <= 0 then y else c (x - 1) + y
    and c n = if n <= 0 then 0 else a (n - 1)
    in b 5
  ]>

let () =
  report "start";
  populate_last_only ();
  populate_middle_only ();
  Printf.printf "last_only 3 = %d\n" (!last_only 3);
  Printf.printf "last_only 6 = %d\n" (!last_only 6);
  Printf.printf "middle_only 0 = %d\n" (!middle_only 0);
  Printf.printf "middle_only 4 = %d\n" (!middle_only 4);
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  Gc.compact ();
  Printf.printf "stable: last 3 = %d, middle 0 = %d\n"
    (!last_only 3) (!middle_only 0);
  report "after 3x Gc.compact (both held)";

  last_only := (fun _ -> 0);
  Gc.compact ();
  Printf.printf "middle still: %d\n" (!middle_only 4);
  report "after release last_only";

  middle_only := (fun _ -> 0);
  Gc.compact ();
  report "after release middle_only"
