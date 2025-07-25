(* TEST
 reference = "${test_source_directory}/voids.reference";
 include stdlib_stable;
 flambda2;
 {
   native;
 }{
   flags = "-Oclassic";
   native;
 }{
   flags = "-O3";
   native;
 }{
   bytecode;
 }
*)

(* This file contains various tests for void. It's not an expect test to make
   sure it gets tested for native code. *)

(***********)
(* Prelude *)

type void : void mod everything
external void : unit -> void = "%unbox_unit"

let test_num = ref 0
let start_test name =
  incr test_num;
  Printf.printf "Test %d: %s\n" !test_num name

let[@inline never] use_void (v : void) =
  let _ : void = v in
  1

(****************************)
(* Test 1: void in closures *)

let test1 () =
  start_test "void in closures";

  (* [go]'s closure should have an [int] (immediate), a [void] and an
     [int array] (value). *)
  let[@inline never] f1 n v steps () =
    let[@inline never] rec go k =
      if k = n
      then 0
      else begin
        let contribution = use_void v in
        let acc = go (k + 1) in
        steps.(k) <- acc + contribution;
        acc + contribution
      end
    in
    go 0
  in

  (* many args - even args are pairs, odd args are voids *)
  let[@inline never] f2_manyargs x0 v1 x2 v3 x4 v5 x6 v7 x8 v9 steps () =
    let (start_k, end_k) = x0 in
    let[@inline never] rec go k =
      if k = end_k
      then 0
      else begin
        let void_contribution =
          use_void v1 + use_void v3 + use_void v5 + use_void v7 + use_void v9 in
        let (x2_1, x2_2) = x2 in
        let (x4_1, x4_2) = x4 in
        let (x6_1, x6_2) = x6 in
        let (x8_1, x8_2) = x8 in
        let sum = x2_1 + x2_2 + x4_1 + x4_2 + x6_1 + x6_2 + x8_1 + x8_2 in
        let acc = go (k + 1) in
        steps.(k) <- acc;
        acc + sum + void_contribution
      end
    in
    go start_k
  in

  (* Test f1 *)
  let steps = Array.init 5 (fun _ -> 0) in
  let v = void () in
  let result = f1 5 v steps () in
  assert (result = 5);
  (* Each step gets the accumulator plus 1 from use_void *)
  (* Expected values: [5, 4, 3, 2, 1] *)
  assert (steps = [|5; 4; 3; 2; 1|]);

  (* Test f2_manyargs *)
  let steps = Array.init 10 (fun _ -> 0) in
  let v = void () in
  let x0 = (2, 6) in
  let x2 = (1, 2) in
  let x4 = (3, 4) in
  let x6 = (5, 6) in
  let x8 = (7, 8) in
  let result = f2_manyargs x0 v x2 v x4 v x6 v x8 v steps () in
  (* 5 + 1+2+3+4+5+6+7+8 = 41 *)
  assert (steps.(5) = 0);
  assert (steps.(4) = 41);
  assert (steps.(3) = 41 * 2);
  assert (steps.(2) = 41 * 3);
  assert (result = 41 * 4)

let _ = test1 ()

(********************************)
(* Test 2: partial applications *)

let test2 () =
  start_test "partial applications";

  let[@inline never] f v1 v2 x v3 y =
    let contribution = use_void v1 + use_void v2 + use_void v3 in
    x + y + contribution
  in

  let v = void () in

  let p1 = (Sys.opaque_identity f) v in
  let p2 = p1 v in
  let p3 = p2 42 in
  let p4 = p3 v in
  assert (p4 100 = 145);  (* 42 + 100 + 3 (from 3 voids) *)
  assert (p4 200 = 245);  (* 42 + 100 + 3 (from 3 voids) *)

  let g = Sys.opaque_identity (f v v 10) in
  assert (g v 20 = 33)  (* 10 + 20 + 3 (from 3 voids) *)

let _ = test2 ()

(****************************)
(* Test 3: over application *)

let test3 () =
  start_test "over application";

  let r = ref [] in
  let cons_r x = r := x :: !r in

  let[@inline never] f v1 v2 =
    let[@inline never] go f =
      f (cons_r (use_void v1); v2)
    in
    go
  in

  let v = void () in

  let #(x, y) =
    (Sys.opaque_identity f) v v (fun (v : void) -> cons_r 2; #(v, v)) in
  ignore (use_void x);
  ignore (use_void y);
  assert (!r = [2; 1]);

  let #(x, y) = f v v (fun (v : void) -> cons_r 3; #(v, v)) in
  ignore (use_void x);
  ignore (use_void y);
  assert (!r = [3; 1; 2; 1])

let _ = test3 ()

(****************************)
(* Test 4: products of void *)

let test4 () =
  start_test "products of void";

  (* Pair of voids *)
  let[@inline never] make_void_pair () = #(void (), void ()) in
  let[@inline never] fst_void #((v1 : void), (_ : void)) = v1 in
  let[@inline never] snd_void #((_ : void), (v2 : void)) = v2 in

  let p = make_void_pair () in
  let contribution1 = use_void (fst_void p) in
  let contribution2 = use_void (snd_void p) in
  assert (contribution1 + contribution2 = 2);

  (* Triple of voids *)
  let[@inline never] make_void_triple () = #(void (), void (), void ()) in
  let #(v1, v2, v3) = make_void_triple () in
  let contribution3 = use_void v1 + use_void v2 + use_void v3 in
  assert (contribution3 = 3);

  (* Nested products *)
  let[@inline never] make_nested () =
    #(#(void (), void ()), #(void (), void ()))
  in
  let #(#(a, b), #(c, d)) = make_nested () in
  let contribution4 = use_void a + use_void b + use_void c + use_void d in
  assert (contribution4 = 4)

let _ = test4 ()

(*******************************************)
(* Test 5: products with void and non-void *)

let test5 () =
  start_test "products with void and non-void";

  (* Void and int *)
  let[@inline never] make_void_int x = #(void (), x) in
  let[@inline never] extract_int_with_void #((v : void), x) = x + use_void v in

  let p1 = make_void_int 42 in
  assert (extract_int_with_void p1 = 43);

  (* Mixed triple *)
  let[@inline never] make_mixed x y = #(void (), x, void (), y, void ()) in
  let #(v1, x, v2, y, v3) = make_mixed 10 20 in
  let void_contribution = use_void v1 + use_void v2 + use_void v3 in
  assert (x = 10 && y = 20 && void_contribution = 3);

  (* More complex mixed products *)
  let[@inline never] make_complex () =
    #(void (), 42, #(void (), "hello"), void ())
  in
  let #(v1, n, inner, v2) = make_complex () in
  let #(v3, s) = inner in
  let total_void_contribution = use_void v1 + use_void v2 + use_void v3 in
  assert (n = 42 && s = "hello" && total_void_contribution = 3)

let _ = test5 ()

(*****************************************)
(* Test 6: variants containing only void *)

type void_holder = Vh of void

(* Multiple constructors *)
type void_variants =
  | V1 of void
  | V2 of void * void
  | V3 of void * #(void * void)

let test6 () =
  start_test "variants containing only void";

  let[@inline never] make_vh () = Vh (void ()) in
  let[@inline never] extract_void (Vh v) = v in

  let vh = make_vh () in
  let v = extract_void vh in
  let contribution = use_void v in
  assert (contribution = 1);

  let[@inline never] match_variants = function
    | V1 v -> 1 + use_void v
    | V2 (v1, v2) -> 2 + use_void v1 + use_void v2
    | V3 (v1, #(v2, v3)) -> 3 + use_void v1 + use_void v2 + use_void v3
  in

  assert (match_variants (V1 (void ())) = 2);
  assert (match_variants (V2 (void (), void ())) = 4);
  assert (match_variants (V3 (void (), #(void (), void ()))) = 6)

let _ = test6 ()

(***********************************)
(* Test 7: records containing void *)

type void_record = {
  v : void;
  x : int;
  vh : void_holder;
}

(* Mutable records *)
type mut_void_record = {
  mutable mv : void;
  y : int;
  mutable mvh : void_holder;
}

let test7 () =
  start_test "records containing void";

  let[@inline never] make_record x = {
    v = void ();
    x;
    vh = Vh (void ());
  } in

  let[@inline never] extract_fields { v; x; vh } =
    let Vh v2 = vh in
    x + use_void v + use_void v2
  in

  let r = make_record 42 in
  assert (extract_fields r = 44);  (* 42 + 2 voids *)

  let mr = {
    mv = void ();
    y = 100;
    mvh = Vh (void ());
  } in

  mr.mv <- void ();
  mr.mvh <- Vh (void ());
  assert (mr.y = 100)

let _ = test7 ()

(*********************************)
(* Test 8: exceptions with void *)

exception Void_exn of void_holder * void_record
exception Void_exn2 of void_holder * int

let test8 () =
  start_test "exceptions with void";

  let vh = Vh (void ()) in
  let vr = { v = void (); x = 42; vh = Vh (void ()) } in

  let[@inline never] raise_and_catch () =
    try
      raise (Void_exn (vh, vr))
    with
    | Void_exn (Vh v, { x; _ }) ->
        let _ : void = v in
        x
  in

  assert (raise_and_catch () = 42);

  (* Exception with void in or-pattern *)
  let[@inline never] match_exn f =
    try f ()
    with
    | Void_exn2 (Vh v, x)
    | Void_exn (Vh v, { x; _ }) ->
        let _ : void = v in
        x
  in
  assert (match_exn (fun () -> raise (Void_exn2 (Vh (void ()), 24))) = 24);
  assert (match_exn (fun () -> raise (Void_exn (Vh (void ()), vr))) = 42);
  assert (match_exn (fun () -> 5) = 5)

let _ = test8 ()

(*************************************)
(* Test 9: void to left of semicolon *)

let test9 () =
  start_test "void to left of semicolon";

  let counter = ref 0 in

  let[@warning "-10"][@inline never] side_effect () =
    incr counter;
    void ()
  in

  let[@warning "-10"][@inline never] test_semicolon () =
    side_effect ();
    let contribution1 = use_void (void ()) in
    counter := !counter + contribution1;
    side_effect ();
    let #(v1, v2) = #(void (), void ()) in
    let contribution2 = use_void v1 + use_void v2 in
    counter := !counter + contribution2;
    side_effect ();
    !counter
  in

  assert (test_semicolon () = 6)

let _ = test9 ()

(**********************************)
(* Test 10: complex void patterns *)

(* Nested pattern matching *)
type complex_void =
  | CV1 of void * void_holder * void_holder
  | CV2 of { cv_v : void; cv_x : int; cv_vh : void_holder }

let test10 () =
  start_test "complex void patterns";

  let[@inline never] match_complex = function
    | CV1 (v1, Vh v2, Vh v3) ->
        1 + use_void v1 + use_void v2 + use_void v3
    | CV2 { cv_v; cv_x; cv_vh = Vh v } ->
        cv_x + use_void cv_v + use_void v
  in

  (* Use Sys.opaque_identity to prevent pattern matching optimizations *)
  let cv1 = Sys.opaque_identity (CV1 (void (), Vh (void ()), Vh (void ()))) in
  let cv2 =
    Sys.opaque_identity
      (CV2 { cv_v = void (); cv_x = 42; cv_vh = Vh (void ()) })
  in

  assert (match_complex cv1 = 4);
  assert (match_complex cv2 = 44)

let _ = test10 ()

(****************************************)
(* Test 11: void in recursive functions *)

let test11 () =
  start_test "void in recursive functions";

  let[@inline never] rec countdown n v acc =
    let contribution = use_void v in
    if n = 0 then acc + contribution
    else countdown (n - 1) (void ()) (acc + n + contribution)
  in

  assert (countdown 5 (void ()) 0 = 21);  (* 15 + 6 void contributions *)

  let[@inline never] rec countdown_non_tail n v acc =
    let contribution = use_void v in
    if n = 0 then acc + contribution
    else countdown_non_tail (n - 1) (void ()) (acc + n + contribution) [@nontail]
  in

  assert (countdown_non_tail 5 (void ()) 0 = 21);  (* 15 + 6 void contributions *)

  let[@inline never] rec countdown_meaningfully_non_tail n v acc =
    let contribution = use_void v in
    if n = 0 then acc + contribution
    else
      1 +
      countdown_meaningfully_non_tail
        (n - 1) (void ()) (acc + n + contribution) [@nontail]
  in

  assert (countdown_meaningfully_non_tail 5 (void ()) 0 = 26);

  (* Mutual recursion *)
  let[@inline never] rec f1 n v =
    let contribution = use_void v in
    if n = 0 then contribution
    else contribution + 1 + f2 (n - 1) (void ())

  and f2 n v =
    let contribution = use_void v in
    if n = 0 then contribution
    else contribution + 1 + f1 (n - 1) (void ())
  in

  assert (f1 10 (void ()) = 21)  (* 10 + 11 void contributions *)

let _ = test11 ()

(*********************************)
(* Test 12: void with references *)

let test12 () =
  start_test "void with references";

  (* Can't have ref of void directly, but can have ref of void_holder *)
  let r = ref (Vh (void ())) in

  let[@inline never] update_ref () =
    r := Vh (void ())
  in

  let[@inline never] read_ref () =
    match !r with
    | Vh v -> use_void v
  in

  update_ref ();
  assert (read_ref () = 1)

let _ = test12 ()

(***************************)
(* Test 13: void with lazy *)

let test13 () =
  start_test "void with lazy values";

  (* Lazy void_holder *)
  let lz = lazy (Vh (void ())) in

  let[@inline never] force_lazy () =
    match Lazy.force lz with
    | Vh v -> use_void v
  in

  assert (force_lazy () = 1)

let _ = test13 ()

(*****************************************************)
(* Test 14: closures over void-void unboxed products *)

let test14 () =
  start_test "closures over void-void unboxed products";

  let[@inline never] make_closure_over_void_pair n =
    let void_pair = #(void (), void ()) in
    let[@inline never] closure k =
      let #(v1, v2) = void_pair in
      (* Use the void values from the captured pair *)
      k + use_void v1 + use_void v2
    in
    closure n
  in

  assert (make_closure_over_void_pair 10 = 12);

  (* More complex: closure over multiple void pairs *)
  let[@inline never] make_closure_multi_pairs () =
    let p1 = #(void (), void ()) in
    let p2 = #(void (), void ()) in
    let p3 = #(void (), void ()) in
    let[@inline never] sum_all () =
      let #(v1_1, v1_2) = p1 in
      let #(v2_1, v2_2) = p2 in
      let #(v3_1, v3_2) = p3 in
      use_void v1_1 + use_void v1_2 + use_void v2_1 + use_void v2_2 +
      use_void v3_1 + use_void v3_2
    in
    sum_all
  in

  let closure = make_closure_multi_pairs () in
  assert (closure () = 6)  (* 3 pairs * 2 voids each *)

let _ = test14 ()

(*********************************************************)
(* Test 15: closures over void-int-void unboxed products *)

let test15 () =
  start_test "closures over void-int-void unboxed products";

  let[@inline never] make_mixed_closure base =
    let mixed = #(void (), base, void ()) in
    let arr = Array.init 5 (fun i -> i) in
    let[@inline never] compute multiplier =
      let #(v1, x, v2) = mixed in
      Array.fold_left (fun acc y ->
        acc + y * multiplier + use_void v1 + use_void v2
      ) x arr
    in
    compute
  in

  let closure = make_mixed_closure 100 in
  assert (closure 2 = 130);  (* 100 + (0+1+2+3+4)*2 + 5*2 from voids *)

  (* Nested mixed products in closure *)
  let[@inline never] make_nested_closure () =
    let nested = #(#(void (), 10), #(20, void ())) in
    let[@inline never] extract_and_sum k =
      let #(#(v1, x), #(y, v2)) = nested in
      k * (x + y) + use_void v1 + use_void v2
    in
    extract_and_sum
  in

  let f = make_nested_closure () in
  assert (f 3 = 92)  (* 3 * 30 + 2 *)

let _ = test15 ()

(********************************************************)
(* Test 16: closures over variants containing only void *)

let test16 () =
  start_test "closures over variants containing only void";

  let[@inline never] make_variant_closure variant =
    let[@inline never] process n =
      match variant with
      | V1 v -> n + use_void v
      | V2 (v1, v2) -> n + use_void v1 + use_void v2
      | V3 (v1, #(v2, v3)) -> n + use_void v1 + use_void v2 + use_void v3
    in
    process
  in

  let f1 = make_variant_closure (V1 (void ())) in
  let f2 = make_variant_closure (V2 (void (), void ())) in
  let f3 = make_variant_closure (V3 (void (), #(void (), void ()))) in

  assert (f1 10 = 11);
  assert (f2 10 = 12);
  assert (f3 10 = 13);

  (* Closure over mutable variant *)
  let[@inline never] make_mutable_variant_closure () =
    let r = ref (V1 (void ())) in
    let[@inline never] update_and_compute new_variant n =
      r := new_variant;
      match !r with
      | V1 v -> n + use_void v
      | V2 (v1, v2) -> n + use_void v1 + use_void v2
      | V3 (v1, #(v2, v3)) -> n + use_void v1 + use_void v2 + use_void v3
    in
    update_and_compute
  in

  let update = make_mutable_variant_closure () in
  assert (update (V2 (void (), void ())) 20 = 22);
  assert (update (V3 (void (), #(void (), void ()))) 30 = 33)

let _ = test16 ()

(***********************************)
(* Test 17: void in local closures *)

let test17 () =
  start_test "void in local closures";

  (* Version of f1 using local functions *)
  let[@inline never] f1_local n v steps () =
    let[@inline never] rec (go @ local) k =
      if k = n
      then 0
      else begin
        let contribution = use_void v in
        let acc = go (k + 1) in
        steps.(k) <- acc + contribution;
        acc + contribution
      end
    in
    go 0 [@nontail]
  in

  (* Test f1_local *)
  let steps = Array.init 5 (fun _ -> 0) in
  let v = void () in
  let result = f1_local 5 v steps () in
  assert (result = 5);
  assert (steps = [|5; 4; 3; 2; 1|]);

  (* Version of f2_manyargs using local functions *)
  let[@inline never] f2_manyargs_local x0 v1 x2 v3 x4 v5 x6 v7 x8 v9 steps () =
    let (start_k, end_k) = x0 in
    let[@inline never] rec (go @ local) k =
      if k = end_k
      then 0
      else begin
        let void_contribution =
          use_void v1 + use_void v3 + use_void v5 + use_void v7 + use_void v9 in
        let (x2_1, x2_2) = x2 in
        let (x4_1, x4_2) = x4 in
        let (x6_1, x6_2) = x6 in
        let (x8_1, x8_2) = x8 in
        let sum = x2_1 + x2_2 + x4_1 + x4_2 + x6_1 + x6_2 + x8_1 + x8_2 in
        let acc = go (k + 1) in
        steps.(k) <- acc;
        acc + sum + void_contribution
      end
    in
    go start_k [@nontail]
  in

  (* Test f2_manyargs_local *)
  let steps = Array.init 10 (fun _ -> 0) in
  let v = void () in
  let x0 = (2, 6) in
  let x2 = (1, 2) in
  let x4 = (3, 4) in
  let x6 = (5, 6) in
  let x8 = (7, 8) in
  let result = f2_manyargs_local x0 v x2 v x4 v x6 v x8 v steps () in
  assert (steps.(5) = 0);
  assert (steps.(4) = 41);
  assert (steps.(3) = 41 * 2);
  assert (steps.(2) = 41 * 3);
  assert (result = 41 * 4)

let _ = test17 ()

(***********************************************)
(* Test 18: Obj.tag of records containing void *)

type record_with_void = {
  void_field : void;
  int_field : int;
  string_field : string;
}

type record_with_multiple_voids = {
  v1 : void;
  x : int;
  v2 : void;
  y : string;
  v3 : void;
}

type variant_with_record =
  | VR of record_with_void

let test18 () =
  start_test "Obj.tag of records containing void";

  let r = { void_field = void (); int_field = 42; string_field = "hello" } in
  Printf.printf "  Obj.tag of record with void: %d\n" (Obj.tag (Obj.repr r));

  let r2 = {
    v1 = void ();
    x = 100;
    v2 = void ();
    y = "world";
    v3 = void ()
  } in
  Printf.printf "  Obj.tag of record with multiple voids: %d\n" (Obj.tag (Obj.repr r2));

  let vr = VR { void_field = void (); int_field = 123; string_field = "variant" } in
  Printf.printf "  Obj.tag of variant containing record with void: %d\n" (Obj.tag (Obj.repr vr))

let _ = test18 ()

let () = print_endline "All tests passed."
