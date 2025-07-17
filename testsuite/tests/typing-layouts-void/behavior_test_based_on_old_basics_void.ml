(* TEST
 reference = "${test_source_directory}/behavior_test_based_on_old_basics_void.reference";
 include stdlib_upstream_compatible;
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

(* [old_basics_void.ml] is a test from an older version of void. Because it is
   an expect test, it does not test the native compiler. This test is similar,
   but written using asserts only *)

(* CR layouts v5: eliminate various restructions about how void is used from
   this file. *)

type t_void : void

type void_rec = { v : t_void } [@@unboxed];;

external t_void : unit -> t_void = "%unbox_unit"

let void_rec () = { v = t_void () }

(**************************************************)
(* Test 1: Evaluation order of records with voids *)
type baz = { a1 : void_rec;
             a2 : void_rec;
             x : int;
             v : void_rec;
             z : int;
             b1 : void_rec;
             b2 : void_rec}

let r = ref []

let cons_r x = r := x :: !r

let id1 {a1; a2; x; v; z; b1; b2} =
  {a1 = (cons_r 11; {v = ((cons_r 12; a1).v)});
   a2 = (cons_r 9; {v = ((cons_r 10; a2).v)});
   x = (cons_r 8; x);
   v = (cons_r 6; {v = ((cons_r 7; v).v)});
   z = (cons_r 5; z);
   b1 = (cons_r 3; {v = ((cons_r 4; b1).v)});
   b2 = (cons_r 1; {v = ((cons_r 2; b2).v)});
  }

let b' : baz = {
  a1 = void_rec ();
  a2 = void_rec ();
  x = 3;
  v = void_rec ();
  z = 42;
  b1 = void_rec ();
  b2 = void_rec ();
}

let b' = id1 b'

let _ = assert (List.for_all2 (=) !r [12;11;10;9;8;7;6;5;4;3;2;1]);;
let _ = assert (b'.x = 3 && b'.z = 42);;

(* Same thing, but showing that it's the order of the declaration that matters
   *)

let () = r := []

let id1' {a1; a2; x; v; z; b1; b2} =
  {a2 = (cons_r 9; {v = ((cons_r 10; a2).v)});
   b2 = (cons_r 1; {v = ((cons_r 2; b2).v)});
   x = (cons_r 8; x);
   a1 = (cons_r 11; {v = ((cons_r 12; a1).v)});
   z = (cons_r 5; z);
   b1 = (cons_r 3; {v = ((cons_r 4; b1).v)});
   v = (cons_r 6; {v = ((cons_r 7; v).v)});
  }

let b' = id1' b'

let _ =
  assert (List.for_all2 (=) !r [12;11;10;9;8;7;6;5;4;3;2;1]);
  assert (b'.x = 3 && b'.z = 42)
;;

(***************************************************)
(* Test 2: evaluation order of variants with voids *)
type void_variant =
    A of t_void * void_rec * int * void_rec * int * void_rec * t_void
  | B of t_void
  | C of void_rec * t_void
  | D of { a1 : t_void;
           a2 : void_rec;
           x : int;
           v : void_rec;
           z : int;
           b1 : void_rec;
           b2 : t_void }

let r = ref []

let cons_r x = r := x :: !r

let id1 = function
  | A (a1, a2, x, v, z, b1, b2) ->
     A ((cons_r 10; a1),
        (cons_r 8; {v = ((cons_r 9; a2).v)}),
        (cons_r 7; x),
        (cons_r 5; {v = ((cons_r 6; v).v)}),
        (cons_r 4; z),
        (cons_r 2; {v = ((cons_r 3; b1).v)}),
        (cons_r 1; b2))
  | B v -> cons_r 1; B (cons_r 2; v)
  | C (vr,v) -> cons_r 1; C ({v = (cons_r 3; vr).v}, (cons_r 2; v))
  | D {a1; a2; x; v; z; b1; b2} ->
    D {a1 = (cons_r 10; a1);
       a2 = (cons_r 8; {v = ((cons_r 9; a2).v)});
       x = (cons_r 7; x);
       v = (cons_r 5; {v = ((cons_r 6; v).v)});
       z = (cons_r 4; z);
       b1 = (cons_r 2; {v = ((cons_r 3; b1).v)});
       b2 = (cons_r 1; b2)}

let magic_A = A (t_void (), void_rec (), 3, void_rec (), 42, void_rec (), t_void ())
let magic_A = id1 magic_A

let _ =
  assert (List.for_all2 (=) !r [10;9;8;7;6;5;4;3;2;1]);
  match magic_A with
  | A (_, _, x, _, z, _, _) -> assert (x = 3 && z = 42)
  | _ -> assert false
;;

let _ = r := []
let magic_B = B (t_void ())
let magic_B = id1 magic_B
let _ =
  assert (List.for_all2 (=) !r [2;1]);
  match magic_B with
  | B _ -> ()
  | _ -> assert false
;;

let _ = r := []
let magic_C : void_variant = C (void_rec (), t_void ())
let magic_C = id1 magic_C
let _ =
  assert (List.for_all2 (=) !r [3;2;1]);
  match magic_C with
  | C (_, _) -> ()
  | _ -> assert false
;;

let _ = r := []

let magic_D = D {
  a1 = t_void ();
  a2 = void_rec ();
  x = 3;
  v = void_rec ();
  z = 42;
  b1 = void_rec ();
  b2 = t_void ();
}
let magic_D = id1 magic_D
let _ =
  assert (List.for_all2 (=) !r [10;9;8;7;6;5;4;3;2;1]);
  match magic_D with
  | D { x; z; _ } -> assert (x = 3 && z = 42)
  | _ -> assert false
;;

type all_void_variant =
  | C2 of void_rec * t_void
  | D2 of t_void

let all_void_id = function
  | C2 (vr,v) -> cons_r 1; C2 ({v = (cons_r 3; vr).v}, (cons_r 2; v))
  | D2 _ -> assert false

let _ = r := []
let magic_C = C2 (void_rec (), t_void ())
let magic_C = all_void_id magic_C
let _ =
  assert (List.for_all2 (=) !r [3;2;1]);
  match magic_C with
  | C2 (_, _) -> ()
  | _ -> assert false
;;

(******************************************)
(* Test 3: top-level void bindings banned *)

(* Skipping tests that don't typecheck *)

let () = r := []
module M3_4 = struct
  (* But it's fine if you don't bind it *)
  let _ =
    match cons_r 1; magic_B with
    | B v -> cons_r 2; v
    | _ -> assert false
end;;
let _ = assert (List.for_all2 (=) !r [2;1]);;

(*************************************)
(* Test 4: Void to left of semicolon *)
let () = r := []

type void_holder = V of t_void
let vh : void_holder = V (t_void ())

let [@warning "-10"] f4 (V v) =
  v;
  cons_r 1;
  (cons_r 2; { v = (cons_r 3; v) });
  cons_r 4;
  (cons_r 5; v);
  cons_r 6

let _ = f4 vh

let _ = assert (List.for_all2 (=) !r [6;5;4;3;2;1]);;

(**********************************************************************)
(* Test 5: local binding of void things is allowed and works sensibly *)
let () = r := []

let local_void_bindings_1 vh =
  let V v = cons_r 1; vh in
  {a1 = {v = (cons_r 8; v)};
   a2 = {v = (cons_r 7; v)};
   x = (cons_r 6; 12);
   v = (cons_r 5; {v});
   z = (cons_r 4; 13);
   b1 = {v = (cons_r 3; v)};
   b2 = (cons_r 2; {v})}

let { x; z; _ } = local_void_bindings_1 vh

let _ =
  assert (List.for_all2 (=) !r [8;7;6;5;4;3;2;1]);
  assert (x = 12 && z = 13)
;;

let local_void_bindings_2 b =
  let {z; a1; b1; x; b2} = b in
  (x, V b2.v, V b1.v, z, V a1.v)

let (x, _, vh2, z, _) = local_void_bindings_2 b'

let _ = assert (x = 3 && z = 42);;

let () = r := []

let local_void_bindings_3 vh1 x y =
  let v1 =
    cons_r 1;
    match vh1 with
    | V v -> v
  in
  let x = cons_r 2; x + y in
  let v2 =
    cons_r 3;
    let _ =
      match {v = v1} with
      | {v} -> cons_r 4; v
    in
    match vh2 with
    | V v -> cons_r 5; v
  in
  let vr = {v = (cons_r 6; v2)} in
  let {v = v3} : void_rec = cons_r 7; vr in
  let z = cons_r 8; y + x in
  cons_r 9;
  {a1 = {v = v1};
   a2 = {v = let V v = vh in v};
   x;
   v = {v = v2};
   z = z;
   b1 = vr;
   b2 = {v = v3}}

let {x;z} = local_void_bindings_3 vh 3 42

let () =
  assert (x = 45 && z = 87)

let _ =
  assert (List.for_all2 (=) !r [9;8;7;6;5;4;3;2;1]);
  assert (x = 45 && z = 87)
;;

(**************************************************************)
(* Test 6: Compilation of exception patterns in void matches. *)

(* CR layouts v5: Once non-values are allowed in extensible variants, port this
   test from [old_basics_void.ml] *)

(*******************************************************)
(* Test 7: compilation of unboxed inlined void records *)
let () = r := []

type unboxed_inlined_void_rec =
  | UIVR of { uivr_v : t_void } [@@unboxed]

type uivr_holder = {uivrh_x : int; uivrh_v : unboxed_inlined_void_rec }

let make_uivr_holder vh =
  let uivrh =
    cons_r 1;
    match cons_r 2; vh with
    | V v -> begin
        cons_r 3;
        { uivrh_x = (cons_r 6; 7);
          uivrh_v = (cons_r 4; UIVR { uivr_v = (cons_r 5; v) }) }
      end
  in
  cons_r uivrh.uivrh_x; uivrh

let _ = make_uivr_holder vh
let _ = assert (List.for_all2 (=) !r [7;6;5;4;3;2;1]);;

(*****************************************************************************)
(* Test 8: void bindings in or patterns that include both normal and exception
   patterns *)
exception Test8 of int * void_holder

type test8_rec = {t8_x : int; t8_v : t_void}

let test8 (f : unit -> test8_rec) : int * void_holder =
  match cons_r 1; f () with
  | ({t8_x = x; t8_v = v} | exception (Test8 (x, V v))) ->
    begin
      cons_r 3;
      x, V (cons_r 4; v)
    end

let () = r := []

let (x, _) = test8 (fun () -> let V v = vh in cons_r 2; {t8_x = 42; t8_v = v})

let () = assert (x = 42)
let () = assert (List.for_all2 (=) !r [4;3;2;1]);;

let () = r := []

let (x, _) = test8 (fun () -> cons_r 2; raise (Test8 (3,vh)))

let () = assert (x = 3)
let () = assert (List.for_all2 (=) !r [4;3;2;1]);;

(***********************************)
(* Test 9: voids in let rec groups *)

(* CR layouts v5: Once non-values are allowed in let-recs, port this test from
   [old_basics_void.ml] *)

(***********************************)

let () = print_endline "All tests passed."
