(* TEST
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   native;
 }{
   bytecode;
 }
*)

(* Tests that inline records in variants with [any] type parameters are built
   with the correct tag and shape. *)

module Float_u = Stdlib_upstream_compatible.Float_u

(* An inline record that is not the first non-constant constructor *)
type ('a : any) t = Ia of 'a | Ib of { x : 'a; y : int }

let () =
  let v : int t = Ib { x = 5; y = 7 } in
  Printf.printf "tag: %d\n" (Obj.tag (Obj.repr v));
  (match v with
   | Ia _ -> print_endline "matched Ia (WRONG)"
   | Ib { x; y } -> Printf.printf "matched Ib x=%d y=%d\n" x y);
  (match (Ia 3 : int t) with
   | Ia a -> Printf.printf "matched Ia a=%d\n" a
   | Ib _ -> print_endline "matched Ib (WRONG)")

(* A mixed inline record *)
let () =
  let v : float# t = Ib { x = #2.5; y = 7 } in
  (match v with
   | Ia _ -> print_endline "matched Ia (WRONG)"
   | Ib { x; y } ->
     Printf.printf "matched Ib x=%.1f y=%d\n" (Float_u.to_float x) y)

(* Several constructors: constant and non-constant interleaved *)
type ('a : any) m =
  | A
  | B of int
  | C of { c : 'a }
  | D of { d : 'a; e : int }

let describe (v : string m) =
  match v with
  | A -> "A"
  | B b -> Printf.sprintf "B %d" b
  | C { c } -> Printf.sprintf "C %s" c
  | D { d; e } -> Printf.sprintf "D %s %d" d e

let () =
  Printf.printf "tags: %d %d %d\n"
    (Obj.tag (Obj.repr (B 1 : string m)))
    (Obj.tag (Obj.repr (C { c = "x" } : string m)))
    (Obj.tag (Obj.repr (D { d = "y"; e = 2 } : string m)));
  print_endline (describe A);
  print_endline (describe (B 1));
  print_endline (describe (C { c = "x" }));
  print_endline (describe (D { d = "y"; e = 2 }))

(* Functional update *)
let () =
  let v : int t = Ib { x = 5; y = 7 } in
  let v' = match v with Ia _ -> v | Ib r -> Ib { r with x = r.x + 100 } in
  (match v' with
   | Ia _ -> print_endline "matched Ia (WRONG)"
   | Ib { x; y } -> Printf.printf "after update: x=%d y=%d\n" x y)

(* Mutable field *)
type ('a : any) mut = Skip of 'a | Mut of { mutable m : 'a; k : int }

let () =
  let v : int mut = Mut { m = 1; k = 2 } in
  (match v with Mut r -> r.m <- r.m + 10 | Skip _ -> ());
  (match v with
   | Mut { m; k } -> Printf.printf "mut: m=%d k=%d\n" m k
   | Skip _ -> print_endline "matched Skip (WRONG)")

(* Inline records never become flat float records *)
type ('a : any) fl = Fa of 'a | Fb of { x : 'a; y : float }

let () =
  let v : float fl = Fb { x = 1.5; y = 2.5 } in
  Printf.printf "float tag: %d (is double_array_tag: %b)\n"
    (Obj.tag (Obj.repr v))
    (Obj.tag (Obj.repr v) = Obj.double_array_tag);
  (match v with
   | Fa _ -> print_endline "matched Fa (WRONG)"
   | Fb { x; y } -> Printf.printf "matched Fb x=%.1f y=%.1f\n" x y)

(* [@@unboxed] inline record with [any]: representation is the field itself *)
type ('a : any) ubx = U of { u : 'a } [@@unboxed]

let () =
  let v : int ubx = U { u = 42 } in
  Printf.printf "unboxed is_int: %b\n" (Obj.is_int (Obj.repr v));
  (match v with U { u } -> Printf.printf "matched U u=%d\n" u)
