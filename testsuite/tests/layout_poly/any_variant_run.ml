(* TEST
 reference = "${test_source_directory}/any_variant_run.reference";
 flambda2;
 {
   flags = "-extension layout_poly_alpha -extension layouts_beta";
   native;
 }{
   flags = "-extension layout_poly_alpha -extension layouts_beta -Oclassic";
   native;
 }{
   flags = "-extension layout_poly_alpha -extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layout_poly_alpha -extension layouts_beta";
   bytecode;
 }
*)

(* [poly_] bindings over records and variants with an [any]-field,
   instantiated at value, float64, and product layouts *)

external box_float : float# -> float = "%box_float"

type ('a : any) t =
  | None
  | Some of 'a

let poly_ map_or f y = function
  | None -> y
  | Some x -> f x

type ('a : any) r = { v : 'a; n : int }

let poly_ mk_r v = { v; n = 7 }
let poly_ get_v (r : _ r) = r.v
let poly_ get_n (r : _ r) = r.n
let poly_ with_n (r : _ r) = { r with n = 100 }

type ('a : any) c = { mutable payload : 'a; tag : int }

let poly_ set_payload (c : _ c) x = c.payload <- x

type ('a : any) ar = { mutable a : int [@atomic]; v : 'a }

let poly_ bump (r : _ ar) = r.a <- r.a + 1
let poly_ read_a (r : _ ar) = r.a

type ('a : any) ir =
  | A of { x : 'a; y : int }
  | B

let poly_ mk_ir x = A { x; y = 21 }
let poly_ get_x = function
  | A { x; _ } -> x
  | B -> assert false

let () =
  Printf.printf "map_or value: %d\n"
    (map_or (fun s -> String.length s) 0 (Some "hello"));
  Printf.printf "map_or value none: %d\n"
    (map_or (fun s -> String.length s) 42 None);
  Printf.printf "map_or float64: %.2f\n"
    (box_float (map_or (fun x -> x) #0.0 (Some #3.25)));
  let #(a, b) = map_or (fun p -> p) #(0, "z") (Some #(5, "five")) in
  Printf.printf "map_or product: %d %s\n" a b;
  let r1 = mk_r "str" in
  Printf.printf "record value: %s %d\n" (get_v r1) (get_n r1);
  let r2 = mk_r #2.5 in
  Printf.printf "record float64: %.2f %d\n" (box_float (get_v r2)) (get_n r2);
  let r3 = mk_r #(1, 2.0) in
  let #(i, f) = get_v r3 in
  Printf.printf "record product: %d %.1f %d\n" i f (get_n r3);
  let r4 = with_n { v = #1.25; n = 0 } in
  Printf.printf "with_n float64: %.2f %d\n" (box_float r4.v) r4.n;
  let r5 = with_n { v = "s"; n = 0 } in
  Printf.printf "with_n value: %s %d\n" r5.v r5.n;
  let c = { payload = #1.5; tag = 9 } in
  set_payload c #6.75;
  Printf.printf "set_payload float64: %.2f %d\n" (box_float c.payload) c.tag;
  let c2 = { payload = "a"; tag = 3 } in
  set_payload c2 "bee";
  Printf.printf "set_payload value: %s %d\n" c2.payload c2.tag;
  let ar = { a = 5; v = #2.5 } in
  bump ar;
  Printf.printf "atomic float64: %d %.2f\n" (read_a ar) (box_float ar.v);
  let ar2 = { a = 8; v = "x" } in
  bump ar2;
  Printf.printf "atomic value: %d %s\n" (read_a ar2) ar2.v;
  (match mk_ir #9.5 with
   | A { x; y } -> Printf.printf "ir float64: %.2f %d\n" (box_float x) y
   | B -> assert false);
  Printf.printf "ir value: %s\n" (get_x (mk_ir "inl"))
