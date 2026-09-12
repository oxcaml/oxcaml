(* TEST
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   native;
 }{
   bytecode;
 }
*)

(* Functional updates that change the representation *)

module Float_u = Stdlib_upstream_compatible.Float_u

type ('a : any) r = { x : 'a; y : int }

(* Boxed to mixed *)
let () =
  let update_x (r : int r) (x : float#) : float# r = { r with x } in
  let r = update_x { x = 42; y = 7 } #2.5 in
  Printf.printf "boxed to mixed: x=%.1f y=%d\n" (Float_u.to_float r.x) r.y

(* Mixed to boxed *)
let () =
  let update_x (r : float# r) (x : int) : int r = { r with x } in
  let r = update_x { x = #2.5; y = 7 } 42 in
  Printf.printf "mixed to boxed: x=%d y=%d\n" r.x r.y

(* Mixed to boxed, with the original's representation determined after the
   update *)
let () =
  let update_x (r : _ r) =
    let r' = { r with x = "hi" } in
    let _ : float# = r.x in
    r'
  in
  let r = update_x { x = #2.5; y = 7 } in
  Printf.printf "deferred: x=%s y=%d\n" r.x r.y

(* Mixed to mixed with different flat layouts *)
type ('a : any) p = { u : 'a; v : float# }

let () =
  let update_u (p : #(float# * float#) p) (u : float#) : float# p =
    { p with u }
  in
  let p = update_u { u = #(#1.25, #1.5); v = #7.5 } #3.5 in
  Printf.printf "mixed to mixed: u=%.1f v=%.1f\n"
    (Float_u.to_float p.u) (Float_u.to_float p.v)

(* Inline record, boxed to mixed *)
type ('a : any) t = Ia of 'a | Ib of { x : 'a; y : int }

let () =
  let v : int t = Ib { x = 5; y = 7 } in
  let v' : float# t =
    match v with
    | Ia _ -> assert false
    | Ib r -> Ib { r with x = #2.5 }
  in
  match v' with
  | Ia _ -> print_endline "inline: matched Ia (WRONG)"
  | Ib { x; y } ->
    Printf.printf "inline: x=%.1f y=%d\n" (Float_u.to_float x) y

(* Mutable record *)
type ('a : any) m = { a : 'a; mutable b : int }

let () =
  let orig : int m = { a = 1; b = 2 } in
  let copy : float# m = { orig with a = #3.5 } in
  copy.b <- 99;
  Printf.printf "mutable: orig.b=%d copy.b=%d copy.a=%.1f\n"
    orig.b copy.b (Float_u.to_float copy.a)

(* Unboxed record whose source and destination differ in unarized layout *)
type ('a : any) ur = #{ ua : 'a; ub : int }

let () =
  let update_ua (r : #(float# * float#) ur) (ua : float#) : float# ur =
    #{ r with ua }
  in
  let #{ ua; ub } = update_ua #{ ua = #(#1.25, #1.5); ub = 7 } #2.5 in
  Printf.printf "unboxed record: ua=%.1f ub=%d\n" (Float_u.to_float ua) ub
