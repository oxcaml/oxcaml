(* TEST
 flambda2;
 {
   native;
 }{
   jsir;
 }{
   ocamlopt_flags = "-Oclassic";
   jsir;
 }
*)

(* Lifted closures and lifted blocks can refer to each other and potentially
   form cycles. Each JSIR variable standing for such a constant must be bound
   before it is read. *)

let[@inline never] hd = function x :: _ -> x | [] -> assert false

let rec init n f = if n <= 0 then [] else f n :: init (n - 1) f

(* A closure that refers to a lifted block containing a captured variable. *)
let () =
  let x = ref 0 in
  match init 2 (fun _ -> hd [x]) with
  | [y; z] ->
    assert (y == x && z == x);
    x := 42;
    assert (!y == 42)
  | _ -> assert false

type t = T of (unit -> t list)

(* A closure that refers to a block containing itself. *)
let rec f () = Sys.opaque_identity [T f]

(* Mutually recursive closures that refer to blocks containing each other. *)
let rec g () = Sys.opaque_identity [T h]
and h () = Sys.opaque_identity [T g; T h]

let () =
  let f' = match hd (f ()) with T f' -> f' in
  assert (f' == f);
  assert (match hd (f' ()) with T f'' -> f'' == f);
  let h' = match hd (g ()) with T h' -> h' in
  assert (h' == h);
  match h' (), h () with
  | [T g'; T h''], [T g''; _] -> assert (g' == g && h'' == h && g'' == g)
  | _ -> assert false

type u = U of (int -> unit -> u * int)

(* A closure whose nested, dynamically allocated closure refers to a block
   containing the outer closure. *)
let rec k y = Sys.opaque_identity (fun () -> hd [U k], y)

let () =
  let U k1, y1 = k 1 () in
  assert (k1 == k && y1 == 1);
  let U k2, y2 = k1 2 () in
  assert (k2 == k && y2 == 2)
