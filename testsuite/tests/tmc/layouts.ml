(* TEST
 flags = "-extension layouts_beta";
 { expect; }
 { expect.opt; }
*)

type ('a : any) seq = Nil | Cons of 'a * 'a seq
[%%expect{|
type ('a : any) seq = Nil | Cons of 'a * 'a seq
|}]

(* Requires the TMC [delay_impure] transformation to know the layout of the
   elements of the [seq]. *)
let[@tail_mod_cons] rec copy (xs : #(int * int) seq) =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (x, (copy [@tailcall]) xs)
[%%expect{|
val copy : #(int * int) seq -> #(int * int) seq = <fun>
|}]

(* Should return (11, 22). The TMC code that writes the recursive result to the
   hole must know that hole is the _third_ thing in the block because of the
   unboxed pair. *)
let[@tail_mod_cons] rec repeat n (x : #(int * int)) =
  if n = 0 then Nil else Cons (x, (repeat [@tailcall]) (n - 1) x)

let _ =
  match repeat 1 #(11, 22) with
  | Nil -> failwith "expected Cons"
  | Cons (#(a, b), _) -> a, b
[%%expect{|
val repeat : int -> #(int * int) -> #(int * int) seq = <fun>
- : int * int = (11, 22)
|}]

(* The value tail precedes the unboxed float in native code. *)
let[@tail_mod_cons] rec copy_float (xs : float# seq) =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (x, (copy_float [@tailcall]) xs)
[%%expect{|
Lines 1-4, characters 35-57:
1 | ...................................(xs : float# seq) =
2 |   match xs with
3 |   | Nil -> Nil
4 |   | Cons (x, xs) -> Cons (x, (copy_float [@tailcall]) xs)
Warning 71 [unused-tmc-attribute]: This function is marked "@tail_mod_cons"
  but is never applied in TMC position.

Line 4, characters 29-56:
4 |   | Cons (x, xs) -> Cons (x, (copy_float [@tailcall]) xs)
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall

Line 4, characters 29-56:
4 |   | Cons (x, xs) -> Cons (x, (copy_float [@tailcall]) xs)
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall

val copy_float : float# seq -> float# seq = <fun>
|}]

external box_float : float# -> float = "%box_float"
let _ =
  match copy_float (Sys.opaque_identity (Cons (#1.5, Cons (#2.5, Nil)))) with
  | Cons (a, Cons (b, Nil)) ->
    box_float a, box_float b
  | _ -> failwith "unexpected sequence"
[%%expect{|
external box_float : float# -> float = "%box_float"
- : float * float = (1.5, 2.5)
|}]
