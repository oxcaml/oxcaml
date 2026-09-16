(* TEST
 flags = "-extension layouts_beta";
 expect.opt;
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
>> Fatal error: CC.close_let: defining_exprs should have the same length as number of variables

Context is: closure converting function
with [our_let_rec_ident] copy_dps/0 (function slot (copy_dps/1 ∷ 𝕍))

Uncaught exception: Misc.Fatal_error

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
- : int * int = (11, 0)
|}]
