(* TEST
 expect;
*)

let a0 = [%eval: int];;
let b0 = a0 <[ 1 ]>;;

[%%expect{|
val a0 : <[ int ]> expr -> int = <fun>
Exception: Invalid_argument "1".
|}]

let a1 = [%eval: int -> int list];;
let b1 = a1 <[ fun x -> [ x ; x + 1 ] ]>;;
let c1 = b1 42;;

[%%expect{|
val a1 : <[ int -> int list ]> expr -> int -> int list = <fun>
Exception: Invalid_argument "fun x -> (::) (x, ((::) ((x + 1), [])))".
|}]

let a2 = [%eval: Buffer.t]
let b2 = a2 <[ Buffer.create 42 ]>;;

[%%expect{|
val a2 : <[ Buffer.t ]> expr -> Buffer.t = <fun>
Exception: Invalid_argument "Stdlib.Buffer.create 42".
|}]

let a3 = [%eval: unit]
(* This quote passes type-checking but fail during compilation, this lets us test exactly
   when compilation of the quote happens (and what happens when it fails). *)
let b3 = a3 <[
    let ignore (_ @ local) = () in
    let[@tail_mod_cons] rec foo x = exclave_
      if x = 0 then [] else x :: foo (x - 1)
    in
    ignore (foo 5)
  ]>;;

[%%expect{|
val a3 : <[ unit ]> expr -> unit = <fun>
Exception:
Invalid_argument
 "let ignore = (fun _ -> ()) in\nlet rec foo =\n(fun x -> exclave_ if (x = 0) then [] else (::) (x, (foo (x - 1)))) in\nignore (foo 5)".
|}]
