(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

let _ : unit = ()
[%%expect {|
- : unit = ()
|}]


(* Cancellations with different numbers of layers *)

(* one *)

let _ : <[$unit]> = ()
[%%expect {|
- : unit = ()
|}]

let _ : <[ $(<[unit]>) ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]

(* two *)

let _ : <[<[$($unit)]>]> = ()
[%%expect {|
- : unit = ()
|}]

let _ : <[<[ $($(<[<[unit]>]>)) ]> expr]> expr = <[<[()]>]>
[%%expect {|
- : <[<[unit]> expr]> expr = <[<[()]>]>
|}]

let _ : <[ $(<[$(<[unit]>)]>) ]> = <[()]>
[%%expect {|
Line 1, characters 35-41:
1 | let _ : <[ $(<[$(<[unit]>)]>) ]> = <[()]>
                                       ^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "<[unit]>" = "<[unit]>"
|}]

let _ : <[$(<[$unit]>)]> = ()
[%%expect {|
- : unit = ()
|}]

let _ : <[ $(<[<[$unit]>]>) ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]

let _ : <[ <[$($(<[unit]>))]> ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]

(* three -- just the trivial ones *)

let _ : <[<[<[$($($unit))]>]>]> = ()
[%%expect {|
- : unit = ()
|}]

let _ : <[<[<[ $($($(<[<[<[unit]>]>]>))) ]> expr]> expr]> expr = <[<[<[()]>]>]>
[%%expect {|
- : <[<[<[unit]> expr]> expr]> expr = <[<[<[()]>]>]>
|}]

(* cancellation without an expected concrete type *)

let _ : <[ <[$('a)]> -> $(<['a]>) ]> expr = <[ fun x -> x ]>
[%%expect {|
- : <[$('a) -> $('a)]> expr = <[fun x -> x]>
|}]

let _ : <[<[$($('a))]>]> -> unit  = fun _ -> ()
[%%expect {|
- : 'a -> unit = <fun>
|}]


(* Flexibility checks -- unifying with variable under quotes/splices *)
