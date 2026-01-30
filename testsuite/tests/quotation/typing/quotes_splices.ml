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


(* Flexibility checks -- unifying variable under quotes/splices *)

(* one *)

let _ : <[ $('a) ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]

(* two *)

let _ : <[ <[ $($('a)) ]> expr ]> expr = <[<[()]>]>
[%%expect {|
- : <[<[unit]> expr]> expr = <[<[()]>]>
|}]

(* three *)

let _ : <[ <[ <[ $($($('a))) ]> expr ]> expr ]> expr = <[<[<[()]>]>]>
[%%expect {|
- : <[<[<[unit]> expr]> expr]> expr = <[<[<[()]>]>]>
|}]


(* Flexibility checks -- unifying locally-equated type under quotes/splices *)

(* one *)

let f (type a) (x : <[$(a)]> expr) (Equal : (a, int) Type.eq) : <[int]> expr = x
[%%expect {|
val f : 'a expr -> ('a, <[int]>) Type.eq -> <[int]> expr = <fun>
|}]
