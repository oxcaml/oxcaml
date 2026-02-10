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
       but an expression was expected of type "<[unit]>"
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

(* FIXME: these are quote-miskinded, but we don't have quoted kinds yet anyway *)

let f (x : Obj.t) (Equal : (<[$Obj.t * int]>, <[<[string]> * int]>) Type.eq) : <[<[string]>]> = x
[%%expect {|
val f :
  Obj.t ->
  (<[$(Obj.t) * int]>, <[<[string]> * int]>) Type.eq -> <[<[string]>]> =
  <fun>
|}]

let f (x : string) (Equal : (<[<[Obj.t]> * int]>, <[$string * int]>) Type.eq) : <[<[Obj.t]>]> = x
[%%expect {|
val f :
  string ->
  (<[<[Obj.t]> * int]>, <[$(string) * int]>) Type.eq -> <[<[Obj.t]>]> = <fun>
|}]

let f (type a) (x : a) (Equal : (<[<[Obj.t]> * int]>, <[$a * int]>) Type.eq) : <[<[Obj.t]>]> = x
[%%expect {|
val f : 'a -> (<[<[Obj.t]> * int]>, <[$('a) * int]>) Type.eq -> <[<[Obj.t]>]> =
  <fun>
|}]

let f (type a) (x : a) (Equal : (<[$a * int]>, <[<[string]> * int]>) Type.eq) : <[<[string]>]> = x
[%%expect {|
val f :
  'a -> (<[$('a) * int]>, <[<[string]> * int]>) Type.eq -> <[<[string]>]> =
  <fun>
|}]

let f (x : int) (Equal : (<[$int * int]>, <[<[string]> * int]>) Type.eq) : <[<[string]>]> = x
[%%expect {|
Line 1, characters 17-22:
1 | let f (x : int) (Equal : (<[$int * int]>, <[<[string]> * int]>) Type.eq) : <[<[string]>]> = x
                     ^^^^^
Error: This pattern matches values of type
         "(<[$(int) * int]>, <[$(int) * int]>) Type.eq"
       but a pattern was expected which matches values of type
         "(<[$(int) * int]>, <[<[string]> * int]>) Type.eq"
       Type "$(int)" is not compatible with type "<[string]>"
|}]
