(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on


(** Quote-splice cancellation in [reduce_head] for [unify] **)

(* One cancellation *)

(* <[$A]> = A *)
let _ : <[$unit]> = ()
[%%expect {|
- : unit = ()
|}]
(* $(<[A]>) = A *)
let _ : <[ $(<[unit]>) ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]

(* Two cancellations *)

(* <[<[$$A]>]> = A *)
let _ : <[<[$($unit)]>]> = ()
[%%expect {|
- : unit = ()
|}]
(* $$<[<[A]>]> = A *)
let _ : <[<[ $($(<[<[unit]>]>)) ]> expr]> expr = <[<[()]>]>
[%%expect {|
- : <[<[unit]> expr]> expr = <[<[()]>]>
|}]
(* $(<[$<[A]>]> = A *)
let _ : <[ $(<[$(<[unit]>)]>) ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]
(* <[$<[$A]>]> = A *)
let _ : <[$(<[$unit]>)]> = ()
[%%expect {|
- : unit = ()
|}]
(* $<[<[$A]>]> = A *)
let _ : <[ $(<[<[$unit]>]>) ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]
(* <[<[$$A]>]> = A *)
let _ : <[ <[$($(<[unit]>))]> ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]

(* Three cancellations *)

(* <[<[<[$$$A]>]>]> = A *)
let _ : <[<[<[$($($unit))]>]>]> = ()
[%%expect {|
- : unit = ()
|}]
(* $$$<[<[<[A]>]>]> = A *)
let _ : <[<[<[ $($($(<[<[<[unit]>]>]>))) ]> expr]> expr]> expr = <[<[<[()]>]>]>
[%%expect {|
- : <[<[<[unit]> expr]> expr]> expr = <[<[<[()]>]>]>
|}]

(* Other cancellation examples *)

let _ : <[ <[$('a)]> -> $(<['a]>) ]> expr = <[ fun (x : 'a) -> (x : 'a) ]>
[%%expect {|
- : <[$('a) -> $('a)]> expr = <[fun (x : 'a) -> (x : 'a)]>
|}]
let _ : <[<[$($('a))]>]> -> unit  = fun (x : 'a) -> ()
[%%expect {|
- : 'a -> unit = <fun>
|}]
