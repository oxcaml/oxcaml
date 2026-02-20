(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on


(* Testing quote-splice cancellation in [reduce_head] for [unify] *)


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

(* <[$A]> = A *)
let _ : <[<[$($unit)]>]> = ()
[%%expect {|
- : unit = ()
|}]
(* $(<[A]>) = A *)
let _ : <[<[ $($(<[<[unit]>]>)) ]> expr]> expr = <[<[()]>]>
[%%expect {|
- : <[<[unit]> expr]> expr = <[<[()]>]>
|}]
(* $(<[A]>) = A *)
let _ : <[ $(<[$(<[unit]>)]>) ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]
(* <[$A]> = A *)
let _ : <[$(<[$unit]>)]> = ()
[%%expect {|
- : unit = ()
|}]
(* $(<[A]>) = A and <[$A]> = A *)
let _ : <[ $(<[<[$unit]>]>) ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]
(* <[$A]> = A and $(<[A]>) = A *)
let _ : <[ <[$($(<[unit]>))]> ]> expr = <[()]>
[%%expect {|
- : <[unit]> expr = <[()]>
|}]

(* Three cancellations *)

(* <[$A]> = A *)
let _ : <[<[<[$($($unit))]>]>]> = ()
[%%expect {|
- : unit = ()
|}]
(* $(<[A]>) = A *)
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


(* Testing [unify]ing a variable under quotes/splices -- flexibility checks *)


(* Note that [generalize] type schemes have their quantified variables
   normalised to stage 0 (i.e. all of these would become ['a -> 'a]).
   This means in all tests with directly quoted variables we will eventually
   remove the quotes, as we cannot have top-level splices.
   However, unification should happen before this. *)

(* <['a]> ~ unit *)
let _ : <['a]> -> <['a]> = fun (() as x) -> x
[%%expect {|
- : unit -> unit = <fun>
|}]
(* <[<['a]>]> ~ unit *)
let _ : <[<['a]>]> -> <[<['a]>]> = fun (() as x) -> x
[%%expect {|
- : unit -> unit = <fun>
|}]
(* <[<[<['a]>]>]> ~ unit *)
let _ : <[<[<['a]>]>]> -> <[<[<['a]>]>]> = fun (() as x) -> x
[%%expect {|
- : unit -> unit = <fun>
|}]

(* In tests with spliced variables, the variable ['a] always occurs at stage 0,
   so we have no concerns about normalisation there. *)

(* $('a) ~ unit *)
let _ : <[ $('a) -> $('a) ]> expr  =
  <[fun (() as x) -> x]>
[%%expect {|
- : <[unit -> unit]> expr = <[fun () as x -> x]>
|}]
(* $($('a)) ~ unit *)
let _ : <[ <[ $($('a)) -> $($('a)) ]> expr ]> expr =
  <[<[fun (() as x) -> x]>]>
[%%expect {|
- : <[<[unit -> unit]> expr]> expr = <[<[fun () as x -> x]>]>
|}]
(* $($($('a))) ~ unit *)
let _ : <[ <[ <[ $($($('a))) -> $($($('a))) ]> expr ]> expr ]> expr =
  <[<[<[fun (() as x) -> x]>]>]>
[%%expect {|
- : <[<[<[unit -> unit]> expr]> expr]> expr = <[<[<[fun () as x -> x]>]>]>
|}]

(* TODO: Add tests for [unify]'s GADT cases
         ([is_instantiable], [is_equatable], [mcomp]. *)
