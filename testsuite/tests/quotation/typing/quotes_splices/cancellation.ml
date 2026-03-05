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
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

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
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

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
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]
(* $(<[A]>) = A and <[$A]> = A *)
let _ : <[ $(<[<[$unit]>]>) ]> expr = <[()]>
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]
(* <[$A]> = A and $(<[A]>) = A *)
let _ : <[ <[$($(<[unit]>))]> ]> expr = <[()]>
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]

(* Three cancellations *)

(* <[$A]> = A *)
let _ : <[<[<[$($($unit))]>]>]> = ()
[%%expect {|
>> Fatal error: estimate_type_jkind: Non-quote-kinded splice type
Uncaught exception: Misc.Fatal_error

|}]
(* $(<[A]>) = A *)
let _ : <[<[<[ $($($(<[<[<[unit]>]>]>))) ]> expr]> expr]> expr = <[<[<[()]>]>]>
[%%expect {|
- : <[<[<[unit]> expr]> expr]> expr = <[<[<[()]>]>]>
|}]

(* Other cancellation examples *)

let _ : <[ <[$('a)]> -> $(<['a]>) ]> expr = <[ fun (x : 'a) -> (x : 'a) ]>
[%%expect {|
Line 1, characters 47-71:
1 | let _ : <[ <[$('a)]> -> $(<['a]>) ]> expr = <[ fun (x : 'a) -> (x : 'a) ]>
                                                   ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of 'a is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of 'a must be representable
         because we must know concretely how to pass a function argument.
|}]
let _ : <[<[$($('a))]>]> -> unit  = fun (x : 'a) -> ()
[%%expect {|
Line 1, characters 36-54:
1 | let _ : <[<[$($('a))]>]> -> unit  = fun (x : 'a) -> ()
                                        ^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of 'a is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of 'a must be representable
         because we must know concretely how to pass a function argument.
|}]
