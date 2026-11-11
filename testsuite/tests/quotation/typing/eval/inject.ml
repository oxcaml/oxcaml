(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* [inject] stub *)
open (struct
  let inject x = x |> Obj.magic
end : sig
  val inject : 'a eval -> 'a expr
end)
[%%expect {|
val inject : 'a eval -> 'a expr = <fun>
|}]

let f x = inject x
[%%expect {|
val f : 'a eval -> 'a expr = <fun>
|}]

let f (x : int) = (x : _ eval)
[%%expect {|
val f : int -> int = <fun>
|}]

let f x : <[int]> expr = inject 0
[%%expect {|
val f : 'a -> <[int]> expr = <fun>
|}]

let f (n : int) = inject n;;
[%%expect {|
val f : int -> <[int]> expr = <fun>
|}];;

let f (r : int ref) = inject r;;
[%%expect {|
val f : int ref -> <[int ref]> expr = <fun>
|}];;

let f (k : int -> string * bool) = inject k;;
[%%expect {|
val f : (int -> string * bool) -> <[int -> string * bool]> expr = <fun>
|}];;

let f (r : int ref) = <[ fun () -> incr $(inject r) ]>;;
[%%expect {|
val f : int ref -> <[unit -> unit]> expr = <fun>
|}];;

let f (x : <[<[int]>]> eval eval) : int = x;;
[%%expect {|
Line 1, characters 42-43:
1 | let f (x : <[<[int]>]> eval eval) : int = x;;
                                              ^
Error: The value "x" has type "<[<[int]>]> eval eval"
       but an expression was expected of type "int"
       Type "$(<[<[int]>]> eval)" is not compatible with type "int"
|}];;
