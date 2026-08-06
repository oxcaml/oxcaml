(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

module type S = sig val x : int end;;
module M0 = struct let x = 42 end;;
#mark_persistent_in_quotations
[%%expect {|
module type S = sig val x : int end
module M0 : sig val x : int end
|}];;

type nat = Z | S of nat

let rec loop acc = function
| S n -> <[let x = 42 in $(loop <[x + $acc]> n)]>
| Z -> acc
[%%expect {|
type nat = Z | S of nat
val loop : <[int]> expr @ once -> nat -> <[int]> expr @ once = <fun>
|}];;

loop <[0]> (S (S (S Z)))
[%%expect {|
- : <[int]> expr =
<[let x = 42 in let x__1 = 42 in let x__2 = 42 in x__2 + (x__1 + (x + 0))]>
|}];;

let rec loop acc = function
| S n -> <[let module M : S = M0 in $(loop <[M.x + $acc]> n)]>
| Z -> acc
[%%expect {|
val loop : <[int]> expr @ once -> nat -> <[int]> expr @ once = <fun>
|}];;

loop <[0]> (S (S (S Z)))
[%%expect {|
- : <[int]> expr =
<[let module M = M0 in
    let module M__1 = M0 in
      let module M__2 = M0 in M__2.x + (M__1.x + (M.x + 0))]>
|}];;

let rec loop acc = function
| S n -> loop <[fun (type a) () -> $acc ()]> n
| Z -> acc
[%%expect {|
val loop : <[unit -> $('a)]> expr -> nat -> <[unit -> $('a)]> expr = <fun>
|}];;

loop <[fun () -> ()]> (S (S (S Z)))
[%%expect {|
- : <[unit -> unit]> expr =
<[fun (type a) () ->
    (fun (type a__1) () -> (fun (type a__2) () -> (fun () -> ()) ()) ()) ()]>
|}];;

let rec loop acc = function
| S n -> loop <[fun (x : 'a) -> $acc x]> n
| Z -> acc
[%%expect {|
val loop : <[$('a) -> $('b)]> expr -> nat -> <[$('a) -> $('b)]> expr = <fun>
|}];;

loop <[fun () -> ()]> (S (S (S Z)))
[%%expect {|
- : <[unit -> unit]> expr =
<[fun (x : 'a) ->
    (fun (x__1 : 'a__1) -> (fun (x__2 : 'a__2) -> (fun () -> ()) x__2) x__1)
      x]>
|}];;

let a = Obj.magic_many <[let f: 'a. 'a -> 'a = fun x -> x in f]>
in
<[
  let p = $a in
  let q = $a in
  let r = $a in
  (p, q, r)
]>
[%%expect {|
- : <[($('a) -> $('a)) * ($('b) -> $('b)) * ($('c) -> $('c))]> expr =
<[let p = let f : 'a . 'a -> 'a = fun x -> x in f in
  let q = let f : 'a . 'a -> 'a = fun x -> x in f in
  let r = let f : 'a . 'a -> 'a = fun x -> x in f in (p, q, r)]>
|}];;
