(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

<[ 42 ]>;;
[%%expect {|
- : <[int]> expr = <[42]>
|}];;

<[ 3.14s ]>;;
[%%expect {|
- : <[float32]> expr = <[3.14s]>
|}];;

<[ 3.14 ]>;;
[%%expect {|
- : <[float]> expr = <[3.14]>
|}];;

<[ 'x' ]>;;
[%%expect {|
- : <[char]> expr = <['x']>
|}];;

<[ '\n' ]>;;
[%%expect {|
- : <[char]> expr = <['\n']>
|}];;

<[ "foo" ]>;;
[%%expect {|
- : <[string]> expr = <["foo"]>
|}];;

<[ "\b\n\t" ]>;;
[%%expect {|
- : <[string]> expr = <["\b\n\t"]>
|}];;

<[ {foo|bar|foo} ]>;;
[%%expect {|
- : <[string]> expr = <[{foo|bar|foo}]>
|}];;

(* Cannot introduce line breaks across quoted strings - default [pp_margin] is 78.
   This is a silly counter-example to a [pp_open_box] after the quote identifier,
   but an easy one to construct. *)
<[ {xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|foobar|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx} ]>;;
[%%expect {|
- : <[string]> expr =
<[
  {xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|foobar|xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}
]>
|}];;

(* No escaping in quoted strings *)
<[ {foo|\b\n\t|foo} ]>;;
[%%expect {|
- : <[string]> expr = <[{foo|\b\n\t|foo}]>
|}];;

<[ true ]>;;
[%%expect {|
- : <[bool]> expr = <[true]>
|}];;

<[ false ]>;;
[%%expect {|
- : <[bool]> expr = <[false]>
|}];;

<[ () ]>;;
[%%expect {|
- : <[unit]> expr = <[()]>
|}];;

<[ (1, 2) ]>;;
[%%expect {|
- : <[int * int]> expr = <[(1, 2)]>
|}];;

<[ (1, 2, 3) ]>;;
[%%expect {|
- : <[int * int * int]> expr = <[(1, 2, 3)]>
|}];;

<[ (~lab:"val", ~lab2:77, 30) ]>;;
[%%expect {|
- : <[lab:string * lab2:int * int]> expr = <[(~lab:"val", ~lab2:77, 30)]>
|}];;

<[ [] ]>;;
[%%expect {|
- : <[$('a) list]> expr = <[[]]>
|}];;

<[ [1; 2; 3] ]>;;
[%%expect {|
- : <[int list]> expr = <[[1; 2; 3]]>
|}];;

<[ [||] ]>;;
[%%expect {|
- : <[$('a) array]> expr = <[[||]]>
|}];;

<[ [| 1; 2; 3 |] ]>;;
[%%expect {|
- : <[int array]> expr = <[[|1; 2; 3|]]>
|}];;

<[ None ]>;;
[%%expect {|
- : <[$('a) option]> expr = <[None]>
|}];;

<[ Some 111 ]>;;
[%%expect {|
- : <[int option]> expr = <[Some 111]>
|}];;

<[ `A 42 ]>;;
[%%expect {|
- : <[[> `A of int ] as '_weak1]> expr = <[`A 42]>
|}];;

let _ = <[ `B 123 ]>;;
[%%expect {|
- : <[[> `B of int ] as '_weak2]> expr = <[`B 123]>
|}];;

let x0 = <[ `C 543 ]>;;
[%%expect {|
val x0 : <[[> `C of int ] as '_weak3]> expr = <[`C 543]>
|}];;

<[ if true then `A 10 else `B ("foo", 42) ]>;;
[%%expect {|
- : <[[> `A of int | `B of string * int ] as '_weak4]> expr =
<[if true then (`A 10) else `B ("foo", 42)]>
|}];;

<[ function | `A x -> x | `B (_, foo) -> foo ]>;;
[%%expect {|
- : <[([< `A of '_weak6 | `B of '_weak7 * '_weak6 ] as '_weak5) -> '_weak6]>
    expr
= <[function | `A x -> x | `B (_, foo) -> foo]>
|}];;

<[ function | `A x -> x | `B (_, foo) -> foo | _ -> 42 ]>;;
[%%expect {|
- : <[([> `A of int | `B of '_weak9 * int ] as '_weak8) -> int]> expr =
<[function | `A x -> x | `B (_, foo) -> foo | _ -> 42]>
|}];;

<[ List.map ]>;;
[%%expect {|
- : <[($('a) -> $('b)) -> $('a) list -> $('b) list]> expr = <[Stdlib.List.map
]>
|}];;

<[ fun x -> 42 ]>;;
[%%expect {|
- : <[$('a) -> int]> expr = <[fun x -> 42]>
|}];;

<[ fun _ -> 42 ]>;;
[%%expect {|
- : <[$('a) -> int]> expr = <[fun _ -> 42]>
|}];;

<[ fun x y -> x ]>;;
[%%expect {|
- : <[$('a) -> $('b) -> $('a)]> expr = <[fun x y -> x]>
|}];;

<[ fun f x y -> f ~a:y ~b:x ]>;;
[%%expect {|
- : <[(a:$('a) -> b:$('b) -> $('c)) -> $('b) -> $('a) -> $('c)]> expr =
<[fun f x y -> f ~a:y ~b:x]>
|}];;

<[ fun f x y -> f ?a:y ?b:x ]>;;
[%%expect {|
- : <[
     (?a:$('a) -> ?b:$('b) -> $('c)) -> $('b) option -> $('a) option -> $('c)
     ]>
    expr
= <[fun f x y -> f ?a:y ?b:x]>
|}];;

<[ fun (x, y) -> x + y ]>;;
[%%expect {|
- : <[int * int -> int]> expr = <[fun (x, y) -> x + y]>
|}];;

<[ function | _ -> 12 ]>;;
[%%expect {|
- : <[$('a) -> int]> expr = <[function | _ -> 12]>
|}];;

<[ function | x -> x ]>;;
[%%expect {|
- : <[$('a) -> $('a)]> expr = <[function | x -> x]>
|}];;

<[ function | 42 -> true | _ -> false ]>;;
[%%expect {|
- : <[int -> bool]> expr = <[function | 42 -> true | _ -> false]>
|}];;

<[ function | "foo" -> true | _ -> false ]>;;
[%%expect {|
- : <[string -> bool]> expr = <[function | "foo" -> true | _ -> false]>
|}];;

<[ function | (x, y) as z -> (x, y, z) ]>;;
[%%expect {|
- : <[$('a) * $('b) -> $('a) * $('b) * ($('a) * $('b))]> expr =
<[function | (x, y) as z -> (x, y, z)]>
|}];;

<[ function | (x, y) -> x + y ]>;;
[%%expect {|
- : <[int * int -> int]> expr = <[function | (x, y) -> x + y]>
|}];;

<[ function | (x, y, z) -> x + y - z ]>;;
[%%expect {|
- : <[int * int * int -> int]> expr = <[function | (x, y, z) -> (x + y) - z]>
|}];;

<[ function | `A -> true | `B -> false ]>;;
[%%expect {|
- : <[([< `A | `B ] as '_weak10) -> bool]> expr =
<[function | `A -> true | `B -> false]>
|}];;

<[ function | `Foo x -> x | `Bar (y, z) -> y + z | `Baz -> 0 ]>;;
[%%expect {|
- : <[([< `Bar of int * int | `Baz | `Foo of int ] as '_weak11) -> int]> expr
= <[function | `Foo x -> x | `Bar (y, z) -> y + z | `Baz -> 0]>
|}];;

<[ function | lazy x as l -> Lazy.force l ]>;;
[%%expect {|
- : <[$('a) Lazy.t -> $('a)]> expr =
<[function | lazy (x) as l -> Stdlib.Lazy.force l]>
|}];;

<[ fun f x d -> match f x with | res -> res | exception e -> d ]>;;
[%%expect {|
- : <[($('a) -> $('b)) -> $('a) -> $('b) -> $('b)]> expr =
<[fun f x d -> match f x with | res -> res | (exception e) -> d]>
|}];;

<[ function | Some x -> x | None -> 0 ]>;;
[%%expect {|
- : <[int option -> int]> expr = <[function | Some (x) -> x | None -> 0]>
|}];;

<[ function | [] -> false | x::xs -> true ]>;;
[%%expect {|
- : <[$('a) list -> bool]> expr = <[function | [] -> false | x::xs -> true]>
|}];;

<[ fun x d -> match x with | Some y -> y | None -> d ]>;;
[%%expect {|
- : <[$('a) option -> $('a) -> $('a)]> expr =
<[fun x d -> match x with | Some (y) -> y | None -> d]>
|}];;

<[ fun l -> List.map (fun x -> 2 * x) l ]>;;
[%%expect {|
- : <[int list -> int list]> expr =
<[fun l -> Stdlib.List.map (fun x -> 2 * x) l]>
|}];;

<[ fun (type a) (f : a -> a) (x : a) -> f (f x) ]>;;
[%%expect {|
- : <[($('a) -> $('a)) -> $('a) -> $('a)]> expr =
<[fun (type a) (f : a -> a) (x : a) -> f (f x)]>
|}];;

<[ fun x (type a) (f : a -> a * a) (g : int -> a) -> f (g x) ]>;;
[%%expect {|
- : <[int -> ($('a) -> $('a) * $('a)) -> (int -> $('a)) -> $('a) * $('a)]>
    expr
= <[fun x (type a) (f : a -> (a) * (a)) (g : int -> a) -> f (g x)]>
|}];;

<[ fun (f : 'a. 'a -> 'a) -> f f ]>;;
[%%expect {|
- : <[('a. 'a -> 'a) -> $('b) -> $('b)]> expr =
<[fun (f : 'a. 'a -> 'a) -> f f]>
|}];;

<[ fun x -> fun x -> fun x -> 42 ]>;;
[%%expect {|
- : <[$('a) -> $('b) -> $('c) -> int]> expr =
<[fun x -> fun x__1 -> fun x__2 -> 42]>
|}];;

<[ fun x -> fun x -> fun x__1 -> 42 ]>;;
[%%expect {|
- : <[$('a) -> $('b) -> $('c) -> int]> expr =
<[fun x -> fun x__1 -> fun x__2 -> 42]>
|}];;

<[ let z = 10 in z ]>;;
[%%expect {|
- : <[int]> expr = <[let z = 10 in z]>
|}];;

<[ let (x, y) = (42, 100) in x + y ]>;;
[%%expect {|
- : <[int]> expr = <[let (x, y) = (42, 100) in x + y]>
|}];;

<[ let Some x = Some "foo" in x ]>;;
[%%expect {|
Line 1, characters 3-31:
1 | <[ let Some x = Some "foo" in x ]>;;
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
None

- : <[string]> expr = <[match Some "foo" with | Some (x) -> x]>
|}];;

<[ let x::xs = [1; 2; 3] in x ]>;;
[%%expect {|
Line 1, characters 3-29:
1 | <[ let x::xs = [1; 2; 3] in x ]>;;
       ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]

- : <[int]> expr = <[match [1; 2; 3] with | x::xs -> x]>
|}];;

<[ let x::xs = [1; 2; 3] in xs ]>;;
[%%expect {|
Line 1, characters 3-30:
1 | <[ let x::xs = [1; 2; 3] in xs ]>;;
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
[]

- : <[int list]> expr = <[match [1; 2; 3] with | x::xs -> xs]>
|}];;

<[ let foo x = (x, x) in foo 42 ]>;;
[%%expect {|
- : <[int * int]> expr = <[let foo = (fun x -> (x, x)) in foo 42]>
|}];;

<[ let foo = 50 and bar = 15 in foo + bar ]>;;
[%%expect {|
- : <[int]> expr = <[let foo = 50 and bar = 15 in foo + bar]>
|}];;

<[ let x = 42 in let x = x in x ]>;;
[%%expect {|
- : <[int]> expr = <[let x = 42 in let x__1 = x in x__1]>
|}];;

<[ let rec f x = if x mod 2 = 0 then x + g (x / 2) else g (x - 1)
   and g x = if x = 0 then 0 else x + f (x - 1) in
   f 20 ]>;;
[%%expect {|
- : <[int]> expr =
<[
  let rec f =
  (fun x -> if ((x mod 2) = 0) then (x + (g (x / 2))) else g (x - 1))
  and g = (fun x__1 -> if (x__1 = 0) then 0 else x__1 + (f (x__1 - 1))) in
  f 20
]>
|}];;

<[
  let (let+) x f = f x in
  let+ a = 42 in a
]>;;
[%%expect {|
- : <[int]> expr = <[let (let+) = (fun x f -> f x) in let+ a = 42 in a]>
|}];;

<[
  let (let+) x f = Option.map f x in
  let+ a = Some 42
  in a * 2
]>;;
[%%expect {|
- : <[int option]> expr =
<[
  let (let+) = (fun x f -> Stdlib.Option.map f x) in
  let+ a = Some 42 in a * 2
]>
|}];;

<[
  let (let*) x f = List.map f x and (and*) = List.combine in
  let* a = [1; 2; 3]
  and* b = [10; 20; 30]
  in a + b
]>;;
[%%expect {|
- : <[int list]> expr =
<[
  let (let*) = (fun x f -> Stdlib.List.map f x)
  and (and*) = Stdlib.List.combine in
  let* a = [1; 2; 3] and* b = [10; 20; 30] in a + b
]>
|}];;

<[ fun (f: int -> int) (x: int) -> f x ]>;;
[%%expect {|
- : <[(int -> int) -> int -> int]> expr =
<[fun (f : int -> int) (x : int) -> f x]>
|}];;

<[ let module M = Set.Make(Int) in M.singleton 100 |> M.elements ]>;;
[%%expect {|
- : <[Int.t list]> expr =
<[let module M = Stdlib.Set.Make(Stdlib.Int) in M.elements (M.singleton 100)
]>
|}];;

<[ ref 42 ]>;;
[%%expect {|
- : <[int ref]> expr = <[Stdlib.ref 42]>
|}];;

<[
  let x = ref 0 in
  for i = 0 to 10 do
    x := !x + i
  done;
  !x
 ]>;;
[%%expect {|
- : <[int]> expr =
<[let x = (Stdlib.ref 0) in for i = 0 to 10 do (x := ((! x) + i)) done; ! x]>
|}];;

<[
  let x = ref 0 in
  for i = 10 downto 0 do
    x := !x + i
  done;
  !x
 ]>;;
[%%expect {|
- : <[int]> expr =
<[
  let x = (Stdlib.ref 0) in
  for i = 10 downto 0 do (x := ((! x) + i)) done; ! x
]>
|}];;

<[ while true do () done ]>;;
[%%expect {|
- : 'a expr = <[while true do  () done]>
|}];;

<[
  let f = ref 1 and i = ref 5 in
  while !i > 0 do
    f := !i * !f;
    i := !i - 1
  done;
  !f
 ]>;;
[%%expect {|
- : <[int]> expr =
<[
  let f = (Stdlib.ref 1) and i = (Stdlib.ref 5) in
  while (! i) > 0 do  (f := ((! i) * (! f)); i := ((! i) - 1)) done; ! f
]>
|}];;

<[ assert true ]>;;
[%%expect {|
- : <[unit]> expr = <[assert true]>
|}];;

<[ assert false ]>;;
[%%expect {|
- : 'a expr = <[assert false]>
|}];;

<[ lazy 42 ]>;;
[%%expect {|
- : <[int lazy_t]> expr = <[lazy 42]>
|}];;

<[ fun () -> #25n ]>;;
[%%expect {|
- : <[unit -> nativeint#]> expr = <[fun () -> #25n]>
|}];;

<[ fun () -> #25l ]>;;
[%%expect {|
- : <[unit -> int32#]> expr = <[fun () -> #25l]>
|}];;

<[ fun () -> #25L ]>;;
[%%expect {|
- : <[unit -> int64#]> expr = <[fun () -> #25L]>
|}];;

<[ fun () -> #6.0 ]>;;
[%%expect {|
- : <[unit -> float#]> expr = <[fun () -> #6.0]>
|}];;

<[ fun () -> #6.0s ]>;;
[%%expect {|
- : <[unit -> float32#]> expr = <[fun () -> #6.0s]>
|}];;

<[ fun () -> #(1, 2, 3) ]>;;
[%%expect {|
- : <[unit -> #(int * int * int)]> expr = <[fun () -> #(1, 2, 3)]>
|}];;

type rcd = {x: int; y: string};;
[%%expect {|
type rcd = { x : int; y : string; }
|}];;

<[ {x = 42; y = "foo"} ]>;;
[%%expect {|
Line 1, characters 4-5:
1 | <[ {x = 42; y = "foo"} ]>;;
        ^
Error: Label "x" used at Line 1, characters 4-5 cannot be used in this context;
       "x" is not defined inside a quotation (<[ ... ]>).
Hint: Label "x" is defined outside any quotations.
|}];;

type rcd_u = #{xu: int; yu: string};;
[%%expect {|
type rcd_u = #{ xu : int; yu : string; }
|}];;

<[ fun () -> #{xu = 42; yu = "foo"} ]>;;
[%%expect {|
Line 1, characters 15-17:
1 | <[ fun () -> #{xu = 42; yu = "foo"} ]>;;
                   ^^
Error: Label "xu" used at Line 1, characters 15-17
       cannot be used in this context;
       "xu" is not defined inside a quotation (<[ ... ]>).
Hint: Label "xu" is defined outside any quotations.
|}];;

<[ fun r -> r.x ]>;;
[%%expect {|
Line 1, characters 14-15:
1 | <[ fun r -> r.x ]>;;
                  ^
Error: Label "x" used at Line 1, characters 14-15
       cannot be used in this context;
       "x" is not defined inside a quotation (<[ ... ]>).
Hint: Label "x" is defined outside any quotations.
|}];;

<[ fun {x; y} -> x ]>;;
[%%expect {|
Line 1, characters 8-9:
1 | <[ fun {x; y} -> x ]>;;
            ^
Error: Label "x" used at Line 1, characters 8-9 cannot be used in this context;
       "x" is not defined inside a quotation (<[ ... ]>).
Hint: Label "x" is defined outside any quotations.
|}];;

<[ raise (Match_failure ("foo", 42, 100)) ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise (Match_failure ("foo", 42, 100))]>
|}];;

<[ raise Out_of_memory ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise Out_of_memory]>
|}];;

<[ raise (Invalid_argument "arg") ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise (Invalid_argument "arg")]>
|}];;

<[ raise (Failure "fail") ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise (Failure "fail")]>
|}];;

<[ raise Not_found ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise Not_found]>
|}];;

<[ raise (Sys_error "err") ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise (Sys_error "err")]>
|}];;

<[ raise End_of_file ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise End_of_file]>
|}];;

<[ raise Division_by_zero ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise Division_by_zero]>
|}];;

<[ raise Stack_overflow ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise Stack_overflow]>
|}];;

<[ raise Sys_blocked_io ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise Sys_blocked_io]>
|}];;

<[ raise (Assert_failure ("assert", 42, 100)) ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise (Assert_failure ("assert", 42, 100))]>
|}];;

<[ raise (Undefined_recursive_module ("M", 42, 100)) ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise (Undefined_recursive_module ("M", 42, 100))]>
|}];;

<[ let exception E in () ]>;;
[%%expect {|
- : <[unit]> expr = <[let exception E in ()]>
|}];;

<[ let exception E in raise E ]>;;
[%%expect {|
- : 'a expr = <[let exception E in Stdlib.raise E]>
|}];;

<[ let module M = Option in M.map ]>;;
[%%expect {|
- : <[($('a) -> $('b)) -> $('a) option -> $('b) option]> expr =
<[let module M = Stdlib.Option in M.map]>
|}];;

<[ let module M = Option in function | M.None -> false | M.Some x -> x ]>;;
[%%expect {|
- : <[bool option -> bool]> expr =
<[let module M = Stdlib.Option in function | None -> false | Some (x) -> x]>
|}];;

<[ fun () -> exclave_ Some 42 ]>;;
[%%expect {|
- : <[unit -> int option @ local]> expr = <[fun () -> exclave_ Some 42]>
|}];;

<[ fun () -> exclave_ stack_ (Some 42) ]>;;
[%%expect {|
- : <[unit -> int option @ local]> expr =
<[fun () -> exclave_ stack_ (Some 42)]>
|}];;

module type S = sig
  type t
  type t2
  val a : t
  val b : t -> int -> t
  val c : t -> int
end;;
[%%expect {|
module type S =
  sig type t type t2 val a : t val b : t -> int -> t val c : t -> int end
|}];;

module Mod = struct
  type t = int
  let mk x = x
end;;
[%%expect {|
module Mod : sig type t = int val mk : 'a -> 'a end
|}];;

<[ fun (module _ : S) x -> 42 ]>;;
[%%expect {|
Line 1, characters 19-20:
1 | <[ fun (module _ : S) x -> 42 ]>;;
                       ^
Error: Identifier "S" is used at Line 1, characters 19-20,
       inside a quotation (<[ ... ]>);
       it is introduced at Lines 1-7, characters 0-3, outside any quotations.
|}];;

<[ let module M = struct type t = int let x = 42 end in M.x ]>;;
[%%expect {|
Line 1, characters 18-52:
1 | <[ let module M = struct type t = int let x = 42 end in M.x ]>;;
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Module definition using "struct..end"
       is not supported inside quoted expressions,
       as seen at Line 1, characters 18-52.
|}];;

<[ Mod.mk 42 ]>;;
[%%expect {|
Line 1, characters 3-9:
1 | <[ Mod.mk 42 ]>;;
       ^^^^^^
Error: Identifier "Mod" is used at Line 1, characters 3-9,
       inside a quotation (<[ ... ]>);
       it is introduced at File "_none_", line 1, outside any quotations.
|}];;

let x = 42 in <[ x ]>;;
[%%expect {|
Line 1, characters 17-18:
1 | let x = 42 in <[ x ]>;;
                     ^
Error: Identifier "x" is used at Line 1, characters 17-18,
       inside a quotation (<[ ... ]>);
       it is introduced at Line 1, characters 4-5, outside any quotations.
|}];;

let x = <[ 123 ]> in <[ $x ]>;;
[%%expect {|
- : <[int]> expr = <[123]>
|}];;

<[ let o = object method f = 1 end in o#f ]>;;
[%%expect {|
Line 1, characters 11-34:
1 | <[ let o = object method f = 1 end in o#f ]>;;
               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Object definition using "object..end"
       is not supported inside quoted expressions,
       as seen at Line 1, characters 11-34.
|}];;

<[ let open List in map ]>;;
[%%expect {|
Line 1, characters 3-23:
1 | <[ let open List in map ]>;;
       ^^^^^^^^^^^^^^^^^^^^
Error: Opening modules is not supported inside quoted expressions,
       as seen at Line 1, characters 3-23.
|}];;

<[ fun x -> $ (<[ x ]>) ]>;;
[%%expect {|
- : <[$('a) -> $('a)]> expr = <[fun x -> x]>
|}];;

<[ $ (<[ 42 ]>) ]>;;
[%%expect {|
- : <[int]> expr = <[42]>
|}];;

<[ $ (<[ "foo" ]>) ]>;;
[%%expect {|
- : <[string]> expr = <["foo"]>
|}];;

<[ fun x -> $((fun y -> <[ $y + $y ]>) <[ x ]>) ]>;;
[%%expect {|
- : <[int -> int]> expr = <[fun x -> x + x]>
|}];;

<[ $((fun y -> <[ $y + $y ]>) <[ 2 ]>) ]>;;
[%%expect {|
- : <[int]> expr = <[2 + 2]>
|}];;

<[ <[ $ (<[ 123 ]>) ]> ]>;;
[%%expect {|
- : <[<[int]> expr]> expr = <[<[$<[123]>]>]>
|}];;

let x = <[ "foo" ]> and y = <[ "bar" ]> in <[ $x ^ $y ]>;;
[%%expect {|
- : <[string]> expr = <["foo" ^ "bar"]>
|}];;

<[ fun x -> <[ <[ $($x) ]> ]> ]>;;
[%%expect {|
- : <[<[$($('a)) expr]> expr -> <[$($('a)) expr]> expr]> expr =
<[fun x -> <[<[$($x)]>]>]>
|}];;

let x = <[<[42]>]> in <[ <[ $($x) ]> ]>;;
[%%expect {|
- : <[<[int]> expr]> expr = <[<[$<[42]>]>]>
|}];;

<[ raise Out_of_fibers ]>;;
[%%expect {|
- : 'a expr = <[Stdlib.raise Out_of_fibers]>
|}];;

<[ fun (f : x:'a -> ?y:'b -> 'c -> unit) x y z -> f ~x ?y:None z ]>
[%%expect {|
- : <[
     (x:$('a) -> ?y:$('b) -> $('c) -> unit) ->
     $('a) -> $('d) -> $('c) -> unit]>
    expr
= <[fun (f : x:'a -> ?y:'b -> 'c -> unit) x y z -> f ~x:x ?y:None z]>
|}];;

<[ fun x -> function None -> 0 | Some x -> x ]>
[%%expect {|
- : <[$('a) -> int option -> int]> expr =
<[fun x -> function | None -> 0 | Some (x__1) -> x__1]>
|}];;
