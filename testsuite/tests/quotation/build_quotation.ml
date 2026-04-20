(* TEST
 flags = "-extension runtime_metaprogramming -extension comprehensions -extension layout_poly_alpha";
 expect;
*)

#syntax quotations on

(* Preamble types for use in tests *)

module E = struct
  type existential = Exists : 'a -> existential
end
[%%expect {|
module E : sig type existential = Exists : 'a -> existential end
|}];;

type pv = [`A | `B]
[%%expect {|
type pv = [ `A | `B ]
|}];;

class cls = object method meth () = 42 end;;
[%%expect {|
class cls : object method meth : unit -> int end
|}];;

#mark_toplevel_in_quotations;;

(* Tests *)

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

<[ function #false -> #true | #true -> #false ]>;;
[%%expect {|
- : <[bool# -> bool#]> expr = <[function | #false -> #true | #true -> #false
]>
|}];;

<[ () ]>;;
[%%expect {|
- : <[unit]> expr = <[()]>
|}];;

<[ fun #() -> #() ]>;;
[%%expect {|
- : <[unit# -> unit#]> expr = <[fun #() -> #()]>
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

<[ `B 123 ]>;;
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

<[ fun (Some x) -> x ]>;;
[%%expect {|
Line 1, characters 7-15:
1 | <[ fun (Some x) -> x ]>;;
           ^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
None

- : <[$('a) option -> $('a)]> expr = <[fun (Some (x)) -> x]>
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
= <[fun x (type a) (f : a -> a * a) (g : int -> a) -> f (g x)]>
|}];;

<[ fun (f : 'a. 'a -> 'a) -> f f ]>;;
[%%expect {|
- : <[('a. 'a -> 'a) -> $('b) -> $('b)]> expr =
<[fun (f : 'a. 'a -> 'a) -> f f]>
|}];;

(* CR metaprogramming jbachurski: This should fail an assertion (ticket 6789).
   We should also support this construct (ticket 6790). *)
<[ let f : type a. a -> a = fun x -> x in f ]>
[%%expect {|
Uncaught exception: Stdlib.Exit

|}];;

(* CR metaprogramming jbachurski: We should support this (ticket 6791). *)
<[ function E.Exists (type a) (x : a) -> ignore (x : a) ]>
[%%expect {|
>> Fatal error: Translquote [at Line 1, characters 12-37]:
Constructor patterns introducing locally abstract types are not supported in quotes.
Uncaught exception: Misc.Fatal_error

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

(* Non-top-level functor *)
module Make = Set.Make;;
<[ let module M = Make(Int) in M.singleton 100 |> M.elements ]>;;
[%%expect {|
module Make = Set.Make
Line 2, characters 18-22:
2 | <[ let module M = Make(Int) in M.singleton 100 |> M.elements ]>;;
                      ^^^^
Error: Identifier "Make" is used at line 2, characters 18-22,
       inside a quotation (<[ ... ]>);
       it is introduced at file "_none_", line 1, outside any quotations.
|}];;

(* Non-top-level functor argument *)
module Int' = Int;;
<[ let module M = Set.Make(Int') in M.singleton 100 |> M.elements ]>;;
[%%expect {|
module Int' = Int
Line 2, characters 27-31:
2 | <[ let module M = Set.Make(Int') in M.singleton 100 |> M.elements ]>;;
                               ^^^^
Error: Identifier "Int'" is used at line 2, characters 27-31,
       inside a quotation (<[ ... ]>);
       it is introduced at file "_none_", line 1, outside any quotations.
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
Error: Label "x" used at line 1, characters 4-5 cannot be used in this context;
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
Error: Label "xu" used at line 1, characters 15-17
       cannot be used in this context;
       "xu" is not defined inside a quotation (<[ ... ]>).
Hint: Label "xu" is defined outside any quotations.
|}];;

<[ fun r -> r.x ]>;;
[%%expect {|
Line 1, characters 14-15:
1 | <[ fun r -> r.x ]>;;
                  ^
Error: Label "x" used at line 1, characters 14-15
       cannot be used in this context;
       "x" is not defined inside a quotation (<[ ... ]>).
Hint: Label "x" is defined outside any quotations.
|}];;

<[ fun {x; y} -> x ]>;;
[%%expect {|
Line 1, characters 8-9:
1 | <[ fun {x; y} -> x ]>;;
            ^
Error: Label "x" used at line 1, characters 8-9 cannot be used in this context;
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

(* Expressions cannot be stack-allocated *)
stack_ <[ 42 ]>;;
[%%expect {|
Line 1, characters 7-15:
1 | stack_ <[ 42 ]>;;
           ^^^^^^^^
Error: This expression is not an allocation site.
|}];;

<[ let x = borrow_ 42 in x + 1 ]>;;
[%%expect {|
- : <[int]> expr = <[let x = (borrow_ 42) in x + 1]>
|}];;

<[ fun x -> let y = borrow_ x in y + 1 ]>;;
[%%expect {|
- : <[int -> int]> expr = <[fun x -> let y = (borrow_ x) in y + 1]>
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

<[fun (module M : Hashtbl.S) x -> M.clear (M.create x)]>;;
[%%expect {|
- : <[(module Hashtbl.S) -> int -> unit]> expr =
<[fun ((module M) : (module Stdlib.Hashtbl.S)) x -> M.clear (M.create x)]>
|}];;

<[ fun (module _ : S) x -> 42 ]>;;
[%%expect {|
Line 1, characters 19-20:
1 | <[ fun (module _ : S) x -> 42 ]>;;
                       ^
Error: Identifier "S" is used at line 1, characters 19-20,
       inside a quotation (<[ ... ]>);
       it is introduced at lines 1-7, characters 0-3, outside any quotations.
|}];;

<[ let module M = struct type t = int let x = 42 end in M.x ]>;;
[%%expect {|
Line 1, characters 18-52:
1 | <[ let module M = struct type t = int let x = 42 end in M.x ]>;;
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Module definition using "struct..end"
       is not supported inside quoted expressions,
       as seen at line 1, characters 18-52.
|}];;

<[ let poly_ foo x = x in foo ]>;;
[%%expect {|
Line 1, characters 3-22:
1 | <[ let poly_ foo x = x in foo ]>;;
       ^^^^^^^^^^^^^^^^^^^
Error: Layout polymorphism is not supported inside quoted expressions,
       as seen at line 1, characters 3-22.
|}];;

<[
  function (x: [pv | `A])
  | #pv -> "something else"
  | `A -> "A"
]>;;
[%%expect {|
Line 3, characters 4-7:
3 |   | #pv -> "something else"
        ^^^
Error: Adding type constraint patterns (here #pv)
       is not supported inside quoted expressions,
       as seen at line 3, characters 4-7.
|}];;

<[ let f (o : #cls) = o#meth () in f ]>;;
[%%expect {|
Line 1, characters 14-18:
1 | <[ let f (o : #cls) = o#meth () in f ]>;;
                  ^^^^
Error: Using class type annotations
       is not supported inside quoted expressions,
       as seen at line 1, characters 14-18.
|}];;

<[ Mod.mk 42 ]>;;
[%%expect {|
Line 1, characters 3-9:
1 | <[ Mod.mk 42 ]>;;
       ^^^^^^
Error: Identifier "Mod" is used at line 1, characters 3-9,
       inside a quotation (<[ ... ]>);
       it is introduced at file "_none_", line 1, outside any quotations.
|}];;

let x = 42 in <[ x ]>;;
[%%expect {|
Line 1, characters 17-18:
1 | let x = 42 in <[ x ]>;;
                     ^
Error: Identifier "x" is used at line 1, characters 17-18,
       inside a quotation (<[ ... ]>);
       it is introduced at line 1, characters 4-5, outside any quotations.
|}];;

let x = <[ 123 ]> in <[ $x ]>;;
[%%expect {|
- : <[int]> expr = <[123]>
|}];;

<[ [ a * b for a = 1 to 10 for b = a to 10 ] ]>;;
[%%expect {|
- : <[int list]> expr = <[[ a * b for a = 1 to 10 for b = a to 10 ]]>
|}];;

<[ [ a * b for a = 1 to 10 and b = 1 to 10 ] ]>;;
[%%expect {|
- : <[int list]> expr = <[[ a * b for a = 1 to 10 and b = 1 to 10 ]]>
|}];;

<[ [ a * b for a = 1 to 10 for b = a to 10 when a + b mod 2 = 0 ] ]>;;
[%%expect {|
- : <[int list]> expr =
<[[ a * b for a = 1 to 10 for b = a to 10 when (a + (b mod 2)) = 0 ]]>
|}];;

<[ [| a * b for a = 1 to 10 for b = a to 10 when a + b mod 2 = 0 |] ]>;;
[%%expect {|
- : <[int array]> expr =
<[[| a * b for a = 1 to 10 for b = a to 10 when (a + (b mod 2)) = 0 |]]>
|}];;

<[ [| a ^ "!" for a in [|"foo"; "bar"|] |] ]>;;
[%%expect {|
- : <[string array]> expr = <[[| a ^ "!" for a in [|"foo"; "bar"|] |]]>
|}];;

<[ [: a ^ "!" for a in [:"foo"; "bar":] :] ]>;;
[%%expect {|
- : <[string iarray]> expr = <[[: a ^ "!" for a in [|"foo"; "bar"|] :]]>
|}];;

<[ [ a ^ b for a in ["foo"; "bar"] and b in ["!"; "?"; "!?"] ] ]>;;
[%%expect {|
- : <[string list]> expr =
<[[ a ^ b for a in ["foo"; "bar"] and b in ["!"; "?"; "!?"] ]]>
|}];;

<[ let o = object method f = 1 end in o#f ]>;;
[%%expect {|
Line 1, characters 11-34:
1 | <[ let o = object method f = 1 end in o#f ]>;;
               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Object definition using "object..end"
       is not supported inside quoted expressions,
       as seen at line 1, characters 11-34.
|}];;

<[ let open List in map ]>;;
[%%expect {|
Line 1, characters 3-23:
1 | <[ let open List in map ]>;;
       ^^^^^^^^^^^^^^^^^^^^
Error: Opening modules is not supported inside quoted expressions,
       as seen at line 1, characters 3-23.
|}];;

module M = struct
  let foo = 42
end;;

<[ let open M in M.foo ]>;;
[%%expect {|
module M : sig val foo : int end
Line 5, characters 3-22:
5 | <[ let open M in M.foo ]>;;
       ^^^^^^^^^^^^^^^^^^^
Error: Opening modules is not supported inside quoted expressions,
       as seen at line 5, characters 3-22.
|}]
;;

<[ fun (x : < foo: int[@warning "-26"] >) -> x ]>;;
[%%expect {|
Line 1, characters 14-38:
1 | <[ fun (x : < foo: int[@warning "-26"] >) -> x ]>;;
                  ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Adding attributes on fields in object types
       is not supported inside quoted expressions,
       as seen at line 1, characters 14-38.
|}]
;;

<[ fun (x : [ `Foo of int[@warning "-26"] ]) -> x ]>;;
[%%expect {|
Line 1, characters 14-41:
1 | <[ fun (x : [ `Foo of int[@warning "-26"] ]) -> x ]>;;
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Adding attributes on tags in polymorphic variant types
       is not supported inside quoted expressions,
       as seen at line 1, characters 14-41.
|}]
;;

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

<[ fun () -> <[ $ (<[ 123 ]>) ]> ]>;;
[%%expect {|
- : <[unit -> <[int]> expr @ once]> expr = <[fun () -> <[$<[123]>]>]>
|}];;

let x = <[ "foo" ]> and y = <[ "bar" ]> in <[ $x ^ $y ]>;;
[%%expect {|
- : <[string]> expr = <["foo" ^ "bar"]>
|}];;

<[ fun x -> <[ fun () -> <[ $($x) ]> ]> ]>;;
[%%expect {|
- : <[<[$($('a)) expr]> expr -> <[unit -> $($('a)) expr @ once]> expr]> expr
= <[fun x -> <[fun () -> <[$($x)]>]>]>
|}];;

let x = <[<[42]>]> in <[ fun () -> <[ $($x) ]> ]>;;
[%%expect {|
- : <[unit -> <[int]> expr @ once]> expr = <[fun () -> <[$<[42]>]>]>
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

<[ let rec add : int * int -> int = fun (x, y) -> x + y in add ]>
[%%expect {|
- : <[int * int -> int]> expr =
<[
  let rec add : int * int -> int = (fun (x, y) -> x + y : int * int -> int)
  in add
]>
|}];;

<[ let rec id : 'a. 'a -> 'a = fun x -> x in id ]>
[%%expect {|
- : <[$('a) -> $('a)]> expr =
<[let rec id : 'a. 'a -> 'a = (fun x -> x) in id]>
|}];;

<[ let rec foo : int -> int = fun x -> if x < 0 then bar x else 0
   and bar : int -> int = fun x -> if x > 0 then foo x else 0
   in foo, bar ]>
[%%expect {|
- : <[(int -> int) * (int -> int)]> expr =
<[
  let rec foo : int -> int =
  (fun x -> if (x < 0) then (bar x) else 0 : int -> int)
  and bar : int -> int =
  (fun x__1 -> if (x__1 > 0) then (foo x__1) else 0 : int -> int) in
  (foo, bar)
]>
|}];;

<[ let rec foo (x : int) : int = if x < 0 then bar x else 0
   and bar (x : int) : int = if x > 0 then foo x else 0
   in foo, bar ]>
[%%expect {|
- : <[(int -> int) * (int -> int)]> expr =
<[
  let rec foo = (fun (x : int) -> (if (x < 0) then (bar x) else 0 : int))
  and bar =
  (fun (x__1 : int) -> (if (x__1 > 0) then (foo x__1) else 0 : int)) in
  (foo, bar)
]>
|}];;

<[ fun x -> function None -> 0 | Some x -> x ]>
[%%expect {|
- : <[$('a) -> int option -> int]> expr =
<[fun x -> function | None -> 0 | Some (x__1) -> x__1]>
|}];;

<[ fun f x -> (f [@inlined]) x [@nontail] ]>
[%%expect {|
- : <[($('a) -> $('b)) -> $('a) -> $('b)]> expr =
<[fun f x -> ((f [@inlined]) x [@nontail])]>
|}];;

<[ fun x -> [ x ; x + 1 ] ]>
[%%expect {|
- : <[int -> int list]> expr = <[fun x -> [x; x + 1]]>
|}];;

(* Constraints must be parenthesised in tuple and list elements *)

<[ fun x -> ((x : int), (x + 1 : int)) ]>
[%%expect {|
- : <[int -> int * int]> expr = <[fun x -> ((x : int), (x + 1 : int))]>
|}];;

<[ fun x -> [(x : int); (x + 1 : int)] ]>
[%%expect {|
- : <[int -> int list]> expr = <[fun x -> [(x : int); (x + 1 : int)]]>
|}];;

<[ (fun f -> (f 42, f "abc") : ('a. 'a -> 'a) -> (int * string)) ]>
[%%expect {|
- : <[('a. 'a -> 'a) -> int * string]> expr =
<[(fun (f : 'a. 'a -> 'a) -> ((f 42), (f "abc")) : ('a__1. 'a__1 -> 'a__1) ->
  int * string)
]>
|}];;

let x = <[ "foo" ]> in <[ let y = (borrow_ $x) in (fun (a @ local) -> ()) y ]>
[%%expect{|
- : <[unit]> expr =
<[let y = (borrow_ "foo") in (fun (a : _ @ local) -> ()) y]>
|}];;

let x = <[ "foo" ]> in <[ let y = (borrow_ x) in (fun (a @ local) -> ()) y ]>
[%%expect{|
Line 1, characters 43-44:
1 | let x = <[ "foo" ]> in <[ let y = (borrow_ x) in (fun (a @ local) -> ()) y ]>
                                               ^
Error: Identifier "x" is used at line 1, characters 43-44,
       inside a quotation (<[ ... ]>);
       it is introduced at line 1, characters 4-5, outside any quotations.
|}];;

(* The following bug numbers are from
   https://github.com/oxcaml/oxcaml/pull/5649 *)

(* Bug 1: Int32 constants must include the 'l' suffix *)
<[ 42l ]>;;
[%%expect {|
- : <[int32]> expr = <[42l]>
|}];;

(* Bug 2: Int64 constants must include the 'L' suffix *)
<[ 42L ]>;;
[%%expect {|
- : <[int64]> expr = <[42L]>
|}];;

(* Bug 3: Nativeint constants must include the 'n' suffix *)
<[ 42n ]>;;
[%%expect {|
- : <[nativeint]> expr = <[42n]>
|}];;

(* Bug 4: Guard keyword must be "when", not "with" *)
<[ fun x -> match x with y when y > 0 -> y | _ -> 0 ]>;;
[%%expect {|
- : <[int -> int]> expr =
<[fun x -> match x with | y when (y > 0) -> y | _ -> 0]>
|}];;

(* Bug 5: Negative constants must be parenthesized in argument positions *)
<[ Some (-42) ]>;;
[%%expect {|
- : <[int option]> expr = <[Some (-42)]>
|}];;

(* Bug 6: Type alias variable must include the tick *)
<[ fun (x : int as 'a) -> (x : 'a) ]>;;
[%%expect {|
- : <[int -> int]> expr = <[fun (x : int as 'a) -> (x : 'a)]>
|}];;

(* Bug 7: Unboxed tuple types must print with '#' prefix *)
<[ fun (x : #(int * string)) -> x ]>;;
[%%expect {|
- : <[#(int * string) -> #(int * string)]> expr =
<[fun (x : #(int * string)) -> x]>
|}];;

(* Bug 8: Closed variant types must preserve "present" tags *)
<[ fun (x : [< `A of int | `B > `A ]) -> x ]>;;
[%%expect {|
- : <[([< `A of int | `B > `A ] as '_weak12) -> '_weak12]> expr =
<[fun (x : [< `A of int | `B > `A ]) -> x]>
|}];;

(* Bug 9: Fun with function cases must have balanced format boxes *)
<[ fun x -> function | 0 -> x | n -> n + x ]>;;
[%%expect {|
- : <[int -> int -> int]> expr = <[fun x -> function | 0 -> x | n -> n + x]>
|}];;

(* Bug 10: Src_pos must not print as "." *)
<[ [%src_pos] ]>;;
[%%expect {|
- : <[lexing_position]> expr = <[[%src_pos]]>
|}];;

(* Bug 2.0: assert/lazy args must be parenthesized *)
<[ assert (if true then true else false) ]>;;
[%%expect {|
- : <[unit]> expr = <[assert (if true then true else false)]>
|}];;

<[ lazy (if true then 1 else 2) ]>;;
[%%expect {|
- : <[int lazy_t]> expr = <[lazy (if true then 1 else 2)]>
|}];;

(* Bug 2.1: PatVariant argument must be parenthesized *)
<[ fun x -> match x with | `A (Some y) -> y | _ -> 0 ]>;;
[%%expect {|
- : <[([> `A of int option ] as '_weak13) -> int]> expr =
<[fun x -> match x with | `A (Some (y)) -> y | _ -> 0]>
|}];;

(* Bug 2.2: Match/try in case RHS must be parenthesized *)
<[ fun x y -> match x with | true -> (match y with | 0 -> "a" | _ -> "b") | false -> "c" ]>;;
[%%expect {|
- : <[bool -> int -> string]> expr =
<[
  fun x y ->
    match x with | true -> (match y with | 0 -> "a" | _ -> "b") | false ->
      "c"
]>
|}];;

<[ fun x -> match x with | true -> (try raise Exit with _ -> 0) | false -> 1 ]>;;
[%%expect {|
- : <[bool -> int]> expr =
<[
  fun x ->
    match x with | true -> (try Stdlib.raise Exit with  | _ -> 0) | false ->
      1
]>
|}];;

(* Bug 2.3: Sequence elements must parenthesize let *)
<[ (let x = 1 in x); 2 ]>;;
[%%expect {|
Line 1, characters 17-18:
1 | <[ (let x = 1 in x); 2 ]>;;
                     ^
Warning 10 [non-unit-statement]: this expression should have type unit.

- : <[int]> expr = <[(let x = 1 in x); 2]>
|}];;

(* Bug 2.4: If-then-else else branch must parenthesize let and sequence *)
<[ if true then 1 else (let x = 2 in x) ]>;;
[%%expect {|
- : <[int]> expr = <[if true then 1 else (let x = 2 in x)]>
|}];;

(* Bug 2.5: Unboxed_field sub-expression must be parenthesized *)
<[ (List.hd [{contents = 42}]).contents ]>;;
[%%expect {|
- : <[int]> expr =
<[(Stdlib.List.hd ([{ Stdlib.contents = 42; }])).Stdlib.contents]>
|}];;

(* Jkind annotations inside quotations *)

(* Ptyp_any with jkind annotation: (_ : value) *)
<[ fun (x : (_ : value)) -> x ]>;;
[%%expect {|
Line 1, characters 17-22:
1 | <[ fun (x : (_ : value)) -> x ]>;;
                     ^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 17-22.
|}];;

(* Ptyp_var with jkind annotation: ('a : value) *)
<[ fun (x : ('a : value)) -> x ]>;;
[%%expect {|
Line 1, characters 18-23:
1 | <[ fun (x : ('a : value)) -> x ]>;;
                      ^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 18-23.
|}];;

(* Ptyp_alias with jkind annotation: t as ('a : value) *)
<[ fun (x : int as ('a : value)) -> x ]>;;
[%%expect {|
Line 1, characters 25-30:
1 | <[ fun (x : int as ('a : value)) -> x ]>;;
                             ^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 25-30.
|}];;

(* Ptyp_poly with jkind annotation: ('a : value). 'a -> 'a *)
<[ fun (f : ('a : value). 'a -> 'a) -> f 42 ]>;;
[%%expect {|
Line 1, characters 18-23:
1 | <[ fun (f : ('a : value). 'a -> 'a) -> f 42 ]>;;
                      ^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 18-23.
|}];;

(* Ptyp_of_kind: (type : value) *)
<[ fun (x : (type : value)) -> x ]>;;
[%%expect {|
Line 1, characters 12-26:
1 | <[ fun (x : (type : value)) -> x ]>;;
                ^^^^^^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 12-26.
|}];;

(* Pexp_newtype with jkind annotation: fun (type t : value) -> *)
<[ fun (type t : value) (x : t) -> x ]>;;
[%%expect {|
Line 1, characters 17-22:
1 | <[ fun (type t : value) (x : t) -> x ]>;;
                     ^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 17-22.
|}];;

(* Pparam_newtype with jkind annotation, multiple newtypes before a val param *)
<[ fun (type t : value) (type u : value) (x : t) (y : u) -> (x, y) ]>;;
[%%expect {|
Line 1, characters 17-22:
1 | <[ fun (type t : value) (type u : value) (x : t) (y : u) -> (x, y) ]>;;
                     ^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 17-22.
|}];;

(* Jkind annotation with & (intersection): fun (type t : value & float) -> *)
<[ fun (type t : value & float) (x : t) -> x ]>;;
[%%expect {|
Line 1, characters 17-30:
1 | <[ fun (type t : value & float) (x : t) -> x ]>;;
                     ^^^^^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 17-30.
|}];;

(** Mode annotations **)

(* Pattern constraints *)
<[ let (x @ unique portable) = "abc" in x ]>
[%%expect {|
- : <[string]> expr = <[let x = ("abc" : _ @ unique portable) in x]>
|}];;

(* Expression constraints *)
<[ fun x -> (x : _  @ unique portable)]>
[%%expect {|
- : <[$('a) @ unique portable -> $('a)]> expr =
<[fun x -> (x : _ @ unique portable)]>
|}];;

<[ fun x -> exclave_ (x : _  @ local)]>
[%%expect {|
- : <[$('a) -> $('a) @ local]> expr = <[fun x -> exclave_ (x : _ @ local)]>
|}];;

(* Function definitions *)
<[ fun (x @ local unique) @ local unique -> x]>
[%%expect {|
- : <[$('a) @ local unique -> $('a) @ local unique]> expr =
<[fun (x : _ @ local unique) -> (x : _ @ local unique)]>
|}];;

<[ let (f @ unique portable) (x @ local unique) @ local unique = x in f ]>
[%%expect {|
- : <[$('a) @ local unique -> $('a) @ local unique]> expr =
<[
  let f =
  (fun (x : _ @ local unique) -> (x : _ @ local unique) :
    _ @ unique portable)
  in f
]>
|}];;

<[ let rec f (x @ local unique) @ local unique = x in f ]>
[%%expect {|
- : <[$('a) @ local unique -> $('a) @ local unique]> expr =
<[let rec f = (fun (x : _ @ local unique) -> (x : _ @ local unique)) in f]>
|}];;

<[ let rec (f @ unique portable) (x @ local unique) = x in f ]>
[%%expect {|
- : <[$('a) @ local unique -> $('a) @ local]> expr =
<[let rec f = (fun (x : _ @ local unique) -> x : _ @ unique portable) in f]>
|}];;

<[ let rec (f @ unique portable) (x @ local unique) @ local unique = x in f ]>
[%%expect {|
- : <[$('a) @ local unique -> $('a) @ local unique]> expr =
<[
  let rec f =
  (fun (x : _ @ local unique) -> (x : _ @ local unique) :
    _ @ unique portable)
  in f
]>
|}];;

<[ let local_ f x = x in f "abc" ]>
[%%expect {|
- : <[string]> expr = <[let f = (fun x -> x : _ @ local) in f "abc"]>
|}];;

(* Function types *)
<[ fun (f : _ @ local unique -> _ @ local unique) -> f]>
[%%expect {|
- : <[
     ($('a) @ local unique -> $('b) @ local unique) ->
     $('a) @ local unique -> $('b) @ local unique]>
    expr
= <[fun (f : _ @ local unique -> _ @ local unique) -> f]>
|}];;

(** Jkind annotations **)

(* Variable *)
<[ fun (x : ('a : immediate)) -> x ]>
[%%expect {|
Line 4, characters 18-27:
4 | <[ fun (x : ('a : immediate)) -> x ]>
                      ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 4, characters 18-27.
|}];;
(* Alias *)
<[ fun (x : ('a as ('b : immediate))) -> x ]>
[%%expect {|
Line 1, characters 25-34:
1 | <[ fun (x : ('a as ('b : immediate))) -> x ]>
                             ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 25-34.
|}];;
(* Universal quantifier *)
<[ let f : ('a : immediate). 'a -> 'a = fun x -> x in f ]>
[%%expect {|
Line 1, characters 17-26:
1 | <[ let f : ('a : immediate). 'a -> 'a = fun x -> x in f ]>
                     ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 17-26.
|}];;
<[ let f : ('a : value) ('b : immediate). #('a * 'b) -> 'a =
    fun #(x, y) -> x
   in f ]>
[%%expect {|
Line 1, characters 17-22:
1 | <[ let f : ('a : value) ('b : immediate). #('a * 'b) -> 'a =
                     ^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 17-22.
|}];;
(* Locally abstract type *)
(* handled differently depending if [type] is the initial parameter *)
<[ fun (type t : immediate) (x : t) -> x ]> (* initial *)
[%%expect {|
Line 1, characters 17-26:
1 | <[ fun (type t : immediate) (x : t) -> x ]> (* initial *)
                     ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 17-26.
|}];;
<[ fun () (type t : immediate) (x : t) -> x ]> (* non-initial *)
[%%expect {|
Line 1, characters 20-29:
1 | <[ fun () (type t : immediate) (x : t) -> x ]> (* non-initial *)
                        ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 20-29.
|}];;
<[ fun (type s : value) (x : s)
       (type t : immediate) (y : t) -> #(x, y) ]> (* both *)
[%%expect {|
Line 1, characters 17-22:
1 | <[ fun (type s : value) (x : s)
                     ^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 17-22.
|}];;
<[ fun (type (s : immediate) (t : immediate))
       (x : s) (y : t) -> #(x, y) ]> (* double initial *)
[%%expect {|
Line 1, characters 18-27:
1 | <[ fun (type (s : immediate) (t : immediate))
                      ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 18-27.
|}];;
<[ fun (type (s : immediate)) (type (t : immediate))
       (x : s) (y : t) -> #(x, y) ]> (* split double initial *)
[%%expect {|
Line 1, characters 18-27:
1 | <[ fun (type (s : immediate)) (type (t : immediate))
                      ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 18-27.
|}];;
<[ fun () (type (s : immediate)) (type (t : immediate))
       (x : s) (y : t) -> #(x, y) ]> (* double non-initial *)
[%%expect {|
Line 1, characters 21-30:
1 | <[ fun () (type (s : immediate)) (type (t : immediate))
                         ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 21-30.
|}];;
<[ fun () (type (s : immediate) (t : immediate))
       (x : s) (y : t) -> #(x, y) ]> (* split double non-initial *)
[%%expect {|
Line 1, characters 21-30:
1 | <[ fun () (type (s : immediate) (t : immediate))
                         ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 21-30.
|}];;
(* Universally quantified locally abstract type *)
<[ let f : type (a : immediate). a -> a = fun x -> x in f ]>
[%%expect {|
Line 1, characters 21-30:
1 | <[ let f : type (a : immediate). a -> a = fun x -> x in f ]>
                         ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 21-30.
|}];;
(* Constructor with locally abstract type *)
<[ function E.Exists (type a : immediate) (x : a) -> ignore (x : a) ]>
[%%expect {|
Line 1, characters 31-40:
1 | <[ function E.Exists (type a : immediate) (x : a) -> ignore (x : a) ]>
                                   ^^^^^^^^^
Error: Annotating types with kinds
       is not supported inside quoted expressions,
       as seen at line 1, characters 31-40.
|}];;
