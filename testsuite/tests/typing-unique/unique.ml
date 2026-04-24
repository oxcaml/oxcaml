(* TEST
 expect;
*)

(* unique means the value is the only usage *)
let dup x = ((x, x) : @ unique)
[%%expect{|
Line 1, characters 17-18:
1 | let dup x = ((x, x) : @ unique)
                     ^
Error: This value is used here, but it is also being used as unique at:
Line 1, characters 14-15:
1 | let dup x = ((x, x) : @ unique)
                  ^

|}]

(* unique value can be used more than once *)
let dup (x @ unique) = (x, x)
[%%expect{|
val dup : 'a @ unique -> 'a * 'a = <fun>
|}]

(* once value can be used only once*)
let dup (x @ once) = (x, x)
[%%expect{|
Line 1, characters 25-26:
1 | let dup (x @ once) = (x, x)
                             ^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 1, characters 22-23:
1 | let dup (x @ once) = (x, x)
                          ^

|}]

let dup (x @ unique) = ((x : @ unique), x, x)
[%%expect{|
Line 1, characters 40-41:
1 | let dup (x @ unique) = ((x : @ unique), x, x)
                                            ^
Error: This value is used here, but it is also being used as unique at:
Line 1, characters 24-38:
1 | let dup (x @ unique) = ((x : @ unique), x, x)
                            ^^^^^^^^^^^^^^

|}]

let dup (x @ unique) = (x, (x : @ unique), x)
[%%expect{|
Line 1, characters 27-41:
1 | let dup (x @ unique) = (x, (x : @ unique), x)
                               ^^^^^^^^^^^^^^
Error: This value is used here as unique, but it is also being used at:
Line 1, characters 24-25:
1 | let dup (x @ unique) = (x, (x : @ unique), x)
                            ^

|}]


let dup (x @ unique) = ((x : @ unique), x)
[%%expect{|
Line 1, characters 40-41:
1 | let dup (x @ unique) = ((x : @ unique), x)
                                            ^
Error: This value is used here, but it is also being used as unique at:
Line 1, characters 24-38:
1 | let dup (x @ unique) = ((x : @ unique), x)
                            ^^^^^^^^^^^^^^

|}]

(* below we define a tuple that can be used multiple times,
  but manually relax it to once *)
let dup x = ((x, x) : @ once)
[%%expect{|
val dup : 'a -> 'a * 'a @ once = <fun>
|}]

(* closing over unique values gives once closure  *)
let f () =
  let (k @ unique) = [1;2;3] in
  let g () = (k : @ unique) @ [1;2;3] in
  g () @ g ()
[%%expect{|
Line 4, characters 9-10:
4 |   g () @ g ()
             ^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 4, characters 2-3:
4 |   g () @ g ()
      ^

|}]

(* but if the closure doesn't utilize the uniqueness,
  then it's not once *)
let f () =
  let (k @ unique) = [1;2;3] in
  let g () = k @ [1;2;3] in
  g () @ g ()
[%%expect{|
val f : unit -> int list = <fun>
|}]

(* closing over once values gives once closure *)
(* note that in g we don't annotate k; because once is already the most relaxed mode *)
let f () =
  let (k @ once) = [(fun x -> x)] in
  let g () = k @ [(fun x -> x)] in
  g () @ g ()
[%%expect{|
Line 3, characters 13-14:
3 |   let g () = k @ [(fun x -> x)] in
                 ^
Error: This value is "once" but is expected to be "many".
|}]

(* variables inside loops will be made both aliased and many *)
(* the following is fine, because k inside loop is aliased *)
let f () =
  let (k @ unique) = "foo" in
  for i = 1 to 5 do
    ignore k
  done
[%%expect{|
val f : unit -> unit = <fun>
|}]


(* The following is bad, because k is once and cannot be used more than once*)
let f () =
  let (k @ once) = fun x -> x in
  for i = 1 to 5 do
    k
  done
[%%expect{|
Line 4, characters 4-5:
4 |     k
        ^
Error: The value "k" is "once"
       but is expected to be "many"
         because it is used in a loop (at lines 3-5, characters 2-6).
|}]

(* The following is bad, because k is used uniquely *)
let f () =
  let (k @ unique) = "foo" in
  for i = 1 to 5 do
    (k : @ unique)
  done
[%%expect{|
Line 4, characters 5-6:
4 |     (k : @ unique)
         ^
Error: This value is "aliased"
         because it is used in a loop (at lines 3-5, characters 2-6).
       However, the highlighted expression is expected to be "unique".
|}]

let f =
  let (a @ unique) = "hello" in
  let g (a @ unique) = a in
  for i = 0 to 5 do
    let _ = g a in ()
  done;
  ()
[%%expect{|
Line 5, characters 14-15:
5 |     let _ = g a in ()
                  ^
Error: This value is "aliased"
         because it is used in a loop (at lines 4-6, characters 2-6).
       However, the highlighted expression is expected to be "unique".
|}]

let f =
  let g (a @ unique) = a in
  for i = 0 to 5 do
    let (a @ unique) = 3 in
    let _ = g a in ()
  done;
  ()
[%%expect{|
val f : unit = ()
|}]

let x = "foo"
[%%expect{|
val x : string = "foo"
|}]

(* Top-level must be many *)
let (foo @ once) = "foo"
[%%expect{|
Line 1, characters 5-24:
1 | let (foo @ once) = "foo"
         ^^^^^^^^^^^^^^^^^^^
Error: This value is "once" but is expected to be "many".
|}]

(* the following is fine - we relax many to once *)
let foo () = (x : @ once)
[%%expect{|
val foo : unit -> string @ once = <fun>
|}];;

(* Top-level expressions (not bindings) can be once *)
("foo" : _ @ once);;
[%%expect{|
- : string = "foo"
|}]

(* top-level must be aliased; the following unique is weakened to aliased *)
let (foo @ unique) = "foo"
[%%expect{|
val foo : string = "foo"
|}]


(* the following is bad - trying to tighten aliased to unique *)
let foo () = (x : @ unique)
[%%expect{|
Line 1, characters 14-15:
1 | let foo () = (x : @ unique)
                  ^
Error: This value is "aliased" but is expected to be "unique".
|}];;

(* Top-level expressions (not bindings) can be unique *)
("foo" : _ @ unique);;
[%%expect{|
- : string = "foo"
|}]

(* CR zqian: [global] should imply [aliased]/[many], once we introduce borrowing whose
scope is controlled by locality *)
type 'a glob = { glob: 'a @@ aliased many } [@@unboxed]
[%%expect{|
type 'a glob = { glob : 'a @@ many aliased; } [@@unboxed]
|}]
let dup (glob : 'a) : 'a glob * 'a glob = (({glob}, {glob}) : @ unique)
[%%expect{|
val dup : 'a -> 'a glob * 'a glob = <fun>
|}]

(* For strict type/mode match we need module *)
module M : sig
  val drop : 'a @ unique -> unit @ unique
  end = struct
  let drop (x @ unique) = (() : @ unique)
end
[%%expect{|
module M : sig val drop : 'a @ unique -> unit @ unique end
|}]

(* In the following we won't use module *)
(* printed modes are imprecise *)
let unique_id : 'a @ unique -> 'a @ unique = fun x -> x
[%%expect{|
val unique_id : 'a @ unique -> 'a @ unique = <fun>
|}]

let aliased_id : 'a -> 'a = fun x -> x
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
|}]

let tail_unique _x =
  let (y @ unique) = "foo" in unique_id y
[%%expect{|
val tail_unique : 'a -> string = <fun>
|}]

let tail_unique : 'a list @ unique -> 'a list @ unique = function
  | [] -> []
  | _ :: xx -> xx
[%%expect{|
val tail_unique : 'a list @ unique -> 'a list @ unique = <fun>
|}]

let higher_order (f : 'a @ unique -> 'b @ unique) (x : 'a @ unique) = (f x : @ unique)
[%%expect{|
val higher_order : ('a @ unique -> 'b @ unique) -> 'a @ unique -> 'b = <fun>
|}]

let higher_order2 (f : 'a -> 'b @ unique) (x : 'a) = (f x : @ unique)
[%%expect{|
val higher_order2 : ('a -> 'b @ unique) -> 'a -> 'b = <fun>
|}]

let higher_order3 (f : 'a -> 'b) (x : 'a @ unique) = (f x : @ unique)
[%%expect{|
Line 1, characters 54-57:
1 | let higher_order3 (f : 'a -> 'b) (x : 'a @ unique) = (f x : @ unique)
                                                          ^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

let higher_order4 (f : 'a @ unique -> 'b) (x : 'a) = f (aliased_id x)
[%%expect{|
Line 1, characters 55-69:
1 | let higher_order4 (f : 'a @ unique -> 'b) (x : 'a) = f (aliased_id x)
                                                           ^^^^^^^^^^^^^^
Error: This value is "aliased" but is expected to be "unique".
|}]

let higher_order5 (x @ unique) = let f (x @ unique) = (x : @ unique) in higher_order f x
[%%expect{|
val higher_order5 : 'a @ unique -> 'a = <fun>
|}]

let higher_order6 (x @ unique) = let f (x @ unique) = (x : @ unique) in higher_order2 f x
[%%expect{|
Line 1, characters 86-87:
1 | let higher_order6 (x @ unique) = let f (x @ unique) = (x : @ unique) in higher_order2 f x
                                                                                          ^
Error: This expression has type "'a @ unique -> 'a"
       but an expression was expected of type "'b -> 'c @ unique"
|}]

let inf1 (x : float @ unique) = (let y = x in y : @ unique)
[%%expect{|
val inf1 : float @ unique -> float = <fun>
|}]

let inf2 (b : bool) (x : float @ unique) = (let y = if b then x else 1.0 in y : @ unique)
[%%expect{|
val inf2 : bool -> float @ unique -> float = <fun>
|}]

let inf3 : bool -> float -> float @ unique -> float = fun b y x ->
  let _ = aliased_id y in let (z @ unique) = if b then x else y in z
[%%expect{|
Line 2, characters 62-63:
2 |   let _ = aliased_id y in let (z @ unique) = if b then x else y in z
                                                                  ^
Error: This value is "aliased" but is expected to be "unique".
|}]

let inf4 (b : bool) (y : float) (x : float @ unique) =
  let _ = aliased_id y in let (z @ unique) = if b then x else y in z
[%%expect{|
Line 2, characters 62-63:
2 |   let _ = aliased_id y in let (z @ unique) = if b then x else y in z
                                                                  ^
Error: This value is used here as unique, but it has already been used at:
Line 2, characters 21-22:
2 |   let _ = aliased_id y in let (z @ unique) = if b then x else y in z
                         ^

|}]


let inf5 (b : bool) (y : float) (x : float @ unique) =
  let z = if b then x else y in (z : @ unique)
[%%expect{|
val inf5 : bool -> float @ unique -> float @ unique -> float = <fun>
|}]

let inf6 (x @ unique) = let f x = x in higher_order f x
[%%expect{|
val inf6 : 'a @ unique -> 'a = <fun>
|}]

let unique_default_args ?(x @ unique = 1.0) () = x
[%%expect{|
val unique_default_args : ?x:float @ unique -> unit -> float = <fun>
|}]

(* Unique Local *)

let ul (x @ unique local) = x
[%%expect{|
val ul : 'a @ local unique -> 'a @ local = <fun>
|}]

let ul_ret x = exclave_ (x : @ unique)
[%%expect{|
val ul_ret : 'a @ unique -> 'a @ local = <fun>
|}]

let rec foo =
  fun (local_ o) ->
  match (o : @ unique) with
  | Some () -> foo None
  | None -> ()
[%%expect{|
val foo : unit option @ local unique -> unit = <fun>
|}]

let rec bar =
  fun (o @ unique) ->
  match o with
  | Some () -> ()
  | None -> bar (local_ Some ()) [@nontail]
[%%expect{|
val bar : unit option @ local unique -> unit = <fun>
|}]

let foo : string @ local unique -> unit = fun (local_ s) -> ()
[%%expect{|
val foo : string @ local unique -> unit = <fun>
|}]

let bar : string @ local unique -> unit = fun (s @ unique) -> ()
[%%expect{|
val bar : string @ local unique -> unit = <fun>
|}]

(* Currying *)

let curry =
  let foo ~a ~b ~c ~d = (a, b, c, (d : @ unique)) in
  foo ~a:3 ~c:4
[%%expect{|
val curry : b:'_weak1 -> d:'_weak2 @ unique -> int * '_weak1 * int * '_weak2 =
  <fun>
|}]

(* the following two failed because top-level must be many *)
(* TODO: maybe a particular error for that? *)
let curry =
  let foo ~a ~b ~c ~d = (a, b, (c : @ unique), (d : @ unique)) in
  foo ~a:3 ~c:4
[%%expect{|
Line 3, characters 2-15:
3 |   foo ~a:3 ~c:4
      ^^^^^^^^^^^^^
Error: This value is "once" but is expected to be "many".
|}]

let curry =
  let foo ~a ~b ~c ~d = ((a : @ unique), b, c, d) in
  foo ~a:3 ~c:4
[%%expect{|
Line 3, characters 2-15:
3 |   foo ~a:3 ~c:4
      ^^^^^^^^^^^^^
Error: This value is "once" but is expected to be "many".
|}]

let curry =
  let foo ~a ~b ~c ~d = (a, (b : @ unique), c, (d : @ unique)) in
  foo ~a:3 ~c:4
[%%expect{|
val curry :
  b:'_weak3 @ unique -> d:'_weak4 @ unique -> int * '_weak3 * int * '_weak4 =
  <fun>
|}]

let curry =
  let foo ~a ~b ~c ~d = (a, b, (c : @ unique), (d : @ unique)) in
  let bar = foo ~a:3 ~b:2 ~c:4 in
  (bar ~d:3, bar ~d:5)
[%%expect{|
Line 4, characters 13-16:
4 |   (bar ~d:3, bar ~d:5)
                 ^^^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 4, characters 3-6:
4 |   (bar ~d:3, bar ~d:5)
       ^^^

|}]

let curry =
  let foo ~a ~b ~c ~d = (a, b, (c : @ unique), (d : @ unique)) in
  let bar = foo ~a:3 ~c:4 in
  let baz = bar ~b:4 in (baz ~d:3, baz ~d:5)
[%%expect{|
Line 4, characters 35-38:
4 |   let baz = bar ~b:4 in (baz ~d:3, baz ~d:5)
                                       ^^^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 4, characters 25-28:
4 |   let baz = bar ~b:4 in (baz ~d:3, baz ~d:5)
                             ^^^

|}]

let curry =
  let (x @ unique) = "foo" in
  let foo () = (x : @ unique) in
  (foo (), foo ())
[%%expect{|
Line 4, characters 11-14:
4 |   (foo (), foo ())
               ^^^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 4, characters 3-6:
4 |   (foo (), foo ())
       ^^^

|}]

type box = { x : int }
[%%expect{|
type box = { x : int; }
|}]

let curry (b1 : box @ unique) (b2 : box @ unique) = ()
[%%expect{|
val curry : box @ unique -> box @ unique -> unit = <fun>
|}]

let curry : box @ unique -> box @ unique -> unit = fun b1 b2 -> ()
[%%expect{|
val curry : box @ unique -> box @ unique -> unit = <fun>
|}]

let curry : box @ unique -> (box @ unique -> unit) = fun b1 b2 -> ()
[%%expect{|
Line 1, characters 53-68:
1 | let curry : box @ unique -> (box @ unique -> unit) = fun b1 b2 -> ()
                                                         ^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "once",
       but expected to be "many".
|}]

let curry : box @ unique -> (box @ unique -> unit) = fun b1 -> function | b2 -> ()
[%%expect{|
Line 1, characters 53-82:
1 | let curry : box @ unique -> (box @ unique -> unit) = fun b1 -> function | b2 -> ()
                                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "once",
       but expected to be "many".
|}]

(* For nested functions, inner functions are not constrained *)
let no_curry : box @ unique -> (box @ unique -> unit) = fun b1 -> fun b2 -> ()
[%%expect{|
val no_curry : box @ unique -> (box @ unique -> unit) = <fun>
|}]

(* If both type and mode are wrong, complain about type *)
let f () =
  let id2 (x : string) = aliased_id x in
  let (r @ unique) = 42 in
  id2 r
[%%expect{|
Line 4, characters 6-7:
4 |   id2 r
          ^
Error: This expression has type "int" but an expression was expected of type
         "string"
|}]

let return_local : local_ 'a -> local_ 'a = fun x -> x
let return_global : local_ 'a -> int = fun x -> 0
[%%expect{|
val return_local : 'a @ local -> 'a @ local = <fun>
val return_global : 'a @ local -> int = <fun>
|}]


(* recursive function *)
(* the following error, because make_tree must return unique
    (which is needed for x to be unique), and therefore
    cannot return (x, x) *)
type tree = Leaf | Node of tree * tree
let rec make_tree = fun n ->
  if n <= 0 then Leaf
  else let (x @ unique) = make_tree (n - 1)
       in Node (x, x)
[%%expect{|
type tree = Leaf | Node of tree * tree
Line 5, characters 19-20:
5 |        in Node (x, x)
                       ^
Error: This value is used here, but it is also being used as unique at:
Line 5, characters 16-17:
5 |        in Node (x, x)
                    ^

|}]

(* Uniqueness is unbroken by the implicit positional argument. *)
let f ~(call_pos : [%call_pos]) () =
  let (x @ unique) = call_pos in
  (x, x)
;;
[%%expect{|
val f : call_pos:[%call_pos] -> unit -> lexing_position * lexing_position =
  <fun>
|}]

let f ~(call_pos : [%call_pos]) () =
  ((call_pos, call_pos) : @ unique)
;;
[%%expect{|
Line 2, characters 14-22:
2 |   ((call_pos, call_pos) : @ unique)
                  ^^^^^^^^
Error: This value is used here, but it is also being used as unique at:
Line 2, characters 4-12:
2 |   ((call_pos, call_pos) : @ unique)
        ^^^^^^^^

|}]

let array_pats (arr : int option array) =
  match arr with
  | [| o |] -> let _ = unique_id arr in aliased_id o
  | _ -> None
[%%expect{|
Line 3, characters 33-36:
3 |   | [| o |] -> let _ = unique_id arr in aliased_id o
                                     ^^^
Error: This value is used here as unique,
       but it has already been used in an array pattern at:
Line 3, characters 4-11:
3 |   | [| o |] -> let _ = unique_id arr in aliased_id o
        ^^^^^^^

|}]

let iarray_pats (arr : int option iarray) =
  match arr with
  | [: o :] -> let _ = unique_id arr in unique_id o
  | _ -> None
[%%expect{|
Line 3, characters 50-51:
3 |   | [: o :] -> let _ = unique_id arr in unique_id o
                                                      ^
Error: This value is used here,
       but it is part of a value that has already been used as unique at:
Line 3, characters 33-36:
3 |   | [: o :] -> let _ = unique_id arr in unique_id o
                                     ^^^

|}]

(* Shadowing of unique variables *)
let shadow x =
  x, (let x = (1, 2) in x)
[%%expect{|
val shadow : 'a -> 'a * (int * int) = <fun>
|}]

let array_pat_barrier (arr : int option array) =
  match arr with
  | [| _ |] -> unique_id arr
  | _ -> [| None |]
[%%expect{|
Line 3, characters 25-28:
3 |   | [| _ |] -> unique_id arr
                             ^^^
Error: This value is used here as unique,
       but it has already been used in an array pattern at:
Line 3, characters 4-11:
3 |   | [| _ |] -> unique_id arr
        ^^^^^^^

|}]

let iarray_pat_barrier (arr : int option iarray) =
  match arr with
  | [: _ :] -> unique_id arr
  | _ -> [: None :]
[%%expect{|
Line 3, characters 25-28:
3 |   | [: _ :] -> unique_id arr
                             ^^^
Error: This value is used here as unique,
       but it has already been used in an array pattern at:
Line 3, characters 4-11:
3 |   | [: _ :] -> unique_id arr
        ^^^^^^^

|}]

let constant_pat_barrier (opt : int option) =
  match opt with
  | Some 1 -> unique_id opt
  | _ -> None
[%%expect{|
Line 3, characters 24-27:
3 |   | Some 1 -> unique_id opt
                            ^^^
Error: This value is used here as unique,
       but part of it has already been used in a constant pattern at:
Line 3, characters 9-10:
3 |   | Some 1 -> unique_id opt
             ^

|}]

let lazy_pat_barrier (l : int Lazy.t) =
  match l with
  | lazy 1 -> unique_id l
  | _ -> lazy 2
[%%expect{|
Line 3, characters 24-25:
3 |   | lazy 1 -> unique_id l
                            ^
Error: This value is used here as unique,
       but it has already been used in a lazy pattern at:
Line 3, characters 4-10:
3 |   | lazy 1 -> unique_id l
        ^^^^^^

|}]
