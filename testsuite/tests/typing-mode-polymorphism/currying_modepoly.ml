(* TEST
  flags+="-rectypes -extension mode_polymorphism_alpha";
  expect;
*)


(*
 * Overapplication (functions that return functions)
 *)
let g : local_ 'a -> int -> _ = fun _ _ -> (fun[@curry] (local_ _) (x : int) -> x)
[%%expect{|
val g :
  'a @ local ->
  int ->
  ('b @ [< 'm.future > local] ->
   (int @ 'n -> int @ [< global]) @ [> 'm.future | local]) =
  <fun>
|}]
let apply1 x = g x
[%%expect{|
Line 1, characters 15-18:
1 | let apply1 x = g x
                   ^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
  Hint: This is a partial application
        Adding 1 more argument will make the value non-local
|}]
let apply2 x = g x x
[%%expect{|
val apply2 :
  int @ [< many uncontended] ->
  ('a @ [< 'n.future & 'p.future > 'p.future | local] ->
   (int @ 'o -> int @ [< global]) @ [< 'm.future > 'm.future | 'n.future | local]) @ [< global > nonportable] =
  <fun>
|}]
let apply3 x = g x x x
[%%expect{|
Line 1, characters 15-20:
1 | let apply3 x = g x x x
                   ^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let apply3_wrapped x = (g x x) x
[%%expect{|
Line 1, characters 23-32:
1 | let apply3_wrapped x = (g x x) x
                           ^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
  Hint: This is a partial application
        Adding 1 more argument will make the value non-local
|}]
let apply4 x = g x x x x
[%%expect{|
Line 1, characters 15-20:
1 | let apply4 x = g x x x x
                   ^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let apply4_wrapped x = (g x x) x x
[%%expect{|
val apply4_wrapped : int @ [< many uncontended] -> int @ [< global] = <fun>
|}]
let ill_typed () = g 1 2 3 4 5
[%%expect{|
Line 1, characters 19-30:
1 | let ill_typed () = g 1 2 3 4 5
                       ^^^^^^^^^^^
Error: The function "g" has type
         'a @ local ->
         int ->
         ('b @ [< 'm.future > local] ->
          (int -> int @ [< global]) @ [> 'm.future | local])
       It is applied to too many arguments
Line 1, characters 29-30:
1 | let ill_typed () = g 1 2 3 4 5
                                 ^
  This extra argument is not expected.
|}]

(*
 * Defaulting of modes in printing, similar to mli-less files
 *)

let f g = g (local_ (1, 2)) 1 2 3 [@nontail]
[%%expect{|
val f :
  (int * int @ [< 'n.future > 'o | local] ->
   (int @ [< 'mm0.future > 'mm1] ->
    (int @ [< 'mm2.future > 'mm3] ->
     (int @ 'mm5 -> 'a @ [< 'mm4 & global]) @ [> 'mm2.future | monadic_to_comonadic_min('mm3) | 'q.future | local]) @ [< 'q.future > 'mm0.future | monadic_to_comonadic_min('mm1) | 'm.future | local]) @ [< 'm.future > 'n.future | monadic_to_comonadic_min('o) | 'p.future | local]) @ [< 'p.future] ->
  'a @ [< global > 'mm4] = <fun>
|}]

(*
 * Labels and reordering
 *)

let app1 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42) ()
[%%expect{|
Line 1, characters 64-79:
1 | let app1 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42) ()
                                                                    ^^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "global"
         because it is captured by a partial application
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let app2 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42)
[%%expect{|
Line 1, characters 64-79:
1 | let app2 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42)
                                                                    ^^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "global"
         because it is captured by a partial application
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let app3 (f : a:int -> b:local_ int ref -> unit) = f ~b:(local_ ref 42)
[%%expect{|
Line 1, characters 56-71:
1 | let app3 (f : a:int -> b:local_ int ref -> unit) = f ~b:(local_ ref 42)
                                                            ^^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "global"
         because it is captured by a partial application
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let app4 (f : b:local_ int ref -> a:int -> unit) = f ~b:(local_ ref 42)
[%%expect{|
Line 1, characters 56-71:
1 | let app4 (f : b:local_ int ref -> a:int -> unit) = f ~b:(local_ ref 42)
                                                            ^^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is an argument in a tail call.
|}]
let app42 (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(local_ ref 1) 2 ~c:4
[%%expect{|
Line 2, characters 2-21:
2 |   f ~a:(local_ ref 1) 2 ~c:4
      ^^^^^^^^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let app42_wrapped (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  (f ~a:(local_ ref 1)) 2 ~c:4
[%%expect{|
val app42_wrapped :
  (a:int ref @ local -> (int -> b:int ref @ local -> c:int -> unit)) @ 'n ->
  (b:int ref @ local -> unit) @ [< global > 'm | close('m) | nonportable] =
  <fun>
|}]
let app43 (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(local_ ref 1) 2
[%%expect{|
Line 2, characters 7-21:
2 |   f ~a:(local_ ref 1) 2
           ^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is an argument in a tail call.
|}]
let app5 (f : b:local_ int ref -> a:int -> unit) = f ~a:42
[%%expect{|
val app5 :
  (b:int ref @ local -> a:int -> unit) @ [< 'n.future & global many] ->
  (b:int ref @ local -> unit) @ [< global > 'm | close('m) | 'n.future | nonportable] =
  <fun>
|}]
let app6 (f : a:local_ int ref -> b:local_ int ref -> c:int -> unit) = f ~c:42
[%%expect{|
val app6 :
  (a:int ref @ local -> b:int ref @ local -> c:int -> unit) @ [< 'o.future & global many > 'p.future] ->
  (a:int ref @ local ->
   (b:int ref @ local -> unit) @ [< 'p.future > close('m) | local nonportable]) @ [< global > 'm | 'n | close('n) | 'o.future | nonportable] =
  <fun>
|}]

let app1' (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42) ()
[%%expect{|
val app1' :
  (a:int -> b:int ref @ local -> unit -> unit) @ [< global many > 'm.future] ->
  (a:int -> unit) @ [< 'm.future & global > aliased nonportable] = <fun>
|}]
let app2' (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42)
[%%expect{|
val app2' :
  (a:int -> b:int ref @ local -> unit -> unit) @ [< global many > 'm.future] ->
  (a:int -> (unit -> unit) @ local) @ [< 'm.future & global > aliased nonportable] =
  <fun>
|}]
let app3' (f : a:int -> b:local_ int ref -> unit) = f ~b:(ref 42)
[%%expect{|
val app3' :
  (a:int -> b:int ref @ local -> unit) @ [< global many > 'm.future] ->
  (a:int -> unit) @ [< 'm.future & global > aliased nonportable] = <fun>
|}]
let app4' (f : b:local_ int ref -> a:int -> unit) = f ~b:(ref 42)
[%%expect{|
Line 1, characters 52-65:
1 | let app4' (f : b:local_ int ref -> a:int -> unit) = f ~b:(ref 42)
                                                        ^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
  Hint: This is a partial application
        Adding 1 more argument will make the value non-local
|}]
let app42' (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(ref 1) 2 ~c:4
[%%expect{|
Line 2, characters 2-14:
2 |   f ~a:(ref 1) 2 ~c:4
      ^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let app42'_wrapped (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  (f ~a:(ref 1)) 2 ~c:4
[%%expect{|
val app42'_wrapped :
  (a:int ref @ local -> (int -> b:int ref @ local -> c:int -> unit)) @ 'n ->
  (b:int ref @ local -> unit) @ [< global > 'm | close('m) | nonportable] =
  <fun>
|}]
let app43' (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(ref 1) 2
[%%expect{|
Line 2, characters 2-14:
2 |   f ~a:(ref 1) 2
      ^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]
let app43'_wrapped (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  (f ~a:(ref 1)) 2
[%%expect{|
val app43'_wrapped :
  (a:int ref @ local -> (int -> b:int ref @ local -> c:int -> unit)) @ 'm ->
  (b:int ref @ local -> c:int -> unit) @ [< global > nonportable] = <fun>
|}]

let rapp1 (f : a:int -> unit -> local_ int ref) = f ()
[%%expect{|
val rapp1 :
  (a:int -> unit -> int ref @ local) @ [< global many > 'm.future] ->
  (a:int -> int ref @ local) @ [< 'm.future & global > 'n | close('n) | nonportable] =
  <fun>
|}]
let rapp2 (f : a:int -> unit -> local_ int ref) = f ~a:1
[%%expect{|
val rapp2 :
  (a:int -> unit -> int ref @ local) @ 'm ->
  (unit -> int ref @ local) @ [< global > nonportable] = <fun>
|}]
let rapp3 (f : a:int -> unit -> local_ int ref) = f ~a:1 ()
[%%expect{|
Line 1, characters 50-59:
1 | let rapp3 (f : a:int -> unit -> local_ int ref) = f ~a:1 ()
                                                      ^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let bug1 () =
  let foo : a:local_ string -> b:local_ string -> c:int -> unit =
    fun ~a ~b ~c -> ()
  in
  let bar = local_ foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 7, characters 2-5:
7 |   res
      ^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let bug2 () =
  let foo : a:local_ string -> (b:local_ string -> (c:int -> unit)) =
    fun ~a -> fun[@curry] ~b -> fun[@curry] ~c -> ()
  in
  let bar = local_ foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 5, characters 19-33:
5 |   let bar = local_ foo ~b:"hello" in
                       ^^^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try splitting the application in two. The arguments that come
  after a in the function's type should be applied separately.
|}]
let bug3 () =
  let foo : a:local_ string -> (b:local_ string -> (c:int -> unit)) =
    fun ~a -> fun[@curry] ~b -> fun[@curry] ~c -> print_string a
  in
  let[@stack] bar = foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 3, characters 63-64:
3 |     fun ~a -> fun[@curry] ~b -> fun[@curry] ~c -> print_string a
                                                                   ^
Error: The value "a" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the function at line 3, characters 14-64
         which is expected to be "global".
|}]
let overapp ~(local_ a) ~b = (); fun ~c ~d -> ()

let () = overapp ~a:1 ~b:2 ~c:3 ~d:4
[%%expect{|
val overapp :
  a:'a @ [< 'm.future > local] ->
  (b:'b @ 'p ->
   (c:'c @ [< 'n.future & global] ->
    (d:'d @ 'o -> unit @ [< global]) @ [< global > 'n.future]) @ [< global]) @ [> 'm.future | local] =
  <fun>
Line 3, characters 9-26:
3 | let () = overapp ~a:1 ~b:2 ~c:3 ~d:4
             ^^^^^^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]

let () = overapp ~b:2 ~a:1 ~c:3 ~d:4
[%%expect{|
Line 1, characters 20-21:
1 | let () = overapp ~b:2 ~a:1 ~c:3 ~d:4
                        ^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try splitting the application in two. The arguments that come
  after this one in the function's type should be applied separately.
|}]

let () = overapp ~c:1 ~b:2
[%%expect{|
Line 1, characters 25-26:
1 | let () = overapp ~c:1 ~b:2
                             ^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try splitting the application in two. The arguments that come
  after this one in the function's type should be applied separately.
|}]

let () = overapp ~d:1 ~a:2
[%%expect{|
Line 1, characters 9-26:
1 | let () = overapp ~d:1 ~a:2
             ^^^^^^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling local values, extra arguments are passed in a separate application.
  Hint: Try splitting the application in two. The arguments that come
  after b in the function's type should be applied separately.
|}]


(* Regression test for bug with mishandled regional function modes *)
let bug4 : local_ (string -> foo:string -> unit) -> (string -> unit) =
  fun f -> f ~foo:"hello"
[%%expect{|
Line 2, characters 11-25:
2 |   fun f -> f ~foo:"hello"
               ^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
  Hint: This is a partial application
        Adding 1 more argument will make the value non-local
|}]

(* The fixed version. Note that in the printed type, local returning is implicit
    *)
let bug4_fixed : local_ (string -> foo:string -> unit) -> local_ (string -> unit) =
  fun f -> exclave_ f ~foo:"hello"
[%%expect{|
val bug4_fixed : (string -> foo:string -> unit) @ local -> string -> unit =
  <fun>
|}]


let bug4' () =
  let local_ f arg ~foo = () in
  let local_ perm ~foo = f ~foo in
  perm ~foo:"foo" "Optional"
[%%expect{|
Line 3, characters 25-31:
3 |   let local_ perm ~foo = f ~foo in
                             ^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
  Hint: This is a partial application
        Adding 1 more argument may make the value non-local
|}]

(* The above tests for the locality axis exhaust cases wrt
  [Known_arg/Unknown_arg/Omitted_optional_arg/Omitted],
  Tests below only try to exhaust mode axes. *)

(* Uniqueness & linearity *)

let _ =
  let f : _ @ unique -> (_ -> _) = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
Line 3, characters 2-11:
3 |   f "hello" "world"
      ^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling once values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]

let _ =
  let f : _ @ once -> (_ -> _) = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
Line 3, characters 2-11:
3 |   f "hello" "world"
      ^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling once values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]

let _ =
  let f : (_ -> (_ -> _)) @ once = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
Line 3, characters 2-11:
3 |   f "hello" "world"
      ^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling once values, extra arguments are passed in a separate application.
  Hint: Try wrapping the marked application in parentheses.
|}]

let _ =
  let f : (_ -> (_ -> _)) @ unique = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

(* portability and contention, due to the choice of legacy modes, don't have
   the same problem. *)
let _ =
  let f : _ @ uncontended -> (_ -> _) = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

let _ =
  let f : _ @ nonportable -> (_ -> _) = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

let _ =
  let f : (_ -> (_ -> _)) @ nonportable = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

let _ =
  let f : (_ -> (_ -> _)) @ uncontended = fun _ -> fun x -> x in
  f "hello" "world"
[%%expect{|
- : string = "world"
|}]

(* printing is similar to generating [cmi] without [mli]. Tests below show that
   the inferred type of [g] doesn't contain extra parenthesis, as they would
   if pushed to legacy; this is prevented by [check_curried_application_complete]. *)
let f g x =
  g (x: _ @ once) x [@nontail]
[%%expect{|
val f :
  ('a @ [< 'm.future > 'n | 'mm0 | once aliased] ->
   ('a @ [> 'q | aliased] -> 'b @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future | once]) @ [< 'o.future & 'mm1.future & global] ->
  ('a @ [< 'q & 'mm0 & many] -> 'b @ [< global > 'p]) @ [< global > 'mm1.future] =
  <fun>
|}]

let f g x y =
  g (x: _ @ unique) y [@nontail]
[%%expect{|
val f :
  ('a @ [< 'm.future > 'mm0 | 'n | 'mm1] ->
   ('b @ [> 'q] -> 'c @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future]) @ [< 'o.future & 'mm2.future & global] ->
  ('a @ [< 'mm3.future & 'mm1 & global unique] ->
   ('b @ [< 'q] -> 'c @ [< global > 'p]) @ [< global > 'mm3.future | close('mm0) | once]) @ [< global > 'mm2.future] =
  <fun>
|}]

let f (g @ unique) x =
  g x x [@nontail]
[%%expect{|
val f :
  ('a @ [< 'm.future > 'n | 'mm0 | aliased] ->
   ('a @ [> 'q | aliased] -> 'b @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future]) @ [< 'o.future & 'mm1.future & global unique] ->
  ('a @ [< 'q & 'mm0 & many] -> 'b @ [< global > 'p]) @ [< global > 'mm1.future] =
  <fun>
|}, Principal{|
val f :
  ('a @ [< 'm.future > 'n | 'mm0 | aliased] ->
   ('a @ [> 'q | aliased] -> 'b @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future]) @ [< 'o.future & 'mm1.future & global unique] ->
  ('a @ [< 'q & 'mm0 & many] -> 'b @ [< global > 'p]) @ [< global > 'mm1.future] =
  <fun>
|}]

let f (g @ once) x =
  g x x [@nontail]
[%%expect{|
val f :
  ('a @ [< 'm.future > 'n | 'mm0 | aliased] ->
   ('a @ [> 'q | aliased] -> 'b @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future | once]) @ [< 'o.future & 'mm1.future & global > once] ->
  ('a @ [< 'q & 'mm0 & many] -> 'b @ [< global > 'p]) @ [< global > 'mm1.future | once] =
  <fun>
|}]

(* portability and contention is not affected due to the choice of legacy modes. *)
let f g x =
  g (x: _ @ nonportable) x [@nontail]
[%%expect{|
val f :
  ('a @ [< 'm.future > 'n | 'mm0 | aliased nonportable] ->
   ('a @ [> 'q | aliased] -> 'b @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future | nonportable]) @ [< 'o.future & 'mm1.future & global] ->
  ('a @ [< 'q & 'mm0 & many] -> 'b @ [< global > 'p]) @ [< global > 'mm1.future] =
  <fun>
|}]

let f g x y =
  g (x: _ @ uncontended) y [@nontail]
[%%expect{|
val f :
  ('a @ [< 'm.future > 'mm0 | 'n | 'mm1] ->
   ('b @ [> 'q] -> 'c @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future]) @ [< 'o.future & 'mm2.future & global] ->
  ('a @ [< 'mm3.future & 'mm1 & global uncontended] ->
   ('b @ [< 'q] -> 'c @ [< global > 'p]) @ [< global > 'mm3.future | close('mm0) | nonportable]) @ [< global > 'mm2.future] =
  <fun>
|}]

let f (g @ uncontended) x =
  g x x [@nontail]
[%%expect{|
val f :
  ('a @ [< 'm.future > 'n | 'mm0 | aliased] ->
   ('a @ [> 'q | aliased] -> 'b @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future]) @ [< 'o.future & 'mm1.future & global uncontended] ->
  ('a @ [< 'q & 'mm0 & many] -> 'b @ [< global > 'p]) @ [< global > 'mm1.future] =
  <fun>
|}]

let f (g @ nonportable) x =
  g x x [@nontail]
[%%expect{|
val f :
  ('a @ [< 'm.future > 'n | 'mm0 | aliased] ->
   ('a @ [> 'q | aliased] -> 'b @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future | nonportable]) @ [< 'o.future & 'mm1.future & global > nonportable] ->
  ('a @ [< 'q & 'mm0 & many] -> 'b @ [< global > 'p]) @ [< global > 'mm1.future | nonportable] =
  <fun>
|}]

type t = (int -> 'f) as 'f
[%%expect{|
type t = int -> 'a as 'a
|}]

type t = (int -> 'f @ local) as 'f
[%%expect{|
type t = int -> 'a @ local as 'a
|}]

(* In the following, [local] on [int] doesn't trigger the mode currying on ['f].
   The printing reproduces the parsing. *)
type t = (int @ local -> 'f) as 'f
[%%expect{|
type t = int @ local -> 'a as 'a
|}]

type t = (int @ local -> 'f @ global) as 'f
[%%expect{|
type t = int @ local -> 'a as 'a
|}]

type t = (int @ local -> 'f @ local) as 'f
[%%expect{|
type t = int @ local -> 'a @ local as 'a
|}]

(* CR zqian: add tests for [Tpoly (_, [])] *)
