(* TEST
 expect;
*)

let leak n =
  let r = stack_ (ref n) in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

external idint : int @ local -> int = "%identity"
[%%expect{|
external idint : int @ local -> int = "%identity"
|}]

let noleak n =
  let r = stack_ (ref n) in
  idint (r.contents)
[%%expect{|
val noleak : int -> int = <fun>
|}]


let (!) = fun (r @ local) -> r.contents
[%%expect{|
val ( ! ) : 'a ref @ local -> 'a = <fun>
|}]

(* Local lets *)

let leak n =
  let r @ local = ref n in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let leak n =
  let r : int ref @ local = ref n in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let leak n =
  let (f @ local) : 'a. 'a -> 'a = fun x -> x in
  f
[%%expect{|
Line 3, characters 2-3:
3 |   f
      ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let leak n =
  let (f @ local) x : int = x in
  f
[%%expect{|
Line 3, characters 2-3:
3 |   f
      ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* If both type and mode are wrong, complain about type *)
let f () =
  let r @ local = ref 42 in
  print_endline r
[%%expect{|
Line 3, characters 16-17:
3 |   print_endline r
                    ^
Error: The value "r" has type "int ref" but an expression was expected of type
         "string"
|}]

(*
 * Type equalities of function types
 *)

  (* When a [local_] argument appears in a function type with multiple arguments,
     return modes are implicitly stack until the final argument. *)
type equ_fn = unit
  constraint
    'a -> 'b @ local -> 'c -> 'd -> 'e
    = 'a -> 'b @ local -> ('c -> ('d -> 'e) @ local) @ local
[%%expect{|
type equ_fn = unit
|}]

type distinct_sarg = unit constraint int @ local -> int = int -> int
[%%expect{|
Line 1, characters 37-68:
1 | type distinct_sarg = unit constraint int @ local -> int = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int @ local -> int" is not compatible with type "int -> int"
|}]
type distinct_sret = unit constraint int -> int @ local = int -> int
[%%expect{|
Line 1, characters 37-68:
1 | type distinct_sret = unit constraint int -> int @ local = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int -> int @ local" is not compatible with type "int -> int"
|}]
type distinct_sarg_sret = unit constraint int @ local -> int = int @ local -> int @ local
[%%expect{|
Line 1, characters 42-89:
1 | type distinct_sarg_sret = unit constraint int @ local -> int = int @ local -> int @ local
                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int @ local -> int" is not compatible with type
         "int @ local -> int @ local"
|}]

type local_higher_order = unit constraint
  (int -> int -> int) @ local -> int = (int -> (int -> int) @ local) @ local -> int
[%%expect{|
type local_higher_order = unit
|}]

type nonlocal_higher_order = unit constraint
  (int -> int -> int) -> int = (int -> (int -> int) @ local) -> int
[%%expect{|
Line 2, characters 2-67:
2 |   (int -> int -> int) -> int = (int -> (int -> int) @ local) -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "(int -> int -> int) -> int" is not compatible with type
         "(int -> (int -> int) @ local) -> int"
       Type "int -> int -> int" is not compatible with type
         "int -> (int -> int) @ local"
|}]

type local_higher_order = unit constraint
  int -> (int -> int -> int) @ local = int -> (int -> (int -> int) @ local) @ local
[%%expect{|
type local_higher_order = unit
|}]

type nonlocal_higher_order = unit constraint
  int -> (int -> int -> int) = int -> (int -> (int -> int) @ local)
[%%expect{|
Line 2, characters 2-67:
2 |   int -> (int -> int -> int) = int -> (int -> (int -> int) @ local)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type "int -> int -> int -> int" is not compatible with type
         "int -> int -> (int -> int) @ local"
       Type "int -> int -> int" is not compatible with type
         "int -> (int -> int) @ local"
|}]

let foo () =
  let (_bar @ local) : int -> int -> int =
    ((fun y z -> z) : int -> (int -> int) @ local) in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let _bar : int -> int -> int =
    ((fun y z -> z) : int -> (int -> int) @ local) in
  ()
[%%expect{|
Line 3, characters 4-50:
3 |     ((fun y z -> z) : int -> (int -> int) @ local) in
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int -> (int -> int) @ local"
       but an expression was expected of type "int -> int -> int"
|}]

let foo () =
  let (_bar @ local) : 'a. 'a -> 'a -> 'a =
    ((fun y z -> z) : _ -> (_ -> _) @ local) in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let _bar : 'a. 'a -> 'a -> 'a =
    ((fun y z -> z) : _ -> (_ -> _) @ local) in
  ()
[%%expect{|
Line 3, characters 4-44:
3 |     ((fun y z -> z) : _ -> (_ -> _) @ local) in
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'b -> ('c -> 'c) @ local"
       but an expression was expected of type "'a -> 'a -> 'a"
|}]

let foo () =
  let (_bar @ local) x : int -> int -> int =
    ((fun y z -> z) : int -> (int -> int) @ local) in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let _bar x : int -> int -> int =
    ((fun y z -> z) : int -> (int -> int) @ local) in
  ()
[%%expect{|
Line 3, characters 4-50:
3 |     ((fun y z -> z) : int -> (int -> int) @ local) in
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int -> (int -> int) @ local"
       but an expression was expected of type "int -> int -> int"
|}]

let foo (bar : (int -> int -> int) @ local) =
  let _ = (bar : int -> (int -> int) @ local) in
  ()
[%%expect{|
val foo : (int -> int -> int) @ local -> unit = <fun>
|}]

let foo (bar : int -> (int -> int) @ local) =
  let _ = (bar : int -> int -> int) in
  ()
[%%expect{|
Line 2, characters 11-14:
2 |   let _ = (bar : int -> int -> int) in
               ^^^
Error: The value "bar" has type "int -> (int -> int) @ local"
       but an expression was expected of type "int -> int -> int"
|}]


(*
 * Curried functions and partial application
 *)

(* f4 results in a local value if it is partially applied to two or
   three arguments, because it closes over the locally-allocated
   second argument. Applications to 1 or 4 arguments are not local. *)
let f4 : int -> 'a @ local -> int -> int -> int =
  fun a _ b c -> a + b + c
[%%expect{|
val f4 : int -> 'a @ local -> int -> int -> int = <fun>
|}]

let apply1 x = f4 x
[%%expect{|
val apply1 : int -> 'a @ local -> int -> int -> int = <fun>
|}]
let apply2 x = f4 x x
[%%expect{|
Line 1, characters 15-21:
1 | let apply2 x = f4 x x
                   ^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
Hint: This is a partial application
      Adding 2 more arguments will make the value non-local
|}]
let apply3 x = f4 x x x
[%%expect{|
Line 1, characters 15-23:
1 | let apply3 x = f4 x x x
                   ^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
Hint: This is a partial application
      Adding 1 more argument will make the value non-local
|}]
let apply4 x =
  f4 x x x x
[%%expect{|
val apply4 : int -> int = <fun>
|}]

(* Partial applications of two or three arguments are OK if bound locally *)
let apply2_stack x =
  let g = f4 x x in
  let res = g x x in
  res
let apply3_stack x =
  let g = f4 x x x in
  let res = g x in
  res
[%%expect{|
val apply2_stack : int -> int = <fun>
val apply3_stack : int -> int = <fun>
|}]

(*
 * Optional arguments
 *)
let appopt1 (f : ?a: int ref @ local -> unit -> unit) =
  let res = f ~a:(let x = stack_ (ref 42) in x) () in
  res
[%%expect{|
val appopt1 : (?a:int ref @ local -> unit -> unit) -> unit = <fun>
|}]
let appopt2 (f : ?a: int ref @ local -> unit -> unit) =
  let res = f ~a:(let x = stack_ (ref 42) in x) in
  res
[%%expect{|
Line 3, characters 2-5:
3 |   res
      ^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* In principle. it would be sound to allow this one:
   we close over a value in Alloc_local mode, but it is known to be immediate *)
let appopt3 (f : ?a: int ref @ local -> int -> int -> unit) =
  let res = f 42 in
  res
[%%expect{|
Line 3, characters 2-5:
3 |   res
      ^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let optret1 (f : ?x:int -> (y:unit -> unit -> int) @ local) = f ()
[%%expect{|
Line 1, characters 62-66:
1 | let optret1 (f : ?x:int -> (y:unit -> unit -> int) @ local) = f ()
                                                                  ^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
Hint: This is a partial application
      Adding 1 more argument will make the value non-local
|}]

(* Optional argument elimination eta-expands and therefore allocates *)
let no_eta (f : (unit -> int) @ local) = (f : unit -> int)
[%%expect{|
val no_eta : (unit -> int) @ local -> unit -> int = <fun>
|}]

let eta (f : (?a:bool -> unit -> int) @ local) = (f : unit -> int)
[%%expect{|
Line 1, characters 50-51:
1 | let eta (f : (?a:bool -> unit -> int) @ local) = (f : unit -> int)
                                                      ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is to omit some parameters by partial application (and thus an allocation)
         which is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let etajoin p (f : ?b:bool -> unit -> int) (g : (unit -> int) @ local) =
  if p then (f : unit -> int) else g
[%%expect{|
val etajoin :
  bool -> (?b:bool -> unit -> int) -> (unit -> int) @ local -> unit -> int =
  <fun>
|}]

(* Default arguments *)

let foo ?(x @ local) () = x;;
[%%expect{|
val foo : ('a : any). ?x:'a @ local -> unit -> 'a option @ local = <fun>
|}]

let foo ?(x @ local = "hello") () = x;;
[%%expect{|
val foo : ?x:string @ local -> unit -> string @ local = <fun>
|}]

let foo ?(x @ local = ("hello" : @ local)) () = x;;
[%%expect{|
Line 1, characters 22-41:
1 | let foo ?(x @ local = ("hello" : @ local)) () = x;;
                          ^^^^^^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

let foo ?(x @ local = ("hello" : @ local)) () = (x : @ local);;
[%%expect{|
Line 1, characters 22-41:
1 | let foo ?(x @ local = ("hello" : @ local)) () = (x : @ local);;
                          ^^^^^^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

(*
 * Closures and context locks
 *)

let heap_closure () =
  let foo = stack_ (ref 1) in
  let fn () =
    let[@stack] fn2 () =
      let[@stack] _baz = foo in
      () in
    let res = fn2 () in
    res
  in
  let _force_heap = ref fn in
  let res = fn () in
  res

[%%expect{|
Line 10, characters 24-26:
10 |   let _force_heap = ref fn in
                             ^^
Error: This value is "local"
         because it closes over the value "foo" at line 5, characters 25-28
         which is "local".
       However, the highlighted expression is expected to be "global".
|}]

let local_closure () =
  let foo = stack_ (ref 1) in
  let (fn @ local) () =
    let (fn2 @ local) () =
      let _baz @ local = foo  in
      ()
    in
    let res = fn2 () in
    res
  in
  let res = fn () in
  res

[%%expect{|
val local_closure : unit -> unit = <fun>
|}]

(*
 * Always-nonlocal things
 *)
let toplevel_stack = stack_ {contents=42}
[%%expect{|
Line 1, characters 21-41:
1 | let toplevel_stack = stack_ {contents=42}
                         ^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "global".
|}]

module M = struct
  let _ = stack_ {contents=42}
end
[%%expect{|
module M : sig end
|}]

let _ = stack_ {contents=42}
[%%expect{|
Line 1, characters 8-28:
1 | let _ = stack_ {contents=42}
            ^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "global".
|}]


module type T = sig val x : int option end
let first_class_module () =
  let thing = stack_ (Some 1) in
  let _m : (module T) @ local = (module struct let x = thing end) in
  ()
[%%expect{|
module type T = sig val x : int option end
Line 4, characters 51-52:
4 |   let _m : (module T) @ local = (module struct let x = thing end) in
                                                       ^
Error: The expression is "local" because it is "stack_"-allocated.
       However, the expression highlighted is expected to be "global"
         because it is the value "x" in the structure at line 4, characters 47-60
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]
let local_module () =
  let thing = stack_ (Some 1) in
  let _ =
    let module M = struct let x = thing end in
    (() : @ local)
  in ()
[%%expect{|
Line 4, characters 30-31:
4 |     let module M = struct let x = thing end in
                                  ^
Error: The expression is "local" because it is "stack_"-allocated.
       However, the expression highlighted is expected to be "global"
         because it is the value "x" in the structure at line 4, characters 26-39
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]
let obj () =
  let thing = stack_ (Some 1) in
  let _obj = object method foo = thing end in
  ()
[%%expect{|
Line 3, characters 33-38:
3 |   let _obj = object method foo = thing end in
                                     ^^^^^
Error: The value "thing" is "local" because it is "stack_"-allocated.
       However, the value "thing" highlighted is expected to be "global"
         because it is used in an object (at line 3, characters 13-42).
|}]


(*
 * Higher order functions, with arguments that promise not to leak
 *)

let use_locally (f : 'a @ local -> 'a) (x : 'a) = f x
(* This version also promises not to leak the closure *)
let use_locally' (f : ('a @ local -> 'a) @ local) (x : 'a) =
  let res = f x in
  res
[%%expect{|
val use_locally : ('a @ local -> 'a) -> 'a -> 'a = <fun>
val use_locally' : ('a @ local -> 'a) @ local -> 'a -> 'a = <fun>
|}]

let no_leak = use_locally (fun x -> 1) 42
let no_leak' = use_locally' (fun x -> 1) 42
[%%expect{|
val no_leak : int = 1
val no_leak' : int = 1
|}]

let leak_id =
  use_locally (fun x -> x) 42
[%%expect{|
Line 2, characters 24-25:
2 |   use_locally (fun x -> x) 42
                            ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let leak_ref =
  let r = ref None in
  use_locally (fun x -> r.contents <- Some x; x) 42

[%%expect{|
Line 3, characters 43-44:
3 |   use_locally (fun x -> r.contents <- Some x; x) 42
                                               ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "Some") in the value at line 3, characters 38-44
         which is expected to be "global".
|}]

let leak_ref_2 =
  let r = stack_ (ref None) in
  use_locally (fun x -> let _ @ local = r  in r.contents <- Some x; x) 42
[%%expect{|
Line 3, characters 40-41:
3 |   use_locally (fun x -> let _ @ local = r  in r.contents <- Some x; x) 42
                                            ^
Error: The value "r" is "local"
       but is expected to be "global"
         because it is used inside the function at line 3, characters 14-70
         which is expected to be "global".
|}]

let leak_ref_3 =
  let r = stack_ (ref None) in
  use_locally' (fun x -> let _ @ local = r  in r.contents <- Some x; x) 42
[%%expect{|
Line 3, characters 66-67:
3 |   use_locally' (fun x -> let _ @ local = r  in r.contents <- Some x; x) 42
                                                                      ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "Some") in the value at line 3, characters 61-67
         which is expected to be "global".
|}]


(* raised exceptions must be global *)
let no_leak_exn =
  use_locally (fun x -> let _exn = stack_ (Invalid_argument x) in "bluh") "blah"
[%%expect{|
val no_leak_exn : string = "bluh"
|}]
let do_leak_exn =
  use_locally (fun x -> let _exn @ local = raise (Invalid_argument x)  in "bluh") "blah"

[%%expect{|
Line 2, characters 67-68:
2 |   use_locally (fun x -> let _exn @ local = raise (Invalid_argument x)  in "bluh") "blah"
                                                                       ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "Invalid_argument") in the value at line 2, characters 49-69
         which is expected to be "global".
|}]

(* handled exceptions are known to be global *)
let catch (f : unit -> string @ local) =
  let a =
    match f () with
    | _ -> "hello"
    | exception (Invalid_argument x) -> x
  in
  let b =
    try let _ = f () in "hello" with
    | Invalid_argument x -> x
  in
  (a, b)
[%%expect{|
val catch : (unit -> string @ local) -> string * string = <fun>
|}]


(* same, but this time the function is allowed to return its argument *)
let use_locally (f : 'a @ local -> 'a @ local) : 'a @ local -> 'a @ local = f
[%%expect{|
val use_locally :
  ('a : any). ('a @ local -> 'a @ local) -> 'a @ local -> 'a @ local = <fun>
|}]

let loc = ((fun x -> (x : @ local)) : int @ local -> int @ local)

let no_leak_id =
  let _ =
    (use_locally ((fun x -> (x : @ local)) : int @ local -> int @ local) 42 : @ local)
  in ()

[%%expect{|
val loc : int @ local -> int @ local = <fun>
val no_leak_id : unit = ()
|}]

module type S = sig val s : string end

(* Currently we can't stack-allocate modules, but it's fine to take modules as
   local parameters. *)

let bar ((m : (module S)) @ local) =
  let (module _) = m in
  ()
[%%expect{|
module type S = sig val s : string end
val bar : (module S) @ local -> unit = <fun>
|}]

(* While it's sound to let modules cross locality (since they are always heap
   allocated), we choose not to expose this for future compatibility. *)
let bar ((m : (module S)) @ local) =
  let (module M) = m in
  M.s
[%%expect{|
val bar : (module S) @ local -> string @ local = <fun>
|}]

let bar (m @ local) =
  let module M = (val m : S) in
  M.s
[%%expect{|
val bar : (module S) @ local -> string @ local = <fun>
|}]

(* packing is allocation, and we can't construct a global module using local values *)
let bar (m @ local) =
  let module M = (val m : S) in
  (module M : S)
[%%expect{|
Line 3, characters 10-11:
3 |   (module M : S)
              ^
Error: Signature mismatch:
       Got "local" to the parent region
       but expected "global"
         because it is a module and thus needs to be allocated on the heap.
|}]

(* Don't escape through a lazy value *)

let foo (x @ local) =
  let _ = lazy (print_string !x) in
  ()
[%%expect{|
Line 2, characters 30-31:
2 |   let _ = lazy (print_string !x) in
                                  ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the lazy expression at line 2, characters 10-32
         which is expected to be "global"
         because lazy expressions always need to be allocated on the heap.
|}]

(* Don't escape through a functor *)

let foo (x @ local) =
  let module Foo (X : sig end) = struct
    let () = print_string !x
  end in
  let module _ = Foo(struct end) in
  ()
[%%expect{|
Line 3, characters 27-28:
3 |     let () = print_string !x
                               ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the functor at lines 2-4, characters 17-5
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]

(* Don't escape through a functor with underscore parameter *)

let foo (x @ local) =
  let module Foo (_ : sig end) = struct
    let () = print_string !x
  end in
  let module _ = Foo(struct end) in
  ()
[%%expect{|
Line 3, characters 27-28:
3 |     let () = print_string !x
                               ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the functor at lines 2-4, characters 17-5
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]

(* Don't escape through a generative functor *)

let foo (x @ local) =
  let module Foo () = struct
    let () = print_string !x
  end in
  let module _ = Foo() in
  ()
[%%expect{|
Line 3, characters 27-28:
3 |     let () = print_string !x
                               ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the functor at lines 2-4, characters 17-5
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]

(* Don't escape through a functor with underscore parameter *)

let foo (x @ local) =
  let module Foo (_ : sig end) = struct
    let () = print_string !x
  end in
  let module _ = Foo(struct end) in
  ()
[%%expect{|
Line 3, characters 27-28:
3 |     let () = print_string !x
                               ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the functor at lines 2-4, characters 17-5
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]

(* Don't escape through a generative functor *)

let foo (x @ local) =
  let module Foo () = struct
    let () = print_string !x
  end in
  let module _ = Foo() in
  ()
[%%expect{|
Line 3, characters 27-28:
3 |     let () = print_string !x
                               ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the functor at lines 2-4, characters 17-5
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]

(* Don't escape through a class method *)

let foo (x @ local) =
  let module M = struct
    class c = object
      method m = !x
    end
  end in new c
[%%expect{|
Line 4, characters 18-19:
4 |       method m = !x
                      ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used in a class (at lines 3-5, characters 14-7).
|}]

(* Don't escape through an object method *)

let foo (x @ local) =
  let o = object
    method m = !x
  end in
  o#m

[%%expect{|
Line 3, characters 16-17:
3 |     method m = !x
                    ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used in an object (at lines 2-4, characters 10-5).
|}]

(* Don't escape through a class instance variable *)

let foo (x @ local) =
  let module M = struct
    class c = object
      val m = !x
    end
  end in new c
[%%expect{|
Line 4, characters 15-16:
4 |       val m = !x
                   ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used in a class (at lines 3-5, characters 14-7).
|}]

(* Don't escape through a class instance variable *)

let foo (x @ local) =
  let o = object
    val m = !x
  end in o
[%%expect{|
Line 3, characters 13-14:
3 |     val m = !x
                 ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used in an object (at lines 2-4, characters 10-5).
|}]

(* Don't escape through a class local variable *)

let foo (x @ local) =
  let module M = struct
    class c =
      let y = x in
      object end
  end in new M.c
[%%expect{|
Line 4, characters 10-11:
4 |       let y = x in
              ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo (x @ local) =
  let module M = struct
    class c =
      let _ = x in
      object end
  end in new M.c
[%%expect{|
val foo : 'a @ local -> <  > = <fun>
|}]

let foo (x : string ref @ local) =
  let module M = struct
    class c =
      let y = !x in
      object method m = y
    end
  end in new M.c
[%%expect{|
val foo : string ref @ local -> < m : string > = <fun>
|}]

(* Don't escape under a class parameter variable *)

let foo (x : string ref @ local) =
  let module M = struct
    class c =
      fun () ->
      let y = !x in
      object method m = y end
  end in new M.c
[%%expect{|
Line 5, characters 15-16:
5 |       let y = !x in
                   ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used in a class (at lines 4-6, characters 10-29).
|}]

let foo (x : string ref @ local) =
  let module M = struct
    class c =
      let y = !x in
      fun () ->
      object method m = y end
  end in new M.c
[%%expect{|
val foo : string ref @ local -> (unit -> < m : string >) = <fun>
|}]

(* Don't escape in inherit expressions *)

class d (p : string) = object method m = p end

let foo (x : string ref @ local) =
  let module M = struct
    class c = object
      inherit d !x
      method n = 42
    end
  end in new M.c
[%%expect{|
class d : string -> object method m : string end
Line 6, characters 17-18:
6 |       inherit d !x
                     ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used in a class (at lines 5-8, characters 14-7).
|}]

(* Don't escape in initializers *)

let foo (x @ local) =
  let o = object
    initializer (print_string !x)
  end in
  o#m

[%%expect{|
Line 3, characters 31-32:
3 |     initializer (print_string !x)
                                   ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used in an object (at lines 2-4, characters 10-5).
|}]

(* Don't escape in non-function 'let rec' bindings *)
let foo (x @ local) =
  (* fine, local recursive function *)
  let rec g () = let _ = x in h (); () and h () = g (); () in
  g (); ()
[%%expect {|
val foo : 'a @ local -> unit = <fun>
|}]

let foo (x @ local) =
  (* fine, local non-recursive binding *)
  let _ = (x, 1) in
  1
[%%expect {|
val foo : 'a @ local -> int = <fun>
|}]

let foo (x @ local) =
  (* not fine, local recursive non-function (needs caml_alloc_dummy) *)
  let rec g = x :: g in
  let _ = g in ()
[%%expect {|
Line 3, characters 14-15:
3 |   let rec g = x :: g in
                  ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "::") in the value at line 3, characters 14-20
         which is expected to be "global".
|}]

(* Cannot pass local values to tail calls *)

let print (x @ local) = print_string !x

let foo x =
  let r = stack_ { contents = x } in
  print r
[%%expect{|
val print : string ref @ local -> unit = <fun>
Line 5, characters 8-9:
5 |   print r
            ^
Error: This value is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is an argument in a tail call.
|}]

let local_cb (f @ local) = f ()
let foo (x @ local) = local_cb (fun () -> x := 17; 42)
[%%expect{|
val local_cb : (unit -> 'a) @ local -> 'a = <fun>
Line 2, characters 42-43:
2 | let foo (x @ local) = local_cb (fun () -> x := 17; 42)
                                              ^
Error: The value "x" is "local" to the parent region
       but is expected to be "global"
         because it is used inside the function at line 2, characters 31-54
         which is expected to be "global" because it is an allocation
         which is expected to be "local" to the parent region or "global"
         because it is an argument in a tail call.
|}]

let foo x =
  let r = stack_ { contents = x } in
  print r;
  ()
[%%expect{|
val foo : string -> unit = <fun>
|}]

let foo x = exclave_
  let r = stack_ { contents = x } in
  print r
[%%expect{|
val foo : string -> unit @ local = <fun>
|}]

(* Can pass local values to calls explicitly marked as nontail *)
let foo x =
  let r @ local = ref x in
  print r [@nontail]
[%%expect{|
val foo : string -> unit = <fun>
|}]

let foo x =
  let (f @ local) () = 42 in
  f () [@nontail]
[%%expect{|
val foo : 'a -> int = <fun>
|}]

(* Cannot call local values in tail calls *)

let foo x =
  let r = stack_ { contents = x } in
  let (foo @ local) () = r.contents in
  foo ()
[%%expect{|
Line 4, characters 2-5:
4 |   foo ()
      ^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is the function in a tail call.
|}]

let foo x =
  let r = stack_ { contents = x } in
  let (foo @ local) () = r.contents in
  let res = foo () in
  res
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]

let foo x = exclave_
  let r = stack_ { contents = x } in
  let (foo @ local) () = r.contents in
  foo ()
[%%expect{|
val foo : 'a -> 'a @ local = <fun>
|}]

(* Cannot return local values without annotations on all exits *)

let foo x =
  let r = stack_ { contents = x } in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: This value is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let foo x = exclave_
  let r = stack_ { contents = x } in
  r
[%%expect{|
val foo : 'a -> 'a ref @ local = <fun>
|}]

let foo p x = exclave_
  let r = stack_ { contents = x } in
  if p then r
  else r
[%%expect{|
val foo : bool -> 'a -> 'a ref @ local = <fun>
|}]

(* Non-local regional values can be passed to tail calls *)
let rec length acc (xl @ local) =
  match xl with
  | [] -> 0
  | x :: xs -> length (acc + 1) xs
[%%expect{|
val length : int -> 'a list @ local -> int = <fun>
|}]

let foo () =
  let r = stack_ (ref 5) in
  let bar x = !x in
  let baz () =
    bar r
  in
  let x = baz () in
  x
[%%expect{|
val foo : unit -> int = <fun>
|}]

(* tail-calling local-returning functions make the current function
   local-returning as well, and must therefore use [exclave_] even if
   the callee does not return an allocation. We allocate to demonstrate
   the potential leaking.  *)
let foo () = exclave_
  let _ = stack_ (52, 24) in
  42
[%%expect{|
val foo : unit -> int @ local = <fun>
|}]

let bar () =
  let _x = 52 in
  foo ()
[%%expect{|
Line 3, characters 2-8:
3 |   foo ()
      ^^^^^^
Error: This local-returning application is in a tail position that is not
       enclosed in an exclave_ expression.
|}]

let bar () : int @ local = foo ()
[%%expect{|
Line 1, characters 27-33:
1 | let bar () : int @ local = foo ()
                               ^^^^^^
Error: This local-returning application is in a tail position that is not
       enclosed in an exclave_ expression.
|}]

let bar () = exclave_
  let _x = 52 in
  foo ()
[%%expect{|
val bar : unit -> int @ local = <fun>
|}]

(* if not at tail, then not affected *)
let bar' () =
  let _x = foo () in
  52
[%%expect{|
val bar' : unit -> int = <fun>
|}]

(* nontail attribute works as well *)
let bar' () =
  foo () [@nontail]
[%%expect{|
val bar' : unit -> int = <fun>
|}]

(* variant of above where f is an argument that is local-returning, but does not
   return an allocation *)
let foo g = let () = g () in ()
[%%expect{|
val foo : (unit -> unit) -> unit = <fun>
|}]

let bar (f : _ -> _ @ local) = foo (fun () -> f ())
[%%expect{|
Line 1, characters 46-50:
1 | let bar (f : _ -> _ @ local) = foo (fun () -> f ())
                                                  ^^^^
Error: This local-returning application is in a tail position that is not
       enclosed in an exclave_ expression.
|}]

(* Parameter modes must be matched by the type *)

let foo : 'a -> unit = fun (x @ local) -> ()
[%%expect{|
Line 1, characters 27-38:
1 | let foo : 'a -> unit = fun (x @ local) -> ()
                               ^^^^^^^^^^^
Error: The parameter is "local" but is expected to be "global".
|}]

let rec f1 () = f2 ()
and f2 () : string @ local = exclave_ "hi"
[%%expect{|
Line 2, characters 29-42:
2 | and f2 () : string @ local = exclave_ "hi"
                                 ^^^^^^^^^^^^^
Error: The function return is "local" but is expected to be "global".
|}]

(* Return mode must be greater than the type *)

let foo : unit -> string @ local = fun () -> "hello"
[%%expect{|
val foo : unit -> string @ local = <fun>
|}]

let foo : unit -> string = fun () -> exclave_ "hello"
[%%expect{|
Line 1, characters 37-53:
1 | let foo : unit -> string = fun () -> exclave_ "hello"
                                         ^^^^^^^^^^^^^^^^
Error: This expression is local because it is an exclave,
       but was expected otherwise.
|}]

(* Unboxed type constructors do not affect regionality *)
type 'a unb1 = A of 'a [@@unboxed]
type 'a unb2 = { foo : 'a } [@@unboxed]
type 'a unb3 = B of { bar : 'a } [@@unboxed]
let f (x @ local) = B { bar = { foo = A x } }
[%%expect{|
type 'a unb1 = A of 'a [@@unboxed]
type 'a unb2 = { foo : 'a; } [@@unboxed]
type 'a unb3 = B of { bar : 'a; } [@@unboxed]
val f : 'a @ local -> 'a unb1 unb2 unb3 @ local = <fun>
|}]


(* Fields have the same mode unless they are global or mutable *)

type 'a imm = { imm : 'a }
type 'a mut = { mutable mut : 'a }
type 'a gbl = { gbl : 'a @@ global }
[%%expect{|
type 'a imm = { imm : 'a; }
type 'a mut = { mutable mut : 'a; }
type 'a gbl = { gbl : 'a @@ global; }
|}]

let foo (x @ local) = x.imm
[%%expect{|
val foo : 'a imm @ local -> 'a @ local = <fun>
|}]
let foo y =
  let x = stack_ { imm = y } in
  x.imm
[%%expect{|
Line 3, characters 2-7:
3 |   x.imm
      ^^^^^
Error: This value is "local"
         because it is the field "imm" of the record at line 3, characters 2-3
         which is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let foo (x @ local) = x.mut
[%%expect{|
val foo : 'a mut @ local -> 'a = <fun>
|}]
let foo y =
  let x = stack_ { mut = y } in
  x.mut
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (x @ local) = x.gbl
[%%expect{|
val foo : 'a gbl @ local -> 'a = <fun>
|}]
let foo y =
  let x = stack_ { gbl = y } in
  x.gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]

let foo ({ imm } @ local) = imm
[%%expect{|
val foo : 'a imm @ local -> 'a @ local = <fun>
|}]
let foo y =
  let { imm } = stack_ { imm = y } in
  imm
[%%expect{|
Line 3, characters 2-5:
3 |   imm
      ^^^
Error: This value is "local"
         because it is the field "imm" of the record at line 2, characters 6-13
         which is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let foo ({ mut } @ local) = mut
[%%expect{|
val foo : 'a mut @ local -> 'a = <fun>
|}]
let foo y =
  let { mut } = stack_ { mut = y } in
  mut
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo ({ gbl } @ local) = gbl
[%%expect{|
val foo : 'a gbl @ local -> 'a = <fun>
|}]
let foo y =
  let { gbl } = stack_ { gbl = y } in
  gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]

let foo (imm @ local) =
  let _ = { imm } in
  ()
[%%expect{|
val foo : 'a @ local -> unit = <fun>
|}]
let foo () =
  let imm = stack_ (ref 5) in
  let _ = { imm } in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]
let foo (mut @ local) =
  let _ = { mut } in
  ()
[%%expect{|
Line 2, characters 12-15:
2 |   let _ = { mut } in
                ^^^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is the field "mut" (with some modality) of the record at line 2, characters 10-17.
|}]
let foo () =
  let mut = stack_ (ref 5) in
  let _ = { mut } in
  ()
[%%expect{|
Line 3, characters 12-15:
3 |   let _ = { mut } in
                ^^^
Error: This value is "local"
       but is expected to be "global"
         because it is the field "mut" (with some modality) of the record at line 3, characters 10-17.
|}]
let foo (gbl @ local) =
  let _ = { gbl } in
  ()
[%%expect{|
Line 2, characters 12-15:
2 |   let _ = { gbl } in
                ^^^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is the field "gbl" (with some modality) of the record at line 2, characters 10-17.
|}]
let foo () =
  let gbl = stack_ (ref 5) in
  let _ = { gbl } in
  ()
[%%expect{|
Line 3, characters 12-15:
3 |   let _ = { gbl } in
                ^^^
Error: This value is "local"
       but is expected to be "global"
         because it is the field "gbl" (with some modality) of the record at line 3, characters 10-17.
|}]

(* Implicit records version of the same test *)

let foo (x @ local) = x.#imm
[%%expect{|
val foo : 'a imm# @ local -> 'a @ local = <fun>
|}]
let foo y =
  let x @ local = #{ imm = y }  in
  x.#imm
[%%expect{|
Line 3, characters 2-8:
3 |   x.#imm
      ^^^^^^
Error: This value is "local"
         because it is the field "imm" of the record at line 3, characters 2-3
         which is "local".
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let foo (x @ local) = x.#mut
[%%expect{|
val foo : 'a mut# @ local -> 'a = <fun>
|}]
let foo y =
  let x @ local = #{ mut = y }  in
  x.#mut
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (x @ local) = x.#gbl
[%%expect{|
val foo : 'a gbl# @ local -> 'a = <fun>
|}]
let foo y =
  let x @ local = #{ gbl = y }  in
  x.#gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]

let foo (#{ imm } @ local) = imm
[%%expect{|
val foo : 'a imm# @ local -> 'a @ local = <fun>
|}]
let foo y =
  let #{ imm } @ local = #{ imm = y }  in
  imm
[%%expect{|
Line 3, characters 2-5:
3 |   imm
      ^^^
Error: This value is "local"
         because it is the field "imm" of the record at line 2, characters 6-14
         which is "local".
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]
let foo (#{ mut } @ local) = mut
[%%expect{|
val foo : 'a mut# @ local -> 'a = <fun>
|}]
let foo y =
  let #{ mut } @ local = #{ mut = y }  in
  mut
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (#{ gbl } @ local) = gbl
[%%expect{|
val foo : 'a gbl# @ local -> 'a = <fun>
|}]
let foo y =
  let #{ gbl } @ local = #{ gbl = y }  in
  gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]

let foo (imm @ local) =
  let _ = #{ imm } in
  ()
[%%expect{|
val foo : 'a @ local -> unit = <fun>
|}]
let foo () =
  let imm = stack_ (ref 5) in
  let _ = #{ imm } in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]
let foo (mut @ local) =
  let _ = #{ mut } in
  ()
[%%expect{|
Line 2, characters 13-16:
2 |   let _ = #{ mut } in
                 ^^^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is the field "mut" (with some modality) of the record at line 2, characters 10-18.
|}]
let foo () =
  let mut = stack_ (ref 5) in
  let _ = #{ mut } in
  ()
[%%expect{|
Line 3, characters 13-16:
3 |   let _ = #{ mut } in
                 ^^^
Error: This value is "local"
       but is expected to be "global"
         because it is the field "mut" (with some modality) of the record at line 3, characters 10-18.
|}]
let foo (gbl @ local) =
  let _ = #{ gbl } in
  ()
[%%expect{|
Line 2, characters 13-16:
2 |   let _ = #{ gbl } in
                 ^^^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is the field "gbl" (with some modality) of the record at line 2, characters 10-18.
|}]
let foo () =
  let gbl = stack_ (ref 5) in
  let _ = #{ gbl } in
  ()
[%%expect{|
Line 3, characters 13-16:
3 |   let _ = #{ gbl } in
                 ^^^
Error: This value is "local"
       but is expected to be "global"
         because it is the field "gbl" (with some modality) of the record at line 3, characters 10-18.
|}]

(* Unboxed records version of the same test *)

type 'a gbl = #{ gbl : 'a @@ global }
[%%expect{|
type 'a gbl = #{ gbl : 'a @@ global; }
|}]

let foo (x @ local) = x.#gbl
[%%expect{|
val foo : 'a gbl @ local -> 'a = <fun>
|}]
let foo y =
  let x @ local = #{ gbl = y }  in
  x.#gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (#{ gbl } @ local) = gbl
[%%expect{|
val foo : 'a gbl @ local -> 'a = <fun>
|}]
let foo y =
  let #{ gbl } @ local = #{ gbl = y }  in
  gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (gbl @ local) =
  let _ = #{ gbl } in
  ()
[%%expect{|
Line 2, characters 13-16:
2 |   let _ = #{ gbl } in
                 ^^^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is the field "gbl" (with some modality) of the record at line 2, characters 10-18.
|}]
let foo () =
  let gbl = stack_ (ref 5) in
  let _ = #{ gbl } in
  ()
[%%expect{|
Line 3, characters 13-16:
3 |   let _ = #{ gbl } in
                 ^^^
Error: This value is "local"
       but is expected to be "global"
         because it is the field "gbl" (with some modality) of the record at line 3, characters 10-18.
|}]

(* Global fields are preserved in module inclusion *)
module M : sig
  type t = { foo : string @@ global }
end = struct
  type t = { foo : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { foo : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { foo : string; } end
       is not included in
         sig type t = { foo : string @@ global; } end
       Type declarations do not match:
         type t = { foo : string; }
       is not included in
         type t = { foo : string @@ global; }
       Fields do not match:
         "foo : string;"
       is not the same as:
         "foo : string @@ global;"
       The second is global and the first is not.
|}]

module M : sig
  type t = { foo : string }
end = struct
  type t = { foo : string @@ global }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { foo : string @@ global }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { foo : string @@ global; } end
       is not included in
         sig type t = { foo : string; } end
       Type declarations do not match:
         type t = { foo : string @@ global; }
       is not included in
         type t = { foo : string; }
       Fields do not match:
         "foo : string @@ global;"
       is not the same as:
         "foo : string;"
       The second is empty and the first is aliased.
|}]

(* Unboxed records version of the same test *)

module M : sig
  type t = #{ foo : string @@ global }
end = struct
  type t = #{ foo : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ foo : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ foo : string; } end
       is not included in
         sig type t = #{ foo : string @@ global; } end
       Type declarations do not match:
         type t = #{ foo : string; }
       is not included in
         type t = #{ foo : string @@ global; }
       Fields do not match:
         "foo : string;"
       is not the same as:
         "foo : string @@ global;"
       The second is global and the first is not.
|}]

module M : sig
  type t = #{ foo : string }
end = struct
  type t = #{ foo : string @@ global }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ foo : string @@ global }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ foo : string @@ global; } end
       is not included in
         sig type t = #{ foo : string; } end
       Type declarations do not match:
         type t = #{ foo : string @@ global; }
       is not included in
         type t = #{ foo : string; }
       Fields do not match:
         "foo : string @@ global;"
       is not the same as:
         "foo : string;"
       The second is empty and the first is aliased.
|}]

(* Special handling of tuples in matches and let bindings *)
let escape : 'a -> unit = fun x -> ()

let foo (x @ local) y =
  match x, y with
  | Some _, Some b -> escape b
  | None, _ -> ()
  | pr  -> let _, _ = pr in ();;
[%%expect{|
val escape : 'a -> unit = <fun>
val foo : 'a option @ local -> 'b option -> unit = <fun>
|}]

let foo (x @ local) y =
  let pr = x, y in
  match pr with
  | Some _, Some b -> escape b
  | None, _ -> ()
  | _  -> ();;
[%%expect{|
Line 4, characters 29-30:
4 |   | Some _, Some b -> escape b
                                 ^
Error: This value is "local"
         because it is contained (via constructor "Some") in the value at line 4, characters 12-18
         which is "local"
         because it is an element of the tuple at line 3, characters 8-10
         which is "local"
         because it is allocated at line 2, characters 11-15 containing data
         which is "local" to the parent region
         because it is a tuple that contains the expression at line 2, characters 11-12
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let foo (x @ local) y =
  match x, y with
  | pr ->
    let _, b = pr in
    escape b
  | _  -> ();;
[%%expect{|
Line 5, characters 11-12:
5 |     escape b
               ^
Error: This value is "local"
         because it is an element of the tuple at line 4, characters 15-17
         which is "local"
         because it is allocated at line 2, characters 8-12 containing data
         which is "local" to the parent region
         because it is a tuple that contains the expression at line 2, characters 8-9
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let foo p (x @ local) y z =
  let (_, b) as pr =
    if p then x, y else z
  in
  let _, _ = pr in
  escape b;;
[%%expect{|
val foo : bool -> 'a @ local -> 'b -> 'a * 'b -> unit = <fun>
|}]

let foo p (x @ local) y (z @ local) =
  let _, b =
    if p then x, y else z
  in
  escape b;;
[%%expect{|
Line 5, characters 9-10:
5 |   escape b;;
             ^
Error: This value is "local" to the parent region
         because it is an element of the tuple at line 3, characters 24-25
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let foo p (x @ local) y z =
  let a, _ =
    if p then x, y else z
  in
  escape a;;
[%%expect{|
Line 5, characters 9-10:
5 |   escape a;;
             ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo p (x @ local) y z =
  let pr =
    if p then x, y else z
  in
  let _, b = pr in
  escape b;;
[%%expect{|
Line 6, characters 9-10:
6 |   escape b;;
             ^
Error: This value is "local"
         because it is an element of the tuple at line 5, characters 13-15
         which is "local"
         because it is allocated at line 3, characters 14-18 containing data
         which is "local" to the parent region
         because it is a tuple that contains the expression at line 3, characters 14-15
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

(* [as] patterns *)

let foo (x @ local) =
  match x with
  | None as y -> escape y
  | Some _ -> ()
[%%expect{|
val foo : 'a option @ local -> unit = <fun>
|}]

let foo (x @ local) =
  match x with
  | None -> ()
  | Some _ as y -> escape y
[%%expect{|
Line 4, characters 26-27:
4 |   | Some _ as y -> escape y
                              ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo (x @ local) =
  match x with
  | 0 as y -> escape y
  | _ -> ()
[%%expect{|
val foo : int @ local -> unit = <fun>
|}, Principal{|
Line 3, characters 21-22:
3 |   | 0 as y -> escape y
                         ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo (x @ local) =
  match x with
  | 'a'..'e' as y -> escape y
  | _ -> ()
[%%expect{|
val foo : char @ local -> unit = <fun>
|}, Principal{|
Line 3, characters 28-29:
3 |   | 'a'..'e' as y -> escape y
                                ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo (x @ local) =
  match x with
  | 1.1 as y -> escape y
  | _ -> ()
[%%expect{|
Line 3, characters 23-24:
3 |   | 1.1 as y -> escape y
                           ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo (x @ local) =
  match x with
  | `Foo as y -> escape y
  | _ -> ()
[%%expect{|
val foo : [> `Foo ] @ local -> unit = <fun>
|}]

let foo (x @ local) =
  match x with
  | (`Foo _) as y -> escape y
  | _ -> ()
[%%expect{|
Line 3, characters 28-29:
3 |   | (`Foo _) as y -> escape y
                                ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo (x @ local) =
  match x with
  | (None | Some _) as y -> escape y
[%%expect{|
Line 3, characters 35-36:
3 |   | (None | Some _) as y -> escape y
                                       ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let foo (x @ local) =
  match x with
  | (Some _|None) as y -> escape y
[%%expect{|
Line 3, characters 33-34:
3 |   | (Some _|None) as y -> escape y
                                     ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

type foo = [`Foo | `Bar]

let foo (x @ local) =
  match x with
  | #foo as y -> escape y
[%%expect{|
type foo = [ `Bar | `Foo ]
val foo : [< foo ] @ local -> unit = <fun>
|}]

type foo = [`Foo | `Bar of int]

let foo (x @ local) =
  match x with
  | #foo as y -> escape y
[%%expect{|
type foo = [ `Bar of int | `Foo ]
Line 5, characters 24-25:
5 |   | #foo as y -> escape y
                            ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* Primitives *)

(* Poly-moded eta expansion *)
module Heap32 : sig val add : int32 -> int32 -> int32 end = Int32
module Heap32E : sig external add : int32 -> int32 -> int32 = "%int32_add" end = Int32
module Local32 : sig val add : int32 @ local -> int32 @ local -> int32 @ local end = Int32
module Local32E : sig external add : int32 @ local -> int32 @ local -> int32 @ local = "%int32_add" end = Int32
[%%expect{|
module Heap32 : sig val add : int32 -> int32 -> int32 end
module Heap32E :
  sig external add : int32 -> int32 -> int32 = "%int32_add" end
module Local32 :
  sig val add : int32 @ local -> int32 @ local -> int32 @ local end
module Local32E :
  sig
    external add : int32 @ local -> int32 @ local -> int32 @ local
      = "%int32_add"
  end
|}]
module Bad32 : sig val add : int32 @ local -> int32 @ local -> int32 end =
  struct let add = Int32.add end
[%%expect{|
Line 2, characters 2-32:
2 |   struct let add = Int32.add end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig
           val add :
             int32 @ local forkable unyielding ->
             int32 @ local forkable unyielding -> int32 @ local forkable
             unyielding
         end
       is not included in
         sig val add : int32 @ local -> int32 @ local -> int32 end
       Values do not match:
         val add :
           int32 @ local forkable unyielding ->
           int32 @ local forkable unyielding -> int32 @ local forkable
           unyielding
       is not included in
         val add : int32 @ local -> int32 @ local -> int32
       The type
         "int32 @ local forkable unyielding ->
         int32 @ local forkable unyielding -> int32 @ local forkable
         unyielding"
       is not compatible with the type
         "int32 @ local -> int32 @ local -> int32"
       Type
         "int32 @ local forkable unyielding -> int32 @ local forkable
         unyielding"
       is not compatible with type "int32 @ local -> int32"
       The return mode was expected to be "global" but is "local"
|}]
module Opt32 : sig external add : (int32[@local_opt]) -> (int32[@local_opt]) -> (int32[@local_opt]) = "%int32_add" end = Int32
module Bad32_2 : sig val add : int32 @ local -> int32 @ local -> int32 end =
  Opt32
[%%expect{|
module Opt32 :
  sig
    external add :
      (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
      = "%int32_add"
  end
Line 3, characters 2-7:
3 |   Opt32
      ^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig
           external add :
             (int32 [@local_opt]) ->
             (int32 [@local_opt]) -> (int32 [@local_opt]) = "%int32_add"
         end
       is not included in
         sig val add : int32 @ local -> int32 @ local -> int32 end
       Values do not match:
         external add :
           (int32 [@local_opt]) ->
           (int32 [@local_opt]) -> (int32 [@local_opt]) = "%int32_add"
       is not included in
         val add : int32 @ local -> int32 @ local -> int32
       The type
         "int32 @ local forkable unyielding ->
         int32 @ local forkable unyielding -> int32 @ local forkable
         unyielding"
       is not compatible with the type
         "int32 @ local -> int32 @ local -> int32"
       Type
         "int32 @ local forkable unyielding -> int32 @ local forkable
         unyielding"
       is not compatible with type "int32 @ local -> int32"
       The return mode was expected to be "global" but is "local"
|}]

module Contravariant_instantiation : sig
  external to_int_trunc : Int64.t -> int = "%int64_to_int"
end = struct
  external to_int_trunc : (Int64.t [@local_opt]) -> int = "%int64_to_int"
end
[%%expect{|
module Contravariant_instantiation :
  sig external to_int_trunc : Int64.t -> int = "%int64_to_int" end
|}]

(* Return modes *)
let zx : int ref -> (int -> unit) = (:=)
let zz : (int ref) @ local -> int -> unit = (:=)
let zy : (int ref) @ local -> (int -> unit) = (:=)
[%%expect{|
val zx : int ref -> int -> unit = <fun>
val zz : int ref @ local -> int -> unit = <fun>
Line 3, characters 46-50:
3 | let zy : (int ref) @ local -> (int -> unit) = (:=)
                                                  ^^^^
Error: The value "(:=)" has type "'a ref @ local -> 'a -> unit"
       but an expression was expected of type
         "int ref @ local -> (int -> unit)"
|}]

let int32 (x @ local) (y @ local) = exclave_
  Int32.(div (logxor (mul x y) (sub x y)) (shift_right y 10))
let int64 (x @ local) (y @ local) = exclave_
  Int64.(div (logxor (mul x y) (sub x y)) (shift_right y 10))
let nativeint (x @ local) (y @ local) = exclave_
  Nativeint.(div (logxor (mul x y) (sub x y)) (shift_right y 10))
let float (x @ local) (y @ local) = exclave_
  (x +. y *. x -. 42.)
[%%expect{|
val int32 : int32 @ local -> int32 @ local -> int32 @ local = <fun>
val int64 : int64 @ local -> int64 @ local -> int64 @ local = <fun>
val nativeint : nativeint @ local -> nativeint @ local -> nativeint @ local =
  <fun>
val float : float @ local -> float @ local -> float @ local = <fun>
|}]

let etapair (x @ local) = exclave_ (fst x, snd x)
[%%expect{|
val etapair : 'a * 'b @ local -> 'a * 'b @ local = <fun>
|}]

(* Arity checking on primitives *)
external goodadd : int32 -> int32 -> int32 = "%int32_add"
[%%expect{|
external goodadd : int32 -> int32 -> int32 = "%int32_add"
|}]
external badadd : int32 -> (int32 -> int32) = "%int32_add"
[%%expect{|
Line 1, characters 0-58:
1 | external badadd : int32 -> (int32 -> int32) = "%int32_add"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong arity for builtin primitive "%int32_add"
|}]

let compare (x @ local) (y @ local) =
  [x = y; x <> y; x < y; x > y; x <= y; x >= y; compare x y = 0; x == y; x != y]
[%%expect{|
val compare : 'a @ local -> 'a @ local -> bool list = <fun>
|}]

(* integer primitives accept local args *)
let intf (x @ local) = x |> Int.succ |> Int.add 42 |> pred |> (/) 100 |> (+) 1
[%%expect{|
val intf : int @ local -> int = <fun>
|}]

(* primitives don't count as tail calls, so you can pass them locals *)
let primloc x = let y @ local = Int32.add x 1l in Int32.to_int y
[%%expect{|
val primloc : int32 -> int = <fun>
|}]

(* (&&) and (||) tail call on the right *)
let testbool1 f = let r @ local = ref 42 in (f r || false) && true

let testbool2 f = let r @ local = ref 42 in true && (false || f r)
[%%expect{|
val testbool1 : (int ref @ local -> bool) -> bool = <fun>
Line 3, characters 64-65:
3 | let testbool2 f = let r @ local = ref 42 in true && (false || f r)
                                                                    ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is an argument in a tail call.
|}]

(* boolean operator when at tail of function makes the function local-returning
   if its RHS is local-returning *)
let foo () = exclave_ let _x @ local = "hello" in true
let testboo3 () =  true && (foo ())
[%%expect{|
val foo : unit -> bool @ local = <fun>
Line 2, characters 27-35:
2 | let testboo3 () =  true && (foo ())
                               ^^^^^^^^
Error: This local-returning application is in a tail position that is not
       enclosed in an exclave_ expression.
|}]
(* since the tailcall calls a local-returning function, it will only type-check
   under an exclave_ *)
let testboo3 () = exclave_ true && (foo ())
[%%expect{|
val testboo3 : unit -> bool @ local = <fun>
|}]

(* Test from Nathanaëlle Courant.
  User can define strange AND. Supposedly [strange_and] will look at its first
  arguments, and returns [None] or tailcall on second argument accordingly.
  The second argument should not cross modes in generall. *)
external strange_and : bool -> 'a option -> 'a option = "%sequand"

let testboo4 () =
  let x @ local = Some "hello" in
  strange_and true x
[%%expect{|
external strange_and : bool -> 'a option -> 'a option = "%sequand"
Line 5, characters 19-20:
5 |   strange_and true x
                       ^
Error: This value is "local" but is expected to be "global".
|}]

(* mode-crossing using unary + *)
let promote (x @ local) = +x
[%%expect{|
val promote : int @ local -> int = <fun>
|}]

(* Or-patterns *)
let foo (x @ local) y =
  match y, x with
  | Some z, None | None, Some z -> z
  | None, None | Some _, Some _ -> assert false
[%%expect{|
val foo : 'a option @ local -> 'a option -> 'a @ local = <fun>
|}]

let foo (x @ local) y =
  match x, y with
  | Some z, None | None, Some z -> z
  | None, None | Some _, Some _ -> assert false
[%%expect{|
val foo : 'a option @ local -> 'a option -> 'a @ local = <fun>
|}]

module M = struct
  let (Some z, _, _) | (None, Some z, _)
      | (None, None, z) = (Some (ref 0), (stack_ (Some (ref 0))), (ref 0))
end
[%%expect{|
Line 2, characters 12-13:
2 |   let (Some z, _, _) | (None, Some z, _)
                ^
Error: The expression is "local"
         because it is contained (via constructor "Some") in the value at line 2, characters 30-36
         which is "local" because it is "stack_"-allocated.
       However, the expression highlighted is expected to be "global"
         because it is the value "z" in the structure at lines 2-3, characters 2-74
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]

module M = struct
  let (Some z, _, _) | (None, Some z, _)
      | (None, None, z) = ((stack_ (Some (ref 0))), (Some (ref 0)), (ref 0))
end
[%%expect{|
Line 2, characters 12-13:
2 |   let (Some z, _, _) | (None, Some z, _)
                ^
Error: The expression is "local"
         because it is contained (via constructor "Some") in the value at line 2, characters 7-13
         which is "local" because it is "stack_"-allocated.
       However, the expression highlighted is expected to be "global"
         because it is the value "z" in the structure at lines 2-3, characters 2-76
         which is expected to be "global"
         because modules always need to be allocated on the heap.
|}]

(* Example of backtracking after mode error *)
let f g n =
  let a = stack_ [n+1] in
  let () = g a in
  ()
let z : (int list -> unit) -> int -> unit = f
[%%expect{|
val f : (int list @ local -> unit) -> int -> unit = <fun>
Line 5, characters 44-45:
5 | let z : (int list -> unit) -> int -> unit = f
                                                ^
Error: The value "f" has type "(int list @ local -> unit) -> int -> unit"
       but an expression was expected of type
         "(int list -> unit) -> int -> unit"
       Type "int list @ local -> unit" is not compatible with type
         "int list -> unit"
|}]

module M = struct
  let f g n =
    let a = stack_ [n+1] in
    let () = g a in
    ()
  let z : (int list -> unit) -> int -> unit = f
end
[%%expect{|
Line 6, characters 46-47:
6 |   let z : (int list -> unit) -> int -> unit = f
                                                  ^
Error: The value "f" has type "(int list @ local -> unit) -> int -> unit"
       but an expression was expected of type
         "(int list -> unit) -> int -> unit"
       Type "int list @ local -> unit" is not compatible with type
         "int list -> unit"
|}]

(* Subtyping *)

let foo f = (f : string @ local -> float :> string -> float)
[%%expect{|
val foo : (string @ local -> float) -> string -> float = <fun>
|}]

let foo f = (f : string -> float :> string -> float @ local)
[%%expect{|
val foo : (string -> float) -> string -> float @ local = <fun>
|}]

let foo f = (f : string -> float @ local :> string -> float)
[%%expect{|
Line 1, characters 12-60:
1 | let foo f = (f : string -> float @ local :> string -> float)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "string -> float @ local" is not a subtype of "string -> float"
|}]

let foo f = (f : string -> float :> string @ local -> float @ local)
[%%expect{|
Line 1, characters 12-68:
1 | let foo f = (f : string -> float :> string @ local -> float @ local)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "string -> float" is not a subtype of
         "string @ local -> float @ local"
|}]

let foo f = ignore (f :> string -> float); ()
[%%expect{|
val foo : (string -> float) -> unit = <fun>
|}]

let global_to_global_to_global (f : float -> string) = f 42.0

let foo f =
  ignore (f :> (float @ local -> string) -> string);
  [f; global_to_global_to_global]
[%%expect{|
val global_to_global_to_global : (float -> string) -> string = <fun>
val foo : ((float -> string) -> string) -> ((float -> string) -> string) list =
  <fun>
|}]

let local_to_global_to_global (f : float @ local -> string) = f 42.0

let foo f =
  ignore (f :> (float -> string) -> string);
  [f; local_to_global_to_global]
[%%expect{|
val local_to_global_to_global : (float @ local -> string) -> string = <fun>
Line 5, characters 6-31:
5 |   [f; local_to_global_to_global]
          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "local_to_global_to_global" has type
         "(float @ local -> string) -> string"
       but an expression was expected of type "(float -> string) -> string"
       Type "float @ local -> string" is not compatible with type
         "float -> string"
|}]

(* Submoding during module inclusion *)

module F (X : sig val foo : float @ local -> string end) : sig
  val foo : float -> string
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : float @ local -> string end) ->
    sig val foo : float -> string end
|}]

module F (X : sig val foo : float -> string end) : sig
  val foo : float -> string @ local
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : float -> string end) ->
    sig val foo : float -> string @ local end
|}]

module F (X : sig val foo : float -> string end) : sig
  val foo : float @ local -> string
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : float -> string end
       is not included in
         sig val foo : float @ local -> string end
       Values do not match:
         val foo : float -> string
       is not included in
         val foo : float @ local -> string
       The type "float -> string" is not compatible with the type
         "float @ local -> string"
       The argument mode was expected to be "global"
       because it crosses with something but is "local"
|}]

module F (X : sig val foo : float -> string @ local end) : sig
  val foo : float -> string
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : float -> string @ local end
       is not included in
         sig val foo : float -> string end
       Values do not match:
         val foo : float -> string @ local
       is not included in
         val foo : float -> string
       The type "float -> string @ local" is not compatible with the type
         "float -> string"
       The return mode was expected to be "global" but is "local"
|}]

module F (X : sig val foo : float @ local -> float -> string end) : sig
  val foo : float -> float -> string
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : float @ local -> float -> string end
       is not included in
         sig val foo : float -> float -> string end
       Values do not match:
         val foo : float @ local -> float -> string
       is not included in
         val foo : float -> float -> string
       The type "float @ local -> float -> string"
       is not compatible with the type "float -> float -> string"
       The return mode was expected to be "global"
       because it crosses with something but is "local"
|}]

module F (X : sig val foo : float @ local -> float -> string end) : sig
  val foo : float -> (float -> string) @ local
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : float @ local -> float -> string end) ->
    sig val foo : float -> (float -> string) @ local end
|}]

module F (X : sig val foo : float -> float -> string end) : sig
  val foo : float -> (float -> string) @ local
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : float -> float -> string end) ->
    sig val foo : float -> (float -> string) @ local end
|}]

type 'a inv = Inv of ('a -> 'a)
type 'a co = Co of 'a
type 'a contra = Contra of ('a -> int)
type 'a bi = Bi

module F (X : sig val foo : (float -> string) inv end) : sig
  val foo : (float -> string @ local) inv
end = X;;
[%%expect{|
type 'a inv = Inv of ('a -> 'a)
type 'a co = Co of 'a
type 'a contra = Contra of ('a -> int)
type 'a bi = Bi
Line 8, characters 6-7:
8 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : (float -> string) inv end
       is not included in
         sig val foo : (float -> string @ local) inv end
       Values do not match:
         val foo : (float -> string) inv
       is not included in
         val foo : (float -> string @ local) inv
       The type "(float -> string) inv" is not compatible with the type
         "(float -> string @ local) inv"
       Type "float -> string" is not compatible with type
         "float -> string @ local"
       The return mode was expected to be "global" but is "local"
|}]

module F (X : sig val foo : (float -> string) co end) : sig
  val foo : (float -> string @ local) co
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : (float -> string) co end) ->
    sig val foo : (float -> string @ local) co end
|}]

module F (X : sig val foo : (float -> string) contra end) : sig
  val foo : (float -> string @ local) contra
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : (float -> string) contra end
       is not included in
         sig val foo : (float -> string @ local) contra end
       Values do not match:
         val foo : (float -> string) contra
       is not included in
         val foo : (float -> string @ local) contra
       The type "(float -> string) contra" is not compatible with the type
         "(float -> string @ local) contra"
       Type "float -> string" is not compatible with type
         "float -> string @ local"
       The return mode was expected to be "global" but is "local"
|}]

module F (X : sig val foo : (float -> string) bi end) : sig
  val foo : (float -> string @ local) bi
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : (float -> string) bi end) ->
    sig val foo : (float -> string @ local) bi end
|}]

module F (X : sig val foo : (float -> string @ local) inv end) : sig
  val foo : (float -> string) inv
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : (float -> string @ local) inv end
       is not included in
         sig val foo : (float -> string) inv end
       Values do not match:
         val foo : (float -> string @ local) inv
       is not included in
         val foo : (float -> string) inv
       The type "(float -> string @ local) inv" is not compatible with the type
         "(float -> string) inv"
       Type "float -> string @ local" is not compatible with type
         "float -> string"
       The return mode was expected to be "global" but is "local"
|}]

module F (X : sig val foo : (float -> string @ local) co end) : sig
  val foo : (float -> string) co
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : (float -> string @ local) co end
       is not included in
         sig val foo : (float -> string) co end
       Values do not match:
         val foo : (float -> string @ local) co
       is not included in
         val foo : (float -> string) co
       The type "(float -> string @ local) co" is not compatible with the type
         "(float -> string) co"
       Type "float -> string @ local" is not compatible with type
         "float -> string"
       The return mode was expected to be "global" but is "local"
|}]

module F (X : sig val foo : (float -> string @ local) contra end) : sig
  val foo : (float -> string) contra
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : (float -> string @ local) contra end) ->
    sig val foo : (float -> string) contra end
|}]

module F (X : sig val foo : (float -> string @ local) bi end) : sig
  val foo : (float -> string) bi
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : (float -> string @ local) bi end) ->
    sig val foo : (float -> string) bi end
|}]


(*
 * constructor arguments global
 *)

(* Global argument are preserved in module inclusion *)
module M : sig
  type t = Bar of int * string @@ global
end = struct
  type t = Bar of int * string
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Bar of int * string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Bar of int * string end
       is not included in
         sig type t = Bar of int * string @@ global end
       Type declarations do not match:
         type t = Bar of int * string
       is not included in
         type t = Bar of int * string @@ global
       Constructors do not match:
         "Bar of int * string"
       is not the same as:
         "Bar of int * string @@ global"
       Modality mismatch at argument position 2:
       The second is global and the first is not.
|}]


module M : sig
  type t = Bar of int * string
end = struct
  type t = Bar of int * string @@ global
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Bar of int * string @@ global
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Bar of int * string @@ global end
       is not included in
         sig type t = Bar of int * string end
       Type declarations do not match:
         type t = Bar of int * string @@ global
       is not included in
         type t = Bar of int * string
       Constructors do not match:
         "Bar of int * string @@ global"
       is not the same as:
         "Bar of int * string"
       Modality mismatch at argument position 2:
       The second is empty and the first is aliased.
|}]

(* global_ binds closer than star *)
type gfoo = GFoo of string @@ global * string
[%%expect{|
type gfoo = GFoo of string @@ global * string
|}]

type gfoo' = GFoo' : string @@ global -> gfoo'
[%%expect{|
type gfoo' = GFoo' : string @@ global -> gfoo'
|}]

(* TESTING OF GLOBAL_ *)

(* global arguments must be global when constructing
   cannot be regional or local
*)
let f (s : string @ local) =
  GFoo (s, "bar")
[%%expect{|
Line 2, characters 8-9:
2 |   GFoo (s, "bar")
            ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "GFoo") (with some modality) in the value at line 2, characters 2-17.
|}]

let f =
  let s @ local = "foo" in
  GFoo (s, "bar")
[%%expect{|
Line 3, characters 8-9:
3 |   GFoo (s, "bar")
            ^
Error: This value is "local"
       but is expected to be "global"
         because it is contained (via constructor "GFoo") in the value at line 3, characters 2-17
         which is expected to be "global".
|}]

(* s' extracted from x as global *)
(* despite x is local or regional*)
let f (s : string) =
  let x @ local = GFoo (s, "bar") in
  match x with
  | GFoo (s', _) -> ref s'

[%%expect{|
val f : string -> string ref = <fun>
|}]

let f (x : gfoo @ local) =
  match x with
  | GFoo (s', _) -> ref s'

[%%expect{|
val f : gfoo @ local -> string ref = <fun>
|}]

(* the argument not marked global remains contingent on construction  *)
(* local gives local *)
let f (s : string) =
  let x @ local = GFoo ("bar", s) in
  match x with
  | GFoo (_, s') -> s'

[%%expect{|
Line 4, characters 20-22:
4 |   | GFoo (_, s') -> s'
                        ^^
Error: This value is "local"
         because it is contained (via constructor "GFoo") in the value at line 4, characters 4-16
         which is "local".
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* and regional gives regional *)
let f (x : gfoo @ local) =
  match x with
  | GFoo (_, s') -> ref s'

[%%expect{|
Line 3, characters 24-26:
3 |   | GFoo (_, s') -> ref s'
                            ^^
Error: This value is "local" to the parent region
         because it is contained (via constructor "GFoo") in the value at line 3, characters 4-16
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

(* Test of array.*)

(* Immutable arrays are like tuples or normal record: local array contains local
elements, both at construction and at projection; global array contains global
elements. *)

(* constructing global iarray from local elements is rejected *)
let f (x : string @ local) = ref [: x; "foo" :]
[%%expect{|
Line 1, characters 36-37:
1 | let f (x : string @ local) = ref [: x; "foo" :]
                                        ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is an element of the array at line 1, characters 33-47
         which is expected to be "global".
|}]

(* constructing local iarray from local elements is fine *)
let f (x : string @ local) = exclave_ [:x; "foo":]
[%%expect{|
val f : string @ local -> string iarray @ local = <fun>
|}]

(* constructing global iarray from global elements is fine *)
let f (x : string) = ref [:x; "foo":]
[%%expect{|
val f : string -> string iarray ref = <fun>
|}]

(* projecting out of local array gives local elements *)
let f (a : string iarray @ local) =
  match a with
  | [: x; _ :] -> ref x
  | _ -> ref "foo"
[%%expect{|
Line 3, characters 22-23:
3 |   | [: x; _ :] -> ref x
                          ^
Error: This value is "local" to the parent region
         because it is an element of the array at line 3, characters 4-14
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

(* a test that was passing type check *)
let unsafe_globalize (s : string @ local) : string =
  match stack_ [:s:] with
  | [:s':] -> s'
  | _ -> assert false
[%%expect{|
Line 3, characters 14-16:
3 |   | [:s':] -> s'
                  ^^
Error: This value is "local"
         because it is an element of the array at line 3, characters 4-10
         which is "local"
         because it is allocated at line 2, characters 15-20 containing data
         which is "local" to the parent region
         because it is an array that contains the expression at line 2, characters 17-18
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let f (a : string iarray @ local) =
  match a with
  | [: x; _ :] -> x
  | _ -> "foo"
[%%expect{|
val f : string iarray @ local -> string @ local = <fun>
|}]

(* projecting out of global iarray gives global elements *)
let f (a : string iarray) =
  match a with
  | [: x :] -> ref x
  | _ -> ref "foo"
[%%expect{|
val f : string iarray -> string ref = <fun>
|}]

(* Mutable array, like references, is dangerous. They must contain global
    elements regardless of the array's mode. *)

(* constructing local array from local elements is rejected *)
let f (x : string @ local) = exclave_ [| x |]
[%%expect{|
Line 1, characters 41-42:
1 | let f (x : string @ local) = exclave_ [| x |]
                                             ^
Error: This value is "local"
       but is expected to be "global"
         because it is an element (with some modality) of the array at line 1, characters 38-45.
|}]

(* constructing local array from global elements is allowed *)
let f (x : string) = exclave_ [| x |]
[%%expect{|
val f : string -> string array @ local = <fun>
|}]

(* projecting out of local array gives global elements *)
let f (a : string array @ local) =
  match a with
  | [| x |] -> ref x
  | _ -> ref "foo"
[%%expect{|
val f : string array @ local -> string ref = <fun>
|}]

(* reported internal to Jane Street as TANDC-1742 *)

module M = struct
  let fold_until :
    'a list -> init:'accum ->
    f:('accum -> 'a -> ('accum, 'final) Either.t) @ local ->
    finish: ('accum -> 'final) @ local ->
    'final =
    fun _ -> assert false

  (* this led to a poor error message about a value that escapes its region,
     but really it's just under-applied *)
  let f () = fold_until [] ~init:0 ~f:(fun _ _ -> Right ())
end

[%%expect {|
Line 11, characters 13-59:
11 |   let f () = fold_until [] ~init:0 ~f:(fun _ _ -> Right ())
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
Hint: This is a partial application
      Adding 1 more argument will make the value non-local
|}]

(* Regression test for printing of [local_] *)

module M : sig
  val f : string -> string -> string @ local
end = struct
  let g x y = exclave_ "foo"
  let f x = exclave_ g x
end;;
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let g x y = exclave_ "foo"
5 |   let f x = exclave_ g x
6 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           val g : 'a -> 'b -> string @ local
           val f : 'a -> ('b -> string @ local) @ local forkable unyielding
         end
       is not included in
         sig val f : string -> string -> string @ local end
       Values do not match:
         val f : 'a -> ('b -> string @ local) @ local forkable unyielding
       is not included in
         val f : string -> string -> string @ local
       The type
         "string -> (string -> string @ local) @ local forkable unyielding"
       is not compatible with the type "string -> string -> string @ local"
       The return mode was expected to be "global"
       because it crosses with something but is "local"
|}]

(* Escaping uncurried functions *)

(* Valid; [local_ string -> string -> string] is [local_ string -> local_ (string -> string)] *)
let f () = ((fun x y -> "") : (string @ local -> string -> string));;
[%%expect{|
val f : unit -> string @ local -> string -> string = <fun>
|}];;

(* Illegal: the return mode on (string -> string) is global. *)
let f () = ((fun x y -> "") : (string @ local -> (string -> string)));;
[%%expect{|
Line 1, characters 12-27:
1 | let f () = ((fun x y -> "") : (string @ local -> (string -> string)));;
                ^^^^^^^^^^^^^^^
Error: This function or one of its parameters escape their region
       when it is partially applied.
|}];;

let f () = ((fun x -> function | y -> "") : (string @ local -> (string -> string)));;
[%%expect{|
Line 1, characters 12-41:
1 | let f () = ((fun x -> function | y -> "") : (string @ local -> (string -> string)));;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function or one of its parameters escape their region
       when it is partially applied.
|}];;

(* ok if curried *)
let f () = ((fun x -> (fun y -> "") [@extension.curry])
            : (string @ local -> (string -> string)));;
[%%expect{|
val f : unit -> string @ local -> (string -> string) = <fun>
|}];;

(* mode crossing - the inner closure is [global] despite closing over [local_
int] *)
let f () = ((fun x y -> x + y) : (int @ local -> (int -> int)));;
[%%expect{|
val f : unit -> int @ local -> (int -> int) = <fun>
|}];;

(* Illegal: the expected mode is global *)
let f () = exclave_ ((fun x y -> x + y) : (_ -> _));;
[%%expect{|
Line 1, characters 21-39:
1 | let f () = exclave_ ((fun x y -> x + y) : (_ -> _));;
                         ^^^^^^^^^^^^^^^^^^
Error: This function or one of its parameters escape their region
       when it is partially applied.
|}];;

let f () = exclave_ ((fun x -> function | 0 -> x | y -> x + y) : (_ -> _));;
[%%expect{|
Line 1, characters 21-62:
1 | let f () = exclave_ ((fun x -> function | 0 -> x | y -> x + y) : (_ -> _));;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function or one of its parameters escape their region
       when it is partially applied.
|}];;

(* For nested functions, inner functions are not constrained *)
let f () = ((fun x -> fun y -> "") : (string @ local -> (string -> string)));;
[%%expect{|
val f : unit -> string @ local -> (string -> string) = <fun>
|}];;

let f () = exclave_ ((fun x -> fun y -> x + y) : (_ -> _));;
[%%expect{|
val f : unit -> (int -> (int -> int)) @ local = <fun>
|}];;

(* ok if curried *)
let f () = exclave_ ((fun x -> (fun y -> x + y) [@extension.curry]) : (_ -> _));;
[%%expect{|
val f : unit -> (int -> (int -> int)) @ local = <fun>
|}];;

(* Type annotations on a [local_] binding are interpreted in a local context,
 * so [int -> int -> int] is secretly [int -> local_ (int -> int)]. Contrast
 * with the below type error where `local_` is omitted on the binding.
 *)
let foo () =
  let (_bar1 @ local) : int -> int -> int = stack_ (fun x y -> x + y) in
  let (_bar2 @ local) z : int -> int -> int = exclave_ (fun x y -> x + y + z) in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}];;

let foo () =
  let _bar : int -> int -> int = stack_ (fun x y -> x + y) in
  ()
[%%expect{|
Line 2, characters 40-58:
2 |   let _bar : int -> int -> int = stack_ (fun x y -> x + y) in
                                            ^^^^^^^^^^^^^^^^^^
Error: The allocation is "local" because it is "stack_"-allocated.
       However, the allocation highlighted is expected to be "global".
|}];;


type f = string @ local local -> string
[%%expect{|
Line 1, characters 24-29:
1 | type f = string @ local local -> string
                            ^^^^^
Error: The locality axis has already been specified.
|}]

let foo () =
  let _x @ local local = "hello" in
  ()
[%%expect{|
Line 2, characters 17-22:
2 |   let _x @ local local = "hello" in
                     ^^^^^
Error: The locality axis has already been specified.
|}]

let foo (_ @ local local) = ()
[%%expect{|
Line 1, characters 19-24:
1 | let foo (_ @ local local) = ()
                       ^^^^^
Error: The locality axis has already been specified.
|}]

(* type-directed disambiguation *)

module M = struct
  type t = M_constructor
end

let foo (_ : M.t @ local) = ();;
let foo_f (_ : (M.t -> unit) @ local) = ();;
[%%expect{|
module M : sig type t = M_constructor end
val foo : M.t @ local -> unit = <fun>
val foo_f : (M.t -> unit) @ local -> unit = <fun>
|}]

let () = foo M_constructor
[%%expect{|
|}]

let () = foo_f (fun M_constructor -> ())
[%%expect{|
|}]

let () = foo (M_constructor : @ local)
[%%expect{|
|}]

let () = foo_f (stack_ (fun M_constructor -> ()))
[%%expect{|
|}]

let _ret () : M.t -> unit = (fun M_constructor -> ())
[%%expect{|
val _ret : unit -> M.t -> unit = <fun>
|}]

let _ret () : M.t -> unit = exclave_ (fun M_constructor -> ())
[%%expect{|
val _ret : unit -> (M.t -> unit) @ local = <fun>
|}]

let _ret () : M.t -> unit = exclave_ (fun M_constructor -> ())
[%%expect{|
val _ret : unit -> (M.t -> unit) @ local = <fun>
|}]

type r = {x : string @@ global; y : string}

let foo () =
  let y @ local = "world" in
  let r @ local = {x = "hello"; y} in
  (* Only using r.x, which is global. So the whole return is global and OK. *)
  {r with y = "foo!" }
[%%expect{|
type r = { x : string @@ global; y : string; }
val foo : unit -> r = <fun>
|}]
