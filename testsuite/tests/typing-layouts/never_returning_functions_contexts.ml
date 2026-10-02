(* TEST
 flags += "-keywords 5.3";
 expect;
*)

(* Only a function none of whose return positions can return may have a result
   of a non-representable layout. Everywhere else, an expression of such a type
   must still be rejected.

   To make every check fail loudly rather than default a type variable to
   [value], the result type is a locally abstract type [a : any]:

   - Section 1 lists each expression form the classification looks through,
     with a variant that cannot return (accepted) and variants that can
     (rejected).
   - Section 2 puts an expression of type [a] in every context that consumes
     its value, inside a function that otherwise cannot return. Every one of
     them must be rejected.
   - Section 3 matches on GADTs and empty types in such functions.
   - Section 4 checks how such functions are generalised, stored and exported.
*)

external raise_any : ('a : any). exn -> 'a = "%raise"
external raise_with_backtrace_any
  : ('a : any). exn -> Printexc.raw_backtrace -> 'a = "%raise_with_backtrace"
type t_any : any
type void = |
let never () = assert false

(* [get w] returns a function whose result type has layout [any], but which
   does return. *)
type ('a : any) w = W : int w
let get : type (a : any). a w -> int -> a = fun w -> match w with W -> succ
[%%expect{|
external raise_any : ('a : any). exn -> 'a = "%raise"
external raise_with_backtrace_any :
  ('a : any). exn -> Printexc.raw_backtrace -> 'a = "%raise_with_backtrace"
type t_any : any
type void = |
val never : ('a : any). unit -> 'a = <fun>
type ('a : any) w = W : int w
val get : ('a : any). 'a w -> int -> 'a = <fun>
|}]

(*************************************************)
(* Section 1: which bodies count as never returning *)

(* Leaves. *)

let leaf_raise (type a : any) () : a = raise_any Exit
let leaf_raise_with_backtrace (type a : any) bt : a =
  raise_with_backtrace_any Exit bt
let leaf_assert_false (type a : any) () : a = assert false
let leaf_assert_false_constrained (type a : any) () : a =
  assert (false : bool)
let leaf_constraint (type a : any) () : a = (raise_any Exit : a)
let leaf_attribute (type a : any) () : a = (raise_any Exit [@attr])
[%%expect{|
val leaf_raise : ('a : any). unit -> 'a = <fun>
val leaf_raise_with_backtrace : ('a : any). Printexc.raw_backtrace -> 'a =
  <fun>
val leaf_assert_false : ('a : any). unit -> 'a = <fun>
val leaf_assert_false_constrained : ('a : any). unit -> 'a = <fun>
val leaf_constraint : ('a : any). unit -> 'a = <fun>
val leaf_attribute : ('a : any). unit -> 'a = <fun>
|}]

let leaf_call (type a : any) () : a = never ()
[%%expect{|
Line 1, characters 14-46:
1 | let leaf_call (type a : any) () : a = never ()
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let leaf_raise_stdlib (type a : any) () : a = raise Exit
[%%expect{|
Line 1, characters 46-56:
1 | let leaf_raise_stdlib (type a : any) () : a = raise Exit
                                                  ^^^^^^^^^^
Error: This expression has type "('a : value_or_null)"
       but an expression was expected of type "a"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout.
|}]

let leaf_failwith (type a : any) () : a = failwith "no"
[%%expect{|
Line 1, characters 42-55:
1 | let leaf_failwith (type a : any) () : a = failwith "no"
                                              ^^^^^^^^^^^^^
Error: This expression has type "('a : value_or_null)"
       but an expression was expected of type "a"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout.
|}]

let leaf_alias (type a : any) () : a =
  let r = raise_any in
  r Exit
[%%expect{|
Lines 1-3, characters 15-8:
1 | ...............(type a : any) () : a =
2 |   let r = raise_any in
3 |   r Exit
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let leaf_eta (type a : any) () : a = (fun e -> raise_any e) Exit
[%%expect{|
Line 1, characters 13-64:
1 | let leaf_eta (type a : any) () : a = (fun e -> raise_any e) Exit
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let leaf_partial_raise (type a : any) () : Printexc.raw_backtrace -> a =
  raise_with_backtrace_any Exit
[%%expect{|
val leaf_partial_raise : ('a : any). unit -> Printexc.raw_backtrace -> 'a =
  <fun>
|}]

let leaf_get (type a : any) (w : a w) : a = get w 3
[%%expect{|
Line 1, characters 13-51:
1 | let leaf_get (type a : any) (w : a w) : a = get w 3
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let rec leaf_recursive (type a : any) () : a = leaf_recursive ()
[%%expect{|
Line 1, characters 47-64:
1 | let rec leaf_recursive (type a : any) () : a = leaf_recursive ()
                                                   ^^^^^^^^^^^^^^^^^
Error: This expression has type "('a : '_representable_layout_1)"
       but an expression was expected of type "a"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let leaf_loop (type a : any) () : a = while true do () done
[%%expect{|
Line 1, characters 14-59:
1 | let leaf_loop (type a : any) () : a = while true do () done
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

(* An over-applied raise is not a leaf: the extra application has a
   representable result. *)

let leaf_over_applied (type a : any) () : a = raise_any Exit ()
[%%expect{|
Line 1, characters 61-63:
1 | let leaf_over_applied (type a : any) () : a = raise_any Exit ()
                                                                 ^^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

Line 1, characters 46-63:
1 | let leaf_over_applied (type a : any) () : a = raise_any Exit ()
                                                  ^^^^^^^^^^^^^^^^^
Error: This expression has type "('a : '_representable_layout_2)"
       but an expression was expected of type "a"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

(* Bindings and sequences: only the body counts. *)

let let_body (type a : any) x : a =
  let y = x + 1 in
  let mutable z = y in
  z <- z + 1;
  let module M = struct let v = z end in
  let exception E of int in
  let open List in
  ignore (length []);
  List.(raise_any (E M.v))
[%%expect{|
val let_body : ('a : any). int -> 'a = <fun>
|}]

let let_bound_raises (type a : any) () : a =
  let x = raise_any Exit in
  x
[%%expect{|
Line 3, characters 2-3:
3 |   x
      ^
Error: The value "x" has type "('a : '_representable_layout_3)"
       but an expression was expected of type "a"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because of the definition of x at line 2, characters 10-24.
|}]

let sequence_returns (type a : any) (x : a w) : a =
  raise_any Exit;
  get x 3
[%%expect{|
Line 2, characters 2-16:
2 |   raise_any Exit;
      ^^^^^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

Lines 1-3, characters 21-9:
1 | .....................(type a : any) (x : a w) : a =
2 |   raise_any Exit;
3 |   get x 3
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let ( let* ) x f = f x
let letop (type a : any) () : a =
  let* x = 1 in
  ignore x;
  raise_any Exit
[%%expect{|
val ( let* ) : 'a -> ('a -> 'b) -> 'b = <fun>
Lines 3-5, characters 2-16:
3 | ..let* x = 1 in
4 |   ignore x;
5 |   raise_any Exit
Error: This expression has type "('a : value_or_null)"
       but an expression was expected of type "a"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout
         because of the definition of let* at line 1, characters 13-22.
|}]

let exclave (type a : any) () : a = exclave_ raise_any Exit
[%%expect{|
val exclave : ('a : any). unit -> 'a @ local = <fun>
|}]

(* Conditionals: every branch. *)

let if_both (type a : any) c : a =
  if c then raise_any Exit else assert false
[%%expect{|
val if_both : ('a : any). bool -> 'a = <fun>
|}]

let if_one (type a : any) c (w : a w) : a =
  if c then raise_any Exit else get w 3
[%%expect{|
Lines 1-2, characters 11-39:
1 | ...........(type a : any) c (w : a w) : a =
2 |   if c then raise_any Exit else get w 3
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

(* Matches: every case, including exception cases and guards. *)

let match_all (type a : any) x : a =
  match x with
  | Some y when y -> raise_any Exit
  | Some _ -> assert false
  | None -> raise_any Exit
  | exception Not_found -> raise_any Not_found
[%%expect{|
val match_all : ('a : any). bool option -> 'a = <fun>
|}]

let match_one_case_returns (type a : any) x (w : a w) : a =
  match x with
  | Some _ -> raise_any Exit
  | None -> get w 3
[%%expect{|
Lines 1-4, characters 27-19:
1 | ...........................(type a : any) x (w : a w) : a =
2 |   match x with
3 |   | Some _ -> raise_any Exit
4 |   | None -> get w 3
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let match_exception_case_returns (type a : any) x (w : a w) : a =
  match x () with
  | () -> raise_any Exit
  | exception Not_found -> get w 3
[%%expect{|
Lines 1-4, characters 33-34:
1 | .................................(type a : any) x (w : a w) : a =
2 |   match x () with
3 |   | () -> raise_any Exit
4 |   | exception Not_found -> get w 3
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let match_partial (type a : any) x : a =
  match x with
  | Some _ -> raise_any Exit
[%%expect{|
Lines 2-3, characters 2-28:
2 | ..match x with
3 |   | Some _ -> raise_any Exit
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "None"

val match_partial : 'b ('a : any). 'b option -> 'a = <fun>
|}]

let function_cases (type a : any) : bool -> a = function
  | true -> raise_any Exit
  | false -> assert false
[%%expect{|
val function_cases : ('a : any). bool -> 'a = <fun>
|}]

let function_cases_returns (type a : any) (w : a w) : bool -> a = function
  | true -> raise_any Exit
  | false -> get w 3
[%%expect{|
Lines 1-3, characters 27-20:
1 | ...........................(type a : any) (w : a w) : bool -> a = function
2 |   | true -> raise_any Exit
3 |   | false -> get w 3
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

type _ Effect.t += E : unit Effect.t
let match_effect (type a : any) f : a =
  match f () with
  | () -> raise_any Exit
  | effect E, _ -> raise_any Exit
[%%expect{|
type _ Stdlib.Effect.t += E : unit Effect.t
Lines 2-5, characters 17-33:
2 | .................(type a : any) f : a =
3 |   match f () with
4 |   | () -> raise_any Exit
5 |   | effect E, _ -> raise_any Exit
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

(* [try]: the body and every handler. *)

let try_all (type a : any) f : a =
  try f (); raise_any Exit with
  | Not_found -> raise_any Exit
  | Exit -> assert false
[%%expect{|
val try_all : 'b ('a : any). (unit -> 'b) -> 'a = <fun>
|}]

let try_body_returns (type a : any) (w : a w) : a =
  try get w 3 with _ -> raise_any Exit
[%%expect{|
Lines 1-2, characters 21-38:
1 | .....................(type a : any) (w : a w) : a =
2 |   try get w 3 with _ -> raise_any Exit
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let try_handler_returns (type a : any) (w : a w) : a =
  try raise_any Exit with _ -> get w 3
[%%expect{|
Lines 1-2, characters 24-38:
1 | ........................(type a : any) (w : a w) : a =
2 |   try raise_any Exit with _ -> get w 3
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let try_effect (type a : any) f : a =
  try f (); raise_any Exit with
  | effect E, _ -> raise_any Exit
[%%expect{|
Lines 1-3, characters 15-33:
1 | ...............(type a : any) f : a =
2 |   try f (); raise_any Exit with
3 |   | effect E, _ -> raise_any Exit
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

(* Nested functions are classified separately. *)

let returns_never_returning (type a : any) () : unit -> a =
  print_string "";
  fun () -> raise_any Exit
[%%expect{|
val returns_never_returning : ('a : any). unit -> unit -> 'a = <fun>
|}]

let returns_returning (type a : any) (w : a w) : unit -> a =
  fun () -> get w 3
[%%expect{|
Line 2, characters 2-19:
2 |   fun () -> get w 3
      ^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let newtype_after_param = fun () (type a : any) -> (assert false : a)
[%%expect{|
val newtype_after_param : ('a : any). unit -> 'a = <fun>
|}]

let optional_argument (type a : any) ?(x = 1) () : a =
  ignore x;
  raise_any Exit
[%%expect{|
val optional_argument : ('a : any). ?x:int -> unit -> 'a = <fun>
|}]

(**********************************************************)
(* Section 2: expressions of type [a] in other positions *)

(* Each function below ends in [raise_any Exit], so its result may have layout
   [any]; the expression of type [a] inside it must still be rejected. *)

let in_let (type a : any) () : a =
  let _ = (raise_any Exit : a) in
  raise_any Exit
[%%expect{|
Line 2, characters 10-30:
2 |   let _ = (raise_any Exit : a) in
              ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_4)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a variable bound by a `let`.
|}]

let in_let_mutable (type a : any) () : a =
  let mutable x = (raise_any Exit : a) in
  raise_any Exit
[%%expect{|
Line 2, characters 18-38:
2 |   let mutable x = (raise_any Exit : a) in
                      ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_5)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a variable bound by a `let`.
|}, Principal{|
Line 2, characters 14-15:
2 |   let mutable x = (raise_any Exit : a) in
                  ^
Error: This pattern matches values of type "a"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_5)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a variable bound by a `let`.
|}]

let in_sequence (type a : any) () : a =
  (raise_any Exit : a);
  raise_any Exit
[%%expect{|
Line 2, characters 2-22:
2 |   (raise_any Exit : a);
      ^^^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 2, characters 3-17:
2 |   (raise_any Exit : a);
       ^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_6)"
       because it is in the left-hand side of a sequence
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a statement.
|}]

let in_sequence_if (type a : any) c : a =
  (if c then (raise_any Exit : a) else assert false);
  raise_any Exit
[%%expect{|
Line 2, characters 2-52:
2 |   (if c then (raise_any Exit : a) else assert false);
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 2, characters 2-52:
2 |   (if c then (raise_any Exit : a) else assert false);
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This "if-then-else" expression has type "a"
       but an expression was expected of type "('a : '_representable_layout_7)"
       because it is in the left-hand side of a sequence
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a statement.
|}]

let in_while (type a : any) () : a =
  while false do (raise_any Exit : a) done;
  raise_any Exit
[%%expect{|
Line 2, characters 17-37:
2 |   while false do (raise_any Exit : a) done;
                     ^^^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 2, characters 18-32:
2 |   while false do (raise_any Exit : a) done;
                      ^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_8)"
       because it is in the body of a while-loop
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a statement.
|}]

let in_for (type a : any) () : a =
  for _ = 1 to 0 do (raise_any Exit : a) done;
  raise_any Exit
[%%expect{|
Line 2, characters 20-40:
2 |   for _ = 1 to 0 do (raise_any Exit : a) done;
                        ^^^^^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 2, characters 21-35:
2 |   for _ = 1 to 0 do (raise_any Exit : a) done;
                         ^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_9)"
       because it is in the body of a for-loop
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a statement.
|}]

let in_argument (type a : any) () : a =
  ignore (raise_any Exit : a);
  raise_any Exit
[%%expect{|
Line 2, characters 9-29:
2 |   ignore (raise_any Exit : a);
             ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : value_or_null)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout.
|}]

let in_labelled_argument (type a : any) (f : x:a -> unit) : a =
  f ~x:(raise_any Exit);
  raise_any Exit
[%%expect{|
Line 2, characters 7-23:
2 |   f ~x:(raise_any Exit);
           ^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

let in_parameter (type a : any) () : a =
  (fun (_ : a) -> ()) (raise_any Exit);
  raise_any Exit
[%%expect{|
Line 2, characters 7-14:
2 |   (fun (_ : a) -> ()) (raise_any Exit);
           ^^^^^^^
Error: This pattern matches values of type "a"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_10)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

let in_tuple (type a : any) () : a =
  ignore ((raise_any Exit : a), 1);
  raise_any Exit
[%%expect{|
Line 2, characters 10-30:
2 |   ignore ((raise_any Exit : a), 1);
              ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_11)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a tuple element.
|}]

let in_unboxed_tuple (type a : any) () : a =
  let #(_, _) = #((raise_any Exit : a), 1) in
  raise_any Exit
[%%expect{|
Line 2, characters 18-38:
2 |   let #(_, _) = #((raise_any Exit : a), 1) in
                      ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_12)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a tuple element.
|}]

let in_record (type a : any) () : a =
  ignore { contents = (raise_any Exit : a) };
  raise_any Exit
[%%expect{|
Line 2, characters 22-42:
2 |   ignore { contents = (raise_any Exit : a) };
                          ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : value_or_null)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout.
|}]

let in_constructor (type a : any) () : a =
  ignore (Some (raise_any Exit : a));
  raise_any Exit
[%%expect{|
Line 2, characters 16-30:
2 |   ignore (Some (raise_any Exit : a));
                    ^^^^^^^^^^^^^^
Error: Constructor arguments must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a constructor argument being assigned a value.
|}]

let in_variant (type a : any) () : a =
  ignore (`A (raise_any Exit : a));
  raise_any Exit
[%%expect{|
Line 2, characters 13-33:
2 |   ignore (`A (raise_any Exit : a));
                 ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : value_or_null)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout
         because it's the type of the field of a polymorphic variant.
|}]

let in_array (type a : any) () : a =
  ignore [| (raise_any Exit : a) |];
  raise_any Exit
[%%expect{|
Line 2, characters 12-32:
2 |   ignore [| (raise_any Exit : a) |];
                ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_13 separable)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of an array element.
|}]

let in_lazy (type a : any) () : a =
  ignore (lazy (raise_any Exit : a));
  raise_any Exit
[%%expect{|
Line 2, characters 15-35:
2 |   ignore (lazy (raise_any Exit : a));
                   ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : value)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout
         because it's the type of a lazy expression.
|}]

let in_scrutinee (type a : any) () : a =
  match (raise_any Exit : a) with
  | _ -> raise_any Exit
[%%expect{|
Line 2, characters 8-28:
2 |   match (raise_any Exit : a) with
            ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_14)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because a value of this type is matched against a pattern.
|}]

let in_scrutinee_exception (type a : any) () : a =
  match (raise_any Exit : a) with
  | _ -> raise_any Exit
  | exception Exit -> raise_any Exit
[%%expect{|
Line 2, characters 8-28:
2 |   match (raise_any Exit : a) with
            ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_15)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because a value of this type is matched against a pattern.
|}]

let in_try_body (type a : any) () : a =
  let _ = try (raise_any Exit : a) with _ -> raise_any Exit in
  raise_any Exit
[%%expect{|
Line 2, characters 14-34:
2 |   let _ = try (raise_any Exit : a) with _ -> raise_any Exit in
                  ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_16)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a variable bound by a `let`.
|}]

let in_letop (type a : any) () : a =
  let* _ = (raise_any Exit : a) in
  raise_any Exit
[%%expect{|
Line 2, characters 11-31:
2 |   let* _ = (raise_any Exit : a) in
               ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : value_or_null)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout
         because of the definition of let* at line 1, characters 13-22.
|}]

let in_method (type a : any) () : a =
  ignore (object method m = (raise_any Exit : a) end);
  raise_any Exit
[%%expect{|
Line 2, characters 28-48:
2 |   ignore (object method m = (raise_any Exit : a) end);
                                ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : value)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout
         because it's the type of an object field.
|}]

let in_local_function (type a : any) () : a =
  let f () : a = Fun.id (raise_any Exit : a) in
  ignore f;
  raise_any Exit
[%%expect{|
Line 2, characters 24-44:
2 |   let f () : a = Fun.id (raise_any Exit : a) in
                            ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : value_or_null)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout.
|}]

let in_let_rec (type a : any) () : a =
  let rec x : a = raise_any Exit in
  raise_any Exit
[%%expect{|
Line 2, characters 10-11:
2 |   let rec x : a = raise_any Exit in
              ^
Error: This pattern matches values of type "a"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_17)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a variable bound by a `let`.
|}]

(* The same holds for the results of calls. *)

let call_in_let (type a : any) () : a =
  let _ = (never () : a) in
  raise_any Exit
[%%expect{|
Line 2, characters 10-24:
2 |   let _ = (never () : a) in
              ^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_18)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a variable bound by a `let`.
|}]

let call_in_sequence (type a : any) (w : a w) : a =
  get w 3;
  raise_any Exit
[%%expect{|
Line 2, characters 2-9:
2 |   get w 3;
      ^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

Line 2, characters 2-9:
2 |   get w 3;
      ^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_19)"
       because it is in the left-hand side of a sequence
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a statement.
|}]

let call_in_scrutinee (type a : any) (w : a w) : a =
  match get w 3 with
  | _ -> raise_any Exit
[%%expect{|
Line 2, characters 8-15:
2 |   match get w 3 with
            ^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_20)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because a value of this type is matched against a pattern.
|}]

let call_in_argument (type a : any) (w : a w) : a =
  ignore (get w 3);
  raise_any Exit
[%%expect{|
Line 2, characters 9-18:
2 |   ignore (get w 3);
             ^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : value_or_null)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a value layout.
|}]

(* And at the top level. *)

let top_level : t_any = raise_any Exit
[%%expect{|
Line 1, characters 4-13:
1 | let top_level : t_any = raise_any Exit
        ^^^^^^^^^
Error: This pattern matches values of type "t_any"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_21)"
       The layout of t_any is any
         because of the definition of t_any at line 4, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a variable bound by a `let`.
|}]

module M = struct
  let x = (raise_any Exit : t_any)
end
[%%expect{|
Line 2, characters 10-34:
2 |   let x = (raise_any Exit : t_any)
              ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "t_any" but an expression was expected of type
         "('a : '_representable_layout_22)"
       The layout of t_any is any
         because of the definition of t_any at line 4, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a variable bound by a `let`.
|}, Principal{|
Line 2, characters 6-7:
2 |   let x = (raise_any Exit : t_any)
          ^
Error: This pattern matches values of type "t_any"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_22)"
       The layout of t_any is any
         because of the definition of t_any at line 4, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a variable bound by a `let`.
|}]

let _ = (raise_any Exit : t_any)
[%%expect{|
Line 1, characters 8-32:
1 | let _ = (raise_any Exit : t_any)
            ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "t_any" but an expression was expected of type
         "('a : '_representable_layout_23)"
       The layout of t_any is any
         because of the definition of t_any at line 4, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a variable bound by a `let`.
|}]

(****************************************************)
(* Section 3: matching on GADTs and empty types *)

let gadt_match (type a : any) (w : a w) : a = match w with W -> raise_any Exit
[%%expect{|
val gadt_match : ('a : any). 'a w -> 'a = <fun>
|}]

let gadt_param (type a : any) (W : a w) : a = raise_any Exit
[%%expect{|
val gadt_param : ('a : any). 'a w -> 'a = <fun>
|}]

let gadt_function (type a : any) : a w -> a = function W -> raise_any Exit
[%%expect{|
val gadt_function : ('a : any). 'a w -> 'a = <fun>
|}]

(* A partial match on a GADT does not let the result layout escape, because
   there is no result layout to speak of. *)

type ('a : any) w2 = W1 : int w2 | W2 : float# w2
let gadt_partial (type a : any) (w : a w2) : a =
  match w with
  | W1 -> raise_any Exit
[%%expect{|
type ('a : any) w2 = W1 : int w2 | W2 : float# w2
Lines 3-4, characters 2-24:
3 | ..match w with
4 |   | W1 -> raise_any Exit
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "W2"

val gadt_partial : ('a : any). 'a w2 -> 'a = <fun>
|}]

let gadt_partial_returns (type a : any) (w : a w2) : a =
  match w with
  | W1 -> 1
[%%expect{|
Lines 2-3, characters 2-11:
2 | ..match w with
3 |   | W1 -> 1
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "W2"

Lines 1-3, characters 25-11:
1 | .........................(type a : any) (w : a w2) : a =
2 |   match w with
3 |   | W1 -> 1
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let gadt_param_returns (type a : any) (W : a w) : a = 1
[%%expect{|
Line 1, characters 23-55:
1 | let gadt_param_returns (type a : any) (W : a w) : a = 1
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let gadt_refined_returns (type a : any) (w : a w2) : a =
  match w with
  | W1 -> 1
  | W2 -> #1.0
[%%expect{|
Lines 1-4, characters 25-14:
1 | .........................(type a : any) (w : a w2) : a =
2 |   match w with
3 |   | W1 -> 1
4 |   | W2 -> #1.0
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let refuted (type a : any) (x : void) : a = match x with _ -> .
let refuted_cases (type a : any) : void -> a = function _ -> .
let refuted_some_cases (type a : any) (x : (int, void) Either.t) : a =
  match x with
  | Left _ -> raise_any Exit
  | Right _ -> .
[%%expect{|
val refuted : ('a : any). void -> 'a = <fun>
val refuted_cases : ('a : any). void -> 'a = <fun>
val refuted_some_cases : ('a : any). (int, void) Either.t -> 'a = <fun>
|}]

(* A match on the result of a never-returning function sees a representable
   type. *)

let match_on_call () =
  match never () with
  | `A -> 1
  | `B -> 2
[%%expect{|
val match_on_call : unit -> int = <fun>
|}]

let match_on_call_any (type a : any) () : a =
  match (never () : a) with
  | _ -> raise_any Exit
[%%expect{|
Line 2, characters 8-22:
2 |   match (never () : a) with
            ^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_24)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because a value of this type is matched against a pattern.
|}]

(******************************************************)
(* Section 4: generalisation, storage and signatures *)

let generalised = fun () -> raise_any Exit
let instance_float () : float# = generalised ()
let instance_string () : string = generalised ()
[%%expect{|
val generalised : ('a : any). unit -> 'a = <fun>
val instance_float : unit -> float# = <fun>
val instance_string : unit -> string = <fun>
|}]

let through_identity = Fun.id generalised
[%%expect{|
val through_identity : ('a : any). unit -> 'a = <fun>
|}]

let r = ref (fun () -> raise_any Exit)
let use_float () : float# = !r ()
[%%expect{|
val r : (unit -> '_weak1) ref = {contents = <fun>}
val use_float : unit -> float# = <fun>
|}]

let use_string () : string = !r ()
[%%expect{|
Line 1, characters 29-34:
1 | let use_string () : string = !r ()
                                 ^^^^^
Error: This expression has type "float#" but an expression was expected of type
         "string"
|}]

module type S = sig val f : ('a : any). unit -> 'a end
[%%expect{|
module type S = sig val f : ('a : any). unit -> 'a end
|}]

module Alias : S = struct let f = never end
module Fun_alias : S = struct let f = generalised end
[%%expect{|
module Alias : S
module Fun_alias : S
|}]

module Call : S = struct let f () = never () end
[%%expect{|
Line 1, characters 18-48:
1 | module Call : S = struct let f () = never () end
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match: sig val f : unit -> 'a end is not included in S
       Values do not match:
         val f : unit -> 'a
       is not included in
         val f : ('a : any). unit -> 'a
       The type "unit -> 'a" is not compatible with the type "unit -> 'b"
       The layout of 'a is any
         because of the definition of f at line 1, characters 20-50.
       But the layout of 'a must be representable
         because of the definition of f at line 1, characters 31-44.
|}]

(* ['a] is a unification variable, so it may stay of layout [any]. *)

module Annotated_result : S = struct let f () : 'a = assert false end
[%%expect{|
module Annotated_result : S
|}]

module Use (X : S) = struct
  let f () : float# = X.f ()
  let g () = X.f ()
end
[%%expect{|
module Use :
  functor (X : S) -> sig val f : unit -> float# val g : unit -> 'a end
|}]

let first_class (module X : S) : float# = X.f ()
[%%expect{|
val first_class : (module S) -> float# = <fun>
|}]

(* Raise primitives with [[@layout_poly]] are instantiated at a representable
   layout, so they cannot end a function with a result of layout [any]. *)

external[@layout_poly] raise_poly : ('a : any). exn -> 'a = "%raise"
let raise_poly_float () : float# = raise_poly Exit
let raise_poly_never (type a : any) () : a = raise_poly Exit
[%%expect{|
external raise_poly : ('a : any). exn -> 'a = "%raise" [@@layout_poly]
val raise_poly_float : unit -> float# = <fun>
Line 3, characters 45-60:
3 | let raise_poly_never (type a : any) () : a = raise_poly Exit
                                                 ^^^^^^^^^^^^^^^
Error: This expression has type "('a : '_representable_layout_25)"
       but an expression was expected of type "a"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(* Raise primitives must still have the right arity. *)

external raise_two : ('a : any). exn -> int -> 'a = "%raise"
[%%expect{|
Line 1, characters 0-60:
1 | external raise_two : ('a : any). exn -> int -> 'a = "%raise"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong arity for builtin primitive "%raise"
|}]

(*******************************************************)
(* Section 5: subsumption and higher-order functions *)

(* A function that never returns is an instance of any function type with the
   same parameters, but a function that returns is not an instance of one
   with a result of layout [any]. *)

let as_float = (leaf_raise : unit -> float#)
let as_abstract = (leaf_raise : unit -> t_any)
let as_value = (leaf_raise : unit -> string)
let coerced = (leaf_raise :> unit -> #(int * float#))
[%%expect{|
val as_float : unit -> float# = <fun>
val as_abstract : unit -> t_any = <fun>
val as_value : unit -> string = <fun>
val coerced : unit -> #(int * float#) = <fun>
|}]

let poly_alias : ('a : any). unit -> 'a = leaf_raise
let poly_fun : ('a : any). unit -> 'a = fun () -> assert false
[%%expect{|
val poly_alias : ('a : any). unit -> 'a = <fun>
val poly_fun : ('a : any). unit -> 'a = <fun>
|}]

let poly_call : ('a : any). unit -> 'a = fun () -> leaf_raise ()
[%%expect{|
Line 1, characters 41-64:
1 | let poly_call : ('a : any). unit -> 'a = fun () -> leaf_raise ()
                                             ^^^^^^^^^^^^^^^^^^^^^^^
Error: This definition has type "unit -> 'b" which is less general than
         "('a : any). unit -> 'a"
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because we must know concretely how to return a function result.
|}]

module Narrow : sig
  val f : unit -> float#
  val g : unit -> t_any
  val h : unit -> 'a
end = struct
  let f = leaf_raise
  let g = leaf_raise
  let h = leaf_raise
end
[%%expect{|
module Narrow :
  sig val f : unit -> float# val g : unit -> t_any val h : unit -> 'a end
|}]

module Annotated_widen : sig
  val f : ('a : any). unit -> 'a
end = struct
  let f : unit -> 'a = leaf_raise
end
[%%expect{|
module Annotated_widen : sig val f : ('a : any). unit -> 'a end
|}]

module Abstract_result : sig
  val f : ('a : any). unit -> 'a
end = struct
  let f : unit -> t_any = leaf_raise
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : unit -> t_any = leaf_raise
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : unit -> t_any end
       is not included in
         sig val f : ('a : any). unit -> 'a end
       Values do not match:
         val f : unit -> t_any
       is not included in
         val f : ('a : any). unit -> 'a
       The type "unit -> t_any" is not compatible with the type "unit -> 'a"
       Hint: Did you forget to wrap the expression using "fun () ->"?
|}]

(* Passing such functions to higher-order functions. *)

let apply f = f ()
let apply_float (f : unit -> float#) = f ()
let ignore_any (type b : any) (f : unit -> b) = ignore f
[%%expect{|
val apply : (unit -> 'a) -> 'a = <fun>
val apply_float : (unit -> float#) -> float# = <fun>
val ignore_any : ('b : any). (unit -> 'b) -> unit = <fun>
|}]

let _ = fun () -> apply leaf_raise
let _ = fun () -> apply_float leaf_raise
let _ = fun () -> apply_float (fun () -> raise_any Exit)
let _ = ignore_any leaf_raise
let _ = ignore_any (fun () -> raise_any Exit)
let _ = fun () -> List.map (fun _ -> raise_any Exit) [1] |> List.length
let _ = fun () -> Fun.flip leaf_raise_with_backtrace
[%%expect{|
- : unit -> 'a = <fun>
- : unit -> float# = <fun>
- : unit -> float# = <fun>
- : unit = ()
- : unit = ()
- : unit -> int = <fun>
- : unit -> 'a -> Printexc.raw_backtrace -> 'b = <fun>
|}]

(* A higher-order function cannot call a function with a result of layout
   [any] and use the result. *)

let call_any (type b : any) (f : unit -> b) : b = f ()
[%%expect{|
Line 1, characters 13-54:
1 | let call_any (type b : any) (f : unit -> b) : b = f ()
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of b is any
         because of the annotation on the abstract type declaration for b.
       But the layout of b must be representable
         because we must know concretely how to return a function result.
|}]

let call_any_tail (type b : any) (f : unit -> b) : b =
  if true then f () else raise_any Exit
[%%expect{|
Lines 1-2, characters 18-39:
1 | ..................(type b : any) (f : unit -> b) : b =
2 |   if true then f () else raise_any Exit
Error: Function arguments and returns must be representable.
       The layout of b is any
         because of the annotation on the abstract type declaration for b.
       But the layout of b must be representable
         because we must know concretely how to return a function result.
|}]

let call_any_ignore (type b : any) (f : unit -> b) = ignore (f ())
[%%expect{|
Line 1, characters 60-66:
1 | let call_any_ignore (type b : any) (f : unit -> b) = ignore (f ())
                                                                ^^^^^^
Error: This expression has type "b" but an expression was expected of type
         "('a : value_or_null)"
       The layout of b is any
         because of the annotation on the abstract type declaration for b.
       But the layout of b must be a value layout.
|}]

let call_any_in_list (type b : any) (f : unit -> b) = [f ()]
[%%expect{|
Line 1, characters 55-59:
1 | let call_any_in_list (type b : any) (f : unit -> b) = [f ()]
                                                           ^^^^
Error: This expression has type "b" but an expression was expected of type
         "('a : value_or_null)"
       The layout of b is any
         because of the annotation on the abstract type declaration for b.
       But the layout of b must be a value layout
         because the type argument of list has layout value_or_null.
|}]

let map_any (type b : any) (f : int -> b) = List.map f [1]
[%%expect{|
Line 1, characters 53-54:
1 | let map_any (type b : any) (f : int -> b) = List.map f [1]
                                                         ^
Error: The value "f" has type "int -> b" but an expression was expected of type
         "int -> 'a"
       The layout of b is any
         because of the annotation on the abstract type declaration for b.
       But the layout of b must be a value layout.
|}]

(* Polymorphic parameters and fields. *)

let use_poly (f : ('a : any). unit -> 'a) : float# =
  if true then f ()
  else
    let #(_, _) : #(int * string) = f () in
    f ()
[%%expect{|
val use_poly : (('a : any). unit -> 'a) -> float# = <fun>
|}]

let _ = fun () -> use_poly leaf_raise
let _ = fun () -> use_poly (fun () -> assert false)
[%%expect{|
- : unit -> float# = <fun>
- : unit -> float# = <fun>
|}]

let _ = fun () -> use_poly (fun () -> leaf_raise ())
[%%expect{|
Line 1, characters 27-52:
1 | let _ = fun () -> use_poly (fun () -> leaf_raise ())
                               ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This argument has type "unit -> 'b" which is less general than
         "('a : any). unit -> 'a"
       The layout of 'a is any
         because of the definition of use_poly at lines 1-5, characters 13-8.
       But the layout of 'a must be representable
         because we must know concretely how to return a function result.
|}]

let _ = fun () -> use_poly never
[%%expect{|
- : unit -> float# = <fun>
|}]

type r = { f : ('a : any). unit -> 'a }
let from_alias = { f = leaf_raise }
let from_fun = { f = (fun () -> raise_any Exit) }
let use_field r : float# = r.f ()
[%%expect{|
type r = { f : ('a : any). unit -> 'a; }
val from_alias : r = {f = <fun>}
val from_fun : r = {f = <fun>}
val use_field : r -> float# = <fun>
|}]

let from_call = { f = (fun () -> leaf_raise ()) }
[%%expect{|
Line 1, characters 22-47:
1 | let from_call = { f = (fun () -> leaf_raise ()) }
                          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This field value has type "unit -> 'b" which is less general than
         "('a : any). unit -> 'a"
       The layout of 'a is any
         because of the definition of r at line 1, characters 0-39.
       But the layout of 'a must be representable
         because we must know concretely how to return a function result.
|}]

let use_field_any (type b : any) r : b = r.f ()
[%%expect{|
Line 1, characters 18-47:
1 | let use_field_any (type b : any) r : b = r.f ()
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of b is any
         because of the annotation on the abstract type declaration for b.
       But the layout of b must be representable
         because we must know concretely how to return a function result.
|}]

(* Partial applications generalise over the result. *)

let two (type a : any) x y : a = ignore (x, y); raise_any Exit
let partial = two 1
let _ = fun () : float# -> partial 2
[%%expect{|
val two : 'b 'c ('a : any). 'b -> 'c -> 'a = <fun>
val partial : ('a : any). '_weak2 -> 'a = <fun>
- : unit -> float# = <fun>
|}]

let _ = fun () : string -> partial 2
[%%expect{|
- : unit -> string = <fun>
|}]

(* Objects with such methods can be coerced like any others. *)

let o =
  object
    method never : ('a : any). unit -> 'a = leaf_raise
    method other = 1
  end
let coerced_object = (o :> < never : ('a : any). unit -> 'a >)
let _ = fun () : float# -> coerced_object#never ()
[%%expect{|
val o : < never : ('a : any). unit -> 'a; other : int > = <obj>
val coerced_object : < never : ('a : any). unit -> 'a > = <obj>
- : unit -> float# = <fun>
|}]

let instance_coercion = (o :> < never : unit -> float# >)
[%%expect{|
val instance_coercion : < never : unit -> float# > = <obj>
|}]
