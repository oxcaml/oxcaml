(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Tests for [Return_sort.classify].*)

type t_any : any
type ('a : any) t = I : int t | F : float# t
[%%expect{|
type t_any : any
type ('a : any) t = I : int t | F : float# t
|}]

(* Return sites refined by GADT equations get their concrete layouts, so two
   arms with different layouts conflict. *)

let first_site : type (a : any). a t -> a = function
  | I -> 42
  | F -> #1.0
[%%expect{|
Line 3, characters 9-13:
3 |   | F -> #1.0
             ^^^^
Error: A function may have at most one direct-return layout:
       this return site has layout float64,
       but another return site of the same function has layout value.
Line 2, characters 9-11:
2 |   | I -> 42
             ^^
  This is the location of a conflicting return site.
|}]

let first_site_rev : type (a : any). a t -> a = function
  | F -> #1.0
  | I -> 42
[%%expect{|
Line 3, characters 9-11:
3 |   | I -> 42
             ^^
Error: A function may have at most one direct-return layout:
       this return site has layout value,
       but another return site of the same function has layout float64.
Line 2, characters 9-13:
2 |   | F -> #1.0
             ^^^^
  This is the location of a conflicting return site.
|}]

(* Arms that agree, or that do not return, are fine: the refined layout is the
   function's return layout. *)

let refined_value : type (a : any). a t -> a = function
  | I -> 42
  | F -> assert false
[%%expect{|
val refined_value : ('a : any). 'a t -> 'a = <fun>
|}]

let refined_float : type (a : any). a t -> a = function
  | I -> assert false
  | F -> #1.0
[%%expect{|
val refined_float : ('a : any). 'a t -> 'a = <fun>
|}]

(* Same, with the conflict nested inside each arm. *)

let conflict : type (a : any). bool -> a t -> a = fun b v ->
  match v with
  | I -> if b then 1 else 2
  | F -> if b then #1.0 else #2.0
[%%expect{|
Line 4, characters 19-23:
4 |   | F -> if b then #1.0 else #2.0
                       ^^^^
Error: A function may have at most one direct-return layout:
       this return site has layout float64,
       but another return site of the same function has layout value.
Line 3, characters 19-20:
3 |   | I -> if b then 1 else 2
                       ^
  This is the location of a conflicting return site.
|}]

let conflict_rev : type (a : any). bool -> a t -> a = fun b v ->
  match v with
  | F -> if b then #1.0 else #2.0
  | I -> if b then 1 else 2
[%%expect{|
Line 4, characters 19-20:
4 |   | I -> if b then 1 else 2
                       ^
Error: A function may have at most one direct-return layout:
       this return site has layout value,
       but another return site of the same function has layout float64.
Line 3, characters 19-23:
3 |   | F -> if b then #1.0 else #2.0
                       ^^^^
  This is the location of a conflicting return site.
|}]

(* Branches that do not return normally can return any. *)

let discharged : type (a : any). bool -> a t -> a = fun b v ->
  match v with
  | I -> assert false
  | F -> if b then #1.0 else #2.0
[%%expect{|
val discharged : ('a : any). bool -> 'a t -> 'a = <fun>
|}]

(* A function that never returns can return any. *)

let never () : t_any = assert false
[%%expect{|
val never : unit -> t_any = <fun>
|}]

(* Tail forwarding allows returning any. *)

let forward (g : unit -> t_any) () = g ()
[%%expect{|
val forward : (unit -> t_any) -> unit -> t_any = <fun>
|}]

let nontail (g : unit -> t_any) () = (g () [@nontail])
[%%expect{|
Line 1, characters 37-54:
1 | let nontail (g : unit -> t_any) () = (g () [@nontail])
                                         ^^^^^^^^^^^^^^^^^
Error: This expression is in return position, so its layout must be
       representable. Only tail calls, whose result is returned directly,
       and expressions that never return normally, such as raise, are exempt.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because we must know concretely how to return a function result.
|}]

(* Raise primitives can return any. *)
external my_raise : ('a : any). exn -> 'a = "%raise"
[%%expect{|
external my_raise : ('a : any). exn -> 'a = "%raise"
|}]

external my_raise_with_backtrace :
  ('a : any). exn -> Printexc.raw_backtrace -> 'a = "%raise_with_backtrace"
[%%expect{|
external my_raise_with_backtrace :
  ('a : any). exn -> Printexc.raw_backtrace -> 'a = "%raise_with_backtrace"
|}]

(* Other primitives may not. *)

external not_raise : ('a : any). unit -> 'a = "%opaque"
[%%expect{|
Line 1, characters 41-43:
1 | external not_raise : ('a : any). unit -> 'a = "%opaque"
                                             ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of the result of an external declaration.
|}]

(* The result can't be unpacked, even though it never materializes. *)

external bad_unpacked : ('a : any). exn -> ('a [@unpacked]) = "%raise"
[%%expect{|
Line 1, characters 44-46:
1 | external bad_unpacked : ('a : any). exn -> ('a [@unpacked]) = "%raise"
                                                ^^
Error: Don't know how to unpack this type.
       Only types with product layouts can be marked "unpacked".
|}]

(* The arguments of raise must be values. *)

external bad_arg : ('a : any). #(int * int) -> 'a = "%raise"
[%%expect{|
Line 1, characters 19-49:
1 | external bad_arg : ('a : any). #(int * int) -> 'a = "%raise"
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%raise] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
Hint: This was expected to be a value-only primitive. You might've
      misspelled the primitive name.
|}]

external bad_any_arg : ('a : any) ('b : any). 'a -> 'b = "%raise"
[%%expect{|
Line 1, characters 46-48:
1 | external bad_any_arg : ('a : any) ('b : any). 'a -> 'b = "%raise"
                                                  ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of an argument in an external declaration.
|}]

(* Layout-any raises can be used at any return layout. *)

let raise_at_float b =
  if b then #1.0 else my_raise Exit
[%%expect{|
val raise_at_float : bool -> float# = <fun>
|}]

let raise_at_float_bt b bt =
  if b then #1.0 else my_raise_with_backtrace Exit bt
[%%expect{|
val raise_at_float_bt : bool -> Printexc.raw_backtrace -> float# = <fun>
|}]

let raise_at_value () = my_raise Exit
[%%expect{|
val raise_at_value : ('a : any). unit -> 'a = <fun>
|}]

let raise_at_any () : t_any = my_raise Exit
[%%expect{|
val raise_at_any : unit -> t_any = <fun>
|}]

(* [Stdlib.raise] itself still declares a value result. *)
let stdlib_raise_at_any () : t_any = raise Exit
[%%expect{|
Line 1, characters 37-47:
1 | let stdlib_raise_at_any () : t_any = raise Exit
                                         ^^^^^^^^^^
Error: This expression has type "('a : value_or_null)"
       but an expression was expected of type "t_any"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be a value layout.
|}]
