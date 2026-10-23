(* TEST
 flags += "-keywords 5.3";
 expect;
*)

(* A function none of whose return positions can return may have a result of
   any layout. *)

(* The raise primitives may be declared with a result of layout [any]. *)

external raise_any : ('a : any). exn -> 'a = "%raise"
external reraise_any : ('a : any). exn -> 'a = "%reraise"
external raise_notrace_any : ('a : any). exn -> 'a = "%raise_notrace"
external raise_with_backtrace_any
  : ('a : any). exn -> Printexc.raw_backtrace -> 'a = "%raise_with_backtrace"
[%%expect{|
external raise_any : ('a : any). exn -> 'a = "%raise"
external reraise_any : ('a : any). exn -> 'a = "%reraise"
external raise_notrace_any : ('a : any). exn -> 'a = "%raise_notrace"
external raise_with_backtrace_any :
  ('a : any). exn -> Printexc.raw_backtrace -> 'a = "%raise_with_backtrace"
|}]

(* The result of a raise primitive need not be a type variable. *)

type t_any_abstract : any
external raise_abstract : exn -> t_any_abstract = "%raise"
[%%expect{|
type t_any_abstract : any
external raise_abstract : exn -> t_any_abstract = "%raise"
|}]

(* A raise primitive declared with a result of layout [any] can be exported
   with a less general type, but not the reverse. *)

module M_narrow : sig
  external raise : exn -> 'a = "%raise"
end = struct
  external raise : ('a : any). exn -> 'a = "%raise"
end
[%%expect{|
module M_narrow : sig external raise : exn -> 'a = "%raise" end
|}]

module M_widen : sig
  external raise : ('a : any). exn -> 'a = "%raise"
end = struct
  external raise : exn -> 'a = "%raise"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external raise : exn -> 'a = "%raise"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external raise : exn -> 'a = "%raise" end
       is not included in
         sig external raise : ('a : any). exn -> 'a = "%raise" end
       Values do not match:
         external raise : exn -> 'a = "%raise"
       is not included in
         external raise : ('a : any). exn -> 'a = "%raise"
       The type "exn -> 'a" is not compatible with the type "exn -> 'b"
       The layout of 'a is any
         because of the definition of raise at line 2, characters 2-51.
       But the layout of 'a must be a value layout
         because of the definition of raise at line 4, characters 2-39.
|}]

(* A representation can still be asked for explicitly. *)

external raise_unboxed_float : exn -> (float [@unboxed]) = "%raise" "%raise"
[%%expect{|
external raise_unboxed_float : exn -> (float [@unboxed]) = "%raise" "%raise"
|}]

(* Other primitives and C stubs may not. *)

external id_any : ('a : any). 'a -> 'a = "%identity"
[%%expect{|
Line 1, characters 30-32:
1 | external id_any : ('a : any). 'a -> 'a = "%identity"
                                  ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of an argument in an external declaration.
|}]

external stub_any : ('a : any). unit -> 'a = "caml_stub_any"
[%%expect{|
Line 1, characters 40-42:
1 | external stub_any : ('a : any). unit -> 'a = "caml_stub_any"
                                            ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of the result of an external declaration.
|}]

external[@unboxed] raise_unboxed : ('a : any). exn -> 'a = "%raise"
[%%expect{|
Line 1, characters 47-50:
1 | external[@unboxed] raise_unboxed : ('a : any). exn -> 'a = "%raise"
                                                   ^^^
Error: Don't know how to unbox this type.
       Only "float", "int8", "int16", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}]

(* A result of layout [any] cannot be given an explicit representation, the
   argument must still be a value, and representations must still be direct
   and agree between signatures. *)

external raise_any_unboxed : ('a : any). exn -> ('a [@unboxed]) = "%raise"
[%%expect{|
Line 1, characters 49-51:
1 | external raise_any_unboxed : ('a : any). exn -> ('a [@unboxed]) = "%raise"
                                                     ^^
Error: Types in an external must have a representable layout.
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of the result of an external declaration.
|}]

external raise_float_u : float# -> int = "%raise"
[%%expect{|
Line 1, characters 25-38:
1 | external raise_float_u : float# -> int = "%raise"
                             ^^^^^^^^^^^^^
Error: The primitive [%raise] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
Hint: This was expected to be a value-only primitive. You might've
      misspelled the primitive name.
|}]

external raise_deep : exn -> (float [@unboxed]) list = "%raise"
[%%expect{|
Line 1, characters 30-35:
1 | external raise_deep : exn -> (float [@unboxed]) list = "%raise"
                                  ^^^^^
Error: The attribute "@unboxed" should be attached to
       a direct argument or result of the primitive,
       it should not occur deeply into its type.
|}]

module Raise_repr : sig
  external r : exn -> float = "%raise" "%raise"
end = struct
  external r : exn -> (float [@unboxed]) = "%raise" "%raise"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external r : exn -> (float [@unboxed]) = "%raise" "%raise"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external r : exn -> (float [@unboxed]) = "%raise" "%raise" end
       is not included in
         sig external r : exn -> float = "%raise" "%raise" end
       Values do not match:
         external r : exn -> (float [@unboxed]) = "%raise" "%raise"
       is not included in
         external r : exn -> float = "%raise" "%raise"
       The two primitives' results have different representations
|}]

(* Leaves that cannot return. *)

let assert_false () = assert false
[%%expect{|
val assert_false : ('a : any). unit -> 'a = <fun>
|}]

let raises () = raise_any Exit
[%%expect{|
val raises : ('a : any). unit -> 'a = <fun>
|}]

let reraises e = reraise_any e
[%%expect{|
val reraises : ('a : any). exn -> 'a = <fun>
|}]

let raises_notrace () = raise_notrace_any Exit
[%%expect{|
val raises_notrace : ('a : any). unit -> 'a = <fun>
|}]

let raises_with_backtrace e bt = raise_with_backtrace_any e bt
[%%expect{|
val raises_with_backtrace : ('a : any). exn -> Printexc.raw_backtrace -> 'a =
  <fun>
|}]

type void = |
let refuted (x : void) = match x with _ -> .
[%%expect{|
type void = |
val refuted : ('a : any). void -> 'a = <fun>
|}]

let refuted_cases : void -> _ = function _ -> .
[%%expect{|
val refuted_cases : ('a : any). void -> 'a = <fun>
|}]

(* [Stdlib.raise] has a result of layout [any]. *)

let stdlib_raise () = raise Exit
[%%expect{|
val stdlib_raise : ('a : any). unit -> 'a = <fun>
|}]

(* Return positions are found through bindings, sequences, conditionals,
   matches and [try]. *)

let through_let x =
  let y = x + 1 in
  let module M = struct let z = y end in
  let exception E of int in
  let open List in
  ignore (length []);
  raise_any (E M.z)
[%%expect{|
val through_let : ('a : any). int -> 'a = <fun>
|}]

let through_if c = if c then raise_any Exit else assert false
[%%expect{|
val through_if : ('a : any). bool -> 'a = <fun>
|}]

let through_match x =
  match x with
  | Some _ -> raise_any Exit
  | None -> assert false
  | exception Not_found -> raise_any Not_found
[%%expect{|
val through_match : 'a ('b : any). 'a option -> 'b = <fun>
|}]

let through_try f =
  try
    f ();
    raise_any Exit
  with
  | Not_found -> raise_any Exit
  | Exit -> assert false
[%%expect{|
val through_try : 'a ('b : any). (unit -> 'a) -> 'b = <fun>
|}]

let through_cases = function
  | 0 -> raise_any Exit
  | _ -> assert false
[%%expect{|
val through_cases : ('a : any). int -> 'a = <fun>
|}]

let several_params x y = function
  | true -> raise_any Exit
  | false -> ignore (x, y); assert false
[%%expect{|
val several_params : 'a 'b ('c : any). 'a -> 'b -> bool -> 'c = <fun>
|}]

let through_exclave () = exclave_ raise_any Exit
[%%expect{|
val through_exclave : ('a : any). unit -> 'a @ local = <fun>
|}]

(* A function that may return gets a representable result, as before. *)

let returns_in_one_branch c = if c then raise_any Exit else 1
[%%expect{|
val returns_in_one_branch : bool -> int = <fun>
|}]

let no_else c = if c then raise_any Exit
[%%expect{|
val no_else : bool -> unit = <fun>
|}]

let not_last () = (assert false; 1)
[%%expect{|
Line 1, characters 19-31:
1 | let not_last () = (assert false; 1)
                       ^^^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

val not_last : unit -> int = <fun>
|}]

let loops () = while true do () done
[%%expect{|
val loops : unit -> 'a = <fun>
|}]

let rec diverges () = diverges ()
[%%expect{|
val diverges : unit -> 'a = <fun>
|}]

let calls_never_returning () = assert_false ()
[%%expect{|
val calls_never_returning : unit -> 'a = <fun>
|}]

let partial_raise = raise_any
[%%expect{|
val partial_raise : ('a : any). exn -> 'a = <fun>
|}]

type _ Effect.t += E : unit Effect.t
let with_effect_cases f =
  match f () with
  | () -> assert false
  | effect E, _ -> assert false
[%%expect{|
type _ Stdlib.Effect.t += E : unit Effect.t
val with_effect_cases : (unit -> unit) -> 'a = <fun>
|}]

let try_with_effect_cases f =
  try f (); assert false with
  | effect E, _ -> assert false
[%%expect{|
val try_with_effect_cases : (unit -> 'a) -> 'b = <fun>
|}]

(* A function is classified by its syntax, not by its type. *)

let never_returns_int () : int = assert false
[%%expect{|
val never_returns_int : unit -> int = <fun>
|}]

type t_any : any
let never_returns_abstract () : t_any = assert false
[%%expect{|
type t_any : any
val never_returns_abstract : unit -> t_any = <fun>
|}]

let returns_abstract (f : unit -> t_any) = f ()
[%%expect{|
Line 1, characters 21-47:
1 | let returns_abstract (f : unit -> t_any) = f ()
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because we must know concretely how to return a function result.
|}]

let returns_abstract_annotated (f : unit -> t_any) : t_any = f ()
[%%expect{|
Line 1, characters 31-65:
1 | let returns_abstract_annotated (f : unit -> t_any) : t_any = f ()
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because we must know concretely how to return a function result.
|}]

(* Annotations. *)

let newtype (type a : any) () : a = assert false
[%%expect{|
val newtype : ('a : any). unit -> 'a = <fun>
|}]

let newtype_poly : type (a : any). unit -> a = fun () -> raise_any Exit
[%%expect{|
val newtype_poly : ('a : any). unit -> 'a = <fun>
|}]

let univar : ('a : any). unit -> 'a = fun () -> raise_any Exit
[%%expect{|
val univar : ('a : any). unit -> 'a = <fun>
|}]

let univar_returns : ('a : any). (unit -> 'a) -> 'a = fun f -> f ()
[%%expect{|
Line 1, characters 54-67:
1 | let univar_returns : ('a : any). (unit -> 'a) -> 'a = fun f -> f ()
                                                          ^^^^^^^^^^^^^
Error: This definition has type "(unit -> 'b) -> 'b" which is less general than
         "('a : any). (unit -> 'a) -> 'a"
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because we must know concretely how to return a function result.
|}]

let newtype_returns (type a : any) (f : unit -> a) : a =
  if true then assert false else f ()
[%%expect{|
Lines 1-2, characters 20-37:
1 | ....................(type a : any) (f : unit -> a) : a =
2 |   if true then assert false else f ()
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let univar_stdlib_raise : ('a : any). unit -> 'a = fun () -> raise Exit
[%%expect{|
val univar_stdlib_raise : ('a : any). unit -> 'a = <fun>
|}]

(* Such functions can be used where any result is expected. *)

let at_float () : float# = assert_false ()
let at_product () : #(int * float#) = raises ()
let at_value () : string = raises ()
[%%expect{|
val at_float : unit -> float# = <fun>
val at_product : unit -> #(int * float#) = <fun>
val at_value : unit -> string = <fun>
|}]

let apply_float (f : unit -> float#) = f ()
let _ = fun () -> apply_float (fun () -> assert false)
let _ = fun () -> apply_float raises
[%%expect{|
val apply_float : (unit -> float#) -> float# = <fun>
- : unit -> float# = <fun>
- : unit -> float# = <fun>
|}]

module M : sig
  val f : ('a : any). unit -> 'a
end = struct
  let f () = assert false
end
[%%expect{|
module M : sig val f : ('a : any). unit -> 'a end
|}]

module M_stdlib_raise : sig
  val f : ('a : any). unit -> 'a
end = struct
  let f () = raise Exit
end
[%%expect{|
module M_stdlib_raise : sig val f : ('a : any). unit -> 'a end
|}]

(* Methods. *)

let obj =
  object
    method never : ('a : any). unit -> 'a = fun () -> raise_any Exit
    method returns : 'a. 'a -> 'a = fun x -> x
  end
[%%expect{|
val obj : < never : ('a : any). unit -> 'a; returns : 'a. 'a -> 'a > = <obj>
|}]

let call_method () : float# = obj#never ()
[%%expect{|
val call_method : unit -> float# = <fun>
|}]

(* Recursive definitions are monomorphic while they are being checked, so a
   sibling that returns the result of a call makes the result representable. *)

let rec never () = assert false
and forwards () = never ()
[%%expect{|
val never : unit -> 'a = <fun>
val forwards : unit -> 'a = <fun>
|}]

let rec never_poly : ('a : any). unit -> 'a = fun () -> assert false
and uses_poly () : float# = never_poly ()
[%%expect{|
val never_poly : ('a : any). unit -> 'a = <fun>
val uses_poly : unit -> float# = <fun>
|}]

(* A result type of layout [any] does not by itself mean that a function
   cannot return: [get] is an ordinary function that returns a function. *)

type ('a : any) w = W : int w
let get : type (a : any). a w -> int -> a = fun w -> match w with W -> succ
[%%expect{|
type ('a : any) w = W : int w
val get : ('a : any). 'a w -> int -> 'a = <fun>
|}]

let _ = get W 3
[%%expect{|
- : int = 4
|}]

let calls_get (type a : any) (w : a w) : a = get w 3
[%%expect{|
Line 1, characters 14-52:
1 | let calls_get (type a : any) (w : a w) : a = get w 3
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let refined_result (type a : any) (w : a w) : a = match w with W -> 3
[%%expect{|
Line 1, characters 19-69:
1 | let refined_result (type a : any) (w : a w) : a = match w with W -> 3
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

(* Arguments must still be representable without relying on a partial match,
   even when the result need not be. *)

type ('a : any) witness =
  | Int : int witness
  | Float_u : float# witness

let refined_argument : type (a : any). a witness -> a -> a =
  fun Int _x -> assert false
[%%expect{|
type ('a : any) witness = Int : int witness | Float_u : float# witness
Line 6, characters 6-9:
6 |   fun Int _x -> assert false
          ^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Float_u"

Line 6, characters 10-12:
6 |   fun Int _x -> assert false
              ^^
Error: This function's argument has type a,
       whose layout is known only from the GADT pattern match at line 6, characters 6-9.
       That match is not exhaustive, so a caller could reach this argument at a different layout.
       Function arguments and results must be representable independently of any non-exhaustive match.
|}]

let refined_argument_cases : type (a : any). a witness -> a -> a =
  fun Int -> function _ -> assert false
[%%expect{|
Line 2, characters 6-9:
2 |   fun Int -> function _ -> assert false
          ^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Float_u"

Line 2, characters 13-39:
2 |   fun Int -> function _ -> assert false
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function's argument has type a,
       whose layout is known only from the GADT pattern match at line 2, characters 6-9.
       That match is not exhaustive, so a caller could reach this argument at a different layout.
       Function arguments and results must be representable independently of any non-exhaustive match.
|}]

(* The same holds for the patterns of optional arguments. *)

type ('a : any) int_witness = Int_only : int int_witness

let optional_refined : type (a : any). ?w:a int_witness -> unit -> a =
  fun ?w:(Int_only = assert false) () -> assert false
[%%expect{|
type ('a : any) int_witness = Int_only : int int_witness
val optional_refined : ('a : any). ?w:'a int_witness -> unit -> 'a = <fun>
|}]

let optional_refined_argument : type (a : any).
  ?w:a int_witness -> a -> unit -> a =
  fun ?w:(Int_only = assert false) _x () -> assert false
[%%expect{|
Line 3, characters 35-37:
3 |   fun ?w:(Int_only = assert false) _x () -> assert false
                                       ^^
Error: This function's argument has type a,
       whose layout is known only from the GADT pattern match at line 3, characters 10-18.
       That pattern matches an optional argument that a caller could omit, so a caller could reach this argument at a different layout.
       Function arguments and results must be representable independently of the patterns of optional arguments.
|}]

(* Raise primitives are recognised through module paths, including functor
   parameters and first-class modules, and a module can only provide such a
   primitive with a primitive. *)

module type Raise = sig
  external r : ('a : any). exn -> 'a = "%raise"
end

module Through_functor (X : Raise) = struct
  let f () = X.r Exit
end

let through_first_class (module X : Raise) () = X.r Exit
[%%expect{|
module type Raise = sig external r : ('a : any). exn -> 'a = "%raise" end
module Through_functor :
  functor (X : Raise) -> sig val f : ('a : any). unit -> 'a end
val through_first_class : ('a : any). (module Raise) -> unit -> 'a = <fun>
|}]

module Not_a_primitive : sig
  val r : ('a : any). exn -> 'a
end = struct
  external r : ('a : any). exn -> 'a = "%raise"
end

module Bad = Through_functor (Not_a_primitive)
[%%expect{|
module Not_a_primitive : sig val r : ('a : any). exn -> 'a end
Line 7, characters 13-46:
7 | module Bad = Through_functor (Not_a_primitive)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match: sig val r : ('a : any). exn -> 'a end
     is not included in Raise
     Values do not match:
       val r : ('a : any). exn -> 'a
     is not included in
       external r : ('a : any). exn -> 'a = "%raise"
     The implementation is not a primitive.
|}]

module Returns (X : sig external r : exn -> int = "%raise" end) = struct
  let f () = X.r Exit
end

module Bad_returns = Returns (struct let r (_ : exn) = 0 end)
[%%expect{|
module Returns :
  functor (X : sig external r : exn -> int = "%raise" end) ->
    sig val f : unit -> int end
Line 5, characters 21-61:
5 | module Bad_returns = Returns (struct let r (_ : exn) = 0 end)
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match: sig val r : exn -> int end is not included in
       sig external r : exn -> int = "%raise" end
     Values do not match:
       val r : exn -> int
     is not included in
       external r : exn -> int = "%raise"
     The implementation is not a primitive.
|}]

(* An existential type still cannot escape. *)

type packed = Pack : ('a : any). (unit -> 'a) -> packed

let escape = function
  | Pack (type (a : any)) (_ : unit -> a) -> (assert false : a)
[%%expect{|
type packed = Pack : ('a : any). (unit -> 'a) -> packed
Line 4, characters 45-63:
4 |   | Pack (type (a : any)) (_ : unit -> a) -> (assert false : a)
                                                 ^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" but an expression was expected of type "'a"
       The type constructor "a" would escape its scope
|}]

(* A weak type variable of layout [any] can be fixed later, but not left
   ungeneralised. *)

let weak = ref (fun () -> assert false)
[%%expect{|
val weak : (unit -> '_weak1) ref = {contents = <fun>}
|}]

let fixed : unit -> t_any_abstract = !weak
[%%expect{|
val fixed : unit -> t_any_abstract = <fun>
|}]

module type Weak = module type of struct
  let weak = ref (fun () -> assert false)
end
[%%expect{|
Lines 1-3, characters 34-3:
1 | ..................................struct
2 |   let weak = ref (fun () -> assert false)
3 | end
Error: The type of this module, sig val weak : (unit -> '_weak2) ref end,
       contains non-generalizable type variable(s).
       (see manual section 6.1.2)
Line 2, characters 6-10:
2 |   let weak = ref (fun () -> assert false)
          ^^^^
  The type of this value, "(unit -> '_weak2) ref",
  contains the non-generalizable type variable(s) "'_weak2".
|}]

(* Erasing an optional argument creates a function that calls the original
   one, so its result must be representable. *)

let optional_never ?(x = 0) () = ignore x; assert false
[%%expect{|
val optional_never : ('a : any). ?x:int -> unit -> 'a = <fun>
|}]

let erased (type a : any) () =
  let accept (_ : unit -> a) = () in
  accept optional_never
[%%expect{|
Line 3, characters 9-23:
3 |   accept optional_never
             ^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

(* Raise primitives are recognised by identity after typing, not by spelling:
   [|>] and [@@] are applications of their argument, an alias of the primitive
   is not. An over-application never returns either, but its result comes from
   applying the primitive's result, so it must be representable. *)

module Raise_alias = struct
  external r : ('a : any). exn -> 'a = "%raise"
end

let through_pipe () = Exit |> raise_any
let through_apply () = raise_any @@ Exit
let through_path () = Raise_alias.r Exit
let through_open () = let open Raise_alias in r Exit
let over_applied () = raise_any Exit ()
[%%expect{|
module Raise_alias : sig external r : ('a : any). exn -> 'a = "%raise" end
val through_pipe : ('a : any). unit -> 'a = <fun>
val through_apply : ('a : any). unit -> 'a = <fun>
val through_path : ('a : any). unit -> 'a = <fun>
val through_open : ('a : any). unit -> 'a = <fun>
Line 9, characters 37-39:
9 | let over_applied () = raise_any Exit ()
                                         ^^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

val over_applied : unit -> 'a = <fun>
|}]

let aliased () =
  let r = raise_any in
  r Exit
[%%expect{|
val aliased : unit -> 'a = <fun>
|}]

let shadowed () =
  let raise_any (_ : exn) = 0 in
  raise_any Exit
[%%expect{|
val shadowed : unit -> int = <fun>
|}]

(* The continuation of a binding operator is not exempt. *)

let ( let* ) () (_ : unit -> t_any_abstract) = ()

let binding_operator () =
  let* () = () in
  (assert false : t_any_abstract)
[%%expect{|
val ( let* ) : unit -> (unit -> t_any_abstract) -> unit = <fun>
Line 4, characters 2-6:
4 |   let* () = () in
      ^^^^
Error: The operator "let*" has type "unit -> (unit -> t_any_abstract) -> unit"
       but it was expected to have type "unit -> (unit -> 'a) -> 'b"
       The layout of t_any_abstract is any
         because of the definition of t_any_abstract at line 1, characters 0-25.
       But the layout of t_any_abstract must be representable
         because we must know concretely how to return a function result.
|}]

(* Guards do not matter, only the right-hand sides do. *)

let all_guarded = function
  | () when Sys.opaque_identity false -> assert false
[%%expect{|
Lines 1-2, characters 18-53:
1 | ..................function
2 |   | () when Sys.opaque_identity false -> assert false
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  All clauses in this pattern-matching are guarded.

val all_guarded : ('a : any). unit -> 'a = <fun>
|}]

let raising_guard = function
  | () when (raise_any Exit : bool) -> 1
  | () -> assert false
[%%expect{|
val raising_guard : unit -> int = <fun>
|}]

(* A raise primitive's result may be [@local_opt]. *)

external raise_local_opt : ('a : any). exn -> ('a [@local_opt]) = "%raise"

let global_result (type a : any) () : a = raise_local_opt Exit

let local_result (type a : any) () : a @ local =
  exclave_ raise_local_opt Exit
[%%expect{|
external raise_local_opt : ('a : any). exn -> ('a [@local_opt]) = "%raise"
val global_result : ('a : any). unit -> 'a = <fun>
val local_result : ('a : any). unit -> 'a @ local = <fun>
|}]

(* Classes. *)

class stops = object
  method stop : ('a : any). unit -> 'a = fun () -> assert false
end
[%%expect{|
class stops : object method stop : ('a : any). unit -> 'a end
|}]

let fail () = raise_any Exit

class bad_override = object
  inherit stops
  method! stop : ('a : any). unit -> 'a = fun () -> fail ()
end
[%%expect{|
val fail : ('a : any). unit -> 'a = <fun>
Line 5, characters 42-59:
5 |   method! stop : ('a : any). unit -> 'a = fun () -> fail ()
                                              ^^^^^^^^^^^^^^^^^
Error: This method has type "unit -> 'b" which is less general than
         "('a : any). unit -> 'a"
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because we must know concretely how to return a function result.
|}]
