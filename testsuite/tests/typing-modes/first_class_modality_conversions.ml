(* TEST
 flags = "-extension mode_alpha -w -220";
 { expect; }
 { flags += " -no-ikinds"; expect; }
*)

type t
let wrap (x : t @ portable) : (t @@ portable) = x
let unwrap (x : (t @@ portable)) : t @ portable = x
let identity_wrap (x : t) : (t @@ nonportable) = x
let identity_unwrap (x : (t @@ nonportable)) : t = x
[%%expect{|
type t
val wrap : t @ portable -> (t @@ portable) = <fun>
val unwrap : (t @@ portable) -> t @ portable = <fun>
val identity_wrap : t -> (t @@ nonportable) = <fun>
val identity_unwrap : (t @@ nonportable) -> t = <fun>
|}]

let ambiguous_unwrap (x : ('a @@ global)) : 'a = x
[%%expect{|
Line 1, characters 49-50:
1 | let ambiguous_unwrap (x : ('a @@ global)) : 'a = x
                                                     ^
Error: The value "x" has type "('a @@ global)"
       but an expression was expected of type "'a"
       The type variable "'a" occurs inside "('a @@ global)"
|}]

let unwrap (type a) (x : (a @@ global)) : a = x
let wrap (type a) (x : a @ global) : (a @@ global) = x
let peel (x : ((t @@ global) @@ global)) : (t @@ global) = unwrap x
[%%expect{|
val unwrap : ('a @@ global) -> 'a = <fun>
val wrap : 'a -> ('a @@ global) = <fun>
val peel : ((t @@ global) @@ global) -> (t @@ global) = <fun>
|}]

let eta : (string @@ global) -> string = fun x -> x
[%%expect{|
val eta : (string @@ global) -> string = <fun>
|}]

let no_arrow_conversion : (string @@ global) -> string = Fun.id
[%%expect{|
Line 1, characters 57-63:
1 | let no_arrow_conversion : (string @@ global) -> string = Fun.id
                                                             ^^^^^^
Error: The value "Fun.id" has type "(string @@ global) -> (string @@ global)"
       but an expression was expected of type "(string @@ global) -> string"
       Type "(string @@ global)" is not compatible with type "string"
|}]

let literal : (int @@ global) = 42
let tuple : ((int * int) @@ global) = 1, 2
let function_ : ((int -> int) @@ global) = fun x -> x + 1
let applied = function_ 3
let option : (int option @@ global) = Some 4
let ignored : (unit @@ global) = ignore 42
[%%expect{|
val literal : (int @@ global) @@ stateless = 42
val tuple : (int * int @@ global) @@ stateless = (1, 2)
val function_ : (int -> int @@ global) = <fun>
val applied : int = 4
val option : (int option @@ global) @@ stateless = Some 4
val ignored : (unit @@ global) @@ stateless = ()
|}]

let tuple_pattern (((x, y) as whole) : ((int * int) @@ global)) =
  x + y, whole
let option_pattern (x : (int option @@ global)) =
  match x with Some n -> n | None -> 0
let nested_pattern (x : ((int option @@ global) @@ global)) =
  match x with Some n -> n | None -> 0
[%%expect{|
val tuple_pattern : (int * int @@ global) -> int * (int * int @@ global) =
  <fun>
val option_pattern : (int option @@ global) -> int = <fun>
val nested_pattern : ((int option @@ global) @@ global) -> int = <fun>
|}]

let let_alias (x : ((int * int) @@ global)) =
  let ((_, _) as whole) = x in
  whole
let variable_pattern (x : ((int * int) @@ global)) =
  match x with whole -> whole
[%%expect{|
val let_alias : (int * int @@ global) -> int * int = <fun>
val variable_pattern : (int * int @@ global) -> (int * int @@ global) = <fun>
|}]

let no_list_conversion (xs : (t @@ portable) list) : t list = xs
[%%expect{|
Line 1, characters 62-64:
1 | let no_list_conversion (xs : (t @@ portable) list) : t list = xs
                                                                  ^^
Error: The value "xs" has type "(t @@ portable) list"
       but an expression was expected of type "t list"
       Type "(t @@ portable)" is not compatible with type "t"
|}]

let no_nested_conversion (x : ((t @@ global) @@ global)) : (t @@ global) = x
[%%expect{|
Line 1, characters 75-76:
1 | let no_nested_conversion (x : ((t @@ global) @@ global)) : (t @@ global) = x
                                                                               ^
Error: The value "x" has type "((t @@ global) @@ global)"
       but an expression was expected of type "(t @@ global)"
       Type "(t @@ global)" is not compatible with type "t"
|}]

let no_label_conversion (x : (t @@ global)) : (t @@ global portable) = x
[%%expect{|
Line 1, characters 71-72:
1 | let no_label_conversion (x : (t @@ global)) : (t @@ global portable) = x
                                                                           ^
Error: The value "x" has type "(t @@ global)"
       but an expression was expected of type "(t @@ global portable)"
|}]

(* Introduction must actually establish the requested modes. *)
let infer_portable (x : t) : (t @@ portable) = x
[%%expect{|
val infer_portable : t @ portable -> (t @@ portable) = <fun>
|}]

let bad_portable (x : t @ nonportable) : (t @@ portable) = x
[%%expect{|
Line 1, characters 59-60:
1 | let bad_portable (x : t @ nonportable) : (t @@ portable) = x
                                                               ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

let bad_global (x : t @ local) : (t @@ global) = x
[%%expect{|
Line 1, characters 49-50:
1 | let bad_global (x : t @ local) : (t @@ global) = x
                                                     ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

type mutable_record = { mutable field : int }
let bad_contention (x : (mutable_record @@ contended)) = x.field
[%%expect{|
type mutable_record = { mutable field : int; }
Line 2, characters 57-58:
2 | let bad_contention (x : (mutable_record @@ contended)) = x.field
                                                             ^
Error: This value is "contended"
       but is expected to be "shared" or "uncontended"
         because its mutable field "field" is being read.
|}]

let consume_unique (_ : t @ unique) = ()
let bad_aliasing (x : (t @@ aliased) @ unique) = consume_unique x
[%%expect{|
val consume_unique : t @ unique -> unit = <fun>
Line 2, characters 64-65:
2 | let bad_aliasing (x : (t @@ aliased) @ unique) = consume_unique x
                                                                    ^
Error: This value is "aliased" but is expected to be "unique".
|}]

let bad_many (x : t @ once) : (t @@ many) = x
[%%expect{|
Line 1, characters 44-45:
1 | let bad_many (x : t @ once) : (t @@ many) = x
                                                ^
Error: This value is "once" but is expected to be "many".
|}]

(* Wrappers neither hide missing cases nor create impossible cases. *)
let incomplete (x : (int option @@ global)) =
  match x with Some n -> n
[%%expect{|
Line 2, characters 2-26:
2 |   match x with Some n -> n
      ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "None"

val incomplete : (int option @@ global) -> int = <fun>
|}]

type empty = |
let impossible (x : (empty @@ global)) = match x with _ -> .
[%%expect{|
type empty = |
val impossible : (empty @@ global) -> 'a = <fun>
|}]

let invalid_refutation (x : (bool @@ global)) = match x with _ -> .
[%%expect{|
Line 1, characters 61-62:
1 | let invalid_refutation (x : (bool @@ global)) = match x with _ -> .
                                                                 ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "_"
|}]

(* Ordinary checking is head-directed; principal checking diagnoses the
   conversion selected using information from an earlier use. *)
let require_list (_ : t list) = ()
let require_wrapped (_ : (t @@ global)) = ()
let plain_first (x : 'a) (xs : 'a list) =
  require_list xs;
  require_wrapped x
[%%expect{|
val require_list : t list -> unit = <fun>
val require_wrapped : (t @@ global) -> unit = <fun>
val plain_first : t -> t list -> unit = <fun>
|}, Principal{|
val require_list : t list -> unit = <fun>
val require_wrapped : (t @@ global) -> unit = <fun>
Line 5, characters 18-19:
5 |   require_wrapped x
                      ^
Warning 18 [not-principal]: this implicit modality conversion is not
  principal.

val plain_first : t -> t list -> unit = <fun>
|}]

let wrapped_first (x : 'a) (xs : 'a list) =
  require_wrapped x;
  require_list xs
[%%expect{|
Line 3, characters 15-17:
3 |   require_list xs
                   ^^
Error: The value "xs" has type "(t @@ global) list"
       but an expression was expected of type "t list"
       Type "(t @@ global)" is not compatible with type "t"
|}]

let annotated (x : t) (xs : t list) =
  require_wrapped x;
  require_list xs
[%%expect{|
val annotated : t -> t list -> unit = <fun>
|}]

let wrapped_identity = ((fun x -> x) : (('a -> 'a) @@ global))
let two_uses = wrapped_identity 1, wrapped_identity true
let bare_identity : 'a -> 'a = wrapped_identity
let two_bare_uses = bare_identity 1, bare_identity true
[%%expect{|
val wrapped_identity : ('a -> 'a @@ global) = <fun>
val two_uses : int * bool = (1, true)
val bare_identity : 'a -> 'a = <fun>
val two_bare_uses : int * bool = (1, true)
|}]

let wrapped_ref = (ref None : (_ option ref @@ global))
let int_ref : int option ref = wrapped_ref
[%%expect{|
val wrapped_ref : ('_weak1 option ref @@ global) = {contents = None}
val int_ref : int option ref @@ stateless = {contents = None}
|}]

let bool_ref : bool option ref = wrapped_ref
[%%expect{|
Line 1, characters 33-44:
1 | let bool_ref : bool option ref = wrapped_ref
                                     ^^^^^^^^^^^
Error: The value "wrapped_ref" has type "int option ref"
       but an expression was expected of type "bool option ref"
       Type "int" is not compatible with type "bool"
|}]

let partial (f : ((int -> int -> int) @@ global)) = f 1
let optional (f : ((?x:int -> int -> int) @@ global)) = f 1
[%%expect{|
val partial : (int -> int -> int @@ global) -> int -> int = <fun>
val optional : (?x:int -> int -> int @@ global) -> int = <fun>
|}]

let omitted_direct (f : x:int -> y:int -> int) =
  (f ~y:2 : (_ @@ global))
[%%expect{|
Line 2, characters 3-9:
2 |   (f ~y:2 : (_ @@ global))
       ^^^^^^
Error: This expression has type "x:int -> int"
       but an expression was expected of type "('a @@ global)"
Hint: This function application is partial, maybe some arguments are missing.
|}]

let omitted (f : x:int -> y:int -> int)
    : ((x:int -> int) @@ global) =
  let partial = f ~y:2 in
  partial
let omitted_annotated (f : x:int -> y:int -> int) =
  ((f ~y:2 : x:int -> int) : (_ @@ global))
let omitted_result = (omitted (fun ~x ~y -> x + y)) ~x:3
let annotated_result = (omitted_annotated (fun ~x ~y -> x + y)) ~x:3
[%%expect{|
val omitted : (x:int -> y:int -> int) -> (x:int -> int @@ global) = <fun>
val omitted_annotated : (x:int -> y:int -> int) -> (x:int -> int @@ global) =
  <fun>
val omitted_result : int = 5
val annotated_result : int = 5
|}]

let bad_partial (f : (x:int -> y:int -> int) @ local)
    : ((x:int -> int) @@ global) =
  let partial = f ~y:2 in
  partial
[%%expect{|
Line 4, characters 2-9:
4 |   partial
      ^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

let use_local ~(y : int) ~(x : t @ local) = let _ = x in y
let bad_partial_arg (x : t @ local) : ((y:int -> int) @@ global) =
  let partial = use_local ~x in
  partial
[%%expect{|
val use_local : y:int -> x:t @ local -> int = <fun>
Line 4, characters 2-9:
4 |   partial
      ^^^^^^^
Error: This value is "local" but is expected to be "global".
|}]

let use_once ~(y : int) ~(x : t @ once) = let _ = x in y
let bad_partial_once (x : t @ once) : ((y:int -> int) @@ many) =
  let partial = use_once ~x in
  partial
[%%expect{|
val use_once : y:int -> x:t @ once -> int = <fun>
Line 4, characters 2-9:
4 |   partial
      ^^^^^^^
Error: This value is "once" but is expected to be "many".
|}]

let result_wrapper x : ((int -> int) @@ global) = fun y -> x + y
let explicit_intermediate = (result_wrapper 1) 2
[%%expect{|
val result_wrapper : int -> (int -> int @@ global) = <fun>
val explicit_intermediate : int = 3
|}]
