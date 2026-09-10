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

(* Explicit coercions preserve the modes of values in covariant containers. *)
let subtype_portable_list (type a)
    (xs : (a @@ portable) list @ nonportable) : a list @ portable =
  (xs :> a list)
[%%expect{|
val subtype_portable_list : ('a @@ portable) list -> 'a list @ portable =
  <fun>
|}]

let subtype_portable_list_explicit (type a)
    (xs : (a @@ portable) list @ portable) : a list @ portable =
  (xs : (a @@ portable) list :> a list)
[%%expect{|
val subtype_portable_list_explicit :
  ('a @@ portable) list @ portable -> 'a list @ portable = <fun>
|}]

let subtype_nested (type a)
    (xs : (a @@ portable) option list option @ portable)
    : a option list option @ portable =
  (xs :> a option list option)
[%%expect{|
val subtype_nested :
  ('a @@ portable) option list option @ portable ->
  'a option list option @ portable = <fun>
|}]

type +'a covariant_box = Box of 'a
let subtype_covariant_box (type a)
    (xs : (a @@ portable) covariant_box list @ portable)
    : a covariant_box list @ portable =
  (xs :> a covariant_box list)
[%%expect{|
type 'a covariant_box = Box of 'a
val subtype_covariant_box :
  ('a @@ portable) covariant_box list @ portable ->
  'a covariant_box list @ portable = <fun>
|}]

let subtype_tuple (type a b)
    (xs : ((a @@ portable) * (b @@ portable)) list @ portable)
    : (a * b) list @ portable =
  (xs :> (a * b) list)
[%%expect{|
val subtype_tuple :
  (('a @@ portable) * ('b @@ portable)) list @ portable ->
  ('a * 'b) list @ portable = <fun>
|}]

let subtype_global_list (type a) (xs : (a @@ global) list)
    : (a @@ aliased) list =
  (xs :> (a @@ aliased) list)
[%%expect{|
val subtype_global_list : ('a @@ global) list -> ('a @@ aliased) list = <fun>
|}]

(* [global] implies [aliased], which cannot be forgotten. *)
let bad_subtype_global_aliasing (type a) (xs : (a @@ global) list) =
  (xs :> a list)
[%%expect{|
Line 2, characters 2-16:
2 |   (xs :> a list)
      ^^^^^^^^^^^^^^
Error: Type "(a @@ global) list" is not a subtype of "a list"
       Type "(a @@ global)" is not a subtype of "a"
|}]

let subtype_weaken_modalities (type a)
    (xs : (a @@ global portable) list)
    : (a @@ global) list =
  (xs :> (a @@ global) list)
[%%expect{|
val subtype_weaken_modalities :
  ('a @@ global portable) list -> ('a @@ global) list = <fun>
|}]

let subtype_identity_modalities (type a)
    (xs : (a @@ nonportable) list) : a list =
  (xs :> a list)
let subtype_introduce_identity (type a) (xs : a list)
    : (a @@ nonportable) list =
  (xs :> (a @@ nonportable) list)
[%%expect{|
val subtype_identity_modalities : ('a @@ nonportable) list -> 'a list = <fun>
val subtype_introduce_identity : 'a list -> ('a @@ nonportable) list = <fun>
|}]

let subtype_nested_modalities (type a)
    (xs : ((a @@ portable) @@ global) list @ portable)
    : (a @@ aliased) list @ portable =
  (xs :> (a @@ aliased) list)
let subtype_nested_weakening (type a)
    (xs : ((a @@ portable) @@ global) list)
    : ((a @@ nonportable) @@ global) list =
  (xs :> ((a @@ nonportable) @@ global) list)
[%%expect{|
val subtype_nested_modalities :
  (('a @@ portable) @@ global) list @ portable ->
  ('a @@ aliased) list @ portable = <fun>
val subtype_nested_weakening :
  (('a @@ portable) @@ global) list -> (('a @@ nonportable) @@ global) list =
  <fun>
|}]

let subtype_combine_modalities (type a)
    (xs : ((a @@ portable) @@ global) list)
    : (a @@ global portable) list =
  (xs :> (a @@ global portable) list)
[%%expect{|
val subtype_combine_modalities :
  (('a @@ portable) @@ global) list -> ('a @@ global portable) list = <fun>
|}]

let subtype_split_modalities (type a)
    (xs : (a @@ global portable) list)
    : ((a @@ portable) @@ global) list =
  (xs :> ((a @@ portable) @@ global) list)
[%%expect{|
val subtype_split_modalities :
  ('a @@ global portable) list -> (('a @@ portable) @@ global) list = <fun>
|}]

type +'a portable_alias = ('a @@ portable)
let subtype_alias (type a)
    (xs : a portable_alias option list @ portable)
    : a option list @ portable =
  (xs :> a option list)
[%%expect{|
type 'a portable_alias = ('a @@ portable)
val subtype_alias :
  'a portable_alias option list @ portable -> 'a option list @ portable =
  <fun>
|}]

(* The full coercion form also supports free type variables. *)
let subtype_free_variables_explicit (xs : ('a @@ portable) list) =
  (xs : ('a @@ portable) list :> 'a list)
[%%expect{|
val subtype_free_variables_explicit : ('a @@ portable) list -> 'a list =
  <fun>
|}]

let subtype_free_variables_alias (xs : 'a portable_alias list) =
  (xs : 'a portable_alias list :> 'a list)
[%%expect{|
val subtype_free_variables_alias : 'a portable_alias list -> 'a list = <fun>
|}]

let bad_subtype_free_variables_portable (xs : 'a list @ nonportable) =
  (xs : 'a list :> ('a @@ portable) list)
[%%expect{|
Line 2, characters 2-41:
2 |   (xs : 'a list :> ('a @@ portable) list)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "'a list" is not a subtype of "('a @@ portable) list"
       Type "'a" is not a subtype of "('a @@ portable)"
|}]

let subtype_unknown_target (type a) (xs : (a @@ portable) list) =
  (xs : (a @@ portable) list :> _ list)
[%%expect{|
val subtype_unknown_target : ('a @@ portable) list -> ('a @@ portable) list =
  <fun>
|}]

(* Shared unknown targets retain exact wrappers in either tuple order. *)
let bad_subtype_shared_target_left
    (xs : ('a @@ global portable) * ('a @@ global)) =
  (xs : ('a @@ global portable) * ('a @@ global) :> 'b * 'b)
[%%expect{|
Line 3, characters 2-60:
3 |   (xs : ('a @@ global portable) * ('a @@ global) :> 'b * 'b)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "('a @@ global portable) * ('a @@ global)" is not a subtype of
         "('a @@ global portable) * ('a @@ global portable)"
       Type "('a @@ global)" is not a subtype of "('a @@ global portable)"
|}]

let bad_subtype_shared_target_right
    (xs : ('a @@ global) * ('a @@ global portable)) =
  (xs : ('a @@ global) * ('a @@ global portable) :> 'b * 'b)
[%%expect{|
Line 3, characters 2-60:
3 |   (xs : ('a @@ global) * ('a @@ global portable) :> 'b * 'b)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "('a @@ global) * ('a @@ global portable)" is not a subtype of
         "('a @@ global) * ('a @@ global)"
       Type "('a @@ global portable)" is not a subtype of "('a @@ global)"
|}]

(* Equality in another component can identify the wrapper's payload. *)
let subtype_equal_payloads_left (xs : 'a * ('a @@ portable)) =
  (xs : 'a * ('a @@ portable) :> 'b * 'b)
[%%expect{|
val subtype_equal_payloads_left : 'b * ('b @@ portable) -> 'b * 'b = <fun>
|}]

let subtype_equal_payloads_right (xs : ('a @@ portable) * 'a) =
  (xs : ('a @@ portable) * 'a :> 'b * 'b)
[%%expect{|
val subtype_equal_payloads_right : ('b @@ portable) * 'b -> 'b * 'b = <fun>
|}]

module Private_payload : sig
  type +'a t = private 'a
end = struct
  type 'a t = 'a
end
[%%expect{|
module Private_payload : sig type +'a t = private 'a end @@ stateless
|}]

let bad_subtype_private_target (type a)
    (xs : (a @@ portable) list) : a Private_payload.t list =
  (xs : (a @@ portable) list :> a Private_payload.t list)
[%%expect{|
Line 3, characters 2-57:
3 |   (xs : (a @@ portable) list :> a Private_payload.t list)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(a @@ portable) list" is not a subtype of "a Private_payload.t list"
       Type "a" is not a subtype of "a Private_payload.t"
|}]

module Private_portable : sig
  type +'a t = private ('a @@ portable)
end = struct
  type 'a t = ('a @@ portable)
end
[%%expect{|
module Private_portable : sig type +'a t = private ('a @@ portable) end @@
  stateless
|}]

let subtype_private_source (type a)
    (xs : a Private_portable.t list @ portable) : a list @ portable =
  (xs :> a list)
[%%expect{|
val subtype_private_source :
  'a Private_portable.t list @ portable -> 'a list @ portable = <fun>
|}]

let bad_subtype_private_portable_target (type a)
    (xs : (a @@ portable) list) : a Private_portable.t list =
  (xs : (a @@ portable) list :> a Private_portable.t list)
[%%expect{|
Line 3, characters 2-58:
3 |   (xs : (a @@ portable) list :> a Private_portable.t list)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(a @@ portable) list" is not a subtype of
         "a Private_portable.t list"
       Type "a" is not a subtype of "a Private_portable.t"
|}]

let bad_subtype_distinct_payloads (type a b)
    (xs : (a @@ portable) list) : b list =
  (xs :> b list)
[%%expect{|
Line 3, characters 2-16:
3 |   (xs :> b list)
      ^^^^^^^^^^^^^^
Error: Type "(a @@ portable) list" is not a subtype of "b list"
       Type "a" is not a subtype of "b"
|}]

(* Function arguments reverse the direction of the conversion. *)
let subtype_consumer (type a) (f : a -> unit)
    : (a @@ portable) -> unit =
  (f :> (a @@ portable) -> unit)
[%%expect{|
val subtype_consumer : ('a -> unit) -> ('a @@ portable) -> unit = <fun>
|}]

let subtype_contended_consumer (type a) (f : (a @@ contended) -> unit)
    : a -> unit =
  (f :> a -> unit)
[%%expect{|
val subtype_contended_consumer : (('a @@ contended) -> unit) -> 'a -> unit =
  <fun>
|}]

let bad_subtype_consumer (type a) (f : (a @@ portable) -> unit)
    : a -> unit =
  (f :> a -> unit)
[%%expect{|
Line 3, characters 2-18:
3 |   (f :> a -> unit)
      ^^^^^^^^^^^^^^^^
Error: Type "(a @@ portable) -> unit" is not a subtype of "a -> unit"
       Type "a" is not a subtype of "(a @@ portable)"
|}]

(* Mutation would allow writing a value without the stored guarantee. *)
let bad_subtype_ref (type a) (xs : (a @@ portable) ref) : a ref =
  (xs :> a ref)
[%%expect{|
Line 2, characters 2-15:
2 |   (xs :> a ref)
      ^^^^^^^^^^^^^
Error: Type "(a @@ portable) ref" is not a subtype of "a ref"
|}]

let bad_subtype_array (type a) (xs : (a @@ portable) array) : a array =
  (xs :> a array)
[%%expect{|
Line 2, characters 2-17:
2 |   (xs :> a array)
      ^^^^^^^^^^^^^^^
Error: Type "(a @@ portable) array" is not a subtype of "a array"
|}]

type (_, _) equality = Refl : ('a, 'a) equality
let bad_subtype_equality (type a)
    (witness : ((a @@ portable), (a @@ portable)) equality)
    : (a, (a @@ portable)) equality =
  (witness :> (a, (a @@ portable)) equality)
[%%expect{|
type (_, _) equality = Refl : ('a, 'a) equality
Line 5, characters 2-44:
5 |   (witness :> (a, (a @@ portable)) equality)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "((a @@ portable), (a @@ portable)) equality" is not a subtype of
         "(a, (a @@ portable)) equality"
|}]

(* Coercions cannot manufacture portability or global allocation. *)
let bad_subtype_portable_list (type a) (xs : a list @ nonportable)
    : (a @@ portable) list =
  (xs :> (a @@ portable) list)
[%%expect{|
Line 3, characters 2-30:
3 |   (xs :> (a @@ portable) list)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "a list" is not a subtype of "(a @@ portable) list"
       Type "a" is not a subtype of "(a @@ portable)"
|}]

let bad_subtype_strengthen_modalities (type a)
    (xs : (a @@ global) list @ nonportable)
    : (a @@ global portable) list =
  (xs :> (a @@ global portable) list)
[%%expect{|
Line 4, characters 2-37:
4 |   (xs :> (a @@ global portable) list)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(a @@ global) list" is not a subtype of
         "(a @@ global portable) list"
       Type "(a @@ global)" is not a subtype of "(a @@ global portable)"
|}]

let bad_subtype_global_list (type a) (xs : a list @ local)
    : (a @@ global) list @ local =
  (xs :> (a @@ global) list)
[%%expect{|
Line 3, characters 2-28:
3 |   (xs :> (a @@ global) list)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "a list" is not a subtype of "(a @@ global) list"
       Type "a" is not a subtype of "(a @@ global)"
|}]

let bad_subtype_uncontended (type a)
    (xs : (a @@ contended) list) : a list =
  (xs :> a list)
[%%expect{|
Line 3, characters 2-16:
3 |   (xs :> a list)
      ^^^^^^^^^^^^^^
Error: Type "(a @@ contended) list" is not a subtype of "a list"
       Type "(a @@ contended)" is not a subtype of "a"
|}]

let subtype_introduce_contended (type a) (xs : a list)
    : (a @@ contended) list =
  (xs :> (a @@ contended) list)
[%%expect{|
val subtype_introduce_contended : 'a list -> ('a @@ contended) list = <fun>
|}]

let bad_subtype_nested_contention (type a)
    (xs : ((a @@ contended) @@ portable) list) : a list =
  (xs :> a list)
[%%expect{|
Line 3, characters 2-16:
3 |   (xs :> a list)
      ^^^^^^^^^^^^^^
Error: Type "((a @@ contended) @@ portable) list" is not a subtype of "a list"
       Type "((a @@ contended) @@ portable)" is not a subtype of "a"
|}]

let bad_subtype_unique (type a) (xs : (a @@ aliased) list @ unique)
    : a list @ unique =
  (xs :> a list)
[%%expect{|
Line 3, characters 2-16:
3 |   (xs :> a list)
      ^^^^^^^^^^^^^^
Error: Type "(a @@ aliased) list" is not a subtype of "a list"
       Type "(a @@ aliased)" is not a subtype of "a"
|}]

let subtype_introduce_aliasing (type a) (xs : a list)
    : (a @@ aliased) list =
  (xs :> (a @@ aliased) list)
[%%expect{|
val subtype_introduce_aliasing : 'a list -> ('a @@ aliased) list = <fun>
|}]
