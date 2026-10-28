(* TEST
   expect;
*)

(*************************************)
(* Positions of dereference patterns *)

(* Dereference patterns in boxed tuples bind immutable addresses *)
let tuple (deref_ a, deref_ b) = #(a, b)
[%%expect{|
val tuple : 'a * 'b -> #('a addr_imm * 'b addr_imm) = <fun>
|}]

(* Addresses into boxed records have the mutability of the field *)
type r = { imm : int; mutable mut : string }
let record { imm = deref_ a; mut = deref_ b } = #(a, b)
[%%expect{|
type r = { imm : int; mutable mut : string; }
val record : r -> #(int addr_imm * string addr) = <fun>
|}]

(* Addresses below address patterns have the mutability of the address *)
let addr (addr_ (deref_ a)) = a
let addr_imm (addr_imm_ (deref_ a)) = a
[%%expect{|
val addr : 'a addr -> 'a addr = <fun>
val addr_imm : 'a addr_imm -> 'a addr_imm = <fun>
|}]

(* Unboxed products keep the mutability of their position *)
type ur = #{ x : int; y : string }
type r2 = { mutable u : ur }
let unboxed_tuple (addr_imm_ #(deref_ a, _)) = a
let unboxed_record { u = #{ x = deref_ a; _ } } = a
[%%expect{|
type ur = #{ x : int; y : string; }
type r2 = { mutable u : ur; }
val unboxed_tuple : #('a * 'b) addr_imm -> 'a addr_imm = <fun>
val unboxed_record : r2 -> int addr = <fun>
|}]

type u_record = { f : int } [@@unboxed]
let unboxed_record_attr (addr_ { f = deref_ a }) = a
[%%expect{|
type u_record = { f : int; } [@@unboxed]
val unboxed_record_attr : u_record addr -> int addr = <fun>
|}]

(* Unboxed products in boxed tuples extend the position of the element *)
let unboxed_tuple_in_tuple (deref_ a, #(deref_ b, deref_ c)) = #(a, b, c)
let unboxed_record_in_tuple (deref_ a, #{ x = deref_ b; y = deref_ c }) =
  #(a, b, c)
[%%expect{|
val unboxed_tuple_in_tuple :
  'a * #('b * 'c) -> #('a addr_imm * 'b addr_imm * 'c addr_imm) = <fun>
val unboxed_record_in_tuple :
  'a * ur -> #('a addr_imm * int addr_imm * string addr_imm) = <fun>
|}]

(* Positions extend through nested unboxed products *)
type nested_inner = #{ ni : string; nj : int }
type nested = { mutable nest : #(nested_inner * int) }
let nested_unboxed { nest = #(#{ ni = deref_ a; _ }, _) } = a
let nested_unboxed_in_tuple (_, #(_, { f = deref_ a })) = a
[%%expect{|
type nested_inner = #{ ni : string; nj : int; }
type nested = { mutable nest : #(nested_inner * int); }
val nested_unboxed : nested -> string addr = <fun>
val nested_unboxed_in_tuple : 'a * #('b * u_record) -> int addr_imm = <fun>
|}]

(* Aliases, constraints and local opens are transparent *)
let transparent ((deref_ a as b), (deref_ c : int), Stdlib.(deref_ d)) =
  #(a, b, c, d)
[%%expect{|
val transparent :
  'a * int * 'b -> #('a addr_imm * 'a * int addr_imm * 'b addr_imm) = <fun>
|}]

(* Dereference patterns are not allowed at the top level... *)
let top (deref_ a) = a
[%%expect{|
Line 1, characters 8-18:
1 | let top (deref_ a) = a
            ^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

(* ...including in unboxed products at the top level *)
let top_unboxed_tuple (#(deref_ a, _) : #(int * int)) = a
[%%expect{|
Line 1, characters 25-33:
1 | let top_unboxed_tuple (#(deref_ a, _) : #(int * int)) = a
                             ^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let top_unboxed_record (#{ x = deref_ a; _ } : ur) = a
[%%expect{|
Line 1, characters 31-39:
1 | let top_unboxed_record (#{ x = deref_ a; _ } : ur) = a
                                   ^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let top_unboxed_record_attr ({ f = deref_ a } : u_record) = a
[%%expect{|
Line 1, characters 35-43:
1 | let top_unboxed_record_attr ({ f = deref_ a } : u_record) = a
                                       ^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

(* Constructors, lazy and arrays don't have positions for their arguments *)
let constr (addr_ (Some (deref_ a))) = a
[%%expect{|
Line 1, characters 24-34:
1 | let constr (addr_ (Some (deref_ a))) = a
                            ^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let variant (`A (deref_ a), _) = a
[%%expect{|
Line 1, characters 16-26:
1 | let variant (`A (deref_ a), _) = a
                    ^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let lazy_ (lazy (deref_ a), _) = a
[%%expect{|
Line 1, characters 16-26:
1 | let lazy_ (lazy (deref_ a), _) = a
                    ^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let array (addr_ [| deref_ a |]) = a
[%%expect{|
Line 1, characters 20-28:
1 | let array (addr_ [| deref_ a |]) = a
                        ^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

(* Neither do unboxed constructors. *)
type u_constr = U of string [@@unboxed]
let unboxed_constr_attr (U (deref_ a), _) = a
[%%expect{|
type u_constr = U of string [@@unboxed]
Line 2, characters 27-37:
2 | let unboxed_constr_attr (U (deref_ a), _) = a
                               ^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

type mut_or_null = { mutable mon : string or_null }
let or_null { mon = This (deref_ a) } = a
[%%expect{|
type mut_or_null = { mutable mon : string or_null; }
Line 2, characters 25-35:
2 | let or_null { mon = This (deref_ a) } = a
                             ^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

(* Or-patterns don't propagate the position... *)
let or_ ((deref_ a | deref_ a), _) = a
[%%expect{|
Line 1, characters 10-18:
1 | let or_ ((deref_ a | deref_ a), _) = a
              ^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

(* ...but are fine around the container *)
let or_around ((`A as x), deref_ a | (`B as x), deref_ a) = #(x, a)
[%%expect{|
val or_around : [< `A | `B ] * 'a -> #([> `A | `B ] * 'a addr_imm) = <fun>
|}]

(* Dereference patterns must bind a variable *)
let bad_addr (deref_ (addr_imm_ x), _) = x
[%%expect{|
Line 1, characters 21-34:
1 | let bad_addr (deref_ (addr_imm_ x), _) = x
                         ^^^^^^^^^^^^^
Error: Dereference patterns must bind a variable, as in "deref_ x".
|}]

let bad_alias (deref_ (a as b), _) = #(a, b)
[%%expect{|
Line 1, characters 22-30:
1 | let bad_alias (deref_ (a as b), _) = #(a, b)
                          ^^^^^^^^
Error: Dereference patterns must bind a variable, as in "deref_ x".
|}]

let bad_constr (deref_ (Some a), _) = a
[%%expect{|
Line 1, characters 23-31:
1 | let bad_constr (deref_ (Some a), _) = a
                           ^^^^^^^^
Error: Dereference patterns must bind a variable, as in "deref_ x".
|}]

(* Not even a wildcard *)
let bad_wildcard (deref_ _, _) = ()
[%%expect{|
Line 1, characters 25-26:
1 | let bad_wildcard (deref_ _, _) = ()
                             ^
Error: Dereference patterns must bind a variable, as in "deref_ x".
|}]

(* In particular, dereference patterns can't be nested *)
let nested (deref_ (deref_ a), _) = a
[%%expect{|
Line 1, characters 19-29:
1 | let nested (deref_ (deref_ a), _) = a
                       ^^^^^^^^^^
Error: Dereference patterns must bind a variable, as in "deref_ x".
|}]

(*************************************************************)
(* Restrictions on the path to the position, as for indices *)

(* Records whose fields are stored flat are not supported... *)
type float_record = { x : float; y : float }
let float_record { x = deref_ a; _ } = a
[%%expect{|
type float_record = { x : float; y : float; }
Line 2, characters 19-20:
2 | let float_record { x = deref_ a; _ } = a
                       ^
Error: Dereference patterns do not support float records.
|}]

type flat_float_record = { f : float; u : float# } [@@flatten_floats]
let flat_float_record { u = deref_ a; _ } = a
[%%expect{|
type flat_float_record = { f : float; u : float#; }
Line 2, characters 24-25:
2 | let flat_float_record { u = deref_ a; _ } = a
                            ^
Error: Dereference patterns do not support [@@flatten_floats] records.
|}]

type float_array_record = { fa : float# } [@@represent_as_float_array]
let float_array_record { fa = deref_ a } = a
[%%expect{|
type float_array_record = { fa : float#; }
Line 2, characters 25-27:
2 | let float_array_record { fa = deref_ a } = a
                             ^^
Error: Dereference patterns do not support [@@represent_as_float_array] records.
|}]

(* ...but mixed records are *)
type mixed_record = { s : string; i : float# }
let mixed_record { i = deref_ a; _ } = a
[%%expect{|
type mixed_record = { s : string; i : float#; }
val mixed_record : mixed_record -> float# addr_imm = <fun>
|}]

(* ...and so are mixed tuples *)
let mixed_tuple ((deref_ a : string), (deref_ b : float#)) = #(a, b)
[%%expect{|
val mixed_tuple : string * float# -> #(string addr_imm * float# addr_imm) =
  <fun>
|}]

(* ...including tuples of unboxed floats *)
let float_tuple ((deref_ a, deref_ b) : float# * float#) = #(a, b)
[%%expect{|
val float_tuple : float# * float# -> #(float# addr_imm * float# addr_imm) =
  <fun>
|}]

(* Inlined records are not supported *)
type inlined = I of { i : string }
let inlined (I { i = deref_ a }) = a
[%%expect{|
type inlined = I of { i : string; }
Line 2, characters 17-18:
2 | let inlined (I { i = deref_ a }) = a
                     ^
Error: Dereference patterns do not support inlined records.
|}]

(* Atomic fields are not supported *)
type atomic = { mutable at : int [@atomic] }
let atomic { at = deref_ a } = a
[%%expect{|
type atomic = { mutable at : int [@atomic]; }
Line 2, characters 13-15:
2 | let atomic { at = deref_ a } = a
                 ^^
Error: Dereference patterns do not support atomic fields.
|}]

(* Polymorphic fields are not supported *)
type poly = { poly : 'a. 'a -> 'a }
let poly { poly = deref_ a } = a
[%%expect{|
type poly = { poly : 'a. 'a -> 'a; }
Line 2, characters 11-15:
2 | let poly { poly = deref_ a } = a
               ^^^^
Error: Dereference patterns do not support polymorphic record fields
       (here "poly").
|}]

(* Private types need to respect mutability *)
module Private : sig
  type t = private { p : string; mutable pm : string }
  type unboxed = private #{ pu : string; other : string }
  type attr_unboxed = private { pa : string } [@@unboxed]
end = struct
  type t = { p : string; mutable pm : string }
  type unboxed = #{ pu : string; other : string }
  type attr_unboxed = { pa : string } [@@unboxed]
end
type has_private =
  { mutable mu : Private.unboxed; mutable ma : Private.attr_unboxed }
[%%expect{|
module Private :
  sig
    type t = private { p : string; mutable pm : string; }
    type unboxed = private #{ pu : string; other : string; }
    type attr_unboxed = private { pa : string; } [@@unboxed]
  end
type has_private = {
  mutable mu : Private.unboxed;
  mutable ma : Private.attr_unboxed;
}
|}]

let private_imm { Private.p = deref_ a; _ } = a
[%%expect{|
val private_imm : Private.t -> string addr_imm = <fun>
|}]

let private_mut { Private.pm = deref_ a; _ } = a
[%%expect{|
Line 1, characters 18-28:
1 | let private_mut { Private.pm = deref_ a; _ } = a
                      ^^^^^^^^^^
Error: Dereference patterns do not support private types in mutable positions.
|}]

(* CR address-patterns: these should be rejected, as they allow mutating the
   contents of private types. *)
let private_unboxed { mu = #{ Private.pu = deref_ a; _ }; _ } = a
[%%expect{|
val private_unboxed : has_private -> string addr = <fun>
|}]

let private_attr_unboxed { ma = { Private.pa = deref_ a }; _ } = a
[%%expect{|
val private_attr_unboxed : has_private -> string addr = <fun>
|}]

(* Addresses to (rather than through) values of private types are fine *)
let to_private { mu = deref_ a; _ } = a
[%%expect{|
val to_private : has_private -> Private.unboxed addr = <fun>
|}]

(* Non-default modalities are not supported... *)
type global_field = { g : string @@ global }
let global_field { g = deref_ a } = a
[%%expect{|
type global_field = { g : string @@ global; }
Line 2, characters 23-31:
2 | let global_field { g = deref_ a } = a
                           ^^^^^^^^
Error: Dereference patterns do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is aliased.
|}]

(* ...including along unboxed paths *)
type unboxed_global = #{ ug : string @@ global; other : string }
type contains_unboxed_global = { u : unboxed_global }
let unboxed_global { u = #{ ug = deref_ a; _ } } = a
[%%expect{|
type unboxed_global = #{ ug : string @@ global; other : string; }
type contains_unboxed_global = { u : unboxed_global; }
Line 3, characters 33-41:
3 | let unboxed_global { u = #{ ug = deref_ a; _ } } = a
                                     ^^^^^^^^
Error: Dereference patterns do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is aliased.
|}]

type unboxed_attr_global = { uag : string @@ global } [@@unboxed]
type contains_unboxed_attr_global = { ua : unboxed_attr_global }
let unboxed_attr_global { ua = { uag = deref_ a } } = a
[%%expect{|
type unboxed_attr_global = { uag : string @@ global; } [@@unboxed]
type contains_unboxed_attr_global = { ua : unboxed_attr_global; }
Line 3, characters 39-47:
3 | let unboxed_attr_global { ua = { uag = deref_ a } } = a
                                           ^^^^^^^^
Error: Dereference patterns do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is aliased.
|}]

(* ...at any depth *)
type contains_nested_unboxed_global = { nu : #(int * unboxed_global) }
let nested_unboxed_global { nu = #(_, #{ ug = deref_ a; _ }) } = a
[%%expect{|
type contains_nested_unboxed_global = { nu : #(int * unboxed_global); }
Line 2, characters 46-54:
2 | let nested_unboxed_global { nu = #(_, #{ ug = deref_ a; _ }) } = a
                                                  ^^^^^^^^
Error: Dereference patterns do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is aliased.
|}]

(* Non-default modalities are fine if they compose to the default *)
type 'a global_u = #{ gu : 'a @@ global; other_gu : int }
type 'a mut_local = { mutable ml : 'a global_u @@ local }
let composed { ml = #{ gu = deref_ a; _ } } = a
[%%expect{|
type 'a global_u = #{ gu : 'a @@ global; other_gu : int; }
type 'a mut_local = { mutable ml : 'a global_u @@ local; }
val composed : 'a mut_local -> 'a addr = <fun>
|}]

type unboxed_contended = #{ uc : string ref @@ contended; other_c : string }
type contains_unboxed_contended_mut = { mutable umc : unboxed_contended }
let unboxed_contended_mut { umc = #{ uc = deref_ a; _ } } = a
[%%expect{|
type unboxed_contended = #{ uc : string ref @@ contended; other_c : string; }
type contains_unboxed_contended_mut = { mutable umc : unboxed_contended; }
Line 3, characters 42-50:
3 | let unboxed_contended_mut { umc = #{ uc = deref_ a; _ } } = a
                                              ^^^^^^^^
Error: Dereference patterns do not yet support non-default modalities. In particular,
       mutable elements must have the identity modality, but this is contended.
|}]

(*********)
(* Modes *)

let use_global_addr_imm : ('a : any). 'a addr_imm @ global -> unit =
  fun _ -> ()
let use_global_addr : ('a : any). 'a addr @ global -> unit = fun _ -> ()
[%%expect{|
val use_global_addr_imm : ('a : any). 'a addr_imm -> unit = <fun>
val use_global_addr : ('a : any). 'a addr -> unit = <fun>
|}]

(* The address has the mode of the container... *)
let bad_local (t : string * string @ local) =
  match t with (deref_ a, _) -> use_global_addr_imm a
[%%expect{|
Line 2, characters 52-53:
2 |   match t with (deref_ a, _) -> use_global_addr_imm a
                                                        ^
Error: This value is "local" to the parent region
         because it is an address derived from the value at line 2, characters 15-28
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

(* ...not the mode of the value at the position, which is global here *)
type mutable_field = { mutable m : string }
let bad_mutable (r : mutable_field @ local) =
  match r with { m = deref_ a } -> use_global_addr a
[%%expect{|
type mutable_field = { mutable m : string; }
Line 3, characters 51-52:
3 |   match r with { m = deref_ a } -> use_global_addr a
                                                       ^
Error: This value is "local" to the parent region
         because it is an address derived from the value at line 3, characters 15-31
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let bad_local_addr (t : string addr @ local) =
  match t with addr_ (deref_ a) -> use_global_addr a
[%%expect{|
Line 2, characters 51-52:
2 |   match t with addr_ (deref_ a) -> use_global_addr a
                                                       ^
Error: This value is "local" to the parent region
         because it is an address derived from the value at line 2, characters 15-31
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

(* Addresses into a tuple expression have the mode of the tuple *)
let bad_escaping_addr (local_ x : string) (y : string) =
  match (x, y) with (deref_ _a, deref_ b) -> b
[%%expect{|
Line 2, characters 45-46:
2 |   match (x, y) with (deref_ _a, deref_ b) -> b
                                                 ^
Error: This value is "local"
         because it is an address derived from the value at line 2, characters 20-41
         which is "local"
         because it is allocated at line 2, characters 8-14 containing data
         which is "local" to the parent region
         because it is a tuple that contains the expression at line 2, characters 9-10
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(**************)
(* Uniqueness *)

let use_addr_imm : ('a : any). 'a addr_imm -> unit = fun _ -> ()
let use_addr : ('a : any). 'a addr -> unit = fun _ -> ()
let use_aliased : 'a -> unit = fun _ -> ()
let consume : 'a @ unique -> unit = fun _ -> ()
let consume_addr_imm : ('a : any). 'a addr_imm @ unique -> unit =
  fun _ -> ()
let consume_addr : ('a : any). 'a addr @ unique -> unit = fun _ -> ()
[%%expect{|
val use_addr_imm : ('a : any). 'a addr_imm -> unit = <fun>
val use_addr : ('a : any). 'a addr -> unit = <fun>
val use_aliased : 'a -> unit = <fun>
val consume : 'a @ unique -> unit = <fun>
val consume_addr_imm : ('a : any). 'a addr_imm @ unique -> unit = <fun>
val consume_addr : ('a : any). 'a addr @ unique -> unit = <fun>
|}]

(* An address is part of its container... *)
let bad_consume_then_use (r : mutable_field @ unique) =
  match r with { m = deref_ a } -> consume r; use_addr a
[%%expect{|
Line 2, characters 55-56:
2 |   match r with { m = deref_ a } -> consume r; use_addr a
                                                           ^
Error: This value is used here, but it has already been used as unique at:
Line 2, characters 43-44:
2 |   match r with { m = deref_ a } -> consume r; use_addr a
                                               ^

|}]

(* ...but taking it doesn't use the container *)
let ok_consume (r : mutable_field @ unique) =
  match r with { m = deref_ _a } -> consume r
[%%expect{|
val ok_consume : mutable_field @ unique -> unit = <fun>
|}]

(* Addresses taken through an address are part of that address *)
let bad_consume_addr (t : string addr @ unique) =
  match t with addr_ (deref_ a) -> consume_addr t; use_addr a
[%%expect{|
Line 2, characters 60-61:
2 |   match t with addr_ (deref_ a) -> consume_addr t; use_addr a
                                                                ^
Error: This value is used here, but it has already been used as unique at:
Line 2, characters 48-49:
2 |   match t with addr_ (deref_ a) -> consume_addr t; use_addr a
                                                    ^

|}]

(* Records in mutable fields are independent of the outer record... *)
type inner = { f : string }
type outer_mut = { mutable om : inner }
type outer_imm = { oi : inner }
let ok_boxed_in_mutable (r : outer_mut @ unique) =
  match r with { om = { f = deref_ a } } -> consume r; use_addr_imm a
[%%expect{|
type inner = { f : string; }
type outer_mut = { mutable om : inner; }
type outer_imm = { oi : inner; }
val ok_boxed_in_mutable : outer_mut @ unique -> unit = <fun>
|}]

(* ...but records in immutable fields are part of it *)
let bad_boxed_in_immutable (r : outer_imm @ unique) =
  match r with { oi = { f = deref_ a } } -> use_addr_imm a; consume r
[%%expect{|
Line 2, characters 68-69:
2 |   match r with { oi = { f = deref_ a } } -> use_addr_imm a; consume r
                                                                        ^
Error: This value is used here as unique,
       but part of it has already been used at:
Line 2, characters 57-58:
2 |   match r with { oi = { f = deref_ a } } -> use_addr_imm a; consume r
                                                             ^

|}]

(* An address owns the value at its position *)
type imm_field = { imm : string }
let bad_consume_value (r : imm_field @ unique) =
  match r with { imm = deref_ a as y } -> consume y; use_addr_imm a
[%%expect{|
type imm_field = { imm : string; }
Line 3, characters 66-67:
3 |   match r with { imm = deref_ a as y } -> consume y; use_addr_imm a
                                                                      ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 50-51:
3 |   match r with { imm = deref_ a as y } -> consume y; use_addr_imm a
                                                      ^

|}]

(* Loading from an address gives the value at the position *)
type outer = { o : inner; n : int }
let bad_load_and_value (r : outer @ unique) =
  match r with
  | { o = deref_ a as v; _ } ->
    (match a with addr_imm_ { f = f1 } -> consume f1);
    (match v with { f } -> consume f)
[%%expect{|
type outer = { o : inner; n : int; }
Line 6, characters 35-36:
6 |     (match v with { f } -> consume f)
                                       ^
Error: This value is used here, but it has already been used as unique at:
Line 5, characters 50-52:
5 |     (match a with addr_imm_ { f = f1 } -> consume f1);
                                                      ^^

|}]

(* Addresses are independent of the values at other positions... *)
type imm_fields = { g1 : string; g2 : string }
let ok_sibling_field (r : imm_fields @ unique) =
  match r with { g1 = deref_ a; g2 = y } -> consume y; use_addr_imm a
[%%expect{|
type imm_fields = { g1 : string; g2 : string; }
val ok_sibling_field : imm_fields @ unique -> unit = <fun>
|}]

(* ...and of addresses of other positions *)
let ok_two_fields (r : imm_fields @ unique) =
  match r with
  | { g1 = deref_ a; g2 = deref_ b } -> consume_addr_imm a; consume_addr_imm b
[%%expect{|
val ok_two_fields : imm_fields @ unique -> unit = <fun>
|}]

let ok_two_tuple_elements (t : (string * string) @ unique) =
  let (deref_ a, deref_ b) = t in
  consume_addr_imm a; consume_addr_imm b
[%%expect{|
val ok_two_tuple_elements : string * string @ unique -> unit = <fun>
|}]

(* Unboxed accesses are tracked like projections *)
type imm_pair = { p : #(string * string) }
let ok_sibling_unboxed_tuple (r : imm_pair @ unique) =
  match r with { p = #(deref_ a, y) } -> consume y; use_addr_imm a
[%%expect{|
type imm_pair = { p : #(string * string); }
val ok_sibling_unboxed_tuple : imm_pair @ unique -> unit = <fun>
|}]

let bad_same_unboxed_tuple (r : imm_pair @ unique) =
  match r with { p = #(deref_ a as y, _) } -> consume y; use_addr_imm a
[%%expect{|
Line 2, characters 70-71:
2 |   match r with { p = #(deref_ a as y, _) } -> consume y; use_addr_imm a
                                                                          ^
Error: This value is used here, but it has already been used as unique at:
Line 2, characters 54-55:
2 |   match r with { p = #(deref_ a as y, _) } -> consume y; use_addr_imm a
                                                          ^

|}]

type ub = #{ u1 : string; u2 : string }
type ub_field = { ubf : ub }
let ok_sibling_unboxed_record (r : ub_field @ unique) =
  match r with
  | { ubf = #{ u1 = deref_ a; u2 = y } } -> consume y; use_addr_imm a
[%%expect{|
type ub = #{ u1 : string; u2 : string; }
type ub_field = { ubf : ub; }
val ok_sibling_unboxed_record : ub_field @ unique -> unit = <fun>
|}]

let bad_same_unboxed_record (r : ub_field @ unique) =
  match r with
  | { ubf = #{ u1 = deref_ a; _ } as y } -> consume y.#u1; use_addr_imm a
[%%expect{|
Line 3, characters 72-73:
3 |   | { ubf = #{ u1 = deref_ a; _ } as y } -> consume y.#u1; use_addr_imm a
                                                                            ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 52-57:
3 |   | { ubf = #{ u1 = deref_ a; _ } as y } -> consume y.#u1; use_addr_imm a
                                                        ^^^^^

|}]

type ub_attr = { uba : string } [@@unboxed]
type ub_attr_field = { ubaf : ub_attr }
let bad_same_unboxed_record_attr (r : ub_attr_field @ unique) =
  match r with
  | { ubaf = { uba = deref_ a as y } } -> consume y; use_addr_imm a
[%%expect{|
type ub_attr = { uba : string; } [@@unboxed]
type ub_attr_field = { ubaf : ub_attr; }
Line 5, characters 66-67:
5 |   | { ubaf = { uba = deref_ a as y } } -> consume y; use_addr_imm a
                                                                      ^
Error: This value is used here, but it has already been used as unique at:
Line 5, characters 50-51:
5 |   | { ubaf = { uba = deref_ a as y } } -> consume y; use_addr_imm a
                                                      ^

|}]

(* Nested unboxed accesses are tracked like nested projections *)
type nested_pair = { np : #(ub * string) }
let ok_siblings_nested (r : nested_pair @ unique) =
  match r with
  | { np = #(#{ u1 = deref_ a; u2 = y }, z) } ->
    consume y; consume z; consume_addr_imm a
[%%expect{|
type nested_pair = { np : #(ub * string); }
val ok_siblings_nested : nested_pair @ unique -> unit = <fun>
|}]

let bad_same_nested (r : nested_pair @ unique) =
  match r with
  | { np = #(#{ u1 = deref_ a; _ } as w, _) } -> consume w.#u1; use_addr_imm a
[%%expect{|
Line 3, characters 77-78:
3 |   | { np = #(#{ u1 = deref_ a; _ } as w, _) } -> consume w.#u1; use_addr_imm a
                                                                                 ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 57-62:
3 |   | { np = #(#{ u1 = deref_ a; _ } as w, _) } -> consume w.#u1; use_addr_imm a
                                                             ^^^^^

|}]

(* Addresses into a tuple expression are identified with its components... *)
let bad_tuple_expr (x : string @ unique) =
  match (x, 0) with
  | (deref_ a, _) -> consume x; (match a with addr_imm_ y -> use_aliased y)
[%%expect{|
Line 3, characters 73-74:
3 |   | (deref_ a, _) -> consume x; (match a with addr_imm_ y -> use_aliased y)
                                                                             ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 29-30:
3 |   | (deref_ a, _) -> consume x; (match a with addr_imm_ y -> use_aliased y)
                                 ^

|}]

let ok_tuple_expr (x : string @ unique) (y : string @ unique) =
  match (x, y) with
  | (deref_ a, deref_ b) -> consume_addr_imm a; consume_addr_imm b
[%%expect{|
val ok_tuple_expr : string @ unique -> string @ unique -> unit = <fun>
|}]

(* ...which they don't use when unused *)
let ok_tuple_expr_unused (x : string @ unique) (y : string @ unique) =
  match (x, y) with (deref_ _a, _) -> consume x
[%%expect{|
val ok_tuple_expr_unused : string @ unique -> string @ unique -> unit = <fun>
|}]

type tuple_u = #{ s : string }
let bad_tuple_expr_unboxed_record (x : tuple_u @ unique) (y : int) =
  match (x, y) with
  | (#{ s = deref_ a }, _) -> consume_addr_imm a; consume x
[%%expect{|
type tuple_u = #{ s : string; }
Line 4, characters 58-59:
4 |   | (#{ s = deref_ a }, _) -> consume_addr_imm a; consume x
                                                              ^
Error: This value is used here,
       but part of it has already been used as unique at:
Line 4, characters 47-48:
4 |   | (#{ s = deref_ a }, _) -> consume_addr_imm a; consume x
                                                   ^

|}]

(* Consuming an address prevents loading through a containing address... *)
let bad_consume_addr_then_load (r : imm_pair @ unique) =
  match r with
  | { p = deref_ q } ->
    (match q with addr_imm_ #(deref_ a, _) -> consume_addr_imm a);
    (match q with addr_imm_ #(_, y) -> consume y)
[%%expect{|
Line 5, characters 18-35:
5 |     (match q with addr_imm_ #(_, y) -> consume y)
                      ^^^^^^^^^^^^^^^^^
Error: This value is read from here,
       but it has already been used as unique at:
Line 4, characters 63-64:
4 |     (match q with addr_imm_ #(deref_ a, _) -> consume_addr_imm a);
                                                                   ^

|}]

(* ...but reading other fields of the block is fine *)
let ok_consume_addr_then_field (r : outer @ unique) =
  match r with { o = deref_ a; _ } -> consume_addr_imm a; use_aliased r.n
[%%expect{|
val ok_consume_addr_then_field : outer @ unique -> unit = <fun>
|}]

let ok_consume_addr_then_wildcards (r : outer @ unique) =
  match r with
  | { o = deref_ a; _ } ->
    consume_addr_imm a;
    (match r with { o = _; n = _ } -> ())
[%%expect{|
val ok_consume_addr_then_wildcards : outer @ unique -> unit = <fun>
|}]

let ok_consume_addr_then_match (r : outer @ unique) =
  match r with
  | { o = deref_ a; _ } ->
    consume_addr_imm a; (match r with { n; _ } -> use_aliased n)
[%%expect{|
val ok_consume_addr_then_match : outer @ unique -> unit = <fun>
|}]

let ok_consume_addr_then_match_tuple (t : (string * string) @ unique) =
  match t with
  | (deref_ a, _) -> consume_addr_imm a; (match t with (_, y) -> consume y)
[%%expect{|
val ok_consume_addr_then_match_tuple : string * string @ unique -> unit =
  <fun>
|}]

type two_fields = { mutable f1 : string; mutable f2 : string }
let ok_consume_addr_then_set (r : two_fields @ unique) =
  match r with { f1 = deref_ a; _ } -> consume_addr a; r.f2 <- ""
[%%expect{|
type two_fields = { mutable f1 : string; mutable f2 : string; }
val ok_consume_addr_then_set : two_fields @ unique -> unit = <fun>
|}]

let ok_consume_addr_then_record_with (r : outer @ unique) =
  match r with
  | { o = deref_ a; _ } ->
    consume_addr_imm a; use_aliased { r with o = { f = "x" } }
[%%expect{|
val ok_consume_addr_then_record_with : outer @ unique -> unit = <fun>
|}]

(* An address into a mutable field owns the memory storing the field, so it
   conflicts with reading or writing the field... *)
let bad_consume_addr_then_read (r : mutable_field @ unique) =
  match r with { m = deref_ a } -> consume_addr a; use_aliased r.m
[%%expect{|
Line 2, characters 63-64:
2 |   match r with { m = deref_ a } -> consume_addr a; use_aliased r.m
                                                                   ^
Error: This value is read from here,
       but it has already been used as unique at:
Line 2, characters 48-49:
2 |   match r with { m = deref_ a } -> consume_addr a; use_aliased r.m
                                                    ^

|}]

let bad_consume_addr_then_write (r : mutable_field @ unique) =
  match r with { m = deref_ a } -> consume_addr a; r.m <- ""
[%%expect{|
Line 2, characters 51-52:
2 |   match r with { m = deref_ a } -> consume_addr a; r.m <- ""
                                                       ^
Error: This value is written to here,
       but it has already been used as unique at:
Line 2, characters 48-49:
2 |   match r with { m = deref_ a } -> consume_addr a; r.m <- ""
                                                    ^

|}]

(* ...but writing the field only borrows that memory *)
let ok_write_then_consume_addr (r : mutable_field @ unique) =
  match r with { m = deref_ a } -> r.m <- ""; consume_addr a
[%%expect{|
val ok_write_then_consume_addr : mutable_field @ unique -> unit = <fun>
|}]

(* The value in a mutable field is aliased, so using it doesn't conflict with
   the address (unlike in [bad_consume_value]) *)
let ok_use_value_then_consume_addr (r : mutable_field @ unique) =
  match r with { m = deref_ a as y } -> use_aliased y; consume_addr a
[%%expect{|
val ok_use_value_then_consume_addr : mutable_field @ unique -> unit = <fun>
|}]

(* A record in a mutable field is aliased, and so are addresses into it *)
let bad_consume_addr_in_mutable (r : outer_mut @ unique) =
  match r with { om = { f = deref_ a } } -> consume_addr_imm a
[%%expect{|
Line 2, characters 61-62:
2 |   match r with { om = { f = deref_ a } } -> consume_addr_imm a
                                                                 ^
Error: This value is "aliased"
         because it is an address derived from the value at line 2, characters 22-38
         which is "aliased"
         because it is the field "om" (with some modality) of the record at line 2, characters 15-40.
       However, the highlighted expression is expected to be "unique".
|}]

(* A functional update reads the memory of the kept fields. For a mutable
   field, the value isn't tracked, so only that memory conflicts with the
   address... *)
type mut_and_imm = { mutable mm : string; mi : string }
let bad_consume_addr_then_keep (r : mut_and_imm @ unique) =
  match r with
  | { mm = deref_ a; _ } -> consume_addr a; use_aliased { r with mi = "" }
[%%expect{|
type mut_and_imm = { mutable mm : string; mi : string; }
Line 4, characters 58-59:
4 |   | { mm = deref_ a; _ } -> consume_addr a; use_aliased { r with mi = "" }
                                                              ^
Error: This value is read from here,
       but it has already been used as unique at:
Line 4, characters 41-42:
4 |   | { mm = deref_ a; _ } -> consume_addr a; use_aliased { r with mi = "" }
                                             ^

|}]

(* ...but not that of the replaced fields, even if all of them are *)
let ok_consume_addr_then_replace_all (r : mut_and_imm @ unique) =
  match r with
  | { mm = deref_ a; _ } ->
    consume_addr a; use_aliased { r with mm = ""; mi = "" } [@warning "-23"]
[%%expect{|
val ok_consume_addr_then_replace_all : mut_and_imm @ unique -> unit = <fun>
|}]

(*************************************)
(* Exhaustiveness and other warnings *)

let refute_deref_below_addr : int addr_imm -> int =
  function addr_imm_ (deref_ _a) -> .
[%%expect{|
Line 2, characters 11-32:
2 |   function addr_imm_ (deref_ _a) -> .
               ^^^^^^^^^^^^^^^^^^^^^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it:
       "addr_imm_ (deref_ _a)"
|}]

type refute_r = { refute_b : bool }
let refute_deref_in_field : refute_r -> int =
  function { refute_b = deref_ _a } -> .
[%%expect{|
type refute_r = { refute_b : bool; }
Line 3, characters 11-35:
3 |   function { refute_b = deref_ _a } -> .
               ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it:
       "{refute_b=deref_ _a}"
|}]

let exh_tuple x = match x with (deref_ _a, 0) -> 1 | (0, 1) -> 2
[%%expect{|
Line 1, characters 18-64:
1 | let exh_tuple x = match x with (deref_ _a, 0) -> 1 | (0, 1) -> 2
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "(0, 2)"

val exh_tuple : int * int -> int = <fun>
|}]

let amb_warn = function
  | (deref_ _a, ((Some x, _) | (_, Some x))) when x > 0 -> x
  | _ -> 1
[%%expect{|
Line 2, characters 4-44:
2 |   | (deref_ _a, ((Some x, _) | (_, Some x))) when x > 0 -> x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under
  guard; variable "x" appears in different places in different or-pattern
  alternatives.
  Only the first match will be used to evaluate the guard expression.
  (see manual section 13.5.4)

val amb_warn : 'a * (int option * int option) -> int = <fun>
|}]

(* Unused variables of dereference patterns are reported like those of
   aliases, unless they start with an underscore *)
let unused_deref (x : int * int) = match x with (deref_ a, _) -> 0
let unused_deref_underscore (x : int * int) =
  match x with (deref_ _a, _) -> 0
[%%expect{|
Line 1, characters 56-57:
1 | let unused_deref (x : int * int) = match x with (deref_ a, _) -> 0
                                                            ^
Warning 26 [unused-var]: unused variable "a".

val unused_deref : int * int -> int = <fun>
val unused_deref_underscore : int * int -> int = <fun>
|}]


(*****************************************************)
(* Alias dereference patterns [P as deref_ x] *)

(* They have the same positions as dereference patterns *)
let tuple_alias ((x as deref_ a), (_ as deref_ b)) = #(x, a, b)
let record_alias ({ imm = (x as deref_ a); mut = (y as deref_ b) } : r) =
  #(x, a, y, b)
[%%expect{|
val tuple_alias : 'a * 'b -> #('a * 'a addr_imm * 'b addr_imm) = <fun>
val record_alias : r -> #(int * int addr_imm * string * string addr) = <fun>
|}]

let addr_alias (addr_ (x as deref_ a)) = #(x, a)
let addr_imm_alias (addr_imm_ (x as deref_ a)) = #(x, a)
[%%expect{|
val addr_alias : 'a addr -> #('a * 'a addr) = <fun>
val addr_imm_alias : 'a addr_imm -> #('a * 'a addr_imm) = <fun>
|}]

let unboxed_tuple_alias (addr_imm_ #((x as deref_ a), _)) = #(x, a)
let unboxed_record_alias ({ u = #{ x = (x as deref_ a); _ } } : r2) = #(x, a)
let unboxed_record_attr_alias (addr_ ({ f = (x as deref_ a) } : u_record)) =
  #(x, a)
[%%expect{|
val unboxed_tuple_alias : #('a * 'b) addr_imm -> #('a * 'a addr_imm) = <fun>
val unboxed_record_alias : r2 -> #(int * int addr) = <fun>
val unboxed_record_attr_alias : u_record addr -> #(int * int addr) = <fun>
|}]

let unboxed_tuple_in_tuple_alias ((_ as deref_ a), #((x as deref_ b), _)) =
  #(a, x, b)
let unboxed_record_in_tuple_alias
      ((_ as deref_ a), (#{ x = (x as deref_ b); _ } : ur)) =
  #(a, x, b)
[%%expect{|
val unboxed_tuple_in_tuple_alias :
  'a * #('b * 'c) -> #('a addr_imm * 'b * 'b addr_imm) = <fun>
val unboxed_record_in_tuple_alias :
  'a * ur -> #('a addr_imm * int * int addr_imm) = <fun>
|}]

let nested_unboxed_alias
      ({ nest = #(#{ ni = (x as deref_ a); _ }, _) } : nested) =
  #(x, a)
let nested_unboxed_in_tuple_alias
      (_, #(_, ({ f = (x as deref_ a) } : u_record))) =
  #(x, a)
[%%expect{|
val nested_unboxed_alias : nested -> #(string * string addr) = <fun>
val nested_unboxed_in_tuple_alias :
  'a * #('b * u_record) -> #(int * int addr_imm) = <fun>
|}]

let transparent_alias
      (((_ as deref_ a) as b), ((_ as deref_ c) : int),
       Stdlib.((_ as deref_ d))) =
  #(a, b, c, d)
[%%expect{|
val transparent_alias :
  'a * int * 'b -> #('a addr_imm * 'a * int addr_imm * 'b addr_imm) = <fun>
|}]

(* They are not allowed at the top level... *)
let top_alias (x as deref_ a) = #(x, a)
[%%expect{|
Line 1, characters 14-29:
1 | let top_alias (x as deref_ a) = #(x, a)
                  ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let top_unboxed_tuple_alias (#((_ as deref_ a), _) : #(int * int)) = a
[%%expect{|
Line 1, characters 31-46:
1 | let top_unboxed_tuple_alias (#((_ as deref_ a), _) : #(int * int)) = a
                                   ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let top_unboxed_record_alias (#{ x = (_ as deref_ a); _ } : ur) = a
[%%expect{|
Line 1, characters 37-52:
1 | let top_unboxed_record_alias (#{ x = (_ as deref_ a); _ } : ur) = a
                                         ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let top_unboxed_record_attr_alias ({ f = (_ as deref_ a) } : u_record) = a
[%%expect{|
Line 1, characters 41-56:
1 | let top_unboxed_record_attr_alias ({ f = (_ as deref_ a) } : u_record) = a
                                             ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

(* ...nor below constructors, lazy, arrays and unboxed constructors *)
let constr_alias (addr_ (Some (_ as deref_ a))) = a
[%%expect{|
Line 1, characters 30-45:
1 | let constr_alias (addr_ (Some (_ as deref_ a))) = a
                                  ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let variant_alias (`A (_ as deref_ a), _) = a
[%%expect{|
Line 1, characters 22-37:
1 | let variant_alias (`A (_ as deref_ a), _) = a
                          ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let lazy_alias (lazy (_ as deref_ a), _) = a
[%%expect{|
Line 1, characters 21-36:
1 | let lazy_alias (lazy (_ as deref_ a), _) = a
                         ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let array_alias (addr_ [| (_ as deref_ a) |]) = a
[%%expect{|
Line 1, characters 26-41:
1 | let array_alias (addr_ [| (_ as deref_ a) |]) = a
                              ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let unboxed_constr_attr_alias (U (_ as deref_ a), _) = a
[%%expect{|
Line 1, characters 33-48:
1 | let unboxed_constr_attr_alias (U (_ as deref_ a), _) = a
                                     ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

let or_null_alias ({ mon = This (_ as deref_ a) } : mut_or_null) = a
[%%expect{|
Line 1, characters 32-47:
1 | let or_null_alias ({ mon = This (_ as deref_ a) } : mut_or_null) = a
                                    ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

(* Or-patterns don't propagate the position... *)
let or_alias (((0 as deref_ a) | (_ as deref_ a)), _) = a
[%%expect{|
Line 1, characters 15-30:
1 | let or_alias (((0 as deref_ a) | (_ as deref_ a)), _) = a
                   ^^^^^^^^^^^^^^^
Error: Dereference patterns must match a value stored in a record,
       a tuple or at an address.
|}]

(* ...but are fine inside [P] *)
let around_or_alias = function
  | (((0 | 1) as deref_ a), _) -> Some a
  | _ -> None
[%%expect{|
val around_or_alias : int * 'a -> int addr_imm option = <fun>
|}]

(* The restrictions on the path to the position apply *)
let float_record_alias ({ x = (_ as deref_ a); _ } : float_record) = a
[%%expect{|
Line 1, characters 26-27:
1 | let float_record_alias ({ x = (_ as deref_ a); _ } : float_record) = a
                              ^
Error: Dereference patterns do not support float records.
|}]

let flat_float_record_alias ({ u = (_ as deref_ a); _ } : flat_float_record) =
  a
[%%expect{|
Line 1, characters 31-32:
1 | let flat_float_record_alias ({ u = (_ as deref_ a); _ } : flat_float_record) =
                                   ^
Error: Dereference patterns do not support [@@flatten_floats] records.
|}]

let float_array_record_alias { fa = (_ as deref_ a) } = a
[%%expect{|
Line 1, characters 31-33:
1 | let float_array_record_alias { fa = (_ as deref_ a) } = a
                                   ^^
Error: Dereference patterns do not support [@@represent_as_float_array] records.
|}]

let mixed_record_alias ({ i = (x as deref_ a); _ } : mixed_record) = #(x, a)
let mixed_tuple_alias (((x : string) as deref_ a), ((y : float#) as deref_ b))
  =
  #(x, a, y, b)
[%%expect{|
val mixed_record_alias : mixed_record -> #(float# * float# addr_imm) = <fun>
val mixed_tuple_alias :
  string * float# -> #(string * string addr_imm * float# * float# addr_imm) =
  <fun>
|}]

let inlined_alias (I { i = (_ as deref_ a) }) = a
[%%expect{|
Line 1, characters 23-24:
1 | let inlined_alias (I { i = (_ as deref_ a) }) = a
                           ^
Error: Dereference patterns do not support inlined records.
|}]

let atomic_alias ({ at = (_ as deref_ a) } : atomic) = a
[%%expect{|
Line 1, characters 20-22:
1 | let atomic_alias ({ at = (_ as deref_ a) } : atomic) = a
                        ^^
Error: Dereference patterns do not support atomic fields.
|}]

let poly_alias ({ poly = (_ as deref_ a) } : poly) = a
[%%expect{|
Line 1, characters 18-22:
1 | let poly_alias ({ poly = (_ as deref_ a) } : poly) = a
                      ^^^^
Error: Dereference patterns do not support polymorphic record fields
       (here "poly").
|}]

let global_field_alias ({ g = (_ as deref_ a) } : global_field) = a
[%%expect{|
Line 1, characters 30-45:
1 | let global_field_alias ({ g = (_ as deref_ a) } : global_field) = a
                                  ^^^^^^^^^^^^^^^
Error: Dereference patterns do not yet support non-default modalities. In particular,
       immutable elements must have the identity modality, but this is aliased.
|}]

(* The address has the type of the position, unlike the variable of an
   alias *)
type pv = { mutable pv : [ `A | `B ] }
let not_narrowed = function { pv = `A as deref_ a } -> Some a | _ -> None
let narrowed = function { pv = `A as v } -> Some v | _ -> None
[%%expect{|
type pv = { mutable pv : [ `A | `B ]; }
val not_narrowed : pv -> [ `A | `B ] addr option = <fun>
val narrowed : pv -> [> `A ] option = <fun>
|}]

(* Dereference patterns in [P] have their own positions... *)
type leaf_r = { mutable leaf : string }
type node_r = { mutable node : leaf_r }
let nested_positions { node = { leaf = deref_ l } as deref_ n } = #(n, l)
[%%expect{|
type leaf_r = { mutable leaf : string; }
type node_r = { mutable node : leaf_r; }
val nested_positions : node_r -> #(leaf_r addr * string addr) = <fun>
|}]

(* ...unless they are at the same position *)
let same_position ((deref_ a as deref_ b), _) = #(a, b)
let alias_chain (((v as deref_ a) as w), _) = #(v, a, w)
[%%expect{|
val same_position : 'a * 'b -> #('a addr_imm * 'a addr_imm) = <fun>
val alias_chain : 'a * 'b -> #('a * 'a addr_imm * 'a) = <fun>
|}]

(* The address has the mode of the container *)
let bad_local_alias (t : string * string @ local) =
  match t with (_ as deref_ a), _ -> use_global_addr_imm a
[%%expect{|
Line 2, characters 57-58:
2 |   match t with (_ as deref_ a), _ -> use_global_addr_imm a
                                                             ^
Error: This value is "local" to the parent region
         because it is an address derived from the value at line 2, characters 15-33
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

(* The address owns the value matched by [P]... *)
let bad_consume_alias_value (r : imm_field @ unique) =
  match r with { imm = y as deref_ a } -> consume y; use_addr_imm a
[%%expect{|
Line 2, characters 66-67:
2 |   match r with { imm = y as deref_ a } -> consume y; use_addr_imm a
                                                                      ^
Error: This value is used here, but it has already been used as unique at:
Line 2, characters 50-51:
2 |   match r with { imm = y as deref_ a } -> consume y; use_addr_imm a
                                                      ^

|}]

(* ...but taking it doesn't use that value *)
let ok_consume_alias_part (r : outer @ unique) =
  match r with { o = { f } as deref_ _a; _ } -> consume f
[%%expect{|
val ok_consume_alias_part : outer @ unique -> unit = <fun>
|}]

(* The address owns the memory storing the value, but [P] doesn't *)
let bad_consume_alias_then_read (r : mutable_field @ unique) =
  match r with { m = _ as deref_ a } -> consume_addr a; use_aliased r.m
[%%expect{|
Line 2, characters 68-69:
2 |   match r with { m = _ as deref_ a } -> consume_addr a; use_aliased r.m
                                                                        ^
Error: This value is read from here,
       but it has already been used as unique at:
Line 2, characters 53-54:
2 |   match r with { m = _ as deref_ a } -> consume_addr a; use_aliased r.m
                                                         ^

|}]

let ok_consume_alias_value_then_memory (r : imm_fields @ unique) =
  match r with
  | { g1 = y as deref_ _a; _ } ->
    consume y; (match r with { g1 = deref_ _b; _ } -> ())
[%%expect{|
val ok_consume_alias_value_then_memory : imm_fields @ unique -> unit = <fun>
|}]

(* The value in a mutable field is aliased, so using it doesn't conflict with
   the address *)
let ok_use_alias_value_then_consume_addr (r : mutable_field @ unique) =
  match r with { m = y as deref_ a } -> use_aliased y; consume_addr a
[%%expect{|
val ok_use_alias_value_then_consume_addr : mutable_field @ unique -> unit =
  <fun>
|}]

(* [P] reads the value as usual *)
let bad_constant_alias (r : imm_fields @ unique) =
  match r with
  | { g1 = "" as deref_ a; _ } -> consume_addr_imm a
  | _ -> ()
[%%expect{|
Line 3, characters 51-52:
3 |   | { g1 = "" as deref_ a; _ } -> consume_addr_imm a
                                                       ^
Error: This value is used here as unique,
       but it has already been used in a constant pattern at:
Line 3, characters 11-13:
3 |   | { g1 = "" as deref_ a; _ } -> consume_addr_imm a
               ^^

|}]

(* Dereference patterns at the same position share the address *)
let bad_same_position_alias (r : imm_field @ unique) =
  match r with
  | { imm = deref_ a as deref_ b } -> consume_addr_imm a; use_addr_imm b
[%%expect{|
Line 3, characters 71-72:
3 |   | { imm = deref_ a as deref_ b } -> consume_addr_imm a; use_addr_imm b
                                                                           ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 55-56:
3 |   | { imm = deref_ a as deref_ b } -> consume_addr_imm a; use_addr_imm b
                                                           ^

|}]

(* Positions in [P] are part of the address... *)
type inner2 = { h1 : string; h2 : string }
type outer2 = { o2 : inner2 }
let bad_nested_position_alias (r : outer2 @ unique) =
  match r with
  | { o2 = { h1 = deref_ b; _ } as deref_ a } ->
    consume_addr_imm b; use_addr_imm a
[%%expect{|
type inner2 = { h1 : string; h2 : string; }
type outer2 = { o2 : inner2; }
Line 6, characters 37-38:
6 |     consume_addr_imm b; use_addr_imm a
                                         ^
Error: This value is used here, but it has already been used as unique at:
Line 6, characters 21-22:
6 |     consume_addr_imm b; use_addr_imm a
                         ^

|}]

(* ...but independent of each other *)
let ok_nested_position_sibling (r : outer2 @ unique) =
  match r with
  | { o2 = { h1 = deref_ b; h2 = y } as deref_ _a } ->
    consume_addr_imm b; consume y
[%%expect{|
val ok_nested_position_sibling : outer2 @ unique -> unit = <fun>
|}]

(* Below an address, [P] matches the loaded value, which the address owns *)
let bad_consume_alias_below_addr_imm (t : string addr_imm @ unique) =
  match t with addr_imm_ (y as deref_ a) -> consume y; use_addr_imm a
[%%expect{|
Line 2, characters 68-69:
2 |   match t with addr_imm_ (y as deref_ a) -> consume y; use_addr_imm a
                                                                        ^
Error: This value is used here, but it has already been used as unique at:
Line 2, characters 52-53:
2 |   match t with addr_imm_ (y as deref_ a) -> consume y; use_addr_imm a
                                                        ^

|}]

(* In a tuple expression, the address is identified with the component *)
let bad_tuple_expr_alias (x : string @ unique) =
  match (x, 0) with
  | ((y as deref_ a), _) -> consume y; use_addr_imm a
[%%expect{|
Line 3, characters 52-53:
3 |   | ((y as deref_ a), _) -> consume y; use_addr_imm a
                                                        ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 36-37:
3 |   | ((y as deref_ a), _) -> consume y; use_addr_imm a
                                        ^

|}]

(* Exhaustiveness and unused cases look inside [P] *)
let exh_alias = function (Some _ as deref_ a), _ -> a
[%%expect{|
Line 1, characters 16-53:
1 | let exh_alias = function (Some _ as deref_ a), _ -> a
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "(None, _)"

val exh_alias : 'a option * 'b -> 'a option addr_imm = <fun>
|}]

let unused_alias = function
  | (_ as deref_ a), _ -> a
  | (Some _ as deref_ a), _ -> a
[%%expect{|
Line 3, characters 4-27:
3 |   | (Some _ as deref_ a), _ -> a
        ^^^^^^^^^^^^^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val unused_alias : 'a option * 'b -> 'a option addr_imm = <fun>
|}]

(* Unused variables of alias dereference patterns are reported, too *)
let unused_alias_deref (x : int option * int) =
  match x with ((_ as deref_ a), _) -> 0
[%%expect{|
Line 2, characters 29-30:
2 |   match x with ((_ as deref_ a), _) -> 0
                                 ^
Warning 26 [unused-var]: unused variable "a".

val unused_alias_deref : int option * int -> int = <fun>
|}]
