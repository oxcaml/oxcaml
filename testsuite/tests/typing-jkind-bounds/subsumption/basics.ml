(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

module M : sig
  type t
end = struct
  type t : value with int
end
[%%expect {|
module M : sig type t end
|}]

module M : sig
  type _ t
end = struct
  type 'a t : value with 'a
end
[%%expect {|
module M : sig type _ t end
|}]

module M : sig
  type t : immediate
end = struct
  type t : immediate with int
end
[%%expect {|
module M : sig type t : immediate end
|}]

module M : sig
  type t : immediate
end = struct
  type t : immediate with string
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : immediate with string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : immutable_data end
       is not included in
         sig type t : immediate end
       Type declarations do not match:
         type t : immutable_data
       is not included in
         type t : immediate
       The kind of the first is immutable_data
         because of the definition of t at line 4, characters 2-32.
       But the kind of the first must be a subkind of immediate
         because of the definition of t at line 2, characters 2-20.
|}]

module M : sig
  type t : float64
end = struct
  type t : float64 with int
end
[%%expect {|
module M : sig type t : float64 end
|}]

type u : immutable_data
type t : immutable_data with int = u
[%%expect {|
type u : immutable_data
type t = u
|}]

type ('a, 'b) t : immutable_data with 'b with 'a

module type S = sig
  type ('a, 'b) t : immutable_data with 'a with 'b
end

module type T = S with type ('a, 'b) t = ('a, 'b) t
[%%expect {|
type ('a, 'b) t : immutable_data with 'a with 'b
module type S = sig type ('a, 'b) t : immutable_data with 'a with 'b end
module type T = sig type ('a, 'b) t = ('a, 'b) t end
|}]

module M : sig
  type ('a, 'b) t : immutable_data with 'a with 'b
end = struct
  type ('a, 'b) t : immutable_data with 'b
end
[%%expect {|
module M : sig type ('a, 'b) t : immutable_data with 'a with 'b end
|}]

module M : sig
  type ('a, 'b) t : immutable_data with 'a
end = struct
  type ('a, 'b) t : immutable_data with 'b with 'a
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t : immutable_data with 'b with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t : immutable_data with 'a with 'b end
       is not included in
         sig type ('a, 'b) t : immutable_data with 'a end
       Type declarations do not match:
         type ('a, 'b) t : immutable_data with 'a with 'b
       is not included in
         type ('a, 'b) t : immutable_data with 'a
       The kind of the first is immutable_data with 'a with 'b
         because of the definition of t at line 4, characters 2-50.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-42.
|}]

type ('a, 'b) u : immutable_data with 'b with 'a
type ('a, 'b) t : immutable_data with 'a = ('a, 'b) u
[%%expect {|
type ('a, 'b) u : immutable_data with 'a with 'b
Line 2, characters 0-53:
2 | type ('a, 'b) t : immutable_data with 'a = ('a, 'b) u
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "('a, 'b) u" is immutable_data with 'a with 'b
         because of the definition of u at line 1, characters 0-48.
       But the kind of type "('a, 'b) u" must be a subkind of
           immutable_data with 'a
         because of the definition of t at line 2, characters 0-53.
|}]

type ('a, 'b) t : immutable_data with 'a with 'b

module type S = sig
  type ('a, 'b) t : immutable_data with 'a
end

module type T = S with type ('a, 'b) t = ('a, 'b) t
[%%expect {|
type ('a, 'b) t : immutable_data with 'a with 'b
module type S = sig type ('a, 'b) t : immutable_data with 'a end
Line 7, characters 16-51:
7 | module type T = S with type ('a, 'b) t = ('a, 'b) t
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type ('a, 'b) t = ('a, 'b) t
       is not included in
         type ('a, 'b) t : immutable_data with 'a
       The kind of the first is immutable_data with 'a with 'b
         because of the definition of t at line 1, characters 0-48.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 4, characters 2-42.
|}]

module M : sig
  type 'a t : mutable_data with 'a
end = struct
  type 'a t : immutable_data with 'a ref
end
[%%expect {|
module M : sig type 'a t : mutable_data with 'a end
|}]

module M : sig
  type 'a t : immutable_data with 'a ref
end = struct
  type 'a t : mutable_data with 'a
end
(* This isn't accepted because ['a ref] is always [many] *)
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : mutable_data with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : mutable_data with 'a end
       is not included in
         sig type 'a t : mutable_data with 'a @@ forkable unyielding many end
       Type declarations do not match:
         type 'a t : mutable_data with 'a
       is not included in
         type 'a t : mutable_data with 'a @@ forkable unyielding many
       The kind of the first is mutable_data with 'a
         because of the definition of t at line 4, characters 2-34.
       But the kind of the first must be a subkind of
           mutable_data with 'a @@ forkable unyielding many
         because of the definition of t at line 2, characters 2-40.

       The first mode-crosses less than the second along:
         linearity: mod many with 'a ≰ mod many
         forkable: mod forkable with 'a ≰ mod forkable
         yielding: mod unyielding with 'a ≰ mod unyielding
|}]

module M : sig
  type 'a t : immutable_data with 'a ref
end = struct
  type 'a t : mutable_data with 'a @@ many unyielding forkable
end
[%%expect {|
module M :
  sig type 'a t : mutable_data with 'a @@ forkable unyielding many end
|}]

(* CR layouts v2.8: 'a u's kind should get normalized to just immutable_data.
   Internal ticket 4770. *)
module M = struct
  type ('a : immutable_data) u : immutable_data with 'a
  type 'a t : immutable_data = 'a u
end
[%%expect {|
module M :
  sig
    type ('a : immutable_data) u : immutable_data with 'a
    type ('a : immutable_data) t = 'a u
  end
|}]

module M : sig
  type t : mutable_data
end = struct
  type a : immediate
  type b : immutable_data with a with int with string with a ref
  type c : mutable_data with b with a with b with a
  type d : immediate with a
  type t : immutable_data mod aliased with d with d with b with d with c with a
end
[%%expect {|
module M : sig type t : mutable_data end
|}]

type u
type t : value mod portable with u
type q : value mod portable with t = { x : t }
[%%expect {|
type u
type t : value mod portable with u
type q = { x : t; }
|}]

type u
type t : value mod portable with u
type v : value mod portable with t
type q : value mod portable with t = { x : v }
[%%expect {|
type u
type t : value mod portable with u
type v : value mod portable with t
type q = { x : v; }
|}]

type u
type t = private u
type v : immutable_data with u = { value : t }
[%%expect {|
type u
type t = private u
type v = { value : t; }
|}]

type t : value = private int
type v : immutable_data = { value : t }
[%%expect{|
type t = private int
type v = { value : t; }
|}]

type t = private int
type v : immutable_data = { value : t }
[%%expect{|
type t = private int
type v = { value : t; }
|}]

type t : immediate with t = [`foo | `bar]
[%%expect {|
type t = [ `bar | `foo ]
|}]

type t
type u : immutable_data with t = [`foo of t]
[%%expect {|
type t
type u = [ `foo of t ]
|}]

type (_, _) eq = Eq : ('a, 'a) eq

type t1
type t2

module M : sig
  type a : immutable_data with t2
  type b : immutable_data with t1
  val eq : (a, b) eq
end = struct
  type a = int
  type b = int
  let eq = Eq
end

let _ =
  match M.eq with
  | Eq ->
    (* M.a = M.b *)
    let module _ : sig
      type t : immutable_data with t1
    end = struct
      type t : immutable_data with M.a
    end in
    ()
(* CR layouts v2.8: Ideally this would be accepted *)
[%%expect {|
type (_, _) eq = Eq : ('a, 'a) eq
type t1
type t2
module M :
  sig
    type a : immutable_data with t2
    type b : immutable_data with t1
    val eq : (a, b) eq
  end
>> Fatal error: Abstract kind with [with]: immutable_data with t1
Uncaught exception: Misc.Fatal_error

|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t = 'a
end
(* CR layouts v2.8: This should get accepted. But we should wait until we have kind_of *)
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a end
       is not included in
         sig type 'a t : immutable_data with 'a end
       Type declarations do not match:
         type 'a t = 'a
       is not included in
         type 'a t : immutable_data with 'a
       The kind of the first is value
         because of the definition of t at line 2, characters 2-36.
       But the kind of the first must be a subkind of immutable_data with 'a
         because of the definition of t at line 2, characters 2-36.
|}]

(* Ben Peters' example *)
(* This surprising example is sound to accept. To see why, 
   we analyze as follows:
   
   Here t1.0 and t2.0 are the "base kind" 
   and t1.1 and t2.1 are the "modalities protecting the argument",
   that is, the kind of 'a t1 is t1.0 JOIN ('a MEET t1.1)
   and similarly for 'a t2.

   The kind in the signature is:
       t2.0 JOIN 
       (t1.0 MEET t2.1) JOIN 
       ('a MEET t1.1 MEET t2.1) 
       JOIN t1.0
   The kind in the implementation is:
       t1.0 JOIN
       (t2.0 MEET t1.1) JOIN
       ('a MEET t2.1 MEET t1.1) 
       JOIN t2.0

   These two formulas both simplify to:
      t1.0 JOIN t2.0 JOIN
      ('a MEET t1.1 MEET t2.1)

    Hence, the kinds are compatible.
*)
module type S = sig
  type 'a t1
  type 'a t2
  type 'a t : immutable_data with 'a t1 t2 * unit t1
end

module M : S = struct
  type 'a t1
  type 'a t2
  type 'a t = 'a t2 t1 * unit t2
end
[%%expect{|
module type S =
  sig
    type 'a t1
    type 'a t2
    type 'a t : immutable_data with 'a t1 t2 with unit t1
  end
module M : S
|}]


(* Ben Peters' example failing version *)
module type S = sig
  type 'a t1
  type 'a t2
  type 'a t : immutable_data with 'a t1 t2 * unit t2
end

module M : S = struct
  type 'a t1
  type 'a t2
  type 'a t = 'a t2 t1 * unit t1
end
[%%expect{|
module type S =
  sig
    type 'a t1
    type 'a t2
    type 'a t : immutable_data with 'a t1 t2 with unit t2
  end
Lines 7-11, characters 15-3:
 7 | ...............struct
 8 |   type 'a t1
 9 |   type 'a t2
10 |   type 'a t = 'a t2 t1 * unit t1
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t1 type 'a t2 type 'a t = 'a t2 t1 * unit t1 end
       is not included in
         S
       Type declarations do not match:
         type 'a t = 'a t2 t1 * unit t1
       is not included in
         type 'a t : immutable_data with 'a t1 t2 with unit t2
       The kind of the first is immutable_data with 'a t2 t1 with unit t1
         because it's a tuple type.
       But the kind of the first must be a subkind of
           immutable_data with 'a t1 t2 with unit t2
         because of the definition of t at line 4, characters 2-52.
|}]

(* Ben Peters' example failing version 2 *)
module type S = sig
  type 'a t1
  type 'a t2
  type 'a t : immutable_data with 'a t1 t2
end

module M : S = struct
  type 'a t1
  type 'a t2
  type 'a t = 'a t2 t1
end
[%%expect{|
module type S =
  sig type 'a t1 type 'a t2 type 'a t : immutable_data with 'a t1 t2 end
Lines 7-11, characters 15-3:
 7 | ...............struct
 8 |   type 'a t1
 9 |   type 'a t2
10 |   type 'a t = 'a t2 t1
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t1 type 'a t2 type 'a t = 'a t2 t1 end
       is not included in
         S
       Type declarations do not match:
         type 'a t = 'a t2 t1
       is not included in
         type 'a t : immutable_data with 'a t1 t2
       The kind of the first is value
         because of the definition of t1 at line 8, characters 2-12.
       But the kind of the first must be a subkind of
           immutable_data with 'a t1 t2
         because of the definition of t at line 4, characters 2-42.
|}]


(* Various types of rose trees *)
module type S = sig
  type 'a rose : immutable_data with 'a
  type 'a lily : immutable_data with 'a
  type 'a tulip : immutable_data
end

module M : S = struct
  type 'a rose = Leaf of 'a | Node of ('a * 'a) rose
  type 'a lily = Node of ('a * ('a list) lily) list
  type 'a tulip = Node of ('a list) tulip list
end
[%%expect{|
module type S =
  sig
    type 'a rose : immutable_data with 'a
    type 'a lily : immutable_data with 'a
    type 'a tulip : immutable_data
  end
module M : S
|}]


type fails : immutable_data = int -> int
[%%expect{|
Line 1, characters 0-40:
1 | type fails : immutable_data = int -> int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int -> int" is value mod aliased immutable non_float
         because it's a function type.
       But the kind of type "int -> int" must be a subkind of immutable_data
         because of the definition of fails at line 1, characters 0-40.
|}]

type should_fail : immutable_data = [`A of int -> int]
[%%expect{|
Line 1, characters 0-54:
1 | type should_fail : immutable_data = [`A of int -> int]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[ `A of int -> int ]" is value mod immutable non_float
         because it's a polymorphic variant type.
       But the kind of type "[ `A of int -> int ]" must be a subkind of
           immutable_data
         because of the definition of should_fail at line 1, characters 0-54.
|}]

type should_also_fail : immutable_data = [`A of int -> int | `B of 'a] as 'a
[%%expect{|
Line 1, characters 0-76:
1 | type should_also_fail : immutable_data = [`A of int -> int | `B of 'a] as 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[ `A of int -> int | `B of 'a ] as 'a" is
           value mod immutable non_float
         because it's a polymorphic variant type.
       But the kind of type "[ `A of int -> int | `B of 'a ] as 'a" must be a subkind of
         immutable_data
         because of the definition of should_also_fail at line 1, characters 0-76.
|}]

type r

type should_fail_too : immutable_data with r = [`A of int ref]
[%%expect{|
type r
Line 3, characters 0-62:
3 | type should_fail_too : immutable_data with r = [`A of int ref]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[ `A of int ref ]" is mutable_data
         because it's a polymorphic variant type.
       But the kind of type "[ `A of int ref ]" must be a subkind of
           immutable_data with r
         because of the definition of should_fail_too at line 3, characters 0-62.
|}]

type should_likewise_fail : immutable_data = (int ref * (int -> int))
[%%expect{|
Line 1, characters 0-69:
1 | type should_likewise_fail : immutable_data = (int ref * (int -> int))
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int ref * (int -> int)" is value mod non_float
         because it's a tuple type.
       But the kind of type "int ref * (int -> int)" must be a subkind of
           immutable_data
         because of the definition of should_likewise_fail at line 1, characters 0-69.
|}]

type and_even_this_should_fail : immutable_data = [`A of [`B of int ref]]
[%%expect{|
Line 1, characters 0-73:
1 | type and_even_this_should_fail : immutable_data = [`A of [`B of int ref]]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[ `A of [ `B of int ref ] ]" is mutable_data
         because it's a polymorphic variant type.
       But the kind of type "[ `A of [ `B of int ref ] ]" must be a subkind of
           immutable_data
         because of the definition of and_even_this_should_fail at line 1, characters 0-73.
|}]

type this_should_succeed : immutable_data = ((int * int) * (int * int))
[%%expect{|
type this_should_succeed = (int * int) * (int * int)
|}]

type this_too : immutable_data = (int * int) list
[%%expect{|
type this_too = (int * int) list
|}]

type this_fails : immutable_data = (int -> int) list
[%%expect{|
Line 1, characters 0-52:
1 | type this_fails : immutable_data = (int -> int) list
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "(int -> int) list" is value mod immutable non_float
         because it's a boxed variant type.
       But the kind of type "(int -> int) list" must be a subkind of
           immutable_data
         because of the definition of this_fails at line 1, characters 0-52.
|}]


(* ok *)
type 'a t1 : value mod portable =
  | A
  | B of 'a t1
[%%expect{|
type 'a t1 = A | B of 'a t1
|}]

(* ok *)
type 'a t2 : value mod portable =
  | A
  | B of t2__unit

and t2__unit = unit t2
[%%expect{|
type 'a t2 = A | B of t2__unit
and t2__unit = unit t2
|}]

(* Previously rejected when the checker ran out of fuel. *)
type 'a t3 : value mod portable =
  | A
  | B of unit t3
[%%expect{|
type 'a t3 = A | B of unit t3
|}]
