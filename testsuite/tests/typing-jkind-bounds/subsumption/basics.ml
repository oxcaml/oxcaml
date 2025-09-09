(* TEST
    flags = "-extension layouts_alpha -extension ikinds";
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
Line 4, characters 2-32:
4 |   type t : immediate with string
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because of the annotation on the declaration of the type t.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
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
         sig type 'a t : mutable_data with 'a @@ unyielding many end
       Type declarations do not match:
         type 'a t : mutable_data with 'a
       is not included in
         type 'a t : mutable_data with 'a @@ unyielding many
       The kind of the first is mutable_data with 'a
         because of the definition of t at line 4, characters 2-34.
       But the kind of the first must be a subkind of
           mutable_data with 'a @@ unyielding many
         because of the definition of t at line 2, characters 2-40.

       The first mode-crosses less than the second along:
         linearity: mod many with 'a ≰ mod many
|}]

module M : sig
  type 'a t : immutable_data with 'a ref
end = struct
  type 'a t : mutable_data with 'a @@ many unyielding
end
[%%expect {|
module M : sig type 'a t : mutable_data with 'a @@ unyielding many end
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

type t : immediate with t = [`foo | `bar]
[%%expect {|
type t = [ `bar | `foo ]
|}]

type t
type u : immutable_data with t = [`foo of t]
(* CR layouts v2.8: This should be accepted. Internal ticket 4294 *)
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

(* Benjamin's example *)
module type S = sig
  type 'a t1
  type 'a t2
  type 'a t : immutable_data with 'a t1 t2 * unit t2
end

module M : S = struct
  type 'a t1 = 'a
  type 'a t2 = 'a
  type 'a t = 'a t2 t1 * unit t1
end
[%%expect{|
module type S =
  sig
    type 'a t1
    type 'a t2
    type 'a t : immutable_data with 'a t1 t2 with unit t2
  end
module M : S
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