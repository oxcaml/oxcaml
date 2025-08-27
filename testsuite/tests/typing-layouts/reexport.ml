(* TEST
 expect;
*)

(* CR-soon: Make these actually error *)
(* CR-soon: Don't print the jkind annotation on a re-export once these error
   because in that world it is impossible for a re-export to change the jkind
   bount on a param. Thus, the bound is always redundant. *)
(* Part 1: Adding jkind constraints during re-exports is an error *)

type 'a t = A of 'a
type ('a : immediate) u = 'a t = A of 'a
[%%expect {|
type 'a t = A of 'a
type ('a : immediate) u = 'a t = A of 'a
|}]

type 'a t = A of 'a
type 'a u = ('a : immediate) t = A of 'a
[%%expect {|
type 'a t = A of 'a
type ('a : immediate) u = 'a t = A of 'a
|}]

type 'a t = A of 'a
type 'a u = 'a t = A of ('a : immediate)
[%%expect {|
type 'a t = A of 'a
type ('a : immediate) u = 'a t = A of 'a
|}]

type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b : immediate) u = ('a, 'b) t = A of 'a | B of 'b
[%%expect {|
type ('a, 'b) t = A of 'a | B of 'b
type ('a, 'b : immediate) u = ('a, 'b) t = A of 'a | B of 'b
|}]

type ('a, 'b) t = A of 'a | B of 'b
type ('a : immediate, 'b : value) u = ('a, 'b) t = A of 'a | B of 'b
[%%expect {|
type ('a, 'b) t = A of 'a | B of 'b
type ('a : immediate, 'b) u = ('a, 'b) t = A of 'a | B of 'b
|}]

type 'a t = { field : 'a }
type 'a u = 'a t = { field : ('a : immediate) }
[%%expect {|
type 'a t = { field : 'a; }
type ('a : immediate) u = 'a t = { field : 'a; }
|}]

type 'a t = A : 'a -> 'a t
type ('a : immediate) u = 'a t = A : 'a -> 'a u
[%%expect {|
type 'a t = A : 'a -> 'a t
Line 2, characters 0-47:
2 | type ('a : immediate) u = 'a t = A : 'a -> 'a u
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "'a t"
       Constructors do not match:
         "A : 'a -> 'a t"
       is not the same as:
         "A : ('a : immediate). 'a -> 'a t"
       The type "'a t" is not equal to the type "'a0 t"
       because the layouts of their variables are different.
       The layout of 'a is value
         because of the definition of t at line 1, characters 0-26.
       The layout of 'a0 is immediate
         because of the definition of u at line 2, characters 0-47.
|}]

type t = Pack : 'a -> t
type u = t = Pack : ('a : immediate) -> u
[%%expect {|
type t = Pack : 'a -> t
Line 2, characters 0-41:
2 | type u = t = Pack : ('a : immediate) -> u
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "t"
       Constructors do not match:
         "Pack : 'a -> t"
       is not the same as:
         "Pack : ('a : immediate). 'a -> t"
       The type "('a : value)" is not equal to the type "('a0 : immediate)"
       because the layouts of their variables are different.
       The layout of 'a is value
         because of the definition of t at line 1, characters 0-23.
       The layout of 'a0 is immediate
         because of the definition of u at line 2, characters 0-41.
|}]

type 'a t = A of 'a
type ('a : bits64) u = 'a t = A of 'a
[%%expect {|
type 'a t = A of 'a
Line 2, characters 23-25:
2 | type ('a : bits64) u = 'a t = A of 'a
                           ^^
Error: This type "('a : bits64)" should be an instance of type "('b : value)"
       The layout of 'a is bits64
         because of the annotation on 'a in the declaration of the type u.
       But the layout of 'a must overlap with value
         because of the definition of t at line 1, characters 0-19.
|}]

type ('a : bits64) t = A of 'a
type 'a u = ('a : bits64 mod portable) t = A of 'a
[%%expect {|
type ('a : bits64) t = A of 'a
type ('a : bits64 mod portable) u = 'a t = A of 'a
|}]

type ('a : value mod portable global) t = A of 'a
type 'a u = ('a : value mod portable contended) t = A of 'a
[%%expect {|
type ('a : value mod global portable) t = A of 'a
type ('a : value mod global contended portable) u = 'a t = A of 'a
|}]

type 'a t = ..
type ('a : immutable_data) u = 'a t = ..
[%%expect {|
type 'a t = ..
type ('a : immutable_data) u = 'a t = ..
|}]

type 'a t = #{ foo : 'a }
type ('a : immediate) u = 'a t = #{ foo : 'a }
[%%expect {|
type 'a t = #{ foo : 'a; }
type ('a : immediate) u = 'a t = #{ foo : 'a; }
|}]

(* Part 2: You cannot loosen the jkind bound via re-exporting. *)

type ('a : immediate) t = A of 'a
type 'a u = 'a t = A of 'a
[%%expect {|
type ('a : immediate) t = A of 'a
type ('a : immediate) u = 'a t = A of 'a
|}]
type v = string u
[%%expect {|
Line 1, characters 9-15:
1 | type v = string u
             ^^^^^^
Error: This type "string" should be an instance of type "('a : immediate)"
       The kind of string is immutable_data
         because it is the primitive type string.
       But the kind of string must be a subkind of immediate
         because of the definition of u at line 2, characters 0-26.
|}]

type ('a : value mod portable contended) t = A of 'a
type ('a : value mod portable) u = 'a t = A of 'a
type t_portable : value mod portable
type v = t_portable u
[%%expect {|
type ('a : value mod contended portable) t = A of 'a
type ('a : value mod contended portable) u = 'a t = A of 'a
type t_portable : value mod portable
Line 4, characters 9-19:
4 | type v = t_portable u
             ^^^^^^^^^^
Error: This type "t_portable" should be an instance of type
         "('a : value mod contended portable)"
       The kind of t_portable is value mod portable
         because of the definition of t_portable at line 3, characters 0-36.
       But the kind of t_portable must be a subkind of
           value mod contended portable
         because of the definition of u at line 2, characters 0-49.
|}]

module M : sig
  type 'a u = A of 'a
end = struct
  type ('a : immediate) t = A of 'a
  type 'a u = 'a t = A of 'a
end
[%%expect {|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   type ('a : immediate) t = A of 'a
5 |   type 'a u = 'a t = A of 'a
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a : immediate) t = A of 'a
           type ('a : immediate) u = 'a t = A of 'a
         end
       is not included in
         sig type 'a u = A of 'a end
       Type declarations do not match:
         type ('a : immediate) u = 'a t = A of 'a
       is not included in
         type 'a u = A of 'a
       The problem is in the kinds of a parameter:
       The kind of 'a is value
         because of the definition of u at line 2, characters 2-21.
       But the kind of 'a must be a subkind of immediate
         because of the definition of u at line 5, characters 2-28.
|}]

module M : sig
  type ('a : immediate) u = A of 'a
end = struct
  type ('a : immediate) t = A of 'a
  type 'a u = 'a t = A of 'a
end
[%%expect {|
module M : sig type ('a : immediate) u = A of 'a end
|}]

type 'a t = A of 'a
type ('a : any) u = 'a t = A of 'a
[%%expect {|
type 'a t = A of 'a
type 'a u = 'a t = A of 'a
|}]
type v = int64# u
[%%expect {|
Line 1, characters 9-15:
1 | type v = int64# u
             ^^^^^^
Error: This type "int64#" should be an instance of type "('a : value)"
       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of value
         because of the definition of u at line 2, characters 0-34.
|}]

type ('a : bits64) t = A of 'a
type 'a u = 'a t = A of 'a
type v = int64# u
type v = string u
[%%expect {|
type ('a : bits64) t = A of 'a
type ('a : bits64) u = 'a t = A of 'a
type v = int64# u
Line 4, characters 9-15:
4 | type v = string u
             ^^^^^^
Error: This type "string" should be an instance of type "('a : bits64)"
       The layout of string is value
         because it is the primitive type string.
       But the layout of string must be a sublayout of bits64
         because of the definition of u at line 2, characters 0-26.
|}]

type t = Pack : ('a : immediate) -> t
type u = t = Pack : 'a -> u
[%%expect {|
type t = Pack : ('a : immediate). 'a -> t
Line 2, characters 0-27:
2 | type u = t = Pack : 'a -> u
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "t"
       Constructors do not match:
         "Pack : ('a : immediate). 'a -> t"
       is not the same as:
         "Pack : 'a -> t"
       The type "('a : immediate)" is not equal to the type "('a0 : value)"
       because the layouts of their variables are different.
       The layout of 'a is immediate
         because of the definition of t at line 1, characters 0-37.
       The layout of 'a0 is value
         because of the definition of u at line 2, characters 0-27.
|}]

type ('a : immutable_data) t = ..
type ('a : value) u = 'a t = ..
type v = string ref u
[%%expect {|
type ('a : immutable_data) t = ..
type ('a : immutable_data) u = 'a t = ..
Line 3, characters 9-19:
3 | type v = string ref u
             ^^^^^^^^^^
Error: This type "string ref" should be an instance of type
         "('a : immutable_data)"
       The kind of string ref is mutable_data.
       But the kind of string ref must be a subkind of immutable_data
         because of the definition of u at line 2, characters 0-31.
|}, Principal{|
type ('a : immutable_data) t = ..
type ('a : immutable_data) u = 'a t = ..
Line 3, characters 9-19:
3 | type v = string ref u
             ^^^^^^^^^^
Error: This type "string ref" should be an instance of type
         "('a : immutable_data)"
       The kind of string ref is mutable_data with string @@ unyielding many.
       But the kind of string ref must be a subkind of immutable_data
         because of the definition of u at line 2, characters 0-31.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended
         portability: mod portable with string ≰ mod portable
         statefulness: mod stateless with string ≰ mod stateless
         visibility: mod read_write ≰ mod immutable
|}]

type 'a t = #{ foo : int }
type ('a : any) u = 'a t = #{ foo : int }
type v = int64# u
[%%expect {|
type 'a t = #{ foo : int; }
type 'a u = 'a t = #{ foo : int; }
Line 3, characters 9-15:
3 | type v = int64# u
             ^^^^^^
Error: This type "int64#" should be an instance of type "('a : value)"
       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of value
         because of the definition of u at line 2, characters 0-41.
|}]
type v = string or_null u
[%%expect {|
Line 1, characters 9-23:
1 | type v = string or_null u
             ^^^^^^^^^^^^^^
Error: This type "string or_null" should be an instance of type "('a : value)"
       The kind of string or_null is
           value_or_null mod many unyielding stateless immutable
         because it is the primitive type or_null.
       But the kind of string or_null must be a subkind of value
         because of the definition of u at line 2, characters 0-41.
|}, Principal{|
Line 1, characters 9-23:
1 | type v = string or_null u
             ^^^^^^^^^^^^^^
Error: This type "string or_null" should be an instance of type "('a : value)"
       The kind of string or_null is value_or_null mod everything with string
         because it is the primitive type or_null.
       But the kind of string or_null must be a subkind of value
         because of the definition of u at line 2, characters 0-41.
|}]

(* Part 3: Defaulted type parameters don't add constraints in a re-export *)

type ('a : value_or_null) t1 = Foo of 'a
type 'a t2 = 'a t1 = Foo of 'a
type t3 = int or_null t2
[%%expect {|
type ('a : value_or_null) t1 = Foo of 'a
type ('a : value_or_null) t2 = 'a t1 = Foo of 'a
type t3 = int or_null t2
|}]

type 'a my_list = 'a list = | [] | (::) of 'a * 'a my_list
type t = int or_null my_list
[%%expect {|
type ('a : value_or_null) my_list = 'a list = [] | (::) of 'a * 'a my_list
type t = int or_null my_list
|}]

module _ : sig
  type ('a : value_or_null) t
end = struct
  type 'a t = 'a option = None | Some of 'a
end
[%%expect{|
|}]

type ('a : any) t1 = { foo : int }
type 'a t2 = 'a t1 = { foo : int }
type t3 = int or_null t2
type t4 = int64# t2
[%%expect {|
type ('a : any) t1 = { foo : int; }
type ('a : any) t2 = 'a t1 = { foo : int; }
type t3 = int or_null t2
type t4 = int64# t2
|}]

type ('a : any) t1 = #{ foo : int }
type 'a t2 = 'a t1 = #{ foo : int }
type t3 = int or_null t2
type t4 = int64# t2
[%%expect {|
type ('a : any) t1 = #{ foo : int; }
type ('a : any) t2 = 'a t1 = #{ foo : int; }
type t3 = int or_null t2
type t4 = int64# t2
|}]

type ('a : value_or_null) t1 = ..
type 'a t2 = 'a t1 = ..
type t3 = int or_null t2
[%%expect {|
type ('a : value_or_null) t1 = ..
type ('a : value_or_null) t2 = 'a t1 = ..
type t3 = int or_null t2
|}]
