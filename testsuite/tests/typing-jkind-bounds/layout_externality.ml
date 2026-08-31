(* TEST
 {
   flags = "-extension small_numbers -no-ikinds -w -181-220";
   expect;
 }{
   flags = "-extension small_numbers -w -181-220";
   expect;
 }
*)

(* Non-value base layouts inherently cross externality (e.g. even given
   with-bounds) *)

type ('a : any mod external_) require_external
type ('a : any mod external64) require_external64
[%%expect{|
type ('a : any mod external_) require_external
type ('a : any mod external64) require_external64
|}]

type 'a t : bits8 with 'a
type ok = string t require_external
[%%expect{|
type 'a t : bits8 with 'a
Line 2, characters 10-18:
2 | type ok = string t require_external
              ^^^^^^^^
Error: This type "string t" should be an instance of type
         "('a : any mod external_)"
       The kind of string t is bits8
         because of the definition of t at line 1, characters 0-25.
       But the kind of string t must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}, Principal{|
type 'a t : bits8 with 'a
Line 2, characters 10-18:
2 | type ok = string t require_external
              ^^^^^^^^
Error: This type "string t" should be an instance of type
         "('a : any mod external_)"
       The kind of string t is bits8 with string
         because of the definition of t at line 1, characters 0-25.
       But the kind of string t must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

type ok64 = string t require_external64
[%%expect{|
Line 1, characters 12-20:
1 | type ok64 = string t require_external64
                ^^^^^^^^
Error: This type "string t" should be an instance of type
         "('a : any mod external64)"
       The kind of string t is bits8
         because of the definition of t at line 1, characters 0-25.
       But the kind of string t must be a subkind of any mod external64
         because of the definition of require_external64 at line 2, characters 0-49.
|}, Principal{|
Line 1, characters 12-20:
1 | type ok64 = string t require_external64
                ^^^^^^^^
Error: This type "string t" should be an instance of type
         "('a : any mod external64)"
       The kind of string t is bits8 with string
         because of the definition of t at line 1, characters 0-25.
       But the kind of string t must be a subkind of any mod external64
         because of the definition of require_external64 at line 2, characters 0-49.
|}]

type 'a f64 : float64 with 'a
type ok_f64 = string f64 require_external
[%%expect{|
type 'a f64 : float64 with 'a
Line 2, characters 14-24:
2 | type ok_f64 = string f64 require_external
                  ^^^^^^^^^^
Error: This type "string f64" should be an instance of type
         "('a : any mod external_)"
       The kind of string f64 is float64
         because of the definition of f64 at line 1, characters 0-29.
       But the kind of string f64 must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}, Principal{|
type 'a f64 : float64 with 'a
Line 2, characters 14-24:
2 | type ok_f64 = string f64 require_external
                  ^^^^^^^^^^
Error: This type "string f64" should be an instance of type
         "('a : any mod external_)"
       The kind of string f64 is float64 with string
         because of the definition of f64 at line 1, characters 0-29.
       But the kind of string f64 must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

type 'a v : void with 'a
type ok_v = string v require_external
[%%expect{|
type 'a v : void with 'a
Line 2, characters 12-20:
2 | type ok_v = string v require_external
                ^^^^^^^^
Error: This type "string v" should be an instance of type
         "('a : any mod external_)"
       The kind of string v is void
         because of the definition of v at line 1, characters 0-24.
       But the kind of string v must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}, Principal{|
type 'a v : void with 'a
Line 2, characters 12-20:
2 | type ok_v = string v require_external
                ^^^^^^^^
Error: This type "string v" should be an instance of type
         "('a : any mod external_)"
       The kind of string v is void with string
         because of the definition of v at line 1, characters 0-24.
       But the kind of string v must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

type 'a w : word with 'a
type ok_w = string w require_external
[%%expect{|
type 'a w : word with 'a
Line 2, characters 12-20:
2 | type ok_w = string w require_external
                ^^^^^^^^
Error: This type "string w" should be an instance of type
         "('a : any mod external_)"
       The kind of string w is word
         because of the definition of w at line 1, characters 0-24.
       But the kind of string w must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}, Principal{|
type 'a w : word with 'a
Line 2, characters 12-20:
2 | type ok_w = string w require_external
                ^^^^^^^^
Error: This type "string w" should be an instance of type
         "('a : any mod external_)"
       The kind of string w is word with string
         because of the definition of w at line 1, characters 0-24.
       But the kind of string w must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

(* Products *)

type ('a : bits8 & bits8) ok_p = 'a require_external
[%%expect{|
type ('a : bits8 & bits8) ok_p = 'a require_external
|}]

type tup : bits8 & value
type bad_p = tup require_external
[%%expect{|
type tup : bits8 & value
Line 2, characters 13-16:
2 | type bad_p = tup require_external
                 ^^^
Error: This type "tup" should be an instance of type "('a : any mod external_)"
       The kind of tup is bits8 & value
         because of the definition of tup at line 1, characters 0-24.
       But the kind of tup must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

(* Kind aliases *)

kind_ kb = bits8
type 'a u : kb with 'a
type ok_alias = string u require_external
[%%expect{|
kind_ kb = bits8
type 'a u : bits8 with 'a
Line 3, characters 16-24:
3 | type ok_alias = string u require_external
                    ^^^^^^^^
Error: This type "string u" should be an instance of type
         "('a : any mod external_)"
       The kind of string u is bits8
         because of the definition of u at line 2, characters 0-22.
       But the kind of string u must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}, Principal{|
kind_ kb = bits8
type 'a u : bits8 with 'a
Line 3, characters 16-24:
3 | type ok_alias = string u require_external
                    ^^^^^^^^
Error: This type "string u" should be an instance of type
         "('a : any mod external_)"
       The kind of string u is bits8 with string
         because of the definition of u at line 2, characters 0-22.
       But the kind of string u must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

(* Learn externality through filling a sort variable *)

let g (y : string t) = (fun (_ : ('a : any mod external_)) -> ()) y
[%%expect{|
Line 1, characters 66-67:
1 | let g (y : string t) = (fun (_ : ('a : any mod external_)) -> ()) y
                                                                      ^
Error: The value "y" has type "string t" but an expression was expected of type
         "('a : bits8)"
       The kind of string t is bits8
         because of the definition of t at line 1, characters 0-25.
       But the kind of string t must be a subkind of bits8
         because of the annotation on the type variable 'a.
|}, Principal{|
Line 1, characters 66-67:
1 | let g (y : string t) = (fun (_ : ('a : any mod external_)) -> ()) y
                                                                      ^
Error: The value "y" has type "string t" but an expression was expected of type
         "('a : bits8)"
       The kind of string t is bits8 with string
         because of the definition of t at line 1, characters 0-25.
       But the kind of string t must be a subkind of bits8
         because of the annotation on the type variable 'a.
|}]

(* ...also in the reverse order: the crossing constraint is recorded on the
   sort variable's kind and re-checked once it is filled *)

let g_rev y =
  (fun (_ : ('a : any mod external_)) -> ()) y;
  (y : string t)
[%%expect{|
Line 3, characters 3-4:
3 |   (y : string t)
       ^
Error: The value "y" has type "('a : bits8)"
       but an expression was expected of type "string t"
       The kind of string t is bits8
         because of the definition of t at line 1, characters 0-25.
       But the kind of string t must be a subkind of bits8
         because of the annotation on the type variable 'a.
|}, Principal{|
Line 3, characters 3-4:
3 |   (y : string t)
       ^
Error: The value "y" has type "('a : bits8)"
       but an expression was expected of type "string t"
       The kind of string t is bits8 with string
         because of the definition of t at line 1, characters 0-25.
       But the kind of string t must be a subkind of bits8
         because of the annotation on the type variable 'a.
|}]

(* With-bounds on a rigid type variable cannot raise externality *)

type 'a ok_rigid = 'a f64 require_external
[%%expect{|
Line 1, characters 19-25:
1 | type 'a ok_rigid = 'a f64 require_external
                       ^^^^^^
Error: This type "'a f64" should be an instance of type
         "('b : any mod external_)"
       The kind of 'a f64 is float64 with 'a
         because of the definition of f64 at line 1, characters 0-29.
       But the kind of 'a f64 must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

(* Signature inclusion *)

module M : sig
  type 'a t : bits8
end = struct
  type 'a t : bits8 with 'a
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t : bits8 with 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t : bits8 with 'a end
       is not included in
         sig type 'a t : bits8 end
       Type declarations do not match:
         type 'a t : bits8 with 'a
       is not included in
         type 'a t : bits8
       The kind of the first is bits8 with 'a
         because of the definition of t at line 4, characters 2-27.
       But the kind of the first must be a subkind of bits8
         because of the definition of t at line 2, characters 2-19.
|}]

(* Abstract kinds are not assumed external *)

module F (X : sig
    kind_ k

    type t : k
  end) =
struct
  type bad = X.t require_external
end
[%%expect{|
Line 7, characters 13-16:
7 |   type bad = X.t require_external
                 ^^^
Error: This type "X.t" should be an instance of type "('a : any mod external_)"
       The kind of X.t is X.k
         because of the definition of t at line 4, characters 4-14.
       But the kind of X.t must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

(* ...but a kind alias in a signature resolves to its manifest *)

module G (X : sig
    kind_ k = bits8

    type 'a t : k with 'a
  end) =
struct
  type ok_g = string X.t require_external
end

module A = G (struct
  kind_ k = bits8

  type 'a t : k with 'a
end)
[%%expect{|
Line 7, characters 14-24:
7 |   type ok_g = string X.t require_external
                  ^^^^^^^^^^
Error: This type "string X.t" should be an instance of type
         "('a : any mod external_)"
       The kind of string X.t is bits8
         because of the definition of t at line 4, characters 4-25.
       But the kind of string X.t must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}, Principal{|
Line 7, characters 14-24:
7 |   type ok_g = string X.t require_external
                  ^^^^^^^^^^
Error: This type "string X.t" should be an instance of type
         "('a : any mod external_)"
       The kind of string X.t is bits8 with string
         because of the definition of t at line 4, characters 4-25.
       But the kind of string X.t must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

(* [any] must not cross *)

type t_any : any
type bad_any = t_any require_external
[%%expect{|
type t_any : any
Line 2, characters 15-20:
2 | type bad_any = t_any require_external
                   ^^^^^
Error: This type "t_any" should be an instance of type "('a : any mod external_)"
       The kind of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the kind of t_any must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

type pa : bits8 & any
type bad_pa = pa require_external
[%%expect{|
type pa : bits8 & any
Line 2, characters 14-16:
2 | type bad_pa = pa require_external
                  ^^
Error: This type "pa" should be an instance of type "('a : any mod external_)"
       The kind of pa is bits8 & any
         because of the definition of pa at line 1, characters 0-21.
       But the kind of pa must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]
