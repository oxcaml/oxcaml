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
type ok = string t require_external
|}]

type ok64 = string t require_external64
[%%expect{|
type ok64 = string t require_external64
|}]

type 'a f64 : float64 with 'a
type ok_f64 = string f64 require_external
[%%expect{|
type 'a f64 : float64 with 'a
type ok_f64 = string f64 require_external
|}]

type 'a v : void with 'a
type ok_v = string v require_external
[%%expect{|
type 'a v : void with 'a
type ok_v = string v require_external
|}]

type 'a w : word with 'a
type ok_w = string w require_external
[%%expect{|
type 'a w : word with 'a
type ok_w = string w require_external
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
type ok_alias = string u require_external
|}]

(* Learn externality through filling a sort variable *)

let g (y : string t) = (fun (_ : ('a : any mod external_)) -> ()) y
[%%expect{|
val g : string t -> unit = <fun>
|}]

(* ...also in the reverse order: the crossing constraint is recorded on the
   sort variable's kind and re-checked once it is filled *)

let g_rev y =
  (fun (_ : ('a : any mod external_)) -> ()) y;
  (y : string t)
[%%expect{|
val g_rev : string t -> string t = <fun>
|}]

(* With-bounds on a rigid type variable cannot raise externality *)

type 'a ok_rigid = 'a f64 require_external
[%%expect{|
type 'a ok_rigid = 'a f64 require_external
|}]

(* Signature inclusion *)

module M : sig
  type 'a t : bits8
end = struct
  type 'a t : bits8 with 'a
end
[%%expect{|
module M : sig type 'a t : bits8 end
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
module G :
  functor (X : sig kind_ k = bits8 type 'a t : bits8 with 'a end) ->
    sig type ok_g = string X.t require_external end
module A : sig type ok_g end
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

(* For a kind that crosses externality due to its layout despite its with-bound,
   we don't print a redundant [@@ external_] *)

type ('a : bits8) c : immediate with 'a
type ok_nested = string t c require_external
[%%expect{|
type ('a : bits8) c : immediate with 'a
type ok_nested = string t c require_external
|}]

type ('a : bits8) c2 : value mod portable with 'a
[%%expect{|
type ('a : bits8) c2 : value mod portable with 'a
|}]
