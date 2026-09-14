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

(* Values are actually treated as having a kind that's [mod external_] *)

module M : sig
  type 'a f : float64 with 'a
  val mk : unit -> string f
end = struct
  type 'a f = #{ x : float# }
  let mk () = #{ x = #1. }
end
let use_external (type (a : float64 mod external_)) (_ : a) = ()
let ok_value = use_external (M.mk ())
[%%expect{|
module M : sig type 'a f : float64 with 'a val mk : unit -> string f end
val use_external : ('a : float64). 'a -> unit = <fun>
val ok_value : unit = ()
|}]

let ok_value_infer y =
  use_external y;
  (y : string M.f)
[%%expect{|
val ok_value_infer : string M.f -> string M.f = <fun>
|}]

(* Scannable layouts with separability [non_pointer] cross externality, and
   [non_pointer64] crosses to [external64] *)

type np : value non_pointer
type ok_np = np require_external
[%%expect{|
type np : value non_pointer
type ok_np = np require_external
|}]

type np64 : value non_pointer64
type ok_np64 = np64 require_external64
[%%expect{|
type np64 : value non_pointer64
type ok_np64 = np64 require_external64
|}]

(* ...but [non_pointer64] does not cross to [external_] *)

type bad_np64 = np64 require_external
[%%expect{|
Line 1, characters 16-20:
1 | type bad_np64 = np64 require_external
                    ^^^^
Error: This type "np64" should be an instance of type "('a : any mod external_)"
       The kind of np64 is value non_pointer64
         because of the definition of np64 at line 1, characters 0-31.
       But the kind of np64 must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

(* Products of [non_pointer] components cross *)

type npp : value non_pointer & value non_pointer
type ok_npp = npp require_external
[%%expect{|
type npp : value non_pointer & value non_pointer
type ok_npp = npp require_external
|}]

(* With-bounds cannot raise the layout-implied externality, and no redundant
   [@@ external_] is printed *)

type 'a npi : immediate with 'a
type ok_npi = string npi require_external
[%%expect{|
type 'a npi : immediate with 'a
type ok_npi = string npi require_external
|}]

type 'a npi64 : immediate64 with 'a
type ok_npi64 = string npi64 require_external64
[%%expect{|
type 'a npi64 : immediate64 with 'a
type ok_npi64 = string npi64 require_external64
|}]

(* No redundant [@@ external64] is printed on a [non_pointer64] kind's
   with-bound *)

type 'a npie : immediate64 with 'a @@ external_
[%%expect{|
type 'a npie : immediate64 with 'a
|}]

(* Learn the separability-implied crossing when a constraint recorded on a
   sort variable's kind is re-checked at fill *)

let g_np_rev y =
  (fun (_ : ('a : any mod external_)) -> ()) y;
  (y : string npi)
[%%expect{|
val g_np_rev : string npi -> string npi = <fun>
|}]

(* [any non_pointer] must not cross: a product of pointerful values is a
   subkind of it *)

type ('a : any non_pointer) require_any_non_pointer
type vv : value & value
type ok_vv_any_np = vv require_any_non_pointer
[%%expect{|
type ('a : any non_pointer) require_any_non_pointer
type vv : value & value
type ok_vv_any_np = vv require_any_non_pointer
|}]

type anp : any non_pointer
type bad_any_np = anp require_external
[%%expect{|
type anp : any non_pointer
Line 2, characters 18-21:
2 | type bad_any_np = anp require_external
                      ^^^
Error: This type "anp" should be an instance of type "('a : any mod external_)"
       The kind of anp is any non_pointer
         because of the definition of anp at line 1, characters 0-26.
       But the kind of anp must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

(* An abstract kind's [non_pointer] must not cross either *)

module F_np (X : sig
    kind_ k

    type t : k non_pointer
  end) =
struct
  type bad_np_abstract = X.t require_external
end
[%%expect{|
Line 7, characters 25-28:
7 |   type bad_np_abstract = X.t require_external
                             ^^^
Error: This type "X.t" should be an instance of type "('a : any mod external_)"
       The kind of X.t is X.k non_pointer
         because of the definition of t at line 4, characters 4-26.
       But the kind of X.t must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]

(* Printing: [mod external_] is redundant on a [non_pointer] kind, and
   [mod external64] on a [non_pointer64] kind *)

type npe : value non_pointer mod external_
[%%expect{|
type npe : value non_pointer
|}]

type npe64 : value non_pointer64 mod external64
[%%expect{|
type npe64 : value non_pointer64
|}]

(* ...but [mod external_] is not redundant on a [non_pointer64] kind *)

type np64e : value non_pointer64 mod external_
[%%expect{|
type np64e : value non_pointer64 mod external_
|}]
