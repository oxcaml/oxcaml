(* TEST
 {
   flags = "-extension small_numbers -no-ikinds -w -181-220";
   expect;
 }{
   flags = "-extension small_numbers -w -181-220";
   expect;
 }
*)

(* Types whose layout is a base non-value layout (or a product of such) are
   never scanned by the GC, so they cross externality regardless of how their
   kind was constructed, not just when it is written with the [bits8]
   etc. abbreviations. *)

type ('a : any mod external_) require_external
type ('a : any mod external64) require_external64
[%%expect{|
type ('a : any mod external_) require_external
type ('a : any mod external64) require_external64
|}]

(* With-bounds should not affect externality on a non-value layout. *)
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

(* Sort variables: the annotated variable's layout is a sort variable, filled
   with [bits8] at the application. *)
let g (y : string t) = (fun (_ : ('a : any mod external_)) -> ()) y
[%%expect{|
val g : string t -> unit = <fun>
|}]

(* Kind aliases are expanded before the layout is consulted. *)
kind_ kb = bits8
type 'a u : kb with 'a
type ok_alias = string u require_external
[%%expect{|
kind_ kb = bits8
type 'a u : bits8 with 'a
type ok_alias = string u require_external
|}]

(* Products cross externality iff every component does. *)
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

(* Value layouts still do not cross. *)
type 'a s : value with 'a
type bad = string s require_external
[%%expect{|
type 'a s
Line 2, characters 11-19:
2 | type bad = string s require_external
               ^^^^^^^^
Error: This type "string s" should be an instance of type
         "('a : any mod external_)"
       The kind of string s is value
         because of the definition of s at line 1, characters 0-25.
       But the kind of string s must be a subkind of any mod external_
         because of the definition of require_external at line 1, characters 0-46.
|}]
