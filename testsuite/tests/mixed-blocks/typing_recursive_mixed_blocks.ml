(* TEST
 expect;
*)

(* Record *)

type rec_t = { rec_t : rec_t; x1 : float# }
type t = { t : rec_t; x2 : float# }

[%%expect {|
type rec_t = { rec_t : rec_t; x1 : float#; }
type t = { t : rec_t; x2 : float#; }
|}];;

(* OK: the recursive use is for a field in the value prefix. *)
let rec rec_t = { rec_t; x1 = #4.0 }

[%%expect {|
val rec_t : rec_t = {rec_t = <cycle>; x1 = <abstr>}
|}];;

(* Error: the recursive variable itself cannot have a flat layout *)
let rec x2 = let _ = { t = rec_t; x2 } in #4.0;;

[%%expect {|
Line 1, characters 34-36:
1 | let rec x2 = let _ = { t = rec_t; x2 } in #4.0;;
                                      ^^
Error: The value "x2" has type "('a : value_or_null)"
       but an expression was expected of type "float#"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a value layout
         because it's the type of the recursive variable x2.
|}];;

(* OK: an adapted version of the above error to show that the difference
   is just in the field layout. *)
let rec rec_t = let _ = { rec_t; x1 = #4.0 } in { rec_t; x1 = #4.0 };;

[%%expect {|
val rec_t : rec_t = {rec_t = <cycle>; x1 = <abstr>}
|}];;

(* Constructor: tupled args *)

type cstr = A of cstr * float#
[%%expect {|
type cstr = A of cstr * float#
|}];;

(* OK: the recursive use is for a field in the value prefix. *)
let rec rec_cstr = A (rec_cstr, #4.0)
[%%expect {|
val rec_cstr : cstr = A (<cycle>, <abstr>)
|}];;

(* Error: the recursive variable itself cannot have a flat layout *)
let rec bad_flat = let _ = A (rec_cstr, bad_flat) in #4.0;;
[%%expect {|
Line 1, characters 40-48:
1 | let rec bad_flat = let _ = A (rec_cstr, bad_flat) in #4.0;;
                                            ^^^^^^^^
Error: The value "bad_flat" has type "('a : value_or_null)"
       but an expression was expected of type "float#"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a value layout
         because it's the type of the recursive variable bad_flat.
|}];;

(* OK: an adapted version of the above error to show that the difference
   is just in the field layout. *)
let rec good_block = let _ = A (good_block, #4.0) in A (good_block, #4.0);;

[%%expect {|
val good_block : cstr = A (<cycle>, <abstr>)
|}];;

(* Constructor: inline record args *)

type cstr = A of { cstr : cstr; flt : float# }
[%%expect {|
type cstr = A of { cstr : cstr; flt : float#; }
|}];;

(* OK: the recursive use is for a field in the value prefix. *)
let rec rec_cstr = A { cstr = rec_cstr; flt = #4.0 }
[%%expect {|
val rec_cstr : cstr = A {cstr = <cycle>; flt = <abstr>}
|}];;

(* Error: the recursive variable itself cannot have a flat layout *)
let rec bad_flat = let _ = A { cstr = rec_cstr; flt = bad_flat } in #4.0;;
[%%expect {|
Line 1, characters 54-62:
1 | let rec bad_flat = let _ = A { cstr = rec_cstr; flt = bad_flat } in #4.0;;
                                                          ^^^^^^^^
Error: The value "bad_flat" has type "('a : value_or_null)"
       but an expression was expected of type "float#"
       The layout of float# is float64
         because it is the unboxed version of the primitive type float.
       But the layout of float# must be a value layout
         because it's the type of the recursive variable bad_flat.
|}];;

(* OK: an adapted version of the above error to show that the difference
   is just in the field layout. *)
let rec good_block = let _ = A { cstr = good_block; flt = #4.0 } in
                     A { cstr = good_block; flt = #4.0 };;

[%%expect {|
val good_block : cstr = A {cstr = <cycle>; flt = <abstr>}
|}];;

(* OK: the recursive variable is stored in the value prefix of a mixed block,
   reached through an unboxed product. *)

type t2 = { t2 : #(t2 option * float#); i : int }
let rec t2 = { t2 = #(Some t2, #4.0); i = 0 };;
[%%expect {|
type t2 = { t2 : #(t2 option * float#); i : int; }
val t2 : t2 = {t2 = #(Some <cycle>, <abstr>); i = 0}
|}];;

type c2 = B of #(c2 option * float#)
let rec c2 = B #(Some c2, #4.0);;
[%%expect {|
type c2 = B of #(c2 option * float#)
val c2 : c2 = B <unboxed product>
|}];;

type c3 = C of { c3 : #(c3 option * float#); i : int }
let rec c3 = C { c3 = #(Some c3, #4.0); i = 0 };;
[%%expect {|
type c3 = C of { c3 : #(c3 option * float#); i : int; }
val c3 : c3 = C {c3 = #(Some <cycle>, <abstr>); i = 0}
|}];;

type v2 = { v2 : #(v2 option * unit#) }
let rec v2 = { v2 = #(Some v2, #()) };;
[%%expect {|
type v2 = { v2 : #(v2 option * unit#); }
val v2 : v2 = {v2 = #(Some <cycle>, <abstr>)}
|}];;

(* OK: a nested recursive mixed block *)
type n = { flt : float#; n : n option }
let rec n = let rec inner = { flt = #0.; n = Some n } in inner;;
[%%expect {|
type n = { flt : float#; n : n option; }
val n : n = {flt = <abstr>; n = Some <cycle>}
|}];;
