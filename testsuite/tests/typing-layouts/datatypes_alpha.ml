(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Tests for jkinds in algebraic datatypes *)

(* CR layouts v5: add mixed block restriction tests. *)

type t_void : void
type t_any : any
type t_value : value
type t_immediate : immediate;;

(***************************************************)
(* Test 1: constructor arguments may have any sort *)
type t1_void = T1_void of t_void
type t1_value = T1_value of t_value
type t1_immediate = T1_immediate of t_immediate

type t1_mixed1 = T1_mixed1 of t_void * t_immediate
type t1_mixed2 = T1_mixed2 of t_immediate * t_value * t_void
type t1_mixed3 = T1_mixed3 of t_value * t_immediate
[%%expect {|
type t_void : void
type t_any : any
type t_value
type t_immediate : immediate
type t1_void = T1_void of t_void
type t1_value = T1_value of t_value
type t1_immediate = T1_immediate of t_immediate
type t1_mixed1 = T1_mixed1 of t_void * t_immediate
type t1_mixed2 = T1_mixed2 of t_immediate * t_value * t_void
type t1_mixed3 = T1_mixed3 of t_value * t_immediate
|}];;

type 'a t1_constraint = T1_con of 'a constraint 'a = 'b t1_constraint'
and 'b t1_constraint' = t_void
[%%expect {|
type 'a t1_constraint = T1_con of 'a constraint 'a = 'b t1_constraint'
and 'b t1_constraint' = t_void
|}]

(************************************)
(* Test 2: but not the "any" layout *)
type t2_any1 = T2_any1 of t_any
[%%expect {|
Line 1, characters 15-31:
1 | type t2_any1 = T2_any1 of t_any
                   ^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a constructor field.
|}];;

type t2_any2 = T2_any2 of t_immediate * t_any
[%%expect {|
Line 1, characters 15-45:
1 | type t2_any2 = T2_any2 of t_immediate * t_any
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a constructor field.
|}];;

type t2_any3 = T2_any3 of t_any * t_value
[%%expect {|
Line 1, characters 15-41:
1 | type t2_any3 = T2_any3 of t_any * t_value
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a constructor field.
|}];;

type 'a t1_constraint = T1_con of 'a constraint 'a = 'b t1_constraint'
and 'b t1_constraint' = t_any
[%%expect {|
Line 2, characters 0-29:
2 | and 'b t1_constraint' = t_any
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of 'b t1_constraint' is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of 'b t1_constraint' must be representable
         because it instantiates an unannotated type parameter of t1_constraint.
|}]
(* CR layouts errors: this error is blamed on the wrong piece *)

(******************************************************)
(* Test 3: void allowed in records, but not by itself *)
type t3_value = { x : t_value }
type t3_immediate = { x : t_immediate }

type t3_cvalue = C of { x : t_value }
type t3_cimmediate = C of { x : t_immediate }


type t3_mixed1 = { x : t_void; y : t_immediate }
type t3_mixed2 = { x : t_immediate; y : t_value; z : t_void }
type t3_mixed3 = { x : t_value; y : t_immediate }

type t3_cmixed1 = C of { x : t_void; y : t_immediate }
type t3_cmixed2 = C of { x : t_immediate; y : t_value; z : t_void }
type t3_cmixed3 = C of { x : t_value; y : t_immediate }
[%%expect {|
type t3_value = { x : t_value; }
type t3_immediate = { x : t_immediate; }
type t3_cvalue = C of { x : t_value; }
type t3_cimmediate = C of { x : t_immediate; }
type t3_mixed1 = { x : t_void; y : t_immediate; }
type t3_mixed2 = { x : t_immediate; y : t_value; z : t_void; }
type t3_mixed3 = { x : t_value; y : t_immediate; }
type t3_cmixed1 = C of { x : t_void; y : t_immediate; }
type t3_cmixed2 = C of { x : t_immediate; y : t_value; z : t_void; }
type t3_cmixed3 = C of { x : t_value; y : t_immediate; }
|}];;

(* CR layouts v5: allow this *)
type t3_void = { x : t_void };;
[%%expect {|
Line 1, characters 0-29:
1 | type t3_void = { x : t_void };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]

type t3_cvoid = C of { x : t_void }
[%%expect{|
Line 1, characters 16-35:
1 | type t3_cvoid = C of { x : t_void }
                    ^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]

(**************************)
(* Test 4: but any is not *)
type t4_any1 = { x : t_any }
[%%expect {|
Line 1, characters 17-26:
1 | type t4_any1 = { x : t_any }
                     ^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it is the type of record field x.
|}];;

type t4_any2 = { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 34-43:
1 | type t4_any2 = { x : t_immediate; y : t_any }
                                      ^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it is the type of record field y.
|}];;

type t4_any3 =  { x : t_any; y : t_value }
[%%expect {|
Line 1, characters 18-28:
1 | type t4_any3 =  { x : t_any; y : t_value }
                      ^^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it is the type of record field x.
|}];;

type t4_cany1 = C of { x : t_any }
[%%expect {|
Line 1, characters 23-32:
1 | type t4_cany1 = C of { x : t_any }
                           ^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it is the type of record field x.
|}];;

type t4_cany2 = C of { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 40-49:
1 | type t4_cany2 = C of { x : t_immediate; y : t_any }
                                            ^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it is the type of record field y.
|}];;

type t4_cany3 = C of { x : t_any; y : t_value }
[%%expect {|
Line 1, characters 23-33:
1 | type t4_cany3 = C of { x : t_any; y : t_value }
                           ^^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it is the type of record field x.
|}];;

(*********************************************************)
(* Test 5: These same rules apply to extensible variants *)
type t5 = ..

type t5 += T5_1 of t_void
type t5 += T5_2 of t_value
type t5 += T5_3 of t_immediate

type t5 += T5_4 of t_void * t_immediate
type t5 += T5_5 of t_immediate * t_value * t_void
type t5 += T5_6 of t_value * t_immediate;;
[%%expect{|
type t5 = ..
Line 3, characters 11-25:
3 | type t5 += T5_1 of t_void
               ^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}]


type t5 += T5_7 of t_any
[%%expect {|
Line 1, characters 11-24:
1 | type t5 += T5_7 of t_any
               ^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a constructor field.
|}];;

type t5 += T5_8 of t_immediate * t_any
[%%expect {|
Line 1, characters 11-38:
1 | type t5 += T5_8 of t_immediate * t_any
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a constructor field.
|}];;

type t5 += T5_9 of t_any * t_value
[%%expect {|
Line 1, characters 11-34:
1 | type t5 += T5_9 of t_any * t_value
               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of a constructor field.
|}];;

type t5 += T5_11 of { x : t_value }
type t5 += T5_12 of { x : t_immediate }

type t5 += T5_13 of { x : t_void; y : t_immediate }
type t5 += T5_14 of { x : t_immediate; y : t_value; z : t_void }
type t5 += T5_15 of { x : t_value; y : t_immediate };;
[%%expect{|
type t5 += T5_11 of { x : t_value; }
type t5 += T5_12 of { x : t_immediate; }
Line 4, characters 11-51:
4 | type t5 += T5_13 of { x : t_void; y : t_immediate }
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}];;

type t5 += T5_16 of { x : t_void }
[%%expect{|
Line 1, characters 11-34:
1 | type t5 += T5_16 of { x : t_void }
               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]

type t5 += T5_17 of { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 39-48:
1 | type t5 += T5_17 of { x : t_immediate; y : t_any }
                                           ^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of t_any is any
         because of the definition of t_any at line 2, characters 0-16.
       But the layout of t_any must be representable
         because it is the type of record field y.
|}];;

(**************************************************************************)
(* Test 6: fields in all-float records get jkind value.  may change in the
   future, but record fields must at least be representable. *)
type t6 = { fld6 : float }
type ('a : immediate) s6 = S6 of 'a

let f6 x =
  let { fld6 = fld6 } = x in fld6

let f6' x =
  let { fld6 = fld6 } = x in S6 fld6;;
[%%expect {|
type t6 = { fld6 : float; }
type ('a : immediate) s6 = S6 of 'a
val f6 : t6 -> float = <fun>
Line 8, characters 32-36:
8 |   let { fld6 = fld6 } = x in S6 fld6;;
                                    ^^^^
Error: This expression has type "float" but an expression was expected of type
         "('a : immediate)"
       The kind of float is value mod many unyielding stateless immutable
         because it is the primitive type float.
       But the kind of float must be a subkind of immediate
         because of the definition of s6 at line 2, characters 0-35.
|}];;

(*****************************************************)
(* Test 7: Recursive propagation of immediacy checks *)

(* See Note [Default layouts in transl_declaration] in Typedecl. *)
type t7 = A | B | C | D of t7_void
and t7_2 = { x : t7 } [@@unboxed]
and t7_void : void mod everything

type t7_3 : immediate = t7_2

[%%expect{|
type t7 = A | B | C | D of t7_void
and t7_2 = { x : t7; } [@@unboxed]
and t7_void : void mod everything
type t7_3 = t7_2
|}]

(***********************************************************************)
(* Test 8: Type parameters in the presence of recursive concrete usage *)

type ('a : void) void_t

[%%expect {|
type ('a : void) void_t
|}]

type 'b t = 'b void_t * t2
and t2 = t_void void_t

[%%expect {|
type ('b : void) t = 'b void_t * t2
and t2 = t_void void_t
|}]

type 'b t = 'b void_t * t2
and t2 = Mk1 of t_void t | Mk2

[%%expect {|
type ('b : void) t = 'b void_t * t2
and t2 = Mk1 of t_void t | Mk2
|}]

type 'a t8_5 = { x : 'a t8_6; y : string}
and 'a t8_6 = 'a void_t;;
[%%expect {|
Line 1, characters 21-28:
1 | type 'a t8_5 = { x : 'a t8_6; y : string}
                         ^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is void
           because of the definition of void_t at line 1, characters 0-23.
         But the layout of 'a must overlap with value
           because it instantiates an unannotated type parameter of t8_5,
           chosen to have layout value.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]

type ('a : immediate) imm_t
type 'a t10 = { x : 'a t10_2; y : string}
and 'a t10_2 = 'a imm_t;;

[%%expect{|
type ('a : immediate) imm_t
Line 2, characters 20-28:
2 | type 'a t10 = { x : 'a t10_2; y : string}
                        ^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The kind of 'a is value
           because it instantiates an unannotated type parameter of t10,
           chosen to have kind value.
         But the kind of 'a must be a subkind of immediate
           because of the definition of imm_t at line 1, characters 0-27.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]

type 'a t = { x : ('b : void). 'b t -> 'b t }
            constraint 'a = float#

[%%expect{|
Line 1, characters 18-43:
1 | type 'a t = { x : ('b : void). 'b t -> 'b t }
                      ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of float# is float64
           because it is the unboxed version of the primitive type float.
         But the layout of float# must be a sublayout of void
           because of the annotation on the universal variable 'b.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]

(*****************************************************************************)
(* Test 9: Looking through polytypes in mutually recursive type declarations *)

(* Doesn't need layouts_alpha. *)

(*****************************************************)
(* Test 10: Constraints and parameter kind inference *)

(* Doesn't need layouts_alpha. *)

(*************************************************)
(* Test 11: Projecting from an @@unboxed record. *)

(* Doesn't need layouts_alpha. *)
