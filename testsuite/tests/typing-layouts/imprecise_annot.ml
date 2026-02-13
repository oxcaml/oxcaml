(* TEST
 include stdlib_upstream_compatible;
 flags = "-w +191";
 expect;
*)

(* Test cases for warnings about imprecise kind annotations.
   These tests check cases where a kind annotation is weaker than
   the resulting kind. Only testing signatures for now. *)

(*********************************************************)
(* Test 1: Type constructor restricts kind to stricter one *)

(* Annotation says 'value' but type constructor requires 'immediate' *)
module type S1 = sig
  type ('a : immediate) imm_t
  type ('a : value) t = 'a imm_t
end
[%%expect{|
Line 3, characters 8-18:
3 |   type ('a : value) t = 'a imm_t
            ^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value'
but was inferred to have kind `immediate'.

module type S1 =
  sig type ('a : immediate) imm_t type ('a : immediate) t = 'a imm_t end
|}]

(* Annotation says 'any' but type constructor requires 'value' *)
module type S1b = sig
  type 'a list_t
  type ('a : any) t = 'a list_t
end
[%%expect{|
Line 3, characters 8-16:
3 |   type ('a : any) t = 'a list_t
            ^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `any'
but was inferred to have kind `value'.

module type S1b = sig type 'a list_t type 'a t = 'a list_t end
|}]

(*********************************************************)
(* Test 2: Universal quantification with stricter annotation elsewhere *)
(* Note: These examples will give errors, which is expected behavior *)

(* Binding site says 'value' but usage site requires 'immediate' *)
module type S2 = sig
  val f : ('a : value). 'a -> (('a : immediate) -> unit) -> unit
end
[%%expect{|
Line 2, characters 10-64:
2 |   val f : ('a : value). 'a -> (('a : immediate) -> unit) -> unit
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value.
       But it was inferred to have kind immediate
         because of the annotation on the type variable 'a.
|}]

(* Binding site says 'any' but usage site requires 'value' *)
module type S2b = sig
  val f : ('a : any). 'a -> (('a : value) -> unit) -> unit
end
[%%expect{|
Line 2, characters 10-58:
2 |   val f : ('a : any). 'a -> (('a : value) -> unit) -> unit
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind any.
       But it was inferred to have kind value
         because of the annotation on the type variable 'a.
|}]

(*********************************************************)
(* Test 3: Conflicting annotations on same variable *)

(* First annotation says 'value', second says 'immediate' *)
module type S3 = sig
  val f : ('a : value) -> ('a : immediate) -> unit
end
[%%expect{|
Line 2, characters 10-22:
2 |   val f : ('a : value) -> ('a : immediate) -> unit
              ^^^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value'
but was inferred to have kind `immediate'.

module type S3 = sig val f : ('a : immediate). 'a -> 'a -> unit end
|}]

(* any vs value *)
module type S3b = sig
  val f : ('a : any) -> ('a : value) -> unit
end
[%%expect{|
Line 2, characters 10-20:
2 |   val f : ('a : any) -> ('a : value) -> unit
              ^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `any'
but was inferred to have kind `value'.

module type S3b = sig val f : 'a -> 'a -> unit end
|}]

(*********************************************************)
(* Test 4: Type variable at weaker nullability than required *)

(* value_or_null in val declaration where value is inferred *)
module type S4 = sig
  val f : ('a : value_or_null) -> 'a list
end
[%%expect{|
Line 2, characters 10-30:
2 |   val f : ('a : value_or_null) -> 'a list
              ^^^^^^^^^^^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value_or_null'
but was inferred to have kind `value'.

module type S4 = sig val f : 'a -> 'a list end
|}]

(*********************************************************)
(* Test 5: Constraint clause does not trigger warning *)

(* Annotation says 'value' but constraint restricts to int (immediate).
   This does not warn since the variable is unified to int. Should it? *)
module type S5 = sig
  type ('a : value) t constraint 'a = int
end
[%%expect{|
module type S5 = sig type 'a t constraint 'a = int end
|}]

(* Same with 'any' annotation constrained to a value type *)
module type S5b = sig
  type ('a : any) t constraint 'a = string
end
[%%expect{|
module type S5b = sig type 'a t constraint 'a = string end
|}]

(*********************************************************)
(* Test 6: Structure-level type definitions *)

(* Type definition in structure with imprecise annotation *)
type ('a : immediate) imm_t = 'a
type ('a : value) t6 = 'a imm_t
[%%expect{|
type ('a : immediate) imm_t = 'a
Line 2, characters 6-16:
2 | type ('a : value) t6 = 'a imm_t
          ^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value'
but was inferred to have kind `immediate'.

type ('a : immediate) t6 = 'a imm_t
|}]

(* any -> value in structure *)
type ('a : any) t6b = 'a list
[%%expect{|
Line 1, characters 6-14:
1 | type ('a : any) t6b = 'a list
          ^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `any'
but was inferred to have kind `value_or_null'.

type ('a : value_or_null) t6b = 'a list
|}]

(*********************************************************)
(* Test 7: Let bindings with imprecise annotations *)

(* First, define a type that requires immediate *)
type ('a : immediate) imm_list = 'a list

(* Simple let with imprecise annotation - value -> immediate *)
let f7 : ('a : value) -> 'a imm_list = fun x -> [x]
[%%expect{|
type ('a : immediate) imm_list = 'a list
Line 4, characters 9-21:
4 | let f7 : ('a : value) -> 'a imm_list = fun x -> [x]
             ^^^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value'
but was inferred to have kind `immediate'.

val f7 : ('a : immediate). 'a -> 'a imm_list = <fun>
|}]

(* Ignored, since those two ['a]s have different scopes. *)
let f7b (x : ('a : any)) : 'a imm_list = [x]
[%%expect{|
val f7b : ('a : immediate). 'a -> 'a imm_list = <fun>
|}]

(* Let with conflicting annotations - first weaker *)
let f7c : ('a : value) -> ('a : immediate) -> unit = fun _ _ -> ()
[%%expect{|
Line 1, characters 10-22:
1 | let f7c : ('a : value) -> ('a : immediate) -> unit = fun _ _ -> ()
              ^^^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value'
but was inferred to have kind `immediate'.

val f7c : ('a : immediate). 'a -> 'a -> unit = <fun>
|}]

(* Let with explicit forall and imprecise annotation - value -> immediate *)
let f7d : type (a : value). a -> a imm_list = fun x -> [x]
[%%expect{|
Line 1, characters 28-43:
1 | let f7d : type (a : value). a -> a imm_list = fun x -> [x]
                                ^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value.
       But it was inferred to have kind immediate
         because of the definition of imm_list at line 1, characters 0-40.
|}]

(* Let with newtypes in parameter position *)
let f7e (type (a : any)) (x : a) : a list = [x]
[%%expect{|
Line 1, characters 25-32:
1 | let f7e (type (a : any)) (x : a) : a list = [x]
                             ^^^^^^^
Error: This pattern matches values of type "a"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1)"
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]

(*********************************************************)
(* Test 8: Module definitions with imprecise annotations *)

module M8 = struct
  type ('a : immediate) imm_t = 'a
  type ('a : value) t = 'a imm_t
end
[%%expect{|
Line 3, characters 8-18:
3 |   type ('a : value) t = 'a imm_t
            ^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value'
but was inferred to have kind `immediate'.

module M8 :
  sig type ('a : immediate) imm_t = 'a type ('a : immediate) t = 'a imm_t end
|}]

module M8b : sig
  type ('a : immediate) imm_t
  type ('a : value) t = 'a imm_t
end = struct
  type ('a : immediate) imm_t = 'a
  type ('a : value) t = 'a imm_t
end
[%%expect{|
Line 6, characters 8-18:
6 |   type ('a : value) t = 'a imm_t
            ^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value'
but was inferred to have kind `immediate'.

Line 3, characters 8-18:
3 |   type ('a : value) t = 'a imm_t
            ^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value'
but was inferred to have kind `immediate'.

module M8b :
  sig type ('a : immediate) imm_t type ('a : immediate) t = 'a imm_t end
|}]

(*********************************************************)
(* Test 9: Local type annotations in expressions *)

let _ =
  let x : ('a : value) -> 'a imm_list = fun y -> [y] in
  x 42
[%%expect{|
Line 2, characters 10-22:
2 |   let x : ('a : value) -> 'a imm_list = fun y -> [y] in
              ^^^^^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `value'
but was inferred to have kind `immediate'.

- : int imm_list = [42]
|}]

let _ =
  let f : type (a : value). a -> a imm_list = fun x -> [x] in
  f 42
[%%expect{|
Line 2, characters 28-43:
2 |   let f : type (a : value). a -> a imm_list = fun x -> [x] in
                                ^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value.
       But it was inferred to have kind immediate
         because of the definition of imm_list at line 1, characters 0-40.
|}]

(*********************************************************)
(* Test 10: Record and variant definitions *)

type ('a : value) record10 = { field : 'a list }
[%%expect{|
type 'a record10 = { field : 'a list; }
|}]

type ('a : any) variant10 = A of 'a list | B
[%%expect{|
Line 1, characters 6-14:
1 | type ('a : any) variant10 = A of 'a list | B
          ^^^^^^^^
Warning 191 [imprecise-kind-annotation]: The type variable `'a'
was annotated with kind `any'
but was inferred to have kind `value_or_null'.

type ('a : value_or_null) variant10 = A of 'a list | B
|}]

(*********************************************************)
(* Test 11: Anonymous type parameters *)

(* It's (seemingly?) impossible to constrain anonymous type parameters,
   since they are never used and thus can't be constrained by anything else.
   Test included for completeness.  *)

type (_ : immediate) imm11
type (_ : value) t11 = int imm11
[%%expect{|
type (_ : immediate) imm11
type _ t11 = int imm11
|}]

(*********************************************************)
(* Test 12: Anonymous type variables in val declarations *)

(* CR dkalinichenko: anonymous type variables also don't trigger warnings,
   as they are not tracked in TyVarEnv. *)

module type S12 = sig
  val f : (_ : value) imm_list -> unit
end
[%%expect{|
module type S12 = sig val f : ('a : immediate). 'a imm_list -> unit end
|}]
