(* TEST
 flags = "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(* A mode polymorphic primitive: identity *)

external magic : 'a @ [< 'm] -> 'b @ [> 'm] = "%identity"
[%%expect{|
external magic : 'a @ [< 'm] -> 'b @ [> 'm] = "%identity"
|}];;

(* we can instantiate it under various annotations *)
(magic : 'a -> 'a);;
[%%expect{|
- : 'a -> 'a = <fun>
|}];;

(magic : 'a @ local -> 'a @ local);;
[%%expect{|
- : 'a @ local -> 'a @ local = <fun>
|}];;

(magic : 'a @ unique -> 'a @ unique);;
[%%expect{|
- : 'a @ unique -> 'a @ unique = <fun>
|}];;

(fun x -> magic x : 'a -> 'a);;
[%%expect{|
- : 'a -> 'a = <fun>
|}];;

(fun x -> magic x : 'a @ unique -> 'a @ unique);;
[%%expect{|
- : 'a @ unique -> 'a @ unique = <fun>
|}];;

(fun x -> exclave_ magic x : 'a @ local -> 'a @ local);;
[%%expect{|
- : 'a @ local -> 'a @ local = <fun>
|}];;

(fun x -> magic x : 'a @ yielding -> 'a @ yielding);;
[%%expect{|
- : 'a @ yielding -> 'a @ yielding = <fun>
|}];;

(* But not under an instantiation that violates the mode polymorphic signature *)
(fun x -> magic x : 'a @ aliased -> 'a @ unique);;
[%%expect{|
Line 1, characters 10-17:
1 | (fun x -> magic x : 'a @ aliased -> 'a @ unique);;
              ^^^^^^^
Error: This value is "aliased" but is expected to be "unique".
|}];;

(* If we expose a primitive as a val, this should be equivalent to eta-expanding
   the primitive. This means we lose some polymorphism over locality: recall
   that [fun x -> id x] pushes a global bound to the argument since we can't remember
   regionality across function calls. *)

module Id_locality_should_fail : sig
  val id : 'a @ [< 'm] -> 'a @ [> 'm]
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity" end
       is not included in
         sig val id : 'a @ [< 'm] -> 'a @ [> 'm] end
       Values do not match:
         external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
       is not included in
         val id : 'a @ [< 'm] -> 'a @ [> 'm]
       The type "'a @ [< 'm > past('n)] -> 'a @ [> 'm | local]"
       is not compatible with the type "'a @ [< 'o & past('n)] -> 'a @ [> 'o]"
       The return mode was expected to be "global" but is "local"
|}];;

(* We can stay polymorphic over other axes *)

module Foo : sig
  val id : 'a @ [< 'm] -> 'a @ [> 'm | local]
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
module Foo : sig val id : 'a @ [< 'm] -> 'a @ [> 'm | local] end
|}]

module Foo : sig
  val id : 'a @ [< 'm & global] -> 'a @ [> 'm]
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
module Foo : sig val id : 'a @ [< 'm & global] -> 'a @ [> 'm] end
|}]

module Foo : sig
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
module Foo : sig external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity" end
|}];;

(* A primitive can have a fully polymorphic curry mode. The following [add] will
   have a curry mode with the mode @ [> close('m) | nonportable stateful dynamic].
   Its locality will follow from the locality the primitive is instantiated at at
   the call-site. *)

external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
  = "%int32_add"
[%%expect{|
external add : int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
  = "%int32_add"
|}];;

(fun x y -> add x y);;
[%%expect{|
- : int32 @ [< global] -> int32 @ [< global] -> int32 @ [> dynamic] = <fun>
|}];;

(fun (x @ local) y -> exclave_ add x y);;
[%%expect{|
- : int32 @ [> local] -> int32 @ 'm -> int32 @ [> local dynamic] = <fun>
|}];;

(add : int32 -> int32 -> int32);;
[%%expect{|
- : int32 -> int32 -> int32 = <fun>
|}];;

(add : int32 @ local -> int32 @ local -> int32 @ local);;
[%%expect{|
- : int32 @ local -> int32 @ local -> int32 @ local = <fun>
|}];;

(fun x -> add x);;
[%%expect{|
- : int32 @ [< 'n mod contended immutable & global] ->
    (int32 @ [< 'm & global] ->
     int32 @ [> 'm | 'n mod many portable forkable unyielding stateless]) @ [> nonportable stateful dynamic]
= <fun>
|}];;

(fun (x @ local) -> exclave_ add x);;
[%%expect{|
- : int32 @ [> local] ->
    (int32 @ [< 'm] -> int32 @ [> 'm | local]) @ [> local nonportable stateful dynamic]
= <fun>
|}];;

let () =
  let use_global (_ @ global) = () in
  let xh = Int32.of_int 3 in
  let xl = stack_ (Int32.of_int 4) in
  use_global (add xh);
  use_global (add xl) (* should fail *)
[%%expect{|
Line 6, characters 13-21:
6 |   use_global (add xl) (* should fail *)
                 ^^^^^^^^
Error: This value is "local" but is expected to be "global".
Hint: This is a partial application
      Adding 1 more argument will make the value non-local
|}];;

module M_stateless = struct
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    @@ stateless = "%int32_add"
end
[%%expect{|
module M_stateless :
  sig
    external add : int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
      = "%int32_add"
  end
|}];;

(fun x -> M_stateless.add x);;
[%%expect{|
- : int32 @ [< 'n mod contended immutable & global] ->
    (int32 @ [< 'm & global] ->
     int32 @ [> 'm | 'n mod many portable forkable unyielding stateless]) @ [> nonportable stateful dynamic]
= <fun>
|}];;

module M_stateless_val : sig
  val add : int32 @ [< 'm & global] -> int32 @ [< 'm & global] -> int32 @ [> 'm]
    @@ stateless
end = struct
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    @@ stateless = "%int32_add"
end
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
6 |     @@ stateless = "%int32_add"
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external add :
             int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
             = "%int32_add"
         end
       is not included in
         sig
           val add :
             int32 @ [< 'n & global] ->
             int32 @ [< 'm & global] -> int32 @ [> 'm | 'n] @@ stateless
         end
       Values do not match:
         external add :
           int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
           = "%int32_add"
       is not included in
         val add :
           int32 @ [< 'n & global] ->
           int32 @ [< 'm & global] -> int32 @ [> 'm | 'n] @@ stateless
       The type
         "int32 @ [< 'n & global] ->
         int32 @ [< 'm & global] -> int32 @ [> 'm | 'n]"
       is not compatible with the type
         "int32 @ [< 'q & past('o) & global] ->
         (int32 @ [< 'p & global] -> int32 @ [> 'p | 'q]) @ [> past('o)]"
       The return mode was expected to be "stateless" but is "stateful"
|}];;

(* We do not lose [@local_opt] *)

external add_old :
  (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
  = "%int32_add"
[%%expect{|
external add_old :
  (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
  = "%int32_add"
|}];;

(add_old : int32 -> int32 -> int32);;
[%%expect{|
- : int32 -> int32 -> int32 = <fun>
|}];;

(fun x -> add_old x);;
[%%expect{|
- : int32 @ [< global] -> (int32 -> int32) @ [> nonportable stateful dynamic]
= <fun>
|}];;

external add_indep : int32 @ [< 'm] -> int32 @ [< 'n] -> int32 @ [> 'm]
  = "%int32_add"
[%%expect{|
external add_indep : int32 @ [< 'm] -> int32 @ 'n -> int32 @ [> 'm]
  = "%int32_add"
|}];;

(* Although [add_indep] shows no relation between the second argument and
   the return, [add_indep x y] is local when [y] is local.

   This is because of our handling of the signature's locality axis: the locality of
   a mode polymorphic argument/return in a primite gets overwritten at the call-site,
   where we conservatively approximate that the locality of a return is the join of all
   preceding arguments. *)

(* CR ageorges: perhaps this is not needed if we are able to distinguish curry-modes
   from the final return *)

(fun x (y @ local) -> add_indep x y);;
[%%expect{|
Line 1, characters 22-35:
1 | (fun x (y @ local) -> add_indep x y);;
                          ^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}];;

(* We can mix constant and polymorphic arguments *)

external add_local_arg : int32 @ local -> int32 @ [< 'm] -> int32 @ [> 'm]
  = "%int32_add"
[%%expect{|
external add_local_arg : int32 @ local -> int32 @ [< 'm] -> int32 @ [> 'm]
  = "%int32_add"
|}];;

(fun (x @ local) y -> add_local_arg x y);;
[%%expect{|
- : int32 @ [> local] -> int32 @ [< global] -> int32 @ [> dynamic] = <fun>
|}];;

(fun x (y @ local) -> exclave_ add_local_arg x y);;
[%%expect{|
- : int32 @ [< global] -> int32 @ [> local] -> int32 @ [> local dynamic] =
<fun>
|}];;

(* Here the locality of the first argument will be considered [Prim_global] *)

external add_global :
  int32 @ [< 'm & global] -> int32 @ [< 'm] -> int32 @ [> 'm] = "%int32_add"
[%%expect{|
external add_global :
  int32 @ [< 'n & global] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
  = "%int32_add"
|}];;

(fun (x @ local) y -> exclave_ add_global x y);;
[%%expect{|
Line 1, characters 42-43:
1 | (fun (x @ local) y -> exclave_ add_global x y);;
                                              ^
Error: This value is "local" but is expected to be "global".
|}];;

(fun x (y @ local) -> exclave_ add_global x y);;
[%%expect{|
- : int32 @ [< global] -> int32 @ [> local] -> int32 @ [> local dynamic] =
<fun>
|}];;

(* Primitives with higher-order functions, and more complex mode signatures *)

external revapply : 'a @ [< 'm] -> ('a @ [> 'm] -> 'b) -> 'b = "%revapply"
[%%expect{|
external revapply : 'a @ [< 'm] -> ('a @ [> 'm] -> 'b) -> 'b = "%revapply"
|}];;

let use_global (_ @ global) = ()
[%%expect{|
val use_global : 'a @ [< global] -> unit @ 'm = <fun>
|}];;

(fun x -> revapply x (fun y -> use_global y));;
[%%expect{|
- : 'a @ [< global] -> unit @ [> dynamic] = <fun>
|}, Principal{|
- : 'a @ [< global] -> unit @ [> aliased nonportable stateful dynamic] =
<fun>
|}];;

(fun (x @ local) -> revapply x (fun y -> use_global y));;
[%%expect{|
Line 1, characters 52-53:
1 | (fun (x @ local) -> revapply x (fun y -> use_global y));;
                                                        ^
Error: This value is "local" to the parent region but is expected to be "global".
|}];;

(* [@local_opt] should never be used in a signature with mode polymorphic annotations *)

external add_opt : (int32 [@local_opt]) -> int32 @ [< 'm] -> int32 @ [> 'm]
  = "%int32_add"
[%%expect{|
Line 1, characters 19-75:
1 | external add_opt : (int32 [@local_opt]) -> int32 @ [< 'm] -> int32 @ [> 'm]
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external add_opt_res :
  int32 @ [< 'm] -> int32 @ [< 'm] -> (int32 [@local_opt]) = "%int32_add"
[%%expect{|
Line 2, characters 2-58:
2 |   int32 @ [< 'm] -> int32 @ [< 'm] -> (int32 [@local_opt]) = "%int32_add"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_arg_var : (int32 [@local_opt]) @ 'm -> int32 = "%int32_neg"
[%%expect{|
Line 1, characters 23-57:
1 | external opt_arg_var : (int32 [@local_opt]) @ 'm -> int32 = "%int32_neg"
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_res_var : int32 -> (int32 [@local_opt]) @ [> 'm] = "%int32_neg"
[%%expect{|
Line 1, characters 23-61:
1 | external opt_res_var : int32 -> (int32 [@local_opt]) @ [> 'm] = "%int32_neg"
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_global_var :
  (int32 [@local_opt]) -> int32 @ [< 'm & global] -> (int32 [@local_opt])
  = "%int32_add"
[%%expect{|
Line 2, characters 2-73:
2 |   (int32 [@local_opt]) -> int32 @ [< 'm & global] -> (int32 [@local_opt])
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_callback_arg :
  ('a [@local_opt]) -> ('a @ [< 'm] -> 'b) -> 'b = "%revapply"
[%%expect{|
Line 2, characters 2-48:
2 |   ('a [@local_opt]) -> ('a @ [< 'm] -> 'b) -> 'b = "%revapply"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_callback_res :
  ('a [@local_opt]) -> ('a -> 'b @ [> 'm]) -> 'b = "%revapply"
[%%expect{|
Line 2, characters 2-48:
2 |   ('a [@local_opt]) -> ('a -> 'b @ [> 'm]) -> 'b = "%revapply"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_variant :
  ('a [@local_opt]) -> [ `A of 'a @ 'm -> unit ] -> unit = "caml_opt_variant"
[%%expect{|
Line 2, characters 2-56:
2 |   ('a [@local_opt]) -> [ `A of 'a @ 'm -> unit ] -> unit = "caml_opt_variant"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_object :
  ('a [@local_opt]) -> < f : 'a @ 'm -> unit > -> unit = "caml_opt_object"
[%%expect{|
Line 2, characters 2-54:
2 |   ('a [@local_opt]) -> < f : 'a @ 'm -> unit > -> unit = "caml_opt_object"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_polytype :
  ('a [@local_opt]) -> ('b. 'b @ 'm -> unit) -> unit = "caml_opt_polytype"
[%%expect{|
Line 2, characters 2-52:
2 |   ('a [@local_opt]) -> ('b. 'b @ 'm -> unit) -> unit = "caml_opt_polytype"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_constr_arg :
  ('a [@local_opt]) -> ('a @ [< 'm] -> unit) list -> unit = "caml_opt_list"
[%%expect{|
Line 2, characters 2-57:
2 |   ('a [@local_opt]) -> ('a @ [< 'm] -> unit) list -> unit = "caml_opt_list"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_returned_closure :
  (int32 [@local_opt]) -> (int32 @ [< 'm] -> int32 @ [> 'm])
  = "caml_opt_closure"
[%%expect{|
Line 2, characters 2-60:
2 |   (int32 [@local_opt]) -> (int32 @ [< 'm] -> int32 @ [> 'm])
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "[@local_opt]" cannot be used in an external declaration
       that also uses mode variables.
|}];;

external opt_only :
  (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
  = "%int32_add"
[%%expect{|
external opt_only :
  (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
  = "%int32_add"
|}];;

(* Mode variables in argument position only *)

(* Note that the printed signature prints this as independent mode variables, which is
   equivalent due to mode weakening. *)
external set32 : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
  = "%caml_bytes_set32"
[%%expect{|
external set32 : bytes @ 'o -> int @ 'n -> int32 @ 'm -> unit
  = "%caml_bytes_set32"
|}];;

(* Various partial applications of [set32] *)

let set32_one_arg = set32 (Bytes.create 4)
[%%expect{|
val set32_one_arg : int -> (int32 -> unit) @ [> nonportable stateful] = <fun>
|}];;

let set32_two_args = set32 (Bytes.create 4) 0
[%%expect{|
val set32_two_args : int32 -> unit = <fun>
|}];;

(fun (b @ local) -> exclave_ set32 b);;
[%%expect{|
- : bytes @ [> local] ->
    (int @ 'n -> int32 @ 'm -> unit) @ [> local nonportable stateful dynamic]
= <fun>
|}];;

(fun (b @ local) -> exclave_ set32 b 0);;
[%%expect{|
- : bytes @ [> local] ->
    (int32 @ 'm -> unit) @ [> local nonportable stateful dynamic]
= <fun>
|}];;

(fun (b @ local) -> set32 b);;
[%%expect{|
Line 1, characters 20-27:
1 | (fun (b @ local) -> set32 b);;
                        ^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
Hint: This is a partial application
      Adding 2 more arguments will make the value non-local
|}];;

module Set32_val : sig
  val set32 : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
end = struct
  external set32 : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
    = "%caml_bytes_set32"
end
[%%expect{|
module Set32_val :
  sig val set32 : bytes @ 'o -> int @ 'n -> int32 @ 'm -> unit end
|}];;

let set32_val_one_arg = Set32_val.set32 (Bytes.create 4)
[%%expect{|
Line 1, characters 24-56:
1 | let set32_val_one_arg = Set32_val.set32 (Bytes.create 4)
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
Hint: This is a partial application
      Adding 2 more arguments will make the value non-local
|}];;

module Set32_legacy : sig val set32 : bytes -> int -> int32 -> unit end =
struct
  external set32 : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
    = "%caml_bytes_set32"
end
[%%expect{|
module Set32_legacy : sig val set32 : bytes -> int -> int32 -> unit end
|}];;

let set32_legacy_one_arg = Set32_legacy.set32 (Bytes.create 4)
[%%expect{|
val set32_legacy_one_arg : int -> int32 -> unit = <fun>
|}];;

(* The return value has constant locality. Unlike [Id_locality_should_fail] the following
   should succeed. *)

module type Same_type = sig
  external e : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
    = "%caml_bytes_set32"
  val v : bytes @ [< 'm] -> int @ [< 'm] -> int32 @ [< 'm] -> unit
end
[%%expect{|
module type Same_type =
  sig
    external e : bytes @ 'o -> int @ 'n -> int32 @ 'm -> unit
      = "%caml_bytes_set32"
    val v : bytes @ 'o -> int @ 'n -> int32 @ 'm -> unit
  end
|}];;

let same_type_external (module X : Same_type) = X.e (Bytes.create 4)
[%%expect{|
val same_type_external :
  (module Same_type) @ [< many] ->
  (int @ [< past('m) & global] ->
   (int32 @ [< global] -> unit) @ [> past('m) | nonportable stateful]) @ [> nonportable stateful dynamic] =
  <fun>
|}];;

let same_type_val (module X : Same_type) = X.v (Bytes.create 4)
[%%expect{|
Line 1, characters 43-63:
1 | let same_type_val (module X : Same_type) = X.v (Bytes.create 4)
                                               ^^^^^^^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
Hint: This is a partial application
      Adding 2 more arguments will make the value non-local
|}];;

external set32_local_bytes :
  bytes @ local -> int @ [< 'm] -> int32 @ [< 'm] -> unit
  = "%caml_bytes_set32"
[%%expect{|
external set32_local_bytes : bytes @ local -> int @ 'n -> int32 @ 'm -> unit
  = "%caml_bytes_set32"
|}];;

(fun (b @ local) -> exclave_ set32_local_bytes b 0);;
[%%expect{|
- : bytes @ [< uncontended read_write > local] ->
    (int32 @ 'm -> unit) @ [> local nonportable unforkable yielding stateful dynamic]
= <fun>
|}];;

(* The partial application of [eq] closes over a type that does not mode cross *)

external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
[%%expect{|
external eq : 'a @ 'n -> 'a @ 'm -> bool = "%equal"
|}];;

let eq_one_arg = eq "a"
[%%expect{|
val eq_one_arg : string -> bool = <fun>
|}];;

(* Since [Eq_val.eq] is a [val], the curry mode will be lower-bounded by [local], and will
   always be stack allocated *)

module Eq_val : sig val eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool end = struct
  external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
end
[%%expect{|
module Eq_val : sig val eq : 'a @ 'n -> 'a @ 'm -> bool end
|}];;

let eq_val_one_arg = Eq_val.eq "a"
[%%expect{|
Line 1, characters 21-34:
1 | let eq_val_one_arg = Eq_val.eq "a"
                         ^^^^^^^^^^^^^
Error: This value is "local" but is expected to be "global".
Hint: This is a partial application
      Adding 1 more argument will make the value non-local
|}];;

(* [Eq_val_close.eq] can't be inhabited, not even by a primitive which by itself
   behaves with a polymorphic curry mode. However, a [val] can't have a curry mode
   that is polymorphic over locality. *)

module Eq_val_close : sig
  val eq : 'a @ [< 'm] -> ('a @ [< 'm] -> bool) @ [> close('m)]
end = struct
  external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external eq : 'a @ 'n -> 'a @ 'm -> bool = "%equal" end
       is not included in
         sig
           val eq :
             'a @ [< past('n)] ->
             ('a @ [< past('m)] -> bool) @ [> past('m) | past('n)]
         end
       Values do not match:
         external eq : 'a @ 'n -> 'a @ 'm -> bool = "%equal"
       is not included in
         val eq :
           'a @ [< past('n)] ->
           ('a @ [< past('m)] -> bool) @ [> past('m) | past('n)]
       The type "'a @ [> past('n)] -> 'a @ [> past('m)] -> bool"
       is not compatible with the type
         "'a @ [< past('p) & past('n)] ->
         ('a @ [< past('o) & past('m)] -> bool) @ [> past('o) | past('p)]"
       The return mode was expected to be "global" but is "local"
|}];;

module Eq_val_shared : sig
  val eq : 'a @ [< 'm] -> ('a @ [< 'm] -> bool) @ [> 'm]
end = struct
  external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external eq : 'a @ [< 'm] -> 'a @ [< 'm] -> bool = "%equal"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig external eq : 'a @ 'n -> 'a @ 'm -> bool = "%equal" end
       is not included in
         sig val eq : 'a @ [< 'n] -> ('a @ [< 'm] -> bool) @ [> 'm | 'n] end
       Values do not match:
         external eq : 'a @ 'n -> 'a @ 'm -> bool = "%equal"
       is not included in
         val eq : 'a @ [< 'n] -> ('a @ [< 'm] -> bool) @ [> 'm | 'n]
       The type "'a @ [> past('n)] -> 'a @ [> past('m)] -> bool"
       is not compatible with the type
         "'a @ [< 'p & past('n)] ->
         ('a @ [< 'o & past('m)] -> bool) @ [> 'o | 'p]"
       The return mode was expected to be "global" but is "local"
|}];;

(* [p] becomes local because we apply it to the local [y], and the conservative estimate
   of the locality of the curry mode takes every argument of the primite into account. *)

(* CR ageorges: This mimics the behavior of [@local_opt] but is it necessary? *)
let eq_later_arg_local (y @ local) =
  let p = eq "a" in
  let _ = p y in
  p
[%%expect{|
Line 4, characters 2-3:
4 |   p
      ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}];;

(* If all uses are global [p] can be heap allocated *)
let eq_later_arg_global (y : string) =
  let p = eq "a" in
  let _ = p y in
  p
[%%expect{|
val eq_later_arg_global :
  string @ [< 'm mod contended immutable & global] ->
  (string @ [< global > 'm mod many portable forkable unyielding stateless] ->
   bool) @ [> aliased nonportable stateful dynamic] =
  <fun>
|}];;

(fun (x @ nonportable) -> eq x);;
[%%expect{|
- : 'a @ [< past('m) & global > nonportable] ->
    ('a @ [< global] -> bool) @ [> past('m) | nonportable stateful dynamic]
= <fun>
|}];;

(* Although [x] is portable, the partial application is nonportable. This is due to
   the mode of [eq] itself. *)
(fun (x @ portable) -> eq x);;
[%%expect{|
- : 'a @ [< past('m) & global portable] ->
    ('a @ [< global] -> bool) @ [> past('m) | nonportable stateful dynamic]
= <fun>
|}];;

(* [add_alias] creates an instance of [add], which is why the curry mode is no longer
   polymorphic over locality *)
let add_alias = add
[%%expect{|
val add_alias :
  int32 @ [< 'n & global] -> int32 @ [< 'm & global] -> int32 @ [> 'm | 'n] =
  <fun>
|}];;

(add_alias : int32 -> int32 -> int32);;
[%%expect{|
- : int32 -> int32 -> int32 = <fun>
|}];;

(add_alias : int32 @ local -> int32 @ local -> int32 @ local);;
[%%expect{|
Line 1, characters 1-10:
1 | (add_alias : int32 @ local -> int32 @ local -> int32 @ local);;
     ^^^^^^^^^
Error: The value "add_alias" has type "int32 -> int32 -> int32"
       but an expression was expected of type
         "int32 @ local -> int32 @ local -> int32"
|}];;

module type Of_struct = module type of struct
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
  let add_val = add
end
[%%expect{|
module type Of_struct =
  sig
    external add : int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
      = "%int32_add"
    val add_val :
      int32 @ [< 'n & global] ->
      int32 @ [< 'm & global] -> int32 @ [> 'm | 'n]
  end
|}];;

(* The implementaiton holds a more general primitive than the in the signature *)

module Ext_legacy : sig
  external add : int32 -> int32 -> int32 = "%int32_add"
end = struct
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
end
[%%expect{|
module Ext_legacy :
  sig external add : int32 -> int32 -> int32 = "%int32_add" end
|}];;

module Ext_local_opt : sig
  external add :
    (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
    = "%int32_add"
end = struct
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
end
[%%expect{|
module Ext_local_opt :
  sig
    external add :
      (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
      = "%int32_add"
  end
|}];;

module Id_local_opt : sig
  external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
end = struct
  external id : 'a @ [< 'm] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
module Id_local_opt :
  sig external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity" end
|}];;

(* The implementation is *less* general than the interface *)

(* [@local_opt] varies yielding together on the argument and the result, so the
   implementation must accept a yielding argument *)
module Id_local_opt_unyielding : sig
  external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
end = struct
  external id : 'a @ [< 'm & unyielding] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external id : 'a @ [< 'm & unyielding] -> 'a @ [> 'm] = "%identity"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : 'a @ [< 'm & unyielding] -> 'a @ [> 'm]
             = "%identity"
         end
       is not included in
         sig
           external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
         end
       Values do not match:
         external id : 'a @ [< 'm & unyielding] -> 'a @ [> 'm] = "%identity"
       is not included in
         external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
       The type "'a @ [< 'm & unyielding] -> 'a @ [> 'm]"
       is not compatible with the type "'a @ yielding -> 'a @ yielding"
       The argument mode was expected to be "unyielding" but is "yielding"
|}];;

(* Similar for forkability *)
module Id_local_opt_forkable : sig
  external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
end = struct
  external id : 'a @ [< 'm & forkable] -> 'a @ [> 'm] = "%identity"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   external id : 'a @ [< 'm & forkable] -> 'a @ [> 'm] = "%identity"
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external id : 'a @ [< 'm & forkable] -> 'a @ [> 'm] = "%identity"
         end
       is not included in
         sig
           external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
         end
       Values do not match:
         external id : 'a @ [< 'm & forkable] -> 'a @ [> 'm] = "%identity"
       is not included in
         external id : ('a [@local_opt]) -> ('a [@local_opt]) = "%identity"
       The type "'a @ [< 'm & forkable] -> 'a @ [> 'm]"
       is not compatible with the type "'a @ unforkable -> 'a @ unforkable"
       The argument mode was expected to be "forkable" but is "unforkable"
|}];;

module Ext_from_legacy : sig
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
end = struct
  external add : int32 -> int32 -> int32 = "%int32_add"
end
[%%expect{|
Lines 4-6, characters 6-3:
4 | ......struct
5 |   external add : int32 -> int32 -> int32 = "%int32_add"
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig external add : int32 -> int32 -> int32 = "%int32_add" end
       is not included in
         sig
           external add :
             int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
             = "%int32_add"
         end
       Values do not match:
         external add : int32 -> int32 -> int32 = "%int32_add"
       is not included in
         external add :
           int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
           = "%int32_add"
       The type "int32 -> int32 -> int32" is not compatible with the type
         "int32 @ [< 'n & global] ->
         int32 @ [< 'm & global] -> int32 @ [> 'm | 'n]"
       Type "int32 -> int32" is not compatible with type
         "int32 @ [< 'm & global] -> int32 @ [> 'm | 'n]"
       The return mode was expected to be "unique" but is "aliased"
|}];;

module Ext_from_local_opt : sig
  external add : int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]
    = "%int32_add"
end = struct
  external add :
    (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
    = "%int32_add"
end
[%%expect{|
Lines 4-8, characters 6-3:
4 | ......struct
5 |   external add :
6 |     (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
7 |     = "%int32_add"
8 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external add :
             (int32 [@local_opt]) ->
             (int32 [@local_opt]) -> (int32 [@local_opt]) = "%int32_add"
         end
       is not included in
         sig
           external add :
             int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
             = "%int32_add"
         end
       Values do not match:
         external add :
           (int32 [@local_opt]) ->
           (int32 [@local_opt]) -> (int32 [@local_opt]) = "%int32_add"
       is not included in
         external add :
           int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]
           = "%int32_add"
       The type "int32 -> int32 -> int32" is not compatible with the type
         "int32 @ [< 'n & global] ->
         int32 @ [< 'm & global] -> int32 @ [> 'm | 'n]"
       Type "int32 -> int32" is not compatible with type
         "int32 @ [< 'm & global] -> int32 @ [> 'm | 'n]"
       The return mode was expected to be "unique" but is "aliased"
|}];;

(* By putting parentheses, the mode of the returned closure is translated as legacy *)

external returns_closure :
  int32 @ [< 'm] -> (int32 @ [< 'm] -> int32 -> int32) = "caml_returns_closure"
[%%expect{|
external returns_closure : int32 @ 'n -> (int32 @ 'm -> int32 -> int32)
  = "caml_returns_closure"
|}];;

(* We can have mode polymorphic callbacks in a primitive *)

external takes_callback :
  (int32 @ [< 'm] -> int32 @ [< 'm] -> int32 @ [> 'm]) -> unit
  = "caml_takes_callback"
[%%expect{|
external takes_callback :
  (int32 @ [< 'n] -> int32 @ [< 'm] -> int32 @ [> 'm | 'n]) -> unit
  = "caml_takes_callback"
|}];;
