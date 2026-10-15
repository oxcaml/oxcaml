(* TEST
   expect;
*)

(* Typing of [@elide_void]. See also [elide_void_runtime.ml]. *)

type w : void
external w : unit -> w = "%unbox_unit"
[%%expect{|
type w : void
external w : unit -> w = "%unbox_unit"
|}]

external id_first : (w [@elide_void]) -> 'a -> 'b = "%identity"
external add_mid : int -> (w [@ocaml.elide_void]) -> int -> int
  = "%addint"
external lab : a:int -> w:(w [@elide_void]) -> int -> int = "elide_stub"
external mixed : (w [@elide_void]) -> w -> int -> int
  = "elide_bytecode" "elide_native"
external file : (w [@elide_void]) -> string = "%loc_FILE"
external untagged :
  (int [@untagged]) -> (w [@elide_void]) -> (int [@untagged])
  = "elide_bytecode" "elide_native" [@@noalloc]
external hypot : float -> (w [@elide_void]) -> float -> float
  = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
[%%expect{|
external id_first : (w [@elide_void]) -> 'a -> 'b = "%identity"
external add_mid : int -> (w [@elide_void]) -> int -> int = "%addint"
external lab : a:int -> w:(w [@elide_void]) -> int -> int = "elide_stub"
external mixed : (w [@elide_void]) -> w -> int -> int = "elide_bytecode"
  "elide_native"
external file : (w [@elide_void]) -> string = "%loc_FILE"
external untagged : int -> (w [@elide_void]) -> int = "elide_bytecode"
  "elide_native" [@@untagged] [@@noalloc]
external hypot : float -> (w [@elide_void]) -> float -> float
  = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
|}]

(* Six source arguments but three runtime ones: one name suffices. *)
external six_source :
  int -> (w [@elide_void]) -> int -> (w [@elide_void]) -> int
  -> (w [@elide_void]) -> int
  = "elide_stub"
[%%expect{|
external six_source :
  int ->
  (w [@elide_void]) ->
  int -> (w [@elide_void]) -> int -> (w [@elide_void]) -> int = "elide_stub"
|}]

(* The legacy "float" form unboxes only retained arguments. *)
external hyp : (w [@elide_void]) -> float -> float -> float
  = "caml_hypot_float" "caml_hypot" "float"
let f x = hyp (w ()) x 4.0
[%%expect{|
Lines 1-2, characters 0-43:
1 | external hyp : (w [@elide_void]) -> float -> float -> float
2 |   = "caml_hypot_float" "caml_hypot" "float"
Error: Cannot use "float" in conjunction with types of non-value layouts.
|}]

(* Without the attribute, the void argument counts. *)
external id_no_attr : w -> int -> int = "%identity"
[%%expect{|
Line 1, characters 0-51:
1 | external id_no_attr : w -> int -> int = "%identity"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong arity for builtin primitive "%identity"
|}]

(* Elided arguments do not satisfy the primitive's required arity. *)
external add_one_retained : (w [@elide_void]) -> int -> int = "%addint"
[%%expect{|
Line 1, characters 0-71:
1 | external add_one_retained : (w [@elide_void]) -> int -> int = "%addint"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong arity for builtin primitive "%addint"
|}]

(* C calls cannot have zero runtime arguments. *)
external no_runtime_args : (w [@elide_void]) -> int = "elide_stub"
[%%expect{|
Line 1, characters 27-51:
1 | external no_runtime_args : (w [@elide_void]) -> int = "elide_stub"
                               ^^^^^^^^^^^^^^^^^^^^^^^^
Error: An external must have at least one non-elided argument.
|}]

(* Non-void arguments *)

external on_int : (int [@elide_void]) -> int -> int = "elide_stub"
[%%expect{|
Line 1, characters 25-35:
1 | external on_int : (int [@elide_void]) -> int -> int = "elide_stub"
                             ^^^^^^^^^^
Error: Only external arguments of layout void can be marked "[@elide_void]".
|}]

external on_void_product : (#(w * w) [@elide_void]) -> int -> int
  = "elide_stub"
[%%expect{|
Line 1, characters 39-49:
1 | external on_void_product : (#(w * w) [@elide_void]) -> int -> int
                                           ^^^^^^^^^^
Error: Only external arguments of layout void can be marked "[@elide_void]".
|}]

external on_result : int -> (w [@elide_void])
  = "elide_bytecode" "elide_native"
[%%expect{|
Line 1, characters 33-43:
1 | external on_result : int -> (w [@elide_void])
                                     ^^^^^^^^^^
Error: "[@elide_void]" is not allowed in return position.
|}]

(* Misplaced *)

module type S = sig
  val f : (w [@elide_void]) -> int -> int
end
[%%expect{|
module type S = sig val f : w -> int -> int end
|}]

(* The source type still requires the witness. *)

let partial = add_mid 1
let full = add_mid 1 (w ()) 2
[%%expect{|
val partial : w -> int -> int = <fun>
val full : int = 3
|}]

let omitted : int = add_mid 1 2
[%%expect{|
Line 1, characters 30-31:
1 | let omitted : int = add_mid 1 2
                                  ^
Error: The constant "2" has type "int" but an expression was expected of type "w"
|}]

let omitted_label : int = lab ~a:1 2
[%%expect{|
Line 1, characters 26-36:
1 | let omitted_label : int = lab ~a:1 2
                              ^^^^^^^^^^
Error: This expression has type "w:w -> int"
       but an expression was expected of type "int"
Hint: This function application is partial, maybe some arguments are missing.
|}]

let wrong : int = add_mid 1 #() 2
[%%expect{|
Line 1, characters 28-31:
1 | let wrong : int = add_mid 1 #() 2
                                ^^^
Error: This expression has type "unit#" but an expression was expected of type
         "w"
|}]

(* Only the retained argument matters for the value restriction. *)
external id_w : w:(w [@elide_void]) -> 'a -> 'a = "%identity"
let gen = id_w ~w:(w ()) (fun x -> x)
let gen_reordered = id_w (fun x -> x) ~w:(w ())
let weak = id_w ~w:(w ()) (ref [])
let weak_over = id_w ~w:(w ()) (fun () -> ref []) ()
[%%expect{|
external id_w : w:(w [@elide_void]) -> 'a -> 'a = "%identity"
val gen : 'a -> 'a = <fun>
val gen_reordered : 'a -> 'a = <fun>
val weak : '_weak1 list ref = {contents = []}
val weak_over : '_weak2 list ref = {contents = []}
|}]

(* The second runtime operand, not the second source argument, is in tail
   position for short-circuiting primitives. *)
external and_witness : (unit# [@elide_void]) -> bool -> bool -> bool
  = "%sequand"
let lhs f =
  let local_ r = ref 42 in
  and_witness #() (f r) true
[%%expect{|
external and_witness : (unit# [@elide_void]) -> bool -> bool -> bool
  = "%sequand"
val lhs : (int ref @ local -> bool) -> bool = <fun>
|}]

let rhs f =
  let local_ r = ref 42 in
  and_witness #() true (f r)
[%%expect{|
Line 3, characters 26-27:
3 |   and_witness #() true (f r)
                              ^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
         because it is an argument in a tail call.
|}]

(* A witness between the operands is not in tail position. *)
external or_mid : bool -> (unit# [@elide_void]) -> bool -> bool
  = "%sequor"
let witness_not_tail f =
  let local_ r = ref 42 in
  or_mid false (f r) true
[%%expect{|
external or_mid : bool -> (unit# [@elide_void]) -> bool -> bool = "%sequor"
val witness_not_tail : (int ref @ local -> unit#) -> bool = <fun>
|}]

(* Witness modes print, reparse and are still checked. *)

module Modes : sig
  external local_w : (w [@elide_void]) @ local -> int -> int = "%identity"
  external poly_w : ((w [@local_opt]) [@elide_void]) -> int -> int
    = "%identity"
end = struct
  external local_w : local_ (w [@elide_void]) -> int -> int = "%identity"
  external poly_w : (w [@local_opt] [@elide_void]) -> int -> int
    = "%identity"
end
let local_ok (local_ x : w) = Modes.local_w x 1 + Modes.poly_w x 2
[%%expect{|
module Modes :
  sig
    external local_w : (w [@elide_void]) @ local -> int -> int = "%identity"
    external poly_w : ((w [@local_opt]) [@elide_void]) -> int -> int
      = "%identity"
  end
val local_ok : w @ local -> int = <fun>
|}]

let global_bad (local_ x : w) = add_mid 1 x 2
[%%expect{|
Line 1, characters 42-43:
1 | let global_bad (local_ x : w) = add_mid 1 x 2
                                              ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* Inclusion *)

module As_val : sig
  val f : int -> w -> int -> int
end = struct
  external f : int -> (w [@elide_void]) -> int -> int = "%addint"
end
[%%expect{|
module As_val : sig val f : int -> w -> int -> int end
|}]

module Elided_in_impl_only : sig
  external f : int -> w -> int -> int = "elide_bytecode" "elide_native"
end = struct
  external f : int -> (w [@elide_void]) -> int -> int
    = "elide_bytecode" "elide_native"
end
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   external f : int -> (w [@elide_void]) -> int -> int
5 |     = "elide_bytecode" "elide_native"
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external f : int -> (w [@elide_void]) -> int -> int
             = "elide_bytecode" "elide_native"
         end
       is not included in
         sig
           external f : int -> w -> int -> int = "elide_bytecode"
             "elide_native"
         end
       Values do not match:
         external f : int -> (w [@elide_void]) -> int -> int
           = "elide_bytecode" "elide_native"
       is not included in
         external f : int -> w -> int -> int = "elide_bytecode"
           "elide_native"
       The two primitives elide different arguments
|}]

module Different_elided_arg : sig
  external f : (w [@elide_void]) -> w -> int -> int
    = "elide_bytecode" "elide_native"
end = struct
  external f : w -> (w [@elide_void]) -> int -> int
    = "elide_bytecode" "elide_native"
end
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   external f : w -> (w [@elide_void]) -> int -> int
6 |     = "elide_bytecode" "elide_native"
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external f : w -> (w [@elide_void]) -> int -> int
             = "elide_bytecode" "elide_native"
         end
       is not included in
         sig
           external f : (w [@elide_void]) -> w -> int -> int
             = "elide_bytecode" "elide_native"
         end
       Values do not match:
         external f : w -> (w [@elide_void]) -> int -> int = "elide_bytecode"
           "elide_native"
       is not included in
         external f : (w [@elide_void]) -> w -> int -> int = "elide_bytecode"
           "elide_native"
       The two primitives elide different arguments
|}]
