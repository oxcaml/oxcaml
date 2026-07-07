(* TEST
   flags = "-dlambda -dno-locations -dno-unique-ids -extension layouts_alpha";
   expect;
*)

module Atomic = struct
  module Loc = struct
    type ('a : value_or_null) t = 'a atomic_loc

    external compare_and_set
      : ('a : value_or_null mod portable).
         'a atomic_loc @ contended local
      -> 'a
      -> 'a
      -> bool @@ portable
      = "%atomic_cas_loc"
  end
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Atomic/293"
  (let (Loc = (makeblock 0)) (makeblock 0 Loc)))
module Atomic :
  sig
    module Loc :
      sig
        type ('a : value_or_null) t = 'a atomic_loc
        external compare_and_set :
          ('a : value_or_null mod portable).
            'a atomic_loc @ local contended -> 'a -> 'a -> bool
          = "%atomic_cas_loc"
      end
  end
|}]

(* Basic usage: redefine atomics. *)

module Basic = struct
  type 'a atomic = { mutable filler: unit; mutable x : 'a [@atomic] }

  let get (type a) (r : a atomic) : a = r.x

  let get_imm (r : int atomic) : int = r.x

  let set (type a) (r : a atomic) (v : a) : unit = r.x <- v

  let set_imm (r : int atomic) (v : int) : unit = r.x <- v

  let cas (type a : value mod portable) (r : a atomic) oldv newv =
    Atomic.Loc.compare_and_set [%atomic.loc r.x] oldv newv

  let[@inline never] get_loc (type a) (r : a atomic) : a Atomic.Loc.t =
    [%atomic.loc r.x]

  let slow_cas (type a : value mod portable) (r : a atomic) oldv newv =
    Atomic.Loc.compare_and_set (get_loc r) oldv newv
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Basic/331"
  (let
    (get = (function {nlocal = 1} r[L] (atomic_load_field_ptr r 1))
     get_imm = (function {nlocal = 1} r[L] : int (atomic_load_field_imm r 1))
     set = (function {nlocal = 0} r[L] v : int (atomic_set_field_ptr r 1 v))
     set_imm =
       (function {nlocal = 1} r[L] v[L][value<int>] : int
         (atomic_set_field_imm r 1 v))
     cas =
       (function {nlocal = 0} r[L] oldv newv : int
         (atomic_compare_set_field_ptr r 1 oldv newv))
     get_loc =
       (function {nlocal = 0} r never_inline
         (makeblock 0 (*,value<int>) r 1))
     slow_cas =
       (function {nlocal = 0} r oldv newv : int
         (let (atomic_arg = (apply get_loc r))
           (atomic_compare_set_field_ptr (field_imm 0 atomic_arg)
             (field_int 1 atomic_arg) oldv newv))))
    (makeblock 0 get get_imm set set_imm cas get_loc slow_cas)))
module Basic :
  sig
    type 'a atomic = { mutable filler : unit; mutable x : 'a [@atomic]; }
    val get : 'a atomic -> 'a
    val get_imm : int atomic -> int
    val set : 'a atomic -> 'a -> unit
    val set_imm : int atomic -> int -> unit
    val cas : ('a : value mod portable). 'a atomic -> 'a -> 'a -> bool
    val get_loc : 'a atomic -> 'a Atomic.Loc.t
    val slow_cas : ('a : value mod portable). 'a atomic -> 'a -> 'a -> bool
  end
|}];;


(* Atomic fields must be mutable. *)
module Error1 = struct
  type t = { x : int [@atomic] }
end
[%%expect{|
Line 2, characters 13-30:
2 |   type t = { x : int [@atomic] }
                 ^^^^^^^^^^^^^^^^^
Error: The label "x" must be mutable to be declared atomic.
|}];;


(* [%atomic.loc _] payload must be a record field access *)
module Error2 = struct
  type t = { mutable x : int [@atomic] }
  let f t = [%atomic.loc t]
end
[%%expect{|
Line 3, characters 12-27:
3 |   let f t = [%atomic.loc t]
                ^^^^^^^^^^^^^^^
Error: Invalid "[%atomic.loc]" payload, a record field access is expected.
|}];;


(* [%atomic.loc _] only works on atomic fields *)
module Error3 = struct
  type t = { x : int }
  let f t = [%atomic.loc t.x]
end
[%%expect{|
Line 3, characters 12-29:
3 |   let f t = [%atomic.loc t.x]
                ^^^^^^^^^^^^^^^^^
Error: The record field "x" is not atomic
|}];;

(* Check module interface checking: it is not allowed to remove or add
   atomic attributes. *)

module Wrong1 = (struct
  type t = { mutable x : int }
end : sig
  (* adding an 'atomic' attribute missing in the implementation: invalid. *)
  type t = { mutable x : int [@atomic] }
end)
[%%expect{|
Lines 1-3, characters 17-3:
1 | .................struct
2 |   type t = { mutable x : int }
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int; } end
       is not included in
         sig type t = { mutable x : int [@atomic]; } end
       Type declarations do not match:
         type t = { mutable x : int; }
       is not included in
         type t = { mutable x : int [@atomic]; }
       Fields do not match:
         "mutable x : int;"
       is not the same as:
         "mutable x : int [@atomic];"
       The second is atomic and the first is not.
|}];;

module Wrong2 = (struct
  type t = { mutable x : int [@atomic] }
end : sig
  (* removing an 'atomic' attribute present in the implementation: invalid. *)
  type t = { mutable x : int }
end)
[%%expect{|
Lines 1-3, characters 17-3:
1 | .................struct
2 |   type t = { mutable x : int [@atomic] }
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int [@atomic]; } end
       is not included in
         sig type t = { mutable x : int; } end
       Type declarations do not match:
         type t = { mutable x : int [@atomic]; }
       is not included in
         type t = { mutable x : int; }
       Fields do not match:
         "mutable x : int [@atomic];"
       is not the same as:
         "mutable x : int;"
       The first is atomic and the second is not.
|}];;

module Ok = (struct
  type t = { mutable x : int [@atomic] }
end : sig
  type t = { mutable x : int [@atomic] }
end)
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Ok/361" (makeblock 0))
module Ok : sig type t = { mutable x : int [@atomic]; } end
|}];;

(* Projecting an atomic field of a record that contains a field with layout any. *)

type ('a : any) t = { a : 'a; mutable f: int [@atomic]}
[%%expect{|
0
type ('a : any) t = { a : 'a; mutable f : int [@atomic]; }
|}];;

let ok_project (t: int t) = t.f
[%%expect{|
(let
  (ok_project =
     (function {nlocal = 1} t[L] : int (atomic_load_field_imm t 1)))
  (apply (field_imm 1 (global Toploop!)) "ok_project" ok_project))
val ok_project : int t -> int = <fun>
|}];;

let wrong_project (t: int64# t) = t.f
[%%expect{|
Line 1, characters 34-35:
1 | let wrong_project (t: int64# t) = t.f
                                      ^
Error: Accessing atomic fields (here "f") of mixed records is not yet
       supported.
|}];;

let ok_set (t: int t) = t.f <- 42
[%%expect{|
(let
  (ok_set = (function {nlocal = 1} t[L] : int (atomic_set_field_imm t 1 42)))
  (apply (field_imm 1 (global Toploop!)) "ok_set" ok_set))
val ok_set : int t -> unit = <fun>
|}];;

let wrong_set (t: int64# t) = t.f <- 42
[%%expect{|
Line 1, characters 30-39:
1 | let wrong_set (t: int64# t) = t.f <- 42
                                  ^^^^^^^^^
Error: Accessing atomic fields (here "f") of mixed records is not yet
       supported.
|}];;

let ok_loc (t: int t) = [%atomic.loc t.f]
[%%expect{|
(let (ok_loc = (function {nlocal = 0} t (makeblock 0 (*,value<int>) t 1)))
  (apply (field_imm 1 (global Toploop!)) "ok_loc" ok_loc))
val ok_loc : int t -> int atomic_loc = <fun>
|}];;

let wrong_loc (t: int64# t) = [%atomic.loc t.f];
[%%expect{|
Line 1, characters 43-44:
1 | let wrong_loc (t: int64# t) = [%atomic.loc t.f];
                                               ^
Error: Use of "[%atomic.loc]" with mixed record fields (here "f") is forbidden.
|}];;

(* Inline records are supported, including in extensions. *)

module Inline_record = struct
  type t = A of { mutable x : int [@atomic] }

  let test : t -> int = fun (A r) -> r.x
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Inline_record/402"
  (let
    (test =
       (function {nlocal = 0} param : int (atomic_load_field_imm param 0)))
    (makeblock 0 test)))
module Inline_record :
  sig type t = A of { mutable x : int [@atomic]; } val test : t -> int end
|}];;

module Extension_with_inline_record = struct
  type t = ..
  type t += A of { mutable x : int [@atomic] }

  (* one should see in the -dlambda output below that the field offset is not 0
     as one could expect, but 1, due to an extra argument in extensible variants. *)
  let test : t -> int = function
    | A r -> r.x
    | _ -> 0

  let () = assert (test (A { x = 42 }) = 42)
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Extension_with_inline_record/410"
  (let
    (A =
       (makeblock_unique 248 "Extension_with_inline_record.A"
         (caml_fresh_oo_id 0))
     test =
       (function {nlocal = 0} param : int
         (if (%eq (field_imm 0 param) A) (atomic_load_field_imm param 1) 0))
     *match* =[value<int>]
       (if (%eq (apply test (makemutable 0 (?,value<int>) A 42)) 42) 0
         (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 11 11]))))
    (makeblock 0 A test)))
module Extension_with_inline_record :
  sig
    type t = ..
    type t += A of { mutable x : int [@atomic]; }
    val test : t -> int
  end
|}]

(* Projecting an atomic field of an inline record that contains a field with layout any. *)

type ('a : any) t = A of { a : 'a; mutable f: int [@atomic]}
[%%expect{|
0
type ('a : any) t = A of { a : 'a; mutable f : int [@atomic]; }
|}];;

let ok_project (t: int t) = match t with A r -> r.f
[%%expect{|
(let
  (ok_project =
     (function {nlocal = 1} t[L] : int (atomic_load_field_imm t 1)))
  (apply (field_imm 1 (global Toploop!)) "ok_project" ok_project))
val ok_project : int t -> int = <fun>
|}];;

let wrong_project (t: int64# t) = match t with A r -> r.f
[%%expect{|
Line 1, characters 54-55:
1 | let wrong_project (t: int64# t) = match t with A r -> r.f
                                                          ^
Error: Accessing atomic fields (here "f") of mixed records is not yet
       supported.
|}];;

let ok_set (t: int t) = match t with A r -> r.f <- 42
[%%expect{|
(let
  (ok_set = (function {nlocal = 1} t[L] : int (atomic_set_field_imm t 1 42)))
  (apply (field_imm 1 (global Toploop!)) "ok_set" ok_set))
val ok_set : int t -> unit = <fun>
|}];;

let wrong_set (t: int64# t) = match t with A r -> r.f <- 42
[%%expect{|
Line 1, characters 50-59:
1 | let wrong_set (t: int64# t) = match t with A r -> r.f <- 42
                                                      ^^^^^^^^^
Error: Accessing atomic fields (here "f") of mixed records is not yet
       supported.
|}];;

let ok_loc (t: int t) = match t with A r -> [%atomic.loc r.f]
[%%expect{|
(let (ok_loc = (function {nlocal = 0} t (makeblock 0 (*,value<int>) t 1)))
  (apply (field_imm 1 (global Toploop!)) "ok_loc" ok_loc))
val ok_loc : int t -> int atomic_loc = <fun>
|}];;

let wrong_loc (t: int64# t) = match t with A r -> [%atomic.loc r.f]
[%%expect{|
Line 1, characters 63-64:
1 | let wrong_loc (t: int64# t) = match t with A r -> [%atomic.loc r.f]
                                                                   ^
Error: Use of "[%atomic.loc]" with mixed record fields (here "f") is forbidden.
|}];;

(* Marking a field [@atomic] in a float-only record disables the unboxing optimization. *)
module Float_records = struct
  type flat = { x : float; mutable y : float }
  type t = { x : float; mutable y : float [@atomic] }

  let mk_flat x y : flat = { x; y }
  let mk_t x y : t = { x; y }
  let get v = v.y
end
[%%expect{|
Line 3, characters 2-53:
3 |   type t = { x : float; mutable y : float [@atomic] }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 214 [atomic-float-record-boxed]: This record contains atomic float fields,
  which prevents the float record optimization.
  The fields of this record will be boxed instead of being
  represented as a flat float array.
(apply (field_imm 1 (global Toploop!)) "Float_records/471"
  (let
    (mk_flat =
       (function {nlocal = 0} x[value<float>] y[value<float>]
         (makefloatblock Mutable x y))
     mk_t =
       (function {nlocal = 0} x[value<float>] y[value<float>]
         (makemutable 0 (value<float>,value<float>) x y))
     get = (function {nlocal = 1} v[L] : float (atomic_load_field_ptr v 1)))
    (makeblock 0 mk_flat mk_t get)))

module Float_records :
  sig
    type flat = { x : float; mutable y : float; }
    type t = { x : float; mutable y : float [@atomic]; }
    val mk_flat : float -> float -> flat
    val mk_t : float -> float -> t
    val get : t -> float
  end
|}]

(* Tests for Warning 214: Atomic float record boxing *)

(* This should trigger warning 214 - all atomic float fields *)
type all_atomic_floats = {
  mutable x : float [@atomic];
  mutable y : float [@atomic];
  mutable z : float [@atomic];
}
[%%expect{|
Lines 1-5, characters 0-1:
1 | type all_atomic_floats = {
2 |   mutable x : float [@atomic];
3 |   mutable y : float [@atomic];
4 |   mutable z : float [@atomic];
5 | }
Warning 214 [atomic-float-record-boxed]: This record contains atomic float fields,
  which prevents the float record optimization.
  The fields of this record will be boxed instead of being
  represented as a flat float array.
0

type all_atomic_floats = {
  mutable x : float [@atomic];
  mutable y : float [@atomic];
  mutable z : float [@atomic];
}
|}]

(* This should trigger warning 214 - mix of atomic and non-atomic float fields *)
type mixed_atomic_floats = {
  mutable a : float;
  mutable b : float [@atomic];
  mutable c : float;
  mutable d : float [@atomic];
}
[%%expect{|
Lines 1-6, characters 0-1:
1 | type mixed_atomic_floats = {
2 |   mutable a : float;
3 |   mutable b : float [@atomic];
4 |   mutable c : float;
5 |   mutable d : float [@atomic];
6 | }
Warning 214 [atomic-float-record-boxed]: This record contains atomic float fields,
  which prevents the float record optimization.
  The fields of this record will be boxed instead of being
  represented as a flat float array.
0

type mixed_atomic_floats = {
  mutable a : float;
  mutable b : float [@atomic];
  mutable c : float;
  mutable d : float [@atomic];
}
|}]

(* This should NOT trigger warning 214 - has non-float fields *)
type atomic_float_with_int = {
  mutable f : float [@atomic];
  mutable i : int;
  mutable g : float;
}
[%%expect{|
0
type atomic_float_with_int = {
  mutable f : float [@atomic];
  mutable i : int;
  mutable g : float;
}
|}]

(* This should NOT trigger warning 214 - no atomic fields *)
type regular_float_record = {
  mutable p : float;
  mutable q : float;
  mutable r : float;
}
[%%expect{|
0
type regular_float_record = {
  mutable p : float;
  mutable q : float;
  mutable r : float;
}
|}]

(* This should NOT trigger warning 214 - immutable float fields (can't be atomic) *)
type immutable_float_record = {
  x : float;
  y : float;
  z : float;
}
[%%expect{|
0
type immutable_float_record = { x : float; y : float; z : float; }
|}]

(* This should trigger warning 214 - single atomic float field *)
type single_atomic_float = {
  mutable value : float [@atomic];
}
[%%expect{|
Lines 1-3, characters 0-1:
1 | type single_atomic_float = {
2 |   mutable value : float [@atomic];
3 | }
Warning 214 [atomic-float-record-boxed]: This record contains atomic float fields,
  which prevents the float record optimization.
  The fields of this record will be boxed instead of being
  represented as a flat float array.
0

type single_atomic_float = { mutable value : float [@atomic]; }
|}]

(* Test warning suppression with [@@@warning "-214"] *)
[@@@warning "-214"]
type suppressed_atomic_float = {
  mutable x : float [@atomic];
  mutable y : float [@atomic];
}
[%%expect{|
0
0
type suppressed_atomic_float = {
  mutable x : float [@atomic];
  mutable y : float [@atomic];
}
|}]

(* Re-enable the warning *)
[@@@warning "+214"]
type not_suppressed_atomic_float = {
  mutable a : float [@atomic];
}
[%%expect{|
0
Lines 2-4, characters 0-1:
2 | type not_suppressed_atomic_float = {
3 |   mutable a : float [@atomic];
4 | }
Warning 214 [atomic-float-record-boxed]: This record contains atomic float fields,
  which prevents the float record optimization.
  The fields of this record will be boxed instead of being
  represented as a flat float array.
0

type not_suppressed_atomic_float = { mutable a : float [@atomic]; }
|}]

type suppressed_directly = { mutable a : float [@atomic] }
[@@warning "-214"]
[%%expect{|
0
type suppressed_directly = { mutable a : float [@atomic]; }
|}]


type suppressed_via_mnemonic = { mutable a : float [@atomic] }
[@@warning "-atomic-float-record-boxed"]
[%%expect{|
0
type suppressed_via_mnemonic = { mutable a : float [@atomic]; }
|}]

(* Pattern-matching on atomic record fields is disallowed. *)
module Pattern_matching = struct
  type t = { x : int; mutable y : int [@atomic] }

  let forbidden { x; y } = x + y
end
[%%expect{|
Line 4, characters 16-24:
4 |   let forbidden { x; y } = x + y
                    ^^^^^^^^
Error: Atomic fields (here "y") are forbidden in patterns,
       as it is difficult to reason about when the atomic read
       will happen during pattern matching: the field may be read
       zero, one or several times depending on the patterns around it.
|}]

(* ... except for wildcards, to allow exhaustive record patterns. *)
module Pattern_matching_wildcard = struct
  type t = { x : int; mutable y : int [@atomic] }

  [@@@warning "+missing-record-field-pattern"]
  let warning { x } = x

  let allowed { x; y = _ } = x
  let also_allowed { x; _ } = x
end
[%%expect{|
Line 5, characters 14-19:
5 |   let warning { x } = x
                  ^^^^^
Warning 9 [missing-record-field-pattern]: the following labels are not bound
  in this record pattern: "y".
  Either bind these labels explicitly or add "; _" to the pattern.
(apply (field_imm 1 (global Toploop!)) "Pattern_matching_wildcard/534"
  (let
    (warning = (function {nlocal = 1} param[L] : int (field_int 0 param))
     allowed = (function {nlocal = 1} param[L] : int (field_int 0 param))
     also_allowed =
       (function {nlocal = 1} param[L] : int (field_int 0 param)))
    (makeblock 0 warning allowed also_allowed)))

module Pattern_matching_wildcard :
  sig
    type t = { x : int; mutable y : int [@atomic]; }
    val warning : t -> int
    val allowed : t -> int
    val also_allowed : t -> int
  end
|}]

(* We disallow functional updates that perform implicit loads of atomic fields. *)
module Functional_update_error = struct
  type t = { x : int ; mutable y : int [@atomic] }

  (* The update performs an implicit atomic load of y. Not allowed! *)
  let forbidden t = { t with x = 42 }
end
[%%expect{|
Line 5, characters 22-23:
5 |   let forbidden t = { t with x = 42 }
                          ^
Error: Functional updates that implicitly read atomic fields (here "y")
       are forbidden. Hint: if you intend to copy the value
       of an atomic field, do so explicitly: "{ t with y = t.y }"
|}]

(* Updates that overwrite all of the old record's atomic fields are allowed. *)

module Functional_update_ok = struct
  type t = { x : int ; mutable y : int [@atomic] }

  (* The update performs no implicit atomic loads. Allowed! *)
  let allowed t = { t with y = 42 }
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Functional_update_ok/550"
  (let
    (allowed =
       (function {nlocal = 1} t[L]
         (makemutable 0 (value<int>,value<int>) (field_int 0 t) 42)))
    (makeblock 0 allowed)))
module Functional_update_ok :
  sig
    type t = { x : int; mutable y : int [@atomic]; }
    val allowed : t -> t
  end
|}]

module Functional_update_copy_ok = struct
  type t = { x : int ; mutable y : int [@atomic] }

  (* The update performs an explicit atomic load. Allowed! *)
  let allowed t = { t with y = t.y }
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Functional_update_copy_ok/558"
  (let
    (allowed =
       (function {nlocal = 1} t[L]
         (makemutable 0 (value<int>,value<int>) (field_int 0 t)
           (atomic_load_field_imm t 1))))
    (makeblock 0 allowed)))
module Functional_update_copy_ok :
  sig
    type t = { x : int; mutable y : int [@atomic]; }
    val allowed : t -> t
  end
|}]

module Functional_update_multi_error = struct
  type t = { x : int ; mutable y : int [@atomic]; mutable z : int [@atomic] }

  let forbidden t = { t with y = 42 } (* implicit atomic load of z *)
end
[%%expect{|
Line 4, characters 22-23:
4 |   let forbidden t = { t with y = 42 } (* implicit atomic load of z *)
                          ^
Error: Functional updates that implicitly read atomic fields (here "z")
       are forbidden. Hint: if you intend to copy the value
       of an atomic field, do so explicitly: "{ t with z = t.z }"
|}]

module Functional_update_multi_ok = struct
  type t = { x : int ; mutable y : int [@atomic]; mutable z : int [@atomic] }

  let allowed t = { t with y = 42; z = 67 } (* no implicit atomic loads *)
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Functional_update_multi_ok/574"
  (let
    (allowed =
       (function {nlocal = 1} t[L]
         (makemutable 0 (value<int>,value<int>,value<int>) (field_int 0 t) 42
           67)))
    (makeblock 0 allowed)))
module Functional_update_multi_ok :
  sig
    type t = {
      x : int;
      mutable y : int [@atomic];
      mutable z : int [@atomic];
    }
    val allowed : t -> t
  end
|}]

module Functional_update_multi_copy_ok = struct
  type t = { x : int ; mutable y : int [@atomic]; mutable z : int [@atomic] }

  let allowed t = { t with y = t.y; z = t.z } (* no implicit atomic loads *)
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Functional_update_multi_copy_ok/583"
  (let
    (allowed =
       (function {nlocal = 1} t[L]
         (makemutable 0 (value<int>,value<int>,value<int>) (field_int 0 t)
           (atomic_load_field_imm t 1) (atomic_load_field_imm t 2))))
    (makeblock 0 allowed)))
module Functional_update_multi_copy_ok :
  sig
    type t = {
      x : int;
      mutable y : int [@atomic];
      mutable z : int [@atomic];
    }
    val allowed : t -> t
  end
|}]
(* Pattern matching follows the same rules in mixed blocks. *)
module Pattern_matching = struct
  type t = { x : int64#; mutable y : int [@atomic] }

  let forbidden { x; y } = x + y
end
[%%expect{|
Line 4, characters 16-24:
4 |   let forbidden { x; y } = x + y
                    ^^^^^^^^
Error: Atomic fields (here "y") are forbidden in patterns,
       as it is difficult to reason about when the atomic read
       will happen during pattern matching: the field may be read
       zero, one or several times depending on the patterns around it.
|}]

(* ... except for wildcards, to allow exhaustive record patterns. *)
module Pattern_matching_wildcard = struct
  type t = { x : int64#; mutable y : int [@atomic] }

  [@@@warning "+missing-record-field-pattern"]
  let warning { x } = x

  let allowed { x; y = _ } = x
  let also_allowed { x; _ } = x
end
[%%expect{|
Line 5, characters 14-19:
5 |   let warning { x } = x
                  ^^^^^
Warning 9 [missing-record-field-pattern]: the following labels are not bound
  in this record pattern: "y".
  Either bind these labels explicitly or add "; _" to the pattern.
(apply (field_imm 1 (global Toploop!)) "Pattern_matching_wildcard/607"
  (let
    (warning =
       (function {nlocal = 1} param[L] : unboxed_int64
         (mixedfield 0  (bits64,value_or_null<int>) param))
     allowed =
       (function {nlocal = 1} param[L] : unboxed_int64
         (mixedfield 0  (bits64,value_or_null<int>) param))
     also_allowed =
       (function {nlocal = 1} param[L] : unboxed_int64
         (mixedfield 0  (bits64,value_or_null<int>) param)))
    (makeblock 0 warning allowed also_allowed)))

module Pattern_matching_wildcard :
  sig
    type t = { x : int64#; mutable y : int [@atomic]; }
    val warning : t -> int64#
    val allowed : t -> int64#
    val also_allowed : t -> int64#
  end
|}]

(* Test atomic record fields in mixed blocks *)

module Mixed_blocks = struct
  type t = {
    padding : #(int * int * int);
    mutable field : int [@atomic]
  }
end

let project (t : Mixed_blocks.t) = t.field
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Mixed_blocks/614" (makeblock 0))
module Mixed_blocks :
  sig
    type t = { padding : #(int * int * int); mutable field : int [@atomic]; }
  end
Line 8, characters 35-36:
8 | let project (t : Mixed_blocks.t) = t.field
                                       ^
Error: Accessing atomic fields (here "field") of mixed records is not yet
       supported.
|}]

let set (t: Mixed_blocks.t) = t.field <- 42
[%%expect{|
Line 1, characters 30-43:
1 | let set (t: Mixed_blocks.t) = t.field <- 42
                                  ^^^^^^^^^^^^^
Error: Accessing atomic fields (here "field") of mixed records is not yet
       supported.
|}]

let loc (t: Mixed_blocks.t) = [%atomic.loc t.field]
[%%expect{|
Line 1, characters 43-44:
1 | let loc (t: Mixed_blocks.t) = [%atomic.loc t.field]
                                               ^
Error: Use of "[%atomic.loc]" with mixed record fields (here "field") is forbidden.
|}]

module Mixed_blocks_2 = struct
  type t = {
    mutable field : int [@atomic];
    padding : #(int * int * int)
  }
end

let project (t : Mixed_blocks_2.t) = t.field
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Mixed_blocks_2/627" (makeblock 0))
module Mixed_blocks_2 :
  sig
    type t = { mutable field : int [@atomic]; padding : #(int * int * int); }
  end
Line 8, characters 37-38:
8 | let project (t : Mixed_blocks_2.t) = t.field
                                         ^
Error: Accessing atomic fields (here "field") of mixed records is not yet
       supported.
|}]

let set (t: Mixed_blocks_2.t) = t.field <- 42
[%%expect{|
Line 1, characters 32-45:
1 | let set (t: Mixed_blocks_2.t) = t.field <- 42
                                    ^^^^^^^^^^^^^
Error: Accessing atomic fields (here "field") of mixed records is not yet
       supported.
|}]

let loc (t: Mixed_blocks_2.t) = [%atomic.loc t.field]
[%%expect{|
Line 1, characters 45-46:
1 | let loc (t: Mixed_blocks_2.t) = [%atomic.loc t.field]
                                                 ^
Error: Use of "[%atomic.loc]" with mixed record fields (here "field") is forbidden.
|}]

module Mixed_blocks_rec = struct
  type t = {
    padding : u;
    mutable field : int [@atomic]
  }

  and u = #{
    x : int#;
    y : float#
  }
end

[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Mixed_blocks_rec/643" (makeblock 0))
module Mixed_blocks_rec :
  sig
    type t = { padding : u; mutable field : int [@atomic]; }
    and u = #{ x : int#; y : float#; }
  end
|}]

let project (t : Mixed_blocks_rec.t) = t.field
[%%expect{|
Line 1, characters 39-40:
1 | let project (t : Mixed_blocks_rec.t) = t.field
                                           ^
Error: Accessing atomic fields (here "field") of mixed records is not yet
       supported.
|}]
let set (t: Mixed_blocks_rec.t) = t.field <- 42
[%%expect{|
Line 1, characters 34-47:
1 | let set (t: Mixed_blocks_rec.t) = t.field <- 42
                                      ^^^^^^^^^^^^^
Error: Accessing atomic fields (here "field") of mixed records is not yet
       supported.
|}]
let loc (t: Mixed_blocks_rec.t) = [%atomic.loc t.field]
[%%expect{|
Line 1, characters 47-48:
1 | let loc (t: Mixed_blocks_rec.t) = [%atomic.loc t.field]
                                                   ^
Error: Use of "[%atomic.loc]" with mixed record fields (here "field") is forbidden.
|}]

(* Test atomic record fields with non-value layouts *)

module Non_value_atomic = struct
  type t = {
    mutable field : #(int * float#) [@atomic]
  }
end

[%%expect{|
Line 3, characters 4-45:
3 |     mutable field : #(int * float#) [@atomic]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Atomic record fields must have layout value.
|}]

module Non_value_atomic_rec = struct
  type t = {
    mutable field : u [@atomic]
  }

  and u = #{
    x : int#
  }
end

[%%expect{|
Line 3, characters 4-31:
3 |     mutable field : u [@atomic]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Atomic record fields must have layout value.
|}]

module Non_value_atomic_single_float64 = struct
type t = { mutable f : float# [@atomic] }
end

[%%expect{|
Line 2, characters 11-39:
2 | type t = { mutable f : float# [@atomic] }
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Atomic record fields must have layout value.
|}]

module Non_value_atomic_single_bits32 = struct
  type t = { mutable f : int32# [@atomic] }
end

[%%expect{|
Line 2, characters 13-41:
2 |   type t = { mutable f : int32# [@atomic] }
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Atomic record fields must have layout value.
|}]

module Inline_record_non_value_atomic = struct
  type t = A of { mutable f : float# [@atomic] }
end

[%%expect{|
Line 2, characters 18-46:
2 |   type t = A of { mutable f : float# [@atomic] }
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Atomic record fields must have layout value.
|}]

module Atomic_float_with_float_hash = struct
  type t = { mutable f : float [@atomic]; u : float# }

  let disallowed t = t.f
end

[%%expect{|
Line 4, characters 21-22:
4 |   let disallowed t = t.f
                         ^
Error: Accessing atomic fields (here "f") of mixed records is not yet
       supported.
|}]

module Inline_record_atomic_in_mixed = struct
  type t = A of { mutable f : int [@atomic]; u : int# }

  let disallowed t = match t with
  | A r -> r.f
end

[%%expect{|
Line 5, characters 11-12:
5 |   | A r -> r.f
               ^
Error: Accessing atomic fields (here "f") of mixed records is not yet
       supported.
|}]
