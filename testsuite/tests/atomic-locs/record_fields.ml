(* TEST
   flags = "-dlambda -dno-locations -dno-unique-ids";
   expect;
*)

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
(apply (field_imm 1 (global Toploop!)) "Ok/305" (makeblock 0))
module Ok : sig type t = { mutable x : int [@atomic]; } end
|}];;

(* Inline records are supported, including in extensions. *)

module Inline_record = struct
  type t = A of { mutable x : int [@atomic] }

  let test : t -> int = fun (A r) -> r.x
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Inline_record/313"
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
(apply (field_imm 1 (global Toploop!)) "Extension_with_inline_record/321"
  (let
    (A =
       (makeblock_unique 248 "Extension_with_inline_record.A"
         (caml_fresh_oo_id 0))
     test =
       (function {nlocal = 0} param : int
         (if (== (field_imm 0 param) A) (atomic_load_field_imm param 1) 0))
     *match* =[value<int>]
       (if (== (apply test (makemutable 0 (?,value<int>) A 42)) 42) 0
         (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 11 11]))))
    (makeblock 0 A test)))
module Extension_with_inline_record :
  sig
    type t = ..
    type t += A of { mutable x : int [@atomic]; }
    val test : t -> int
  end
|}]

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
Warning 214 [atomic-float-record-boxed]: This record contains atomic
float fields, which prevents the float record optimization. The
fields of this record will be boxed instead of being
represented as a flat float array.
(apply (field_imm 1 (global Toploop!)) "Float_records/347"
  (let
    (mk_flat =
       (function {nlocal = 0} x[value<float>] y[value<float>]
         (makefloatblock Mutable x y))
     mk_t =
       (function {nlocal = 0} x[value<float>] y[value<float>]
         (makemutable 0 (value<float>,value<float>) x y))
     get = (function {nlocal = 0} v : float (atomic_load_field_ptr v 1)))
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
Warning 214 [atomic-float-record-boxed]: This record contains atomic
float fields, which prevents the float record optimization. The
fields of this record will be boxed instead of being
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
Warning 214 [atomic-float-record-boxed]: This record contains atomic
float fields, which prevents the float record optimization. The
fields of this record will be boxed instead of being
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
Warning 214 [atomic-float-record-boxed]: This record contains atomic
float fields, which prevents the float record optimization. The
fields of this record will be boxed instead of being
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
Warning 214 [atomic-float-record-boxed]: This record contains atomic
float fields, which prevents the float record optimization. The
fields of this record will be boxed instead of being
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
Warning 9 [missing-record-field-pattern]: the following labels are not bound in this record pattern:
y
Either bind these labels explicitly or add '; _' to the pattern.
(apply (field_imm 1 (global Toploop!)) "Pattern_matching_wildcard/430"
  (let
    (warning = (function {nlocal = 0} param : int (field_int 0 param))
     allowed = (function {nlocal = 0} param : int (field_int 0 param))
     also_allowed = (function {nlocal = 0} param : int (field_int 0 param)))
    (makeblock 0 warning allowed also_allowed)))

module Pattern_matching_wildcard :
  sig
    type t = { x : int; mutable y : int [@atomic]; }
    val warning : t -> int
    val allowed : t -> int
    val also_allowed : t -> int
  end
|}]
