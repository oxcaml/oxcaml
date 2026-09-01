(* TEST
   flags = "-extension layouts_alpha";
   expect;
*)

(* Atomic field test cases that use mixed records (contain both
   values and nonvalues). *)

(* Atomic fields must be mutable. *)
module Error1_mixed = struct
  type t = { padding: #(int * int * int); x : int [@atomic] }
end
[%%expect{|
Line 2, characters 42-59:
2 |   type t = { padding: #(int * int * int); x : int [@atomic] }
                                              ^^^^^^^^^^^^^^^^^
Error: The label "x" must be mutable to be declared atomic.
|}];;


(* [%atomic.loc _] payload must be a record field access *)
module Error2_mixed = struct
  type t = { padding: #(int * int * int); mutable x : int [@atomic] }
  let f t = [%atomic.loc t]
end
[%%expect{|
Line 3, characters 12-27:
3 |   let f t = [%atomic.loc t]
                ^^^^^^^^^^^^^^^
Error: Invalid "[%atomic.loc]" payload, a record field access is expected.
|}];;


(* [%atomic.loc _] only works on atomic fields *)
module Error3_mixed = struct
  type t = { padding: #(int * int * int); x : int }
  let f t = [%atomic.loc t.x]
end
[%%expect{|
Line 3, characters 12-29:
3 |   let f t = [%atomic.loc t.x]
                ^^^^^^^^^^^^^^^^^
Error: The record field "x" is not atomic
|}];;


(* Module interface checking also works for atomic fields in mixed records. *)

module Wrong1 = (struct
  type t = { mutable x : int; y: int64# }
end : sig
  (* adding an 'atomic' attribute missing in the implementation: invalid. *)
  type t = { mutable x : int [@atomic]; y: int64# }
end)
[%%expect{|
Lines 1-3, characters 17-3:
1 | .................struct
2 |   type t = { mutable x : int; y: int64# }
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int; y : int64#; } end
       is not included in
         sig type t = { mutable x : int [@atomic]; y : int64#; } end
       Type declarations do not match:
         type t = { mutable x : int; y : int64#; }
       is not included in
         type t = { mutable x : int [@atomic]; y : int64#; }
       Fields do not match:
         "mutable x : int;"
       is not the same as:
         "mutable x : int [@atomic];"
       The second is atomic and the first is not.
|}];;

module Wrong2 = (struct
  type t = { mutable x : int [@atomic]; y: int64# }
end : sig
  (* removing an 'atomic' attribute present in the implementation: invalid. *)
  type t = { mutable x : int; y: int64# }
end)
[%%expect{|
Lines 1-3, characters 17-3:
1 | .................struct
2 |   type t = { mutable x : int [@atomic]; y: int64# }
3 | end......
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int [@atomic]; y : int64#; } end
       is not included in
         sig type t = { mutable x : int; y : int64#; } end
       Type declarations do not match:
         type t = { mutable x : int [@atomic]; y : int64#; }
       is not included in
         type t = { mutable x : int; y : int64#; }
       Fields do not match:
         "mutable x : int [@atomic];"
       is not the same as:
         "mutable x : int;"
       The first is atomic and the second is not.
|}];;

module Ok = (struct
  type t = { mutable x : int [@atomic]; y: int64# }
end : sig
  type t = { mutable x : int [@atomic]; y: int64# }
end)
[%%expect{|
module Ok : sig type t = { mutable x : int [@atomic]; y : int64#; } end
|}];;

(* Pattern matching on atomic fields is not permitted in mixed blocks, either. *)
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

module Pattern_matching_wildcard :
  sig
    type t = { x : int64#; mutable y : int [@atomic]; }
    val warning : t -> int64#
    val allowed : t -> int64#
    val also_allowed : t -> int64#
  end
|}]

(* Atomic fields are permitted in mixed blocks. *)
module Mixed_record_atomics = struct
  type t = {
    mutable a : int [@atomic];
    mutable b : int64#
  }

  let access t =
    let _ = t.a in
    let _ = t.b in
    t.a <- 42;
    t.b <- #42L
end
[%%expect{|
module Mixed_record_atomics :
  sig
    type t = { mutable a : int [@atomic]; mutable b : int64#; }
    val access : t -> unit
  end
|}]

(* Atomic fields are permitted in mixed inline records. *)
module Mixed_inline_record_atomics = struct
  type t = A of {
    mutable a : int [@atomic];
    mutable b : int64#
  }

  let access (A t) =
    let _ = t.a in
    let _ = t.b in
    t.a <- 42;
    t.b <- #42L
end
[%%expect{|
module Mixed_inline_record_atomics :
  sig
    type t = A of { mutable a : int [@atomic]; mutable b : int64#; }
    val access : t -> unit
  end
|}]

(* We forbid taking atomic.loc of fields from mixed records. *)
let forbidden (t : Mixed_record_atomics.t) = [%atomic.loc t.a]
[%%expect{|
Line 1, characters 45-62:
1 | let forbidden (t : Mixed_record_atomics.t) = [%atomic.loc t.a]
                                                 ^^^^^^^^^^^^^^^^^
Error: Use of "[%atomic.loc]" with mixed record fields (here "a") is forbidden.
|}]

let forbidden_inline (A t : Mixed_inline_record_atomics.t) = [%atomic.loc t.a]
[%%expect{|
Line 1, characters 61-78:
1 | let forbidden_inline (A t : Mixed_inline_record_atomics.t) = [%atomic.loc t.a]
                                                                 ^^^^^^^^^^^^^^^^^
Error: Use of "[%atomic.loc]" with mixed record fields (here "a") is forbidden.
|}]

(* Test mixed record atomic fields with non-value layouts *)

module Non_value_atomic_mixed = struct
  type t = {
    padding : #(int * int * int);
    mutable field : #(int * float#) [@atomic]
  }
end

[%%expect{|
Line 4, characters 4-45:
4 |     mutable field : #(int * float#) [@atomic]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Atomic record fields must have layout value.
|}]

module Non_value_atomic_mixed_rec = struct
  type t = {
    padding : #(int * int * int);
    mutable field : u [@atomic]
  }

  and u = #{
    x : int#
  }
end

[%%expect{|
Line 4, characters 4-31:
4 |     mutable field : u [@atomic]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Atomic record fields must have layout value.
|}]

module Mixed_inline_record_non_value_atomic = struct
  type t = A of { mutable f : float# [@atomic]; g : int64# }
end
[%%expect{|
Line 2, characters 18-47:
2 |   type t = A of { mutable f : float# [@atomic]; g : int64# }
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Atomic record fields must have layout value.
|}]

(* Test functional updates of mixed records with atomics. *)

(* We disallow functional updates that perform implicit loads of atomic fields. *)
module Functional_update_error = struct
  type t = { x : int64# ; mutable y : int [@atomic] }

  (* The update performs an implicit atomic load of y. Not allowed! *)
  let forbidden t = { t with x = #42L }
end
[%%expect{|
Line 5, characters 22-23:
5 |   let forbidden t = { t with x = #42L }
                          ^
Error: Functional updates that implicitly read atomic fields (here "y")
       are forbidden. Hint: if you intend to copy the value
       of an atomic field, do so explicitly: "{ t with y = t.y }"
|}]

(* Updates that overwrite all of the old record's atomic fields are allowed. *)

module Functional_update_ok = struct
  type t = { x : int64# ; mutable y : int [@atomic] }

  (* The update performs no implicit atomic loads. Allowed! *)
  let allowed t = { t with y = 42 }
end
[%%expect{|
module Functional_update_ok :
  sig
    type t = { x : int64#; mutable y : int [@atomic]; }
    val allowed : t -> t
  end
|}]

module Functional_update_copy_ok = struct
  type t = { x : int64# ; mutable y : int [@atomic] }

  (* The update performs an explicit atomic load. Allowed! *)
  let allowed t = { t with y = t.y }
end
[%%expect{|
module Functional_update_copy_ok :
  sig
    type t = { x : int64#; mutable y : int [@atomic]; }
    val allowed : t -> t
  end
|}]

module Functional_update_multi_error = struct
  type t = { x : int64# ; mutable y : int [@atomic]; mutable z : int [@atomic] }

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
  type t = { x : int64# ; mutable y : int [@atomic]; mutable z : int [@atomic] }

  let allowed t = { t with y = 42; z = 67 } (* no implicit atomic loads *)
end
[%%expect{|
module Functional_update_multi_ok :
  sig
    type t = {
      x : int64#;
      mutable y : int [@atomic];
      mutable z : int [@atomic];
    }
    val allowed : t -> t
  end
|}]

module Functional_update_multi_copy_ok = struct
  type t = { x : int64# ; mutable y : int [@atomic]; mutable z : int [@atomic] }

  let allowed t = { t with y = t.y; z = t.z } (* no implicit atomic loads *)
end
[%%expect{|
module Functional_update_multi_copy_ok :
  sig
    type t = {
      x : int64#;
      mutable y : int [@atomic];
      mutable z : int [@atomic];
    }
    val allowed : t -> t
  end
|}]
