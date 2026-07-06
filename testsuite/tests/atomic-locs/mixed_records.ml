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

(* Defining atomic fields in a mixed block is permitted. *)
module Mixed_blocks_ok = struct
  type t = {
    padding : #(int * int * int);
    mutable field : int [@atomic]
  }
end

[%%expect{|
module Mixed_blocks_ok :
  sig
    type t = { padding : #(int * int * int); mutable field : int [@atomic]; }
  end
|}]

(* Test access of nonatomic fields in mixed record with atomic fields *)
type t = { i : int64#; mutable a : int [@atomic]; mutable b : int }
[%%expect {|
type t = { i : int64#; mutable a : int [@atomic]; mutable b : int; }
|}]

let ok_project (t : t) = t.b
[%%expect {|
val ok_project : t -> int = <fun>
|}]
let ok_set (t : t) = t.b <- 42
[%%expect {|
val ok_set : t -> unit = <fun>
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
