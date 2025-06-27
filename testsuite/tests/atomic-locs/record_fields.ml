(* TEST
   flags = "-dlambda -dno-locations -dno-unique-ids";
   expect;
*)

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
(apply (field_imm 1 (global Toploop!)) "Ok/303" (makeblock 0))
module Ok : sig type t = { mutable x : int [@atomic]; } end
|}];;

(* Inline records are supported, including in extensions. *)

module Inline_record = struct
  type t = A of { mutable x : int [@atomic] }

  let test : t -> int = fun (A r) -> r.x
end
[%%expect{|
(apply (field_imm 1 (global Toploop!)) "Inline_record/311"
  (let (test = (function {nlocal = 0} param : int (field_int 0 param)))
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
(apply (field_imm 1 (global Toploop!)) "Extension_with_inline_record/319"
  (let
    (A =
       (makeblock_unique 248 "Extension_with_inline_record.A"
         (caml_fresh_oo_id 0))
     test =
       (function {nlocal = 0} param : int
         (if (== (field_imm 0 param) A) (field_int 1 param) 0))
     *match* =[int]
       (if (== (apply test (makemutable 0 (*,int) A 42)) 42) 0
         (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 11 11]))))
    (makeblock 0 A test)))
module Extension_with_inline_record :
  sig
    type t = ..
    type t += A of { mutable x : int [@atomic]; }
    val test : t -> int
  end
|}]
