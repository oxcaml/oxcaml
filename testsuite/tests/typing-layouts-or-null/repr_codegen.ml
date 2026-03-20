(* TEST
 flags = "-dlambda -dno-unique-ids";
 expect;
*)

type with_boxed =
  | Nullish_boxed [@repr null]
  | Int_boxed of int [@repr immediate]
  | Boxed of string

let inject_i n = Int_boxed n

[%%expect{|
0
type with_boxed =
    Nullish_boxed
  [@repr null]
  | Int_boxed of int
  [@repr immediate]
  | Boxed of string
(let (inject_i = (function {nlocal = 0} n[value<int>] n))
  (apply (field_imm 1 (global Toploop!)) "inject_i" inject_i))
val inject_i : int -> with_boxed = <fun>
|}]

let classify = function
  | Nullish_boxed -> 0
  | Int_boxed n -> n
  | Boxed _ -> -1

[%%expect{|
(let
  (classify =
     (function {nlocal = 0} param? : int
       (if (isnull param) 0 (if (isint param) param -1))))
  (apply (field_imm 1 (global Toploop!)) "classify" classify))
val classify : with_boxed -> int = <fun>
|}]

type ('a : value pointer) null_immediate_pointer =
  | NIP [@repr null]
  | IIP of int [@repr immediate]
  | PIP of 'a [@repr pointer]

let classify_nip = function
  | NIP -> 0
  | IIP n -> n
  | PIP _ -> -1

[%%expect{|
0
type ('a : value pointer) null_immediate_pointer =
    NIP
  [@repr null]
  | IIP of int
  [@repr immediate]
  | PIP of 'a
  [@repr pointer]
(let
  (classify_nip =
     (function {nlocal = 0} param? : int
       (if (isnull param) 0 (if (isint param) param -1))))
  (apply (field_imm 1 (global Toploop!)) "classify_nip" classify_nip))
val classify_nip : ('a : value pointer). 'a null_immediate_pointer -> int =
  <fun>
|}]
