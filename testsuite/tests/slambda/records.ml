(* TEST
 flags = "-dslambda -dno-unique-ids";
 expect;
*)

let one = 1
let two = 2
let two_u = #2m
type t = { a : int; b : int }
type m = { c : int; d : int# }
type s = { e : t; f : t }
[%%expect{|
{ c = (missing);
  r = ⟪ (let (one =[value<int>] 1)
          (apply (field_imm 1 (global Toploop!)) "one" one)) ⟫ }
val one : int = 1
{ c = (missing);
  r = ⟪ (let (two =[value<int>] 2)
          (apply (field_imm 1 (global Toploop!)) "two" two)) ⟫ }
val two : int = 2
{ c = (missing);
  r = ⟪ (let (two_u =[int] #2m)
          (apply (field_imm 1 (global Toploop!)) "two_u" two_u)) ⟫ }
val two_u : int# = <abstr>
{ c = (missing); r = ⟪ 0 ⟫ }
type t = { a : int; b : int; }
{ c = (missing); r = ⟪ 0 ⟫ }
type m = { c : int; d : int#; }
{ c = (missing); r = ⟪ 0 ⟫ }
type s = { e : t; f : t; }
|}];;

{ a = one; b = two }
[%%expect{|
{ c = (let
        (two =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "two") ⟫ }
         one =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "one") ⟫ })
        [ one.c; two.c; ]);
  r = ⟪ (let
          (two =? (apply (field_imm 0 (global Toploop!)) "two")
           one =? (apply (field_imm 0 (global Toploop!)) "one"))
          (makeblock 0 (value<int>,value<int>) one two)) ⟫ }
- : t = {a = 1; b = 2}
|}];;

{ a = one; b = two }.a
[%%expect{|
{ c = (let
        (two =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "two") ⟫ }
         one =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "one") ⟫ })
        [ one.c; two.c; ].0);
  r = ⟪ (let
          (two =? (apply (field_imm 0 (global Toploop!)) "two")
           one =? (apply (field_imm 0 (global Toploop!)) "one"))
          (region
            (field_int 0 (makelocalblock 0 (value<int>,value<int>) one two)))) ⟫ }
- : int = 1
|}];;

{ c = one; d = two_u }.c
[%%expect{|
{ c = (let
        (two_u =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "two_u") ⟫ }
         one =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "one") ⟫ })
        [ one.c; two_u.c; ].0);
  r = ⟪ (let
          (two_u =? (apply (field_imm 0 (global Toploop!)) "two_u")
           one =? (apply (field_imm 0 (global Toploop!)) "one"))
          (region
            (mixedfield 0  (value<int>,untagged_immediate)
              (makelocalblock 0 (?,untagged_immediate) one two_u)))) ⟫ }
- : int = 1
|}];;

let x = { a = one; b = two } in
let y = { a = two; b = one } in
{ e = x; f = y }
[%%expect{|
{ c = (let
        (two =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "two") ⟫ }
         one =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "one") ⟫ }
         x =
           { c = [ one.c; two.c; ];
             r = ⟪ (makeblock 0 (value<int>,value<int>) one two) ⟫ }
         y =
           { c = [ two.c; one.c; ];
             r = ⟪ (makeblock 0 (value<int>,value<int>) two one) ⟫ })
        [ x.c; y.c; ]);
  r = ⟪ (let
          (two =? (apply (field_imm 0 (global Toploop!)) "two")
           one =? (apply (field_imm 0 (global Toploop!)) "one")
           x =[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
             (makeblock 0 (value<int>,value<int>) one two)
           y =[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
             (makeblock 0 (value<int>,value<int>) two one))
          (makeblock 0 (value<
                         (consts ())
                          (non_consts ([0: value<int>, value<int>]))>,
            value<(consts ()) (non_consts ([0: value<int>, value<int>]))>) x
            y)) ⟫ }
- : s = {e = {a = 1; b = 2}; f = {a = 2; b = 1}}
|}];;
