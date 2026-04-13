(* TEST
 flags = "-dslambda -dno-unique-ids -extension layout_poly_alpha";
 expect;
*)

(* Simplest layout polymorphic function *)
let poly_ id x = x
[%%expect{|
(let (id =
   { c =
     (template layout!2 ->
       { c = (missing)
       ; r =
         ⟪(function {nlocal = 0} closure
            (function {nlocal = 0} x[$layout!2] : $layout!2 x))⟫ })
   ; r = ⟪(makeblock 0)⟫ })
  { c = (missing)
  ; r = ⟪(let (id = $id.r) (apply (field_imm 1 (global Toploop!)) "id" id))⟫ })
val id : layout_ l. ('a : l). 'a -> 'a = <fun>
|}]

(* Test closure conversion *)
let bool = true
let foo = 1
[%%expect{|
{ c = (missing)
; r =
  ⟪(let (bool =[value<int>] 1)
     (apply (field_imm 1 (global Toploop!)) "bool" bool))⟫ }
val bool : bool = true
{ c = (missing)
; r =
  ⟪(let (foo =[value<int>] 1)
     (apply (field_imm 1 (global Toploop!)) "foo" foo))⟫ }
val foo : int = 1
|}]
let poly_ captures x y = if bool then #(x, foo) else #(y, 2)
[%%expect{|
(let
  (foo =
     { c = (missing); r = ⟪(apply (field_imm 0 (global Toploop!)) "foo")⟫ }
   #body =
     (let
       (bool =
          { c = (missing)
          ; r = ⟪(apply (field_imm 0 (global Toploop!)) "bool")⟫ }
        #body =
          (let (captures =
             { c =
               (template layout!15 ->
                 { c = (missing)
                 ; r =
                   ⟪(function {nlocal = 0} closure
                      (let
                        (foo =a[value<int>]
                           (mixedfield 1  (value<int>,value<int>) closure)
                         bool =a[value<int>]
                           (mixedfield 0  (value<int>,value<int>) closure))
                        (function {nlocal = 0} x[$layout!15] y[$layout!15]
                          : #($layout!15, ?)
                          (if bool
                            (make_unboxed_product #($layout!15, value<int>) x
                              foo)
                            (make_unboxed_product #($layout!15, value<int>) y
                              2)))))⟫ })
             ; r = ⟪(makeblock 0 (value<int>,value<int>) bool foo)⟫ })
            { c = (missing)
            ; r =
              ⟪(let (captures = $captures.r)
                 (apply (field_imm 1 (global Toploop!)) "captures" captures))⟫ }))
       { c = #body.c; r = ⟪(let (bool =? $bool.r) $#body.r)⟫ }))
  { c = #body.c; r = ⟪(let (foo =? $foo.r) $#body.r)⟫ })
val captures : layout_ l. ('a : l). 'a -> 'a -> #('a * int) = <fun>
|}]

(* Multiple static arguments *)
let poly_ f x y = #(x, y)
[%%expect{|
(let (f =
   { c =
     (template layout!28 layout!29 ->
       { c = (missing)
       ; r =
         ⟪(function {nlocal = 0} closure
            (function {nlocal = 0} x[$layout!28] y[$layout!29]
              : #($layout!28, $layout!29)
              (make_unboxed_product #($layout!28, $layout!29) x y)))⟫ })
   ; r = ⟪(makeblock 0)⟫ })
  { c = (missing)
  ; r = ⟪(let (f = $f.r) (apply (field_imm 1 (global Toploop!)) "f" f))⟫ })
val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> #('a * 'b) = <fun>
|}]
