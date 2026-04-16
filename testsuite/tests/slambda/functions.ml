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
  ; r = ⟪(let (id = $id) (apply (field_imm 1 (global Toploop!)) "id" id))⟫ })
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
               (template layout!24 ->
                 { c = (missing)
                 ; r =
                   ⟪(function {nlocal = 0} closure
                      (let
                        (foo =a[value<int>] (field_imm 1 closure)
                         bool =a[value<int>] (field_imm 0 closure))
                        (function {nlocal = 0} x[$layout!24] y[$layout!24]
                          : #($layout!24, ?)
                          (if bool
                            (make_unboxed_product #($layout!24, value<int>) x
                              foo)
                            (make_unboxed_product #($layout!24, value<int>) y
                              2)))))⟫ })
             ; r = ⟪(makeblock 0 (value<int>,value<int>) bool foo)⟫ })
            { c = (missing)
            ; r =
              ⟪(let (captures = $captures)
                 (apply (field_imm 1 (global Toploop!)) "captures" captures))⟫ }))
       { c = #body.c; r = ⟪(let (bool =? $bool) $#body)⟫ }))
  { c = #body.c; r = ⟪(let (foo =? $foo) $#body)⟫ })
val captures : layout_ l. ('a : l). 'a -> 'a -> #('a * int) = <fun>
|}]

(* Multiple static arguments *)
let poly_ f x y = #(x, y)
[%%expect{|
(let (f =
   { c =
     (template layout!46 layout!47 ->
       { c = (missing)
       ; r =
         ⟪(function {nlocal = 0} closure
            (function {nlocal = 0} x[$layout!46] y[$layout!47]
              : #($layout!46, $layout!47)
              (make_unboxed_product #($layout!46, $layout!47) x y)))⟫ })
   ; r = ⟪(makeblock 0)⟫ })
  { c = (missing)
  ; r = ⟪(let (f = $f) (apply (field_imm 1 (global Toploop!)) "f" f))⟫ })
val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> #('a * 'b) = <fun>
|}]
