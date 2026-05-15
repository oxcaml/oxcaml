(* TEST
 flags = "-dslambda -dno-unique-ids -extension layout_poly_alpha";
 expect;
*)

(* Simplest layout polymorphic function *)
let poly_ id x = x
[%%expect{|
(let (id =
   { c =
     (template layout_2 ->
       { c = (missing)
       ; r =
         ⟪(function {nlocal = 0} closure
            (function {nlocal = 0} x[$layout_2] : $layout_2 x))⟫ })
   ; r = ⟪(makeblock 0)⟫ })
  { c = (missing)
  ; r = ⟪(let (id = $id) (apply (field_imm 1 (global Toploop!)) "id" id))⟫ })
val id : layout_ l. ('a : l). 'a -> 'a = <lpoly>
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
               (template layout_15 ->
                 { c = (missing)
                 ; r =
                   ⟪(function {nlocal = 0} closure
                      (let
                        (foo =a[value<int>] (field_imm 1 closure)
                         bool =a[value<int>] (field_imm 0 closure))
                        (function {nlocal = 0} x[$layout_15] y[$layout_15]
                          : #($layout_15, ?)
                          (if bool
                            (make_unboxed_product #($layout_15, value<int>) x
                              foo)
                            (make_unboxed_product #($layout_15, value<int>) y
                              2)))))⟫ })
             ; r = ⟪(makeblock 0 (value<int>,value<int>) bool foo)⟫ })
            { c = (missing)
            ; r =
              ⟪(let (captures = $captures)
                 (apply (field_imm 1 (global Toploop!)) "captures" captures))⟫ }))
       { c = #body.c; r = ⟪(let (bool =? $bool) $#body)⟫ }))
  { c = #body.c; r = ⟪(let (foo =? $foo) $#body)⟫ })
val captures : layout_ l. ('a : l). 'a -> 'a -> #('a * int) = <lpoly>
|}]

(* Multiple static arguments *)
let poly_ f x y = #(x, y)
[%%expect{|
(let (f =
   { c =
     (template layout_28 layout_29 ->
       { c = (missing)
       ; r =
         ⟪(function {nlocal = 0} closure
            (function {nlocal = 0} x[$layout_28] y[$layout_29]
              : #($layout_28, $layout_29)
              (make_unboxed_product #($layout_28, $layout_29) x y)))⟫ })
   ; r = ⟪(makeblock 0)⟫ })
  { c = (missing)
  ; r = ⟪(let (f = $f) (apply (field_imm 1 (global Toploop!)) "f" f))⟫ })
val f : layout_ l l0. ('a : l) ('b : l0). 'a -> 'b -> #('a * 'b) = <lpoly>
|}]

(* Application *)
let _ =
  let poly_ f x = x in
  f ()
;;
[%%expect{|
(let (#body =
   (let
     (f =
        { c =
          (template layout_36 ->
            { c = (missing)
            ; r =
              ⟪(function[L] {nlocal = 1} closure[L] : local
                 (function[L] {nlocal = 1} x[$layout_36] : $layout_36 x))⟫ })
        ; r = ⟪(makelocalblock 0)⟫ }
      #body =
        { c = (missing)
        ; r =
          ⟪(apply
             $(let (#app = (f.c ⟪layout ?⟫))
                { c = #app.c; r = ⟪(apply[L] $#app f)⟫ })
             0)⟫ })
     { c = #body.c; r = ⟪(let (f = $f) $#body)⟫ }))
  { c = #body.c; r = ⟪(region $#body)⟫ })
- : unit = ()
|}]

let _ =
  let poly_ f = (2, fun x -> x) in
  let a, b = f in
  #(a, b #3L)
;;
[%%expect{|
(let (#body =
   (let
     (f =
        { c =
          (template layout_42 ->
            { c = [ (missing); (missing); ]
            ; r =
              ⟪(function[L] {nlocal = 1} closure[L]
                 : (consts ()) (non_consts ([0: value<int>, *]))
                 (makelocalblock 0 (value<int>,*) 2
                   (function[L] {nlocal = 1} x[$layout_42] : $layout_42 x)))⟫ })
        ; r = ⟪(makelocalblock 0)⟫ }
      #body =
        (let (*match* =
           (let (#app = (f.c ⟪layout int64⟫))
             { c = #app.c; r = ⟪(apply[L] $#app f)⟫ }))
          { c = (missing)
          ; r =
            ⟪(let
               (*match* =[value<
                           (consts ()) (non_consts ([0: value<int>, *]))>]
                  $*match*
                b =a? (field_imm 1 *match*)
                a =a? (field_imm 0 *match*))
               (make_unboxed_product #(value<int>, int64) a (apply b #3L)))⟫ }))
     { c = #body.c; r = ⟪(let (f = $f) $#body)⟫ }))
  { c = #body.c; r = ⟪(region $#body)⟫ })
- : #(int * int64#) = #(2, <abstr>)
|}]
