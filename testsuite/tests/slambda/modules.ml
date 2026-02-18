(* TEST
 flags = "-dslambda -dno-unique-ids";
 expect;
*)

let one = 1
let two = 2
let two_u = #2m
module type S = sig
  val a : int
  val b : int
end
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
module type S = sig val a : int val b : int end
|}];;

(module struct
  let a = one
  let b = two
end : S)
[%%expect{|
{ c = (let
        (two =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "two") ⟫ }
         one =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "one") ⟫ }
         a = { c = one.c; r = ⟪ one ⟫ }
         b = { c = two.c; r = ⟪ two ⟫ })
        [ a.c; b.c; ]);
  r = ⟪ (let
          (two =? (apply (field_imm 0 (global Toploop!)) "two")
           one =? (apply (field_imm 0 (global Toploop!)) "one")
           a =[value<int>] one
           b =[value<int>] two)
          (makeblock 0 a b)) ⟫ }
- : (module S) = <module>
|}];;

let module M = struct
  let a = one
  let b = two_u
end in
M.a
[%%expect{|
{ c = (let
        (two_u =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "two_u") ⟫ }
         one =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "one") ⟫ }
         M =
           { c = (let
                   (a = { c = one.c; r = ⟪ one ⟫ }
                    b = { c = two_u.c; r = ⟪ two_u ⟫ })
                   [ a.c; b.c; ]);
             r = ⟪ (let (a =[value<int>] one b =[int] two_u)
                     (makeblock 0 (?,untagged_immediate) a b)) ⟫ })
        M.c.0);
  r = ⟪ (let
          (two_u =? (apply (field_imm 0 (global Toploop!)) "two_u")
           one =? (apply (field_imm 0 (global Toploop!)) "one")
           M =
             (let (a =[value<int>] one b =[int] two_u)
               (makeblock 0 (?,untagged_immediate) a b)))
          (mixedfield 0  (?,untagged_immediate) M)) ⟫ }
- : int = 1
|}];;


let module M = struct
  module N = struct
    let a = one
  end
end in
M.N.a
[%%expect{|
{ c = (let
        (one =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "one") ⟫ }
         M =
           { c = (let
                   (N =
                      { c = (let (a = { c = one.c; r = ⟪ one ⟫ }) [ a.c; ]);
                        r = ⟪ (let (a =[value<int>] one) (makeblock 0 a)) ⟫ })
                   [ N.c; ]);
             r = ⟪ (let (N = (let (a =[value<int>] one) (makeblock 0 a)))
                     (makeblock 0 N)) ⟫ })
        M.c.0.0);
  r = ⟪ (let
          (one =? (apply (field_imm 0 (global Toploop!)) "one")
           M =
             (let (N = (let (a =[value<int>] one) (makeblock 0 a)))
               (makeblock 0 N)))
          (field_imm 0 (field_imm 0 M))) ⟫ }
- : int = 1
|}];;

let module M = struct
  one;;
  let a = one
  external b : 'a -> 'a = "%identity"
  type c = ..
  type c += D
  exception E
  module F = struct
    let fa = two
  end
  module type G = sig end
  module rec H : G = H and I : G = I
  open F
  class j = object end
  class type k = object end
  include H
  [@@@warnings "-191"]
  kind_ l
end in
M.a
[%%expect{|
Line 18, characters 2-9:
18 |   kind_ l
       ^^^^^^^
Warning 191 [unused-kind-declaration]: unused kind l.
{ c = (let
        (two =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "two") ⟫ }
         one =
           { c = (missing);
             r = ⟪ (apply (field_imm 0 (global Toploop!)) "one") ⟫ }
         M =
           { c = (let
                   (a = { c = one.c; r = ⟪ one ⟫ }
                    D =
                      { c = [ (missing); (missing); ];
                        r = ⟪ (makeblock_unique 248 "D" (caml_fresh_oo_id 0)) ⟫ }
                    E =
                      { c = [ (missing); (missing); ];
                        r = ⟪ (makeblock_unique 248 "E" (caml_fresh_oo_id 0)) ⟫ }
                    F =
                      { c = (let (fa = { c = two.c; r = ⟪ two ⟫ }) [ fa.c; ]);
                        r = ⟪ (let (fa =[value<int>] two) (makeblock 0 fa)) ⟫ }
                    H =
                      { c = (missing);
                        r = ⟪ (apply (field_imm 0 (global CamlinternalMod!))
                                [0: "" 12 21] [0: [0]]) ⟫ }
                    I =
                      { c = (missing);
                        r = ⟪ (apply (field_imm 0 (global CamlinternalMod!))
                                [0: "" 12 35] [0: [0]]) ⟫ }
                    j =
                      { c = (missing);
                        r = ⟪ (let
                                (j_tables =o (opaque (makemutable 0 0 0 0))
                                 j_init =
                                   (function {nlocal = 0} class
                                     (function {nlocal = 0} env
                                       self[value<genarray>]
                                       (opaque
                                         (apply
                                           (field_imm 23
                                             (global CamlinternalOO!))
                                           self class)))))
                                (opaque
                                  (apply
                                    (field_imm 18 (global CamlinternalOO!)) 0
                                    j_init))) ⟫ }
                    include = { c = H.c; r = ⟪ H ⟫ })
                   [ a.c; D.c; E.c; F.c; H.c; I.c; j.c; ]);
             r = ⟪ (seq one
                     (let
                       (a =[value<int>] one
                        D = (makeblock_unique 248 "D" (caml_fresh_oo_id 0))
                        E = (makeblock_unique 248 "E" (caml_fresh_oo_id 0))
                        F = (let (fa =[value<int>] two) (makeblock 0 fa))
                        H =
                          (apply (field_imm 0 (global CamlinternalMod!))
                            [0: "" 12 21] [0: [0]])
                        I =
                          (apply (field_imm 0 (global CamlinternalMod!))
                            [0: "" 12 35] [0: [0]]))
                       (seq
                         (apply (field_imm 1 (global CamlinternalMod!))
                           [0: [0]] H H)
                         (apply (field_imm 1 (global CamlinternalMod!))
                           [0: [0]] I I)
                         (let
                           (j =?
                              (let
                                (j_tables =o (opaque (makemutable 0 0 0 0))
                                 j_init =
                                   (function {nlocal = 0} class
                                     (function {nlocal = 0} env
                                       self[value<genarray>]
                                       (opaque
                                         (apply
                                           (field_imm 23
                                             (global CamlinternalOO!))
                                           self class)))))
                                (opaque
                                  (apply
                                    (field_imm 18 (global CamlinternalOO!)) 0
                                    j_init)))
                            include =a H)
                           (makeblock 0 a D E F H I j))))) ⟫ })
        M.c.0);
  r = ⟪ (let
          (two =? (apply (field_imm 0 (global Toploop!)) "two")
           one =? (apply (field_imm 0 (global Toploop!)) "one")
           M =
             (seq one
               (let
                 (a =[value<int>] one
                  D = (makeblock_unique 248 "D" (caml_fresh_oo_id 0))
                  E = (makeblock_unique 248 "E" (caml_fresh_oo_id 0))
                  F = (let (fa =[value<int>] two) (makeblock 0 fa))
                  H =
                    (apply (field_imm 0 (global CamlinternalMod!))
                      [0: "" 12 21] [0: [0]])
                  I =
                    (apply (field_imm 0 (global CamlinternalMod!))
                      [0: "" 12 35] [0: [0]]))
                 (seq
                   (apply (field_imm 1 (global CamlinternalMod!)) [0: [0]] H
                     H)
                   (apply (field_imm 1 (global CamlinternalMod!)) [0: [0]] I
                     I)
                   (let
                     (j =?
                        (let
                          (j_tables =o (opaque (makemutable 0 0 0 0))
                           j_init =
                             (function {nlocal = 0} class
                               (function {nlocal = 0} env
                                 self[value<genarray>]
                                 (opaque
                                   (apply
                                     (field_imm 23 (global CamlinternalOO!))
                                     self class)))))
                          (opaque
                            (apply (field_imm 18 (global CamlinternalOO!)) 0
                              j_init)))
                      include =a H)
                     (makeblock 0 a D E F H I j))))))
          (field_imm 0 M)) ⟫ }

- : int = 1
|}]
