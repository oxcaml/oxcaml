(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* The SMT-LIB renderer, exercised on hand-built obligations.  The same
   renderer feeds the z3 backend, so these baselines are the bytes z3
   receives. *)

open Vox_logic

let origin label = { Origin.label; location = Location.none }

let hyp id term =
  { Obligation.id; term; origin = origin (Printf.sprintf "h%d" id) }

let obligation ?(signature = Signature.empty) ?(hypotheses = []) goal =
  { Obligation.signature; hypotheses; goal; location = Location.none }

let render ?timeout_ms query o =
  match Vox_smtlib.render ?timeout_ms query o with
  | Ok script -> Format.printf "%s" script
  | Error message -> Format.printf "ill-formed: %s@." message

[%%expect{|
val origin : string -> Vox_logic.Origin.t = <fun>
val hyp : int -> Vox_logic.Term.t -> Vox_logic.Obligation.hypothesis = <fun>
val obligation :
  ?signature:Vox_logic.Signature.t ->
  ?hypotheses:Vox_logic.Obligation.hypothesis list ->
  Vox_logic.Term.t -> Vox_logic.Obligation.t = <fun>
val render :
  ?timeout_ms:int -> Vox_smtlib.query -> Vox_logic.Obligation.t -> unit =
  <fun>
|}]

(* Every sort, rendered through variable declarations; every literal form.
   [Bitvec 63] is OCaml's [int]. *)

let () =
  render Prove
    (obligation
       ~signature:
         { Signature.empty with
           sorts = ["opaque"]
         ; variables =
             [ "b", Sort.Bool
             ; "n", Sort.Int
             ; "m", Sort.Bitvec 63
             ; "u", Sort.Uninterpreted "opaque"
             ]
         }
       ~hypotheses:
         [ hyp 0 (App (Eq, [Var "b"; Const (Bool false)]))
         ; hyp 1 (App (Eq, [Var "n"; Const (Int "-12345678901234567890123")]))
         ; hyp 2 (App (Eq, [Var "m"; Const (Literal.ocaml_int (-1))]))
         ]
       (App (Eq, [Var "u"; Var "u"])))

[%%expect{|
(set-option :produce-unsat-cores true)
(declare-sort opaque 0)
(declare-const b Bool)
(declare-const n Int)
(declare-const m (_ BitVec 63))
(declare-const u opaque)
(assert (! (= b false) :named h0))
(assert (! (= n (- 12345678901234567890123)) :named h1))
(assert (! (= m (_ bv9223372036854775807 63)) :named h2))
(assert (not (= u u)))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
|}]

(* Every operator.  Interpreted operators only: OCaml semantics (division by
   zero, shift ranges) is the translation's job. *)

let () =
  let n = Term.Var "n" in
  let int_ops =
    Term.App
    ( And,
      [ App (Eq, [App (Neg, [n]); App (Add, [n; n])])
      ; App (Eq, [App (Sub, [n; n]); App (Mul, [n; n])])
      ; App (Eq, [App (Div, [n; n]); App (Mod, [n; n])])
      ; App (Lt, [n; n]); App (Le, [n; n]); App (Gt, [n; n]); App (Ge, [n; n])
      ; App (Distinct, [n; n])
      ] )
  in
  let m = Term.Var "m" in
  let eq x y = Term.App (Eq, [x; y]) in
  let bitvec_ops =
    Term.App
    ( And,
      [ eq (App (Bv_neg, [m])) (App (Bv_add, [m; m]))
      ; eq (App (Bv_sub, [m; m])) (App (Bv_mul, [m; m]))
      ; eq (App (Bv_sdiv, [m; m])) (App (Bv_srem, [m; m]))
      ; eq (App (Bv_not, [m])) (App (Bv_and, [m; m]))
      ; eq (App (Bv_or, [m; m])) (App (Bv_xor, [m; m]))
      ; eq (App (Bv_shl, [m; m])) (App (Bv_lshr, [m; m]))
      ; eq (App (Bv_ashr, [m; m])) m
      ; App (Bv_slt, [m; m]); App (Bv_sle, [m; m])
      ; App (Bv_sgt, [m; m]); App (Bv_sge, [m; m])
      ] )
  in
  render Prove
    (obligation
       ~signature:
         { Signature.empty with
           variables = ["b", Sort.Bool; "n", Sort.Int; "m", Sort.Bitvec 63]
         ; functions = ["f", [Sort.Int; Sort.Bool], Sort.Int]
         }
       ~hypotheses:[hyp 0 int_ops; hyp 1 bitvec_ops]
       (App
          ( Or,
            [ App (Not, [Var "b"])
            ; App (Implies, [Var "b"; Var "b"])
            ; App
                ( Eq,
                  [ Term.Ite (Var "b", Var "n", Call ("f", [Var "n"; Var "b"]))
                  ; Var "n"
                  ] )
            ] )))

[%%expect{|
(set-option :produce-unsat-cores true)
(declare-const b Bool)
(declare-const n Int)
(declare-const m (_ BitVec 63))
(declare-fun f (Int Bool) Int)
(assert (! (and (= (- n) (+ n n)) (= (- n n) (* n n)) (= (div n n) (mod n n)) (< n n) (<= n n) (> n n) (>= n n) (distinct n n)) :named h0))
(assert (! (and (= (bvneg m) (bvadd m m)) (= (bvsub m m) (bvmul m m)) (= (bvsdiv m m) (bvsrem m m)) (= (bvnot m) (bvand m m)) (= (bvor m m) (bvxor m m)) (= (bvshl m m) (bvlshr m m)) (= (bvashr m m) m) (bvslt m m) (bvsle m m) (bvsgt m m) (bvsge m m)) :named h1))
(assert (not (or (not b) (=> b b) (= (ite b n (f n b)) n))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
|}]

(* The disprove query asserts the goal itself, un-negated, and asks for no
   core and no model.  Polarity is the whole protocol; see the design doc. *)

let () =
  render Disprove
    (obligation
       ~signature:{ Signature.empty with variables = ["n", Sort.Int] }
       ~hypotheses:[hyp 0 (App (Ge, [Var "n"; Const (Int "0")]))]
       (App (Gt, [Var "n"; Const (Int "0")])))

[%%expect{|
(declare-const n Int)
(assert (>= n 0))
(assert (> n 0))
(check-sat)
(get-info :reason-unknown)
|}]

(* The timeout budget appears in the script itself. *)

let () =
  render ~timeout_ms:250 Disprove (obligation (Const (Bool true)))

[%%expect{|
(set-option :timeout 250)
(assert true)
(check-sat)
(get-info :reason-unknown)
|}]

(* Datatypes.  A parametric declaration is monomorphised: one ground
   instance per instantiation, constructors and selectors suffixed the same
   way.  Instances at different arguments coexist. *)

let instantiate decls roots k =
  match Signature.instantiate decls roots with
  | Error message -> Format.printf "rejected: %s@." message
  | Ok (datatypes, sorts) -> k datatypes sorts

let () =
  let list_decl : Datatype.decl =
    { decl_name = "list"
    ; params = ["a"]
    ; constructors =
        [ { constructor_name = "Nil"; fields = [] }
        ; { constructor_name = "Cons"
          ; fields = ["head", Param "a"; "tail", Apply ("list", [Param "a"])]
          }
        ]
    }
  in
  instantiate [list_decl]
    ["list", [Datatype.Int]; "list", [Apply ("list", [Datatype.Int])]]
    (fun datatypes sorts ->
       render Prove
         (obligation
            ~signature:
              { Signature.empty with
                datatypes
              ; variables =
                  ["xs", List.nth sorts 0; "xss", List.nth sorts 1]
              }
            ~hypotheses:
              [ hyp 0 (Test ("Cons<Int>", Var "xs"))
              ; hyp 1
                  (App
                     ( Eq,
                       [ Var "xss"
                       ; Construct
                           ( "Cons<list<Int>>",
                             [Var "xs"; Construct ("Nil<list<Int>>", [])] )
                       ] ))
              ]
            (App
               ( Eq,
                 [ Select ("Cons<Int>", 0, Var "xs")
                 ; Select ("Cons<Int>", 0, Select ("Cons<Int>", 1, Var "xs"))
                 ] ))))

[%%expect{|
val instantiate :
  Vox_logic.Datatype.decl list ->
  (string * Vox_logic.Datatype.ty list) list ->
  (Vox_logic.Signature.datatype list -> Vox_logic.Sort.t list -> unit) ->
  unit = <fun>
(set-option :produce-unsat-cores true)
(declare-datatypes ((list<Int> 0)) (
  ((Nil<Int>) (Cons<Int> (head<Int> Int) (tail<Int> list<Int>)))))
(declare-datatypes ((list<list<Int>> 0)) (
  ((Nil<list<Int>>) (Cons<list<Int>> (head<list<Int>> list<Int>) (tail<list<Int>> list<list<Int>>)))))
(declare-const xs list<Int>)
(declare-const xss list<list<Int>>)
(assert (! ((_ is Cons<Int>) xs) :named h0))
(assert (! (= xss (Cons<list<Int>> xs Nil<list<Int>>)) :named h1))
(assert (not (= (head<Int> xs) (head<Int> (tail<Int> xs)))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
|}]

(* A mutually recursive group lands in one [declare-datatypes]; a group its
   fields reference is declared before it. *)

let () =
  let tree_decls : Datatype.decl list =
    [ { decl_name = "pair"
      ; params = []
      ; constructors =
          [ { constructor_name = "Pair"
            ; fields = ["fst", Datatype.Int; "snd", Datatype.Bool]
            }
          ]
      }
    ; { decl_name = "tree"
      ; params = []
      ; constructors =
          [ { constructor_name = "Leaf"; fields = ["label", Apply ("pair", [])] }
          ; { constructor_name = "Node"; fields = ["children", Apply ("forest", [])] }
          ]
      }
    ; { decl_name = "forest"
      ; params = []
      ; constructors =
          [ { constructor_name = "Empty"; fields = [] }
          ; { constructor_name = "Grow"
            ; fields = ["first", Apply ("tree", []); "rest", Apply ("forest", [])]
            }
          ]
      }
    ]
  in
  instantiate tree_decls ["tree", []] (fun datatypes sorts ->
    render Prove
      (obligation
         ~signature:
           { Signature.empty with
             datatypes
           ; variables = ["t", List.nth sorts 0]
           }
         (Test ("Leaf", Var "t"))))

[%%expect{|
(set-option :produce-unsat-cores true)
(declare-datatypes ((pair 0)) (
  ((Pair (fst Int) (snd Bool)))))
(declare-datatypes ((forest 0) (tree 0)) (
  ((Empty) (Grow (first tree) (rest forest)))
  ((Leaf (label pair)) (Node (children forest)))))
(declare-const t tree)
(assert (not ((_ is Leaf) t)))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
|}]

(* An abstract type is a [declare-sort] plus uninterpreted functions; the
   laws are all that relate them.  (This is the client's view of [int_set]:
   inside the defining module the same type would be a datatype.) *)

let () =
  render Prove
    (obligation
       ~signature:
         { Signature.empty with
           sorts = ["int_set"]
         ; variables = ["s", Sort.Uninterpreted "int_set"; "x", Sort.Bitvec 63]
         ; functions =
             [ "member", [Sort.Bitvec 63; Sort.Uninterpreted "int_set"], Sort.Bool
             ; "insert",
               [Sort.Bitvec 63; Sort.Uninterpreted "int_set"],
               Sort.Uninterpreted "int_set"
             ]
         }
       (Call ("member", [Var "x"; Call ("insert", [Var "x"; Var "s"])])))

[%%expect{|
(set-option :produce-unsat-cores true)
(declare-sort int_set 0)
(declare-const s int_set)
(declare-const x (_ BitVec 63))
(declare-fun member ((_ BitVec 63) int_set) Bool)
(declare-fun insert ((_ BitVec 63) int_set) int_set)
(assert (not (member x (insert x s))))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
|}]

(* Deliberate rejections.  Non-regular recursion would need infinitely many
   instances; SMT datatypes cannot store functions. *)

let () =
  instantiate
    [ { decl_name = "nest"
      ; params = ["a"]
      ; constructors =
          [ { constructor_name = "One"; fields = ["it", Param "a"] }
          ; { constructor_name = "Deeper"
            ; fields =
                ["inside", Apply ("nest", [Apply ("pair", [Param "a"])])]
            }
          ]
      }
    ; { decl_name = "pair"
      ; params = ["a"]
      ; constructors =
          [ { constructor_name = "P"
            ; fields = ["l", Param "a"; "r", Param "a"]
            }
          ]
      }
    ]
    ["nest", [Datatype.Int]]
    (fun _ _ -> Format.printf "accepted@.")

[%%expect{|
rejected: non-regular recursive datatype nest is not supported
|}]

let () =
  instantiate
    [ { decl_name = "wrap"
      ; params = []
      ; constructors =
          [ { constructor_name = "Wrap"
            ; fields = ["run", Datatype.Arrow (Datatype.Int, Datatype.Bool)]
            }
          ]
      }
    ]
    ["wrap", []]
    (fun _ _ -> Format.printf "accepted@.")

[%%expect{|
rejected: function-valued datatype fields are not supported
|}]

(* Well-formedness: what the signature does not declare, a term cannot use.
   Ill-sortedness of a variable is unrepresentable (the signature gives the
   sort, occurrences carry none); these are the mistakes that remain. *)

let () = render Prove (obligation (Var "ghost"))

[%%expect{|
ill-formed: undeclared variable ghost
|}]

let () = render Prove (obligation (Call ("f", [])))

[%%expect{|
ill-formed: undeclared function f
|}]

let () =
  render Prove
    (obligation
       ~signature:{ Signature.empty with variables = ["n", Sort.Int] }
       (App (Not, [Var "n"; Var "n"])))

[%%expect{|
ill-formed: operator not expects 1 argument(s) but was given 2
|}]

let () = render Prove (obligation (Const (Int "12three4")))

[%%expect{|
ill-formed: malformed integer literal "12three4"
|}]

let () = render Prove (obligation (Test ("Cons", Var "xs")))

[%%expect{|
ill-formed: undeclared constructor Cons
|}]

let () =
  render Prove
    (obligation
       ~hypotheses:
         [hyp 3 (Const (Bool true)); hyp 3 (Const (Bool true))]
       (Const (Bool true)))

[%%expect{|
ill-formed: duplicate symbol h3 (as hypothesis label)
|}]

(* A symbol SMT-LIB cannot spell simply is quoted; one it cannot spell at
   all is an error. *)

let () =
  render Prove
    (obligation
       ~signature:{ Signature.empty with variables = ["poison,name", Sort.Bool] }
       (Var "poison,name"))

[%%expect{|
(set-option :produce-unsat-cores true)
(declare-const |poison,name| Bool)
(assert (not |poison,name|))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
|}]

let () =
  render Prove
    (obligation
       ~signature:{ Signature.empty with variables = ["bad|bar", Sort.Bool] }
       (Var "bad|bar"))

[%%expect{|
ill-formed: symbol "bad|bar" cannot be represented in SMT-LIB
|}]

(* Renderer-generated hypothesis labels live in the same solver namespace as
   the signature's symbols.  A variable named like a label must therefore be
   ill-formed: z3 4.8.5 otherwise drops the colliding assertion with an
   (error ...) line and answers from what remains, which can invert a
   verdict (see the z3 test). *)

let () =
  render Prove
    (obligation
       ~signature:{ Signature.empty with variables = ["h0", Sort.Bool] }
       ~hypotheses:[hyp 0 (Const (Bool false))]
       (Const (Bool false)))

[%%expect{|
ill-formed: duplicate symbol h0 (as hypothesis label)
|}]

(* A negative hypothesis id would render as a label the core reader cannot
   read back. *)

let () =
  render Prove
    (obligation ~hypotheses:[hyp (-1) (Const (Bool true))] (Const (Bool true)))

[%%expect{|
ill-formed: negative hypothesis id -1
|}]

(* A declared nullary function application is an atom in SMT-LIB, not [(f)]. *)

let () =
  render Prove
    (obligation
       ~signature:{ Signature.empty with functions = ["f", [], Sort.Bool] }
       (Call ("f", [])))

[%%expect{|
(set-option :produce-unsat-cores true)
(declare-fun f () Bool)
(assert (not f))
(check-sat)
(get-unsat-core)
(get-model)
(get-info :reason-unknown)
|}]

(* Field indices out of range on either side. *)

let () =
  instantiate
    [ { decl_name = "pair"
      ; params = []
      ; constructors =
          [ { constructor_name = "P"
            ; fields = ["l", Datatype.Int; "r", Datatype.Int]
            }
          ]
      }
    ]
    ["pair", []]
    (fun datatypes sorts ->
       let signature =
         { Signature.empty with datatypes
         ; variables = ["p", List.nth sorts 0] }
       in
       render Prove
         (obligation ~signature
            (App (Eq, [Select ("P", 2, Var "p"); Const (Int "0")])));
       render Prove
         (obligation ~signature
            (App (Eq, [Select ("P", -1, Var "p"); Const (Int "0")]))))

[%%expect{|
ill-formed: constructor P has no field 2
ill-formed: constructor P has no field -1
|}]

(* Duplicate declarations are ill-formed, in each namespace. *)

let () =
  render Prove
    (obligation
       ~signature:
         { Signature.empty with variables = ["x", Sort.Bool; "x", Sort.Int] }
       (Var "x"))

[%%expect{|
ill-formed: duplicate symbol x (as variable)
|}]

(* An integer literal with leading zeros is not an SMT-LIB numeral (z3
   happens to accept it; other solvers need not). *)

let () = render Prove (obligation (App (Eq, [Const (Int "007"); Const (Int "7")])))

[%%expect{|
ill-formed: integer literal "007" has leading zeros
|}]

(* Two different instantiations may not share a mangled instance name: a
   sort literally named "Int" would otherwise silently alias the instance
   at the builtin Int, identifying two different types. *)

let () =
  instantiate
    [ { decl_name = "box"
      ; params = ["a"]
      ; constructors =
          [ { constructor_name = "Box"; fields = ["it", Param "a"] } ]
      }
    ]
    [ "box", [Datatype.Int]; "box", [Datatype.Uninterpreted "Int"] ]
    (fun datatypes _ ->
       List.iter
         (fun (datatype : Signature.datatype) ->
            Format.printf "instance: %s@." datatype.datatype_name)
         datatypes)

[%%expect{|
rejected: two distinct instantiations produce the same instance name box<Int>
|}]

(* A symbol that spells an operator the renderer itself emits.  Quoting does
   not help: z3 4.8.5 treats |not| and not as the same symbol (a probe shows
   a declared |not| shadowing the boolean operator), so these names must be
   rejected outright.  Likewise the builtin sort names. *)

let () =
  render Prove
    (obligation
       ~signature:{ Signature.empty with variables = ["not", Sort.Bool] }
       (Var "not"))

[%%expect{|
ill-formed: symbol not collides with an SMT-LIB builtin
|}]

let () =
  render Prove
    (obligation
       ~signature:
         { Signature.empty with
           sorts = ["Int"]
         ; variables = ["a", Sort.Uninterpreted "Int"]
         }
       (App (Eq, [Var "a"; Var "a"])))

[%%expect{|
ill-formed: sort name Int collides with an SMT-LIB builtin sort
|}]

(* Bitvector literals at the width boundaries: 1, 63, and 64 bits (the
   64-bit case exercises the unsigned print without masking). *)

let () =
  render Disprove
    (obligation
       ~signature:
         { Signature.empty with
           variables = ["t", Sort.Bitvec 1; "w", Sort.Bitvec 64]
         }
       (App
          ( And,
            [ App (Eq, [Var "t"; Const (Bitvec { width = 1; value = -1L })])
            ; App (Eq, [Var "w"; Const (Bitvec { width = 64; value = -1L })])
            ] )))

[%%expect{|
(declare-const t (_ BitVec 1))
(declare-const w (_ BitVec 64))
(assert (and (= t (_ bv1 1)) (= w (_ bv18446744073709551615 64))))
(check-sat)
(get-info :reason-unknown)
|}]
