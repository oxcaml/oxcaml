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

let int_ops =
  let n = Term.Var "n" in
  Term.App
    ( And,
      [ App (Eq, [App (Neg, [n]); App (Add, [n; n])])
      ; App (Eq, [App (Sub, [n; n]); App (Mul, [n; n])])
      ; App (Eq, [App (Div, [n; n]); App (Mod, [n; n])])
      ; App (Lt, [n; n]); App (Le, [n; n]); App (Gt, [n; n]); App (Ge, [n; n])
      ; App (Distinct, [n; n])
      ] )

let bitvec_ops =
  let m = Term.Var "m" in
  let eq x y = Term.App (Eq, [x; y]) in
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

let () =
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
val int_ops : Vox_logic.Term.t =
  Term.App (Op.And,
   [Term.App (Op.Eq,
     [Term.App (Op.Neg, [Term.Var "n"]);
      Term.App (Op.Add, [Term.Var "n"; Term.Var "n"])]);
    Term.App (Op.Eq,
     [Term.App (Op.Sub, [Term.Var "n"; Term.Var "n"]);
      Term.App (Op.Mul, [Term.Var "n"; Term.Var "n"])]);
    Term.App (Op.Eq,
     [Term.App (Op.Div, [Term.Var "n"; Term.Var "n"]);
      Term.App (Op.Mod, [Term.Var "n"; Term.Var "n"])]);
    Term.App (Op.Lt, [Term.Var "n"; Term.Var "n"]);
    Term.App (Op.Le, [Term.Var "n"; Term.Var "n"]);
    Term.App (Op.Gt, [Term.Var "n"; Term.Var "n"]);
    Term.App (Op.Ge, [Term.Var "n"; Term.Var "n"]);
    Term.App (Op.Distinct, [Term.Var "n"; Term.Var "n"])])
val bitvec_ops : Vox_logic.Term.t =
  Term.App (Op.And,
   [Term.App (Op.Eq,
     [Term.App (Op.Bv_neg, [Term.Var "m"]);
      Term.App (Op.Bv_add, [Term.Var "m"; Term.Var "m"])]);
    Term.App (Op.Eq,
     [Term.App (Op.Bv_sub, [Term.Var "m"; Term.Var "m"]);
      Term.App (Op.Bv_mul, [Term.Var "m"; Term.Var "m"])]);
    Term.App (Op.Eq,
     [Term.App (Op.Bv_sdiv, [Term.Var "m"; Term.Var "m"]);
      Term.App (Op.Bv_srem, [Term.Var "m"; Term.Var "m"])]);
    Term.App (Op.Eq,
     [Term.App (Op.Bv_not, [Term.Var "m"]);
      Term.App (Op.Bv_and, [Term.Var "m"; Term.Var "m"])]);
    Term.App (Op.Eq,
     [Term.App (Op.Bv_or, [Term.Var "m"; Term.Var "m"]);
      Term.App (Op.Bv_xor, [Term.Var "m"; Term.Var "m"])]);
    Term.App (Op.Eq,
     [Term.App (Op.Bv_shl, [Term.Var "m"; Term.Var "m"]);
      Term.App (Op.Bv_lshr, [Term.Var "m"; Term.Var "m"])]);
    Term.App (Op.Eq,
     [Term.App (Op.Bv_ashr, [Term.Var "m"; Term.Var "m"]); Term.Var "m"]);
    Term.App (Op.Bv_slt, [Term.Var "m"; Term.Var "m"]);
    Term.App (Op.Bv_sle, [Term.Var "m"; Term.Var "m"]);
    Term.App (Op.Bv_sgt, [Term.Var "m"; Term.Var "m"]);
    Term.App (Op.Bv_sge, [Term.Var "m"; Term.Var "m"])])
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

let instantiate decls roots k =
  match Signature.instantiate decls roots with
  | Error message -> Format.printf "rejected: %s@." message
  | Ok (datatypes, sorts) -> k datatypes sorts

let () =
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
val list_decl : Vox_logic.Datatype.decl =
  {Datatype.decl_name = "list"; params = ["a"];
   constructors =
    [{Datatype.constructor_name = "Nil"; fields = []};
     {Datatype.constructor_name = "Cons";
      fields =
       [("head", Datatype.Param "a");
        ("tail", Datatype.Apply ("list", [Datatype.Param "a"]))]}]}
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

let () =
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
val tree_decls : Vox_logic.Datatype.decl list =
  [{Datatype.decl_name = "pair"; params = [];
    constructors =
     [{Datatype.constructor_name = "Pair";
       fields = [("fst", Datatype.Int); ("snd", Datatype.Bool)]}]};
   {Datatype.decl_name = "tree"; params = [];
    constructors =
     [{Datatype.constructor_name = "Leaf";
       fields = [("label", Datatype.Apply ("pair", []))]};
      {Datatype.constructor_name = "Node";
       fields = [("children", Datatype.Apply ("forest", []))]}]};
   {Datatype.decl_name = "forest"; params = [];
    constructors =
     [{Datatype.constructor_name = "Empty"; fields = []};
      {Datatype.constructor_name = "Grow";
       fields =
        [("first", Datatype.Apply ("tree", []));
         ("rest", Datatype.Apply ("forest", []))]}]}]
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
ill-formed: duplicate hypothesis id
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
