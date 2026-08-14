(* TEST
 include ocamlcommon;
 readonly_files = "has_z3.sh";
 script = "sh ${test_source_directory}/has_z3.sh";
 script;
 stdout = "${test_build_directory}/z3_backend.byte.output";
 stderr = "${test_build_directory}/z3_backend.byte.output";
 bytecode;
*)

(* The z3 backend against a real solver (skipped when none is installed; see
   has_z3.sh).  The stdout/stderr assignments above restore the redirection
   of the program's output: the [script] action defines both variables as a
   side effect, which would otherwise stop the [run] action from writing the
   .output file that check-program-output reads. *)

open Vox_logic
open Vox_backend

let z3_command =
  match Sys.getenv_opt "VOX_TEST_Z3" with
  | Some command -> command
  | None ->
    let pinned = "/j/office/app/z3/prod/4.8.5/install/bin/z3" in
    if Sys.file_exists pinned then pinned else "z3"

let config = { Config.timeout_seconds = Some 10.; z3_command = Some z3_command }

let origin label = { Origin.label; location = Location.none }

let hyp id term =
  { Obligation.id; term; origin = origin (Printf.sprintf "h%d" id) }

let obligation ?(signature = Signature.empty) ?(hypotheses = []) goal =
  { Obligation.signature; hypotheses; goal; location = Location.none }

let rec term_to_string : Term.t -> string = function
  | Var name -> name
  | Const (Bool b) -> Bool.to_string b
  | Const (Int digits) -> digits
  | Const (Bitvec { width; value }) -> Printf.sprintf "%Ld:bv%d" value width
  | Construct (constructor, []) -> constructor
  | Construct (constructor, arguments) ->
    Printf.sprintf "(%s %s)" constructor
      (String.concat " " (List.map term_to_string arguments))
  | App _ | Call _ | Ite _ | Select _ | Test _ -> "<term>"

let describe = function
  | Ok (Proved { unused_hypotheses = None }) -> "proved (no core)"
  | Ok (Proved { unused_hypotheses = Some [] }) ->
    "proved (no unused hypotheses found)"
  | Ok (Proved { unused_hypotheses = Some unused }) ->
    Printf.sprintf "proved (unused hypotheses: %s)"
      (String.concat ", " (List.map string_of_int unused))
  | Ok (Refuted None) -> "refuted"
  | Ok (Refuted (Some model)) ->
    Printf.sprintf "refuted, e.g. %s"
      (String.concat ", "
         (List.map
            (fun (name, value) -> name ^ " = " ^ term_to_string value)
            model))
  | Ok (Unknown Timeout) -> "unknown (timeout)"
  | Ok (Unknown (Incomplete reason)) -> "unknown (" ^ reason ^ ")"
  | Result.Error (Unavailable message) -> "unavailable: " ^ message
  | Result.Error (Error { cause; raw = _ }) -> "error: " ^ cause

let check title ?(config = config) o =
  print_endline (title ^ ": " ^ describe (Z3.discharge ~config o))

let ocaml_int n = Term.Const (Literal.ocaml_int n)

(* An OCaml int is a 63-bit vector, so [x >= 0 |- x + 1 >= 0] must NOT
   prove: it wraps at max_int.  With the wrap excluded it proves, and the
   unsat core exposes the hypothesis that was never needed. *)

let x = Term.Var "x"
let ge a b = Term.App (Bv_sge, [a; b])
let int_signature = { Signature.empty with variables = ["x", Sort.Bitvec 63] }

let () =
  check "x >= 0 |- x + 1 >= 0"
    (obligation ~signature:int_signature
       ~hypotheses:[hyp 0 (ge x (ocaml_int 0))]
       (ge (App (Bv_add, [x; ocaml_int 1])) (ocaml_int 0)));
  check "x >= 0, x < max_int, x < 100 |- x + 1 >= 1"
    (obligation ~signature:int_signature
       ~hypotheses:
         [ hyp 0 (ge x (ocaml_int 0))
         ; hyp 1 (App (Bv_slt, [x; ocaml_int max_int]))
         ; hyp 2 (App (Bv_slt, [x; ocaml_int 100]))
         ]
       (ge (App (Bv_add, [x; ocaml_int 1])) (ocaml_int 1)))

(* Refuted means the disprove query succeeded: the goal is false in every
   state satisfying the hypotheses, with a term-valued witness.  A goal
   that is merely not provable -- true in some states, false in others --
   stays Unknown; a prove-query model is not a refutation.  This is the
   discriminating test for the two-query protocol: rewiring discharge to
   one query flips it to "refuted". *)

let () =
  check "x > 5 |- x < 3"
    (obligation ~signature:int_signature
       ~hypotheses:[hyp 0 (App (Bv_sgt, [x; ocaml_int 5]))]
       (App (Bv_slt, [x; ocaml_int 3])));
  check "|- x > 5"
    (obligation ~signature:int_signature (App (Bv_sgt, [x; ocaml_int 5])))

(* A datatype counterexample carries its constructor tree. *)

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
  match Signature.instantiate [list_decl] ["list", [Datatype.Int]] with
  | Error message -> print_endline ("rejected: " ^ message)
  | Ok (datatypes, [list_int]) ->
    check "xs = Cons(42, Nil) |- is-Nil xs"
      (obligation
         ~signature:{ Signature.empty with datatypes
                    ; variables = ["xs", list_int] }
         ~hypotheses:
           [ hyp 0
               (App
                  ( Eq,
                    [ Var "xs"
                    ; Construct
                        ( "Cons<Int>",
                          [Const (Int "42"); Construct ("Nil<Int>", [])] )
                    ] ))
           ]
         (Test ("Nil<Int>", Var "xs")))
  | Ok _ -> assert false

(* A tiny budget turns a hard goal into Unknown Timeout, distinguishable
   from ordinary incompleteness.  Factoring a 62-bit semiprime (the factor
   bounds exclude wrap-around) is reliably interrupted mid-search. *)

let () =
  let semiprime = 2147483647 * 2147483629 in
  let between lo v hi =
    Term.App (And, [App (Bv_slt, [ocaml_int lo; v]); App (Bv_slt, [v; ocaml_int hi])])
  in
  check "factoring, briefly"
    ~config:{ config with timeout_seconds = Some 0.1 }
    (obligation
       ~signature:
         { Signature.empty with
           variables = ["p", Sort.Bitvec 63; "q", Sort.Bitvec 63]
         }
       ~hypotheses:
         [ hyp 0 (between 1 (Var "p") 4294967296)
         ; hyp 1 (between 1 (Var "q") 4294967296)
         ]
       (App
          ( Not,
            [App (Eq, [App (Bv_mul, [Var "p"; Var "q"]); ocaml_int semiprime])]
          )))

(* The uninterpreted view: the type is an opaque sort, and the laws (already
   instantiated by the translation -- the term language is quantifier-free)
   are all that relate member and insert. *)

let () =
  let set = Sort.Uninterpreted "int_set" in
  let member x s = Term.Call ("member", [x; s]) in
  let insert x s = Term.Call ("insert", [x; s]) in
  let s = Term.Var "s" in
  let x = Term.Var "x" in
  check "insert law at x |- member x (insert x s)"
    (obligation
       ~signature:
         { Signature.empty with
           sorts = ["int_set"]
         ; variables = ["s", set; "x", Sort.Int]
         ; functions =
             [ "member", [Sort.Int; set], Sort.Bool
             ; "insert", [Sort.Int; set], set
             ]
         }
       ~hypotheses:
         [ hyp 0
             (App
                ( Eq,
                  [ member x (insert x s)
                  ; App (Or, [App (Eq, [x; x]); member x s])
                  ] ))
         ]
       (member x (insert x s)))

(* An empty unsat core: the goal holds on its own, so every hypothesis is
   unused.  Distinguishes Some [] handling from "no claim". *)

let () =
  check "x > 0 |- x = x"
    (obligation ~signature:int_signature
       ~hypotheses:[hyp 0 (App (Bv_sgt, [x; ocaml_int 0]))]
       (App (Eq, [x; x])))

(* A refutation whose witness lives in an uninterpreted sort: opaque
   universe elements read back as Term.Var. *)

let () =
  let set = Sort.Uninterpreted "int_set" in
  let member x s = Term.Call ("member", [x; s]) in
  check "not (member x s) |- member x s"
    (obligation
       ~signature:
         { Signature.empty with
           sorts = ["int_set"]
         ; variables = ["s", set; "x", Sort.Int]
         ; functions = ["member", [Sort.Int; set], Sort.Bool]
         }
       ~hypotheses:[hyp 0 (App (Not, [member (Var "x") (Var "s")]))]
       (member (Var "x") (Var "s")))

(* A declared nullary function is an atom in the script; [f |- f] must
   prove. *)

let () =
  let f = Term.Call ("f", []) in
  check "f |- f"
    (obligation
       ~signature:{ Signature.empty with functions = ["f", [], Sort.Bool] }
       ~hypotheses:[hyp 0 f]
       f)

(* A variable named like a hypothesis label.  z3 4.8.5 drops the colliding
   named assertion with an (error ...) line and answers from what remains:
   the prove query answers sat, the disprove query unsat, and a vacuously
   provable obligation comes back "refuted".  The obligation must instead
   be rejected. *)

let () =
  check "h0 : bool, hypothesis false |- false"
    (obligation
       ~signature:{ Signature.empty with variables = ["h0", Sort.Bool] }
       ~hypotheses:[hyp 0 (Const (Bool false))]
       (Const (Bool false)))

(* A signature sort named like a builtin.  z3 4.8.5 reports the redeclaration
   with an (error ...) line before the status and then uses the builtin Int,
   so an obligation about an abstract sort is answered with integer
   semantics.  The error must surface as a failure, not be swallowed. *)

let () =
  check "x : (an abstract sort named Int) |- x >= x"
    (obligation
       ~signature:
         { Signature.empty with
           sorts = ["Int"]
         ; variables = ["a", Sort.Uninterpreted "Int"]
         }
       (App (Ge, [Var "a"; Var "a"])))

(* A script z3 rejects must be a failure, not a verdict.  The renderer does
   not sort-check on purpose, so an ill-sorted hypothesis reaches z3, which
   drops it with an (error ...) line and answers from what remains: without
   the pre-status error check this contradictory goal would be reported
   "refuted" on the strength of a hypothesis that was never asserted. *)

let () =
  check "n = b (ill-sorted) |- n > 0 && n < 0"
    (obligation
       ~signature:
         { Signature.empty with
           variables = ["n", Sort.Int; "b", Sort.Bool]
         }
       ~hypotheses:[hyp 0 (App (Eq, [Var "n"; Var "b"]))]
       (App
          ( And,
            [ App (Gt, [Var "n"; Const (Int "0")])
            ; App (Lt, [Var "n"; Const (Int "0")])
            ] )))

(* A wedged solver process is killed by the wall clock (exit 124 from
   timeout(1)) and reported as a timeout. *)

let () =
  check "wedged solver"
    ~config:
      { Config.timeout_seconds = Some 0.05
      ; z3_command = Some "sh -c 'sleep 30' wedged"
      }
    (obligation (Const (Bool true)))
