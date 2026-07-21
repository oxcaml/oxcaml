(* Type-directed program generator. See DESIGN.md, "Generation".

      Structure follows the in-repo precedent of
   testsuite/tests/comprehensions/quickcheck_lists_arrays_haskell_python.ml: a typed AST
   ([Expr]), a type-directed generator that only produces well-typed terms, and a
   pretty-printer. A single program is emitted, carrying both annotations on the
   function under test (see [Sample]).

   Tier A (milestone 2): construction of one layer -- tuples, records (int and
   float), variants, lists, arrays -- plain vs [exclave_]-wrapped, plus
   mode-crossing immediates, glued together with integer/float arithmetic,
   comparisons, [let], and [if].

   Per DESIGN.md, the generator does not predict allocation (the oracles decide
   ground truth); [Mode] only steers the distribution: [Soundness] biases toward
   constructs the frontend is likely to accept (immediates, [exclave_]),
   [Completeness] toward non-allocating computation the frontend may
   conservatively reject (arithmetic, comparisons, branching). *)

module Mode = struct
  type t =
    | Soundness (* bias toward programs the frontend accepts *)
    | Completeness
  (* bias toward non-allocating but conservatively-rejected forms *)

  let to_string = function
    | Soundness -> "soundness"
    | Completeness -> "completeness"

  let of_string = function
    | "soundness" -> Some Soundness
    | "completeness" -> Some Completeness
    | _ -> None
end

module Sample = struct
  type t = { source : string }

  let to_string { source } = source
end

(* Shared type declarations emitted into both files. [r] is an
   all-immediate-field record, [fr] a float record (flat float layout), [v] a
   variant with an immediate and a boxed constructor. *)
let prelude =
  "type r = { a : int; b : int }\n\
   type fr = { fx : float; fy : float }\n\
   type v = A | B of int\n\n"

module Ty = struct
  type t =
    | Int
    | Float
    | Pair of t * t
    | List of t
    | Array of t
    | Record_int (* r *)
    | Record_float (* fr *)
    | Variant (* v *)

  let rec equal t1 t2 =
    match t1, t2 with
    | Int, Int | Float, Float -> true
    | Record_int, Record_int | Record_float, Record_float | Variant, Variant ->
      true
    | Pair (a1, b1), Pair (a2, b2) -> equal a1 a2 && equal b1 b2
    | List a, List b | Array a, Array b -> equal a b
    | ( ( Int | Float | Pair _ | List _ | Array _ | Record_int | Record_float
        | Variant ),
        _ ) ->
      false

  let rec to_string = function
    | Int -> "int"
    | Float -> "float"
    | Pair (a, b) -> "(" ^ to_string a ^ " * " ^ to_string b ^ ")"
    | List t -> to_string t ^ " list"
    | Array t -> to_string t ^ " array"
    | Record_int -> "r"
    | Record_float -> "fr"
    | Variant -> "v"

  (* Types whose construction could plausibly be wrapped in [exclave_]. This is
     distribution steering, not an allocation prediction. *)
  let exclave_candidate = function
    | Pair _ | List _ | Array _ | Record_int | Record_float | Variant -> true
    | Int | Float -> false
end

module Expr = struct
  type t =
    | Var of string
    | Int_lit of int
    | Float_lit of float
    | Add of t * t (* int + int *)
    | Fadd of t * t (* float +. float *)
    | Leq of t * t (* int <= int, used only as an [If] condition *)
    | If of t * t * t
    | Let of string * t * t
    | Pair of t * t
    | Nil
    | Cons of t * t
    | Array_lit of t list
    | Record_int of t * t (* { a; b } *)
    | Record_float of t * t (* { fx; fy } *)
    | Constr_a (* A -- immediate *)
    | Constr_b of t (* B e -- boxed *)
    | Exclave of t (* exclave_ e; only ever placed in tail position *)

  (* Fully parenthesized: ugly but unambiguous, and shrinking (milestone 3) is
     the readability story, not the printer. *)
  let rec to_string = function
    | Var x -> x
    | Int_lit n ->
      if n < 0 then "(" ^ string_of_int n ^ ")" else string_of_int n
    | Float_lit f ->
      let s = Printf.sprintf "%F" f in
      if f < 0.0 then "(" ^ s ^ ")" else s
    | Add (a, b) -> "(" ^ to_string a ^ " + " ^ to_string b ^ ")"
    | Fadd (a, b) -> "(" ^ to_string a ^ " +. " ^ to_string b ^ ")"
    | Leq (a, b) -> "(" ^ to_string a ^ " <= " ^ to_string b ^ ")"
    | If (c, t, e) ->
      "(if " ^ to_string c ^ " then " ^ to_string t ^ " else " ^ to_string e
      ^ ")"
    | Let (x, e1, e2) ->
      "(let " ^ x ^ " = " ^ to_string e1 ^ " in " ^ to_string e2 ^ ")"
    | Pair (a, b) -> "(" ^ to_string a ^ ", " ^ to_string b ^ ")"
    | Nil -> "[]"
    | Cons (h, t) -> "(" ^ to_string h ^ " :: " ^ to_string t ^ ")"
    | Array_lit es -> "[| " ^ String.concat "; " (List.map to_string es) ^ " |]"
    | Record_int (a, b) ->
      "{ a = " ^ to_string a ^ "; b = " ^ to_string b ^ " }"
    | Record_float (a, b) ->
      "{ fx = " ^ to_string a ^ "; fy = " ^ to_string b ^ " }"
    | Constr_a -> "A"
    | Constr_b e -> "(B " ^ to_string e ^ ")"
    | Exclave e -> "exclave_ " ^ to_string e
end

(* Generation context: PRNG state and a fresh-name supply. The typed environment
   is threaded functionally so bindings scope correctly. *)
type ctx =
  { rng : Random.State.t;
    mutable next_var : int;
    mode : Mode.t
  }

let fresh ctx =
  let n = ctx.next_var in
  ctx.next_var <- n + 1;
  Printf.sprintf "v%d" n

(* Pick among weighted alternatives; entries with weight 0 are disabled. *)
let choose ctx options =
  let options = List.filter (fun (w, _) -> w > 0) options in
  let total = List.fold_left (fun acc (w, _) -> acc + w) 0 options in
  let rec pick n = function
    | [] -> assert false
    | (w, f) :: rest -> if n < w then f () else pick (n - w) rest
  in
  pick (Random.State.int ctx.rng total) options

(* Component types for pairs / lists / arrays are leaf types only: Tier A is
   about one layer of construction, and it also guarantees the generator
   terminates (composite children generated at exhausted fuel bottom out in
   literals). *)
let small_ty ctx =
  choose ctx
    [(3, fun () -> Ty.Int); (2, fun () -> Ty.Float); (2, fun () -> Ty.Variant)]

let goal_ty ctx =
  let pair () = Ty.Pair (small_ty ctx, small_ty ctx) in
  match ctx.mode with
  | Mode.Soundness ->
    choose ctx
      [ (3, fun () -> Ty.Int);
        (2, fun () -> Ty.Float);
        (2, fun () -> Ty.Variant);
        2, pair;
        (2, fun () -> Ty.Record_float);
        (1, fun () -> Ty.Record_int);
        (1, fun () -> Ty.List (small_ty ctx));
        (1, fun () -> Ty.Array (small_ty ctx)) ]
  | Mode.Completeness ->
    choose ctx
      [ (6, fun () -> Ty.Int);
        (2, fun () -> Ty.Float);
        (2, fun () -> Ty.Variant);
        1, pair;
        (1, fun () -> Ty.List (small_ty ctx)) ]

let int_lit ctx = Expr.Int_lit (Random.State.int ctx.rng 13 - 3)

let float_lit ctx =
  Expr.Float_lit (float_of_int (Random.State.int ctx.rng 13 - 3) *. 0.5)

(* Generate a well-typed expression of type [ty] in environment [env]. Every
   recursive call decreases [fuel]; once fuel is exhausted only non-recursive
   productions (and one-layer construction with literal children) remain, so
   generation terminates. *)
let rec gen_expr ctx env ty fuel =
  let completeness =
    match ctx.mode with Mode.Completeness -> true | Mode.Soundness -> false
  in
  let deep w = if fuel > 0 then w else 0 in
  (* Productions available at every type. *)
  let vars = List.filter (fun (_, ty') -> Ty.equal ty' ty) env in
  let use_var () =
    let x, _ = List.nth vars (Random.State.int ctx.rng (List.length vars)) in
    Expr.Var x
  in
  let let_in () =
    let bound_ty = small_ty ctx in
    let x = fresh ctx in
    let e1 = gen_expr ctx env bound_ty (fuel - 1) in
    let e2 = gen_expr ctx ((x, bound_ty) :: env) ty (fuel - 1) in
    Expr.Let (x, e1, e2)
  in
  let if_ () =
    let c =
      Expr.Leq
        (gen_expr ctx env Ty.Int (fuel - 1), gen_expr ctx env Ty.Int (fuel - 1))
    in
    Expr.If (c, gen_expr ctx env ty (fuel - 1), gen_expr ctx env ty (fuel - 1))
  in
  let generic =
    [ (if vars = [] then 0 else 3), use_var;
      deep 1, let_in;
      deep (if completeness then 3 else 1), if_ ]
  in
  (* Productions specific to the goal type. *)
  let structural =
    match ty with
    | Ty.Int ->
      [ (2, fun () -> int_lit ctx);
        ( deep (if completeness then 4 else 2),
          fun () ->
            Expr.Add
              ( gen_expr ctx env Ty.Int (fuel - 1),
                gen_expr ctx env Ty.Int (fuel - 1) ) ) ]
    | Ty.Float ->
      [ (2, fun () -> float_lit ctx);
        ( deep 2,
          fun () ->
            Expr.Fadd
              ( gen_expr ctx env Ty.Float (fuel - 1),
                gen_expr ctx env Ty.Float (fuel - 1) ) ) ]
    | Ty.Pair (t1, t2) ->
      [ ( 4,
          fun () ->
            Expr.Pair
              (gen_expr ctx env t1 (fuel - 1), gen_expr ctx env t2 (fuel - 1))
        ) ]
    | Ty.List t ->
      [ (2, fun () -> Expr.Nil);
        ( deep 3,
          fun () ->
            Expr.Cons
              (gen_expr ctx env t (fuel - 1), gen_expr ctx env ty (fuel - 1)) )
      ]
    | Ty.Array t ->
      [ ( 3,
          fun () ->
            let n = Random.State.int ctx.rng 4 in
            Expr.Array_lit
              (List.init n (fun _ -> gen_expr ctx env t (fuel - 1))) ) ]
    | Ty.Record_int ->
      [ ( 4,
          fun () ->
            Expr.Record_int
              ( gen_expr ctx env Ty.Int (fuel - 1),
                gen_expr ctx env Ty.Int (fuel - 1) ) ) ]
    | Ty.Record_float ->
      [ ( 4,
          fun () ->
            Expr.Record_float
              ( gen_expr ctx env Ty.Float (fuel - 1),
                gen_expr ctx env Ty.Float (fuel - 1) ) ) ]
    | Ty.Variant ->
      [ (* Soundness mode favors the immediate constructor (mode crossing). *)
        ((if completeness then 2 else 4), fun () -> Expr.Constr_a);
        (deep 2, fun () -> Expr.Constr_b (gen_expr ctx env Ty.Int (fuel - 1)))
      ]
  in
  choose ctx (generic @ structural)

(* CR: [exclave_] is only placed at the very top of the body (a tail position by
   construction). Extend to tail positions of [if]/[let] when Tier B lands. *)
let maybe_exclave ctx ty body =
  match ctx.mode with
  | Mode.Soundness
    when Ty.exclave_candidate ty && Random.State.int ctx.rng 3 = 0 ->
    Expr.Exclave body
  | Mode.Soundness | Mode.Completeness -> body

let initial_fuel = 5

let generate ~mode ~seed =
  let ctx = { rng = Random.State.make [| seed |]; next_var = 0; mode } in
  let env = ["x0", Ty.Int; "x1", Ty.Float] in
  let ty = goal_ty ctx in
  let body = maybe_exclave ctx ty (gen_expr ctx env ty initial_fuel) in
    (* No return-type annotation is emitted since a plain [: ty] could interact with
     mode inference. Both annotations go on the same function: [@ noalloc_strict]
     drives the frontend check (and forces the body's allocations local), and
     [@zero_alloc strict] makes the backend check exactly that typed program. *)
  let header = Printf.sprintf "(* goal: %s *)\n" (Ty.to_string ty) in
  let body_str = Expr.to_string body in
  let source =
    Printf.sprintf
      "%s%slet[@zero_alloc strict] (f @ noalloc_strict) (x0 : int) (x1 : float) = %s\n"
      prelude
      header
      body_str
  in
  { Sample.source }
;;
