(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

open Ppxlib
open Ast_builder.Default

(* let ocaml_version = Versions.ocaml_408 *)

module Cset = Sedlex_cset

(* Decision tree for partitions *)

let default_loc = Location.none

type decision_tree =
  | Lte of int * decision_tree * decision_tree
  | Table of int * int array
  | Return of int

let rec simplify_decision_tree (x : decision_tree) =
  match x with
    | Table _ | Return _ -> x
    | Lte (_, (Return a as l), Return b) when a = b -> l
    | Lte (i, l, r) -> (
        let l = simplify_decision_tree l in
        let r = simplify_decision_tree r in
        match (l, r) with
          | Return a, Return b when a = b -> l
          | _ -> Lte (i, l, r))

let decision l =
  let l = List.map (fun (a, b, i) -> (a, b, Return i)) l in
  let rec merge2 = function
    | (a1, b1, d1) :: (a2, b2, d2) :: rest ->
        let x = if b1 + 1 = a2 then d2 else Lte (a2 - 1, Return (-1), d2) in
        (a1, b2, Lte (b1, d1, x)) :: merge2 rest
    | rest -> rest
  in
  let rec aux = function
    | [(a, b, d)] -> Lte (a - 1, Return (-1), Lte (b, d, Return (-1)))
    | [] -> Return (-1)
    | l -> aux (merge2 l)
  in
  aux l

let limit = 8192

let decision_table l =
  let rec aux m accu = function
    | ((a, b, i) as x) :: rem when b < limit && i < 255 ->
        aux (min a m) (x :: accu) rem
    | rem -> (m, accu, rem)
  in
  let min, table, rest = aux max_int [] l in
  match table with
    | [] -> decision l
    | [(min, max, i)] ->
        Lte (min - 1, Return (-1), Lte (max, Return i, decision rest))
    | (_, max, _) :: _ ->
        let arr = Array.make (max - min + 1) 0 in
        let set (a, b, i) =
          for j = a to b do
            arr.(j - min) <- i + 1
          done
        in
        List.iter set table;
        Lte (min - 1, Return (-1), Lte (max, Table (min, arr), decision rest))

let rec simplify min max = function
  | Lte (i, yes, no) ->
      if i >= max then simplify min max yes
      else if i < min then simplify min max no
      else Lte (i, simplify min i yes, simplify (i + 1) max no)
  | x -> x

let segments_of_partition p =
  let seg = ref [] in
  Array.iteri
    (fun i c ->
      List.iter
        (fun (a, b) -> seg := (a, b, i) :: !seg)
        (c : Sedlex_cset.t :> (int * int) list))
    p;
  List.sort (fun (a1, _, _) (a2, _, _) -> compare a1 a2) !seg

let decision_table p =
  simplify (-1) Cset.max_code (decision_table (segments_of_partition p))

(* Helpers to build AST *)

let appfun s l =
  let loc = default_loc in
  eapply ~loc (evar ~loc s) l

let glb_value name def =
  let loc = default_loc in
  pstr_value ~loc Nonrecursive
    [value_binding ~loc ~pat:(pvar ~loc name) ~expr:def]

(* Named regexps *)

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

let builtin_regexps =
  List.fold_left
    (fun acc (n, c) -> StringMap.add n (Sedlex.chars c) acc)
    StringMap.empty
    ([
       ("any", Cset.any);
       ("eof", Cset.eof);
       ("xml_letter", Xml.letter);
       ("xml_digit", Xml.digit);
       ("xml_extender", Xml.extender);
       ("xml_base_char", Xml.base_char);
       ("xml_ideographic", Xml.ideographic);
       ("xml_combining_char", Xml.combining_char);
       ("xml_blank", Xml.blank);
       ("tr8876_ident_char", Iso.tr8876_ident_char);
     ]
    @ Unicode.Categories.list @ Unicode.Properties.list)

(* Tables (indexed mapping: codepoint -> next state) *)

let tables = Hashtbl.create 31
let table_counter = ref 0
let get_tables () = Hashtbl.fold (fun key x accu -> (x, key) :: accu) tables []

let table_name x =
  try Hashtbl.find tables x
  with Not_found ->
    incr table_counter;
    let s = Printf.sprintf "__sedlex_table_%i" !table_counter in
    Hashtbl.add tables x s;
    s

let table (name, v) =
  let n = Array.length v in
  let s = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set s i (Char.chr v.(i))
  done;
  glb_value name (estring ~loc:default_loc (Bytes.to_string s))

(* Partition (function: codepoint -> next state) *)

let partitions = Hashtbl.create 31
let partition_counter = ref 0

let get_partitions () =
  Hashtbl.fold (fun key x accu -> (x, key) :: accu) partitions []

let partition_name x =
  try Hashtbl.find partitions x
  with Not_found ->
    incr partition_counter;
    let s = Printf.sprintf "__sedlex_partition_%i" !partition_counter in
    Hashtbl.add partitions x s;
    s

(* We duplicate the body for the EOF (-1) case rather than creating
   an interior utility function. *)
let partition (name, p) =
  let loc = default_loc in
  let rec gen_tree = function
    | Lte (i, yes, no) ->
        [%expr
          if c <= [%e eint ~loc i] then [%e gen_tree yes] else [%e gen_tree no]]
    | Return i -> eint ~loc:default_loc i
    | Table (offset, t) ->
        let c =
          if offset = 0 then [%expr c] else [%expr c - [%e eint ~loc offset]]
        in
        [%expr
          Char.code (String.unsafe_get [%e evar ~loc (table_name t)] [%e c]) - 1]
  in
  let body = gen_tree (simplify_decision_tree (decision_table p)) in
  glb_value name
    [%expr
      fun c ->
        let open! Stdlib in
        [%e body]]

(* Code generation for the automata *)

let best_final final =
  let fin = ref None in
  for i = Array.length final - 1 downto 0 do
    if final.(i) then fin := Some i
  done;
  !fin

let state_fun state = Printf.sprintf "__sedlex_state_%i" state

let call_state lexbuf auto state =
  let trans, final = auto.(state) in
  if Array.length trans = 0 then (
    match best_final final with
      | Some i -> eint ~loc:default_loc i
      | None -> assert false)
  else appfun (state_fun state) [lexbuf]

let gen_state (lexbuf_name, lexbuf) auto i (trans, final) =
  let loc = default_loc in
  let partition = Array.map fst trans in
  let cases =
    Array.mapi
      (fun i (_, j) ->
        case ~lhs:(pint ~loc i) ~guard:None ~rhs:(call_state lexbuf auto j))
      trans
  in
  let cases = Array.to_list cases in
  let body () =
    pexp_match ~loc
      (appfun (partition_name partition)
         [[%expr Sedlexing.__private__next_int [%e lexbuf]]])
      (cases
      @ [
          case
            ~lhs:[%pat? _]
            ~guard:None
            ~rhs:[%expr Sedlexing.backtrack [%e lexbuf]];
        ])
  in
  let ret body =
    let lhs = pvar ~loc:lexbuf.pexp_loc lexbuf_name in
    [
      value_binding ~loc
        ~pat:(pvar ~loc (state_fun i))
        ~expr:(
          Ppxlib_jane.Ast_builder.Default.add_fun_param ~loc Nolabel None lhs body);
    ]
  in
  match best_final final with
    | None -> ret (body ())
    | Some _ when Array.length trans = 0 -> []
    | Some i ->
        ret
          [%expr
            Sedlexing.mark [%e lexbuf] [%e eint ~loc i];
            [%e body ()]]

let gen_recflag auto =
  (* The generated function is not recursive if the transitions end
     in states with no further transitions. *)
  try
    Array.iter
      (fun (trans_i, _) ->
        Array.iter
          (fun (_, j) ->
            let trans_j, _ = auto.(j) in
            if Array.length trans_j > 0 then raise Exit)
          trans_i)
      auto;
    Nonrecursive
  with Exit -> Recursive

let gen_definition ((_, lexbuf) as lexbuf_with_name) l error =
  let loc = default_loc in
  let brs = Array.of_list l in
  let auto = Sedlex.compile (Array.map fst brs) in
  let cases =
    Array.to_list
      (Array.mapi
         (fun i (_, e) -> case ~lhs:(pint ~loc i) ~guard:None ~rhs:e)
         brs)
  in
  let states = Array.mapi (gen_state lexbuf_with_name auto) auto in
  let states = List.flatten (Array.to_list states) in
  pexp_let ~loc (gen_recflag auto) states
    (pexp_sequence ~loc
       [%expr Sedlexing.start [%e lexbuf]]
       (pexp_match ~loc
          (appfun (state_fun 0) [lexbuf])
          (cases @ [case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:error])))

(* Lexer specification parser *)

let codepoint i =
  if i < 0 || i > Cset.max_code then
    failwith (Printf.sprintf "Invalid Unicode code point: %i" i);
  i

let char c = Cset.singleton (Char.code c)
let uchar c = Cset.singleton (Uchar.to_int c)

let err loc fmt =
  Printf.ksprintf
    (fun s ->
      raise (Location.Error (Location.Error.createf ~loc "Sedlex: %s" s)))
    fmt

type encoding = Utf8 | Latin1 | Ascii

let string_of_encoding = function
  | Utf8 -> "UTF-8"
  | Latin1 -> "Latin-1"
  | Ascii -> "ASCII"

let rev_csets_of_string ~loc ~encoding s =
  match encoding with
    | Utf8 ->
        Utf8.fold
          ~f:(fun acc _ x ->
            match x with
              | `Malformed _ ->
                  err loc "Malformed %s string" (string_of_encoding encoding)
              | `Uchar c -> uchar c :: acc)
          [] s
    | Latin1 ->
        let l = ref [] in
        for i = 0 to String.length s - 1 do
          l := char s.[i] :: !l
        done;
        !l
    | Ascii ->
        let l = ref [] in
        for i = 0 to String.length s - 1 do
          match s.[i] with
            | '\x00' .. '\x7F' as c -> l := char c :: !l
            | _ -> err loc "Malformed %s string" (string_of_encoding encoding)
        done;
        !l

let rec repeat r = function
  | 0, 0 -> Sedlex.eps
  | 0, m -> Sedlex.alt Sedlex.eps (Sedlex.seq r (repeat r (0, m - 1)))
  | n, m -> Sedlex.seq r (repeat r (n - 1, m - 1))

let regexp_of_pattern env =
  let rec char_pair_op func name ~encoding p tuple =
    (* Construct something like Sub(a,b) *)
      match tuple with
      | Some { ppat_desc = Ppat_tuple ([None, p0; None, p1], Closed) } -> begin
          match func (aux ~encoding p0) (aux ~encoding p1) with
            | Some r -> r
            | None ->
                err p.ppat_loc
                  "the %s operator can only applied to single-character length \
                   regexps"
                  name
        end
      | _ ->
          err p.ppat_loc "the %s operator requires two arguments, like %s(a,b)"
            name name
  and aux ~encoding p =
    (* interpret one pattern node *)
      match p.ppat_desc with
      | Ppat_or (p1, p2) -> Sedlex.alt (aux ~encoding p1) (aux ~encoding p2)
      | Ppat_tuple (labeled_pl, Closed) ->
        (match Ppxlib_jane.as_unlabeled_tuple labeled_pl with
         | Some (p :: pl) ->
            List.fold_left
              (fun r p -> Sedlex.seq r (aux ~encoding p))
              (aux ~encoding p) pl
         | Some [] -> err p.ppat_loc "empty tuple unexpected"
         | None -> err p.ppat_loc "labeled tuples not allowed in regexps")
      | Ppat_construct ({ txt = Lident "Star" }, Some (_, p)) ->
          Sedlex.rep (aux ~encoding p)
      | Ppat_construct ({ txt = Lident "Plus" }, Some (_, p)) ->
          Sedlex.plus (aux ~encoding p)
      | Ppat_construct ({ txt = Lident "Utf8" }, Some (_, p)) ->
          aux ~encoding:Utf8 p
      | Ppat_construct ({ txt = Lident "Latin1" }, Some (_, p)) ->
          aux ~encoding:Latin1 p
      | Ppat_construct ({ txt = Lident "Ascii" }, Some (_, p)) ->
          aux ~encoding:Ascii p
      | Ppat_construct
          ( { txt = Lident "Rep" },
            Some
              ( _,
                {
                  ppat_desc =
                    Ppat_tuple
                      ([
                        None, p0;
                        None, {
                          ppat_desc =
                            Ppat_constant (i1 as i2) | Ppat_interval (i1, i2);
                        };
                      ], Closed);
                } ) ) -> begin
          match (i1, i2) with
            | Pconst_integer (i1, _), Pconst_integer (i2, _) ->
                let i1 = int_of_string i1 in
                let i2 = int_of_string i2 in
                if 0 <= i1 && i1 <= i2 then repeat (aux ~encoding p0) (i1, i2)
                else err p.ppat_loc "Invalid range for Rep operator"
            | _ ->
                err p.ppat_loc "Rep must take an integer constant or interval"
        end
      | Ppat_construct ({ txt = Lident "Rep" }, _) ->
          err p.ppat_loc "the Rep operator takes 2 arguments"
      | Ppat_construct ({ txt = Lident "Opt" }, Some (_, p)) ->
          Sedlex.alt Sedlex.eps (aux ~encoding p)
      | Ppat_construct ({ txt = Lident "Compl" }, arg) -> begin
          match arg with
            | Some (_, p0) -> begin
                match Sedlex.compl (aux ~encoding p0) with
                  | Some r -> r
                  | None ->
                      err p.ppat_loc
                        "the Compl operator can only applied to a \
                         single-character length regexp"
              end
            | _ -> err p.ppat_loc "the Compl operator requires an argument"
        end
      | Ppat_construct ({ txt = Lident "Sub" }, arg) ->
          char_pair_op ~encoding Sedlex.subtract "Sub" p
            (Option.map (fun (_, arg) -> arg) arg)
      | Ppat_construct ({ txt = Lident "Intersect" }, arg) ->
          char_pair_op ~encoding Sedlex.intersection "Intersect" p
            (Option.map (fun (_, arg) -> arg) arg)
      | Ppat_construct ({ txt = Lident "Chars" }, arg) -> (
          let const =
            match arg with
              | Some (_, { ppat_desc = Ppat_constant const }) -> Some const
              | _ -> None
          in
          match const with
            | Some (Pconst_string (s, _, _)) ->
                let l = rev_csets_of_string ~loc:p.ppat_loc ~encoding s in
                let chars = List.fold_left Cset.union Cset.empty l in
                Sedlex.chars chars
            | _ ->
                err p.ppat_loc "the Chars operator requires a string argument")
      | Ppat_interval (i_start, i_end) -> begin
          match (i_start, i_end) with
            | Pconst_char c1, Pconst_char c2 ->
                let valid =
                  match encoding with
                    (* utf8 char interval can only match ascii because
                       of the OCaml lexer. *)
                    | Ascii | Utf8 -> (
                        function '\x00' .. '\x7f' -> true | _ -> false)
                    | Latin1 -> ( function _ -> true)
                in
                if not (valid c1 && valid c2) then
                  err p.ppat_loc
                    "this pattern is not a valid %s interval regexp"
                    (string_of_encoding encoding);
                Sedlex.chars (Cset.interval (Char.code c1) (Char.code c2))
            | Pconst_integer (i1, _), Pconst_integer (i2, _) ->
                Sedlex.chars
                  (Cset.interval
                     (codepoint (int_of_string i1))
                     (codepoint (int_of_string i2)))
            | _ -> err p.ppat_loc "this pattern is not a valid interval regexp"
        end
      | Ppat_constant const -> begin
          match const with
            | Pconst_string (s, _, _) ->
                let rev_l = rev_csets_of_string s ~loc:p.ppat_loc ~encoding in
                List.fold_left
                  (fun acc cset -> Sedlex.seq (Sedlex.chars cset) acc)
                  Sedlex.eps rev_l
            | Pconst_char c -> Sedlex.chars (char c)
            | Pconst_integer (i, _) ->
                Sedlex.chars (Cset.singleton (codepoint (int_of_string i)))
            | _ -> err p.ppat_loc "this pattern is not a valid regexp"
        end
      | Ppat_var { txt = x } -> begin
          try StringMap.find x env
          with Not_found -> err p.ppat_loc "unbound regexp %s" x
        end
      | _ -> err p.ppat_loc "this pattern is not a valid regexp"
  in
  aux ~encoding:Ascii

let previous = ref []
let regexps = ref []
let should_set_cookies = ref false

let loc_ghoster =
  object
    inherit Ast_traverse.map as super
    method! location location = super#location { location with loc_ghost = true }
  end
;;

let mapper =
  object (this)
    inherit Ast_traverse.map as super
    val env = builtin_regexps

    method define_regexp name p =
      {<env = StringMap.add name (regexp_of_pattern env p) env>}

    method! expression e =
      match e with
        | [%expr [%sedlex [%e? { pexp_desc = Pexp_match (lexbuf, cases) }]]] ->
            let lexbuf =
              match lexbuf with
                | { pexp_desc = Pexp_ident { txt = Lident txt } } ->
                    (txt, lexbuf)
                | _ ->
                    err lexbuf.pexp_loc
                      "the matched expression must be a single identifier"
            in
            let cases = List.rev cases in
            let error =
              match List.hd cases with
                | { pc_lhs = [%pat? _]; pc_rhs = e; pc_guard = None } ->
                    this#expression e
                | { pc_lhs = p } ->
                    err p.ppat_loc
                      "the last branch must be a catch-all error case"
            in
            let cases = List.rev (List.tl cases) in
            let cases =
              List.map
                (function
                  | { pc_lhs = p; pc_rhs = e; pc_guard = None } ->
                      (regexp_of_pattern env p, this#expression e)
                  | { pc_guard = Some e } ->
                      err e.pexp_loc "'when' guards are not supported")
                cases
            in
            loc_ghoster#expression @@ gen_definition lexbuf cases error
        | [%expr
            let [%p? { ppat_desc = Ppat_var { txt = name } }] =
              [%sedlex.regexp? [%p? p]]
            in
            [%e? body]] ->
            loc_ghoster#expression @@ (this#define_regexp name p)#expression body
        | [%expr [%sedlex [%e? _]]] ->
            err e.pexp_loc
              "the %%sedlex extension is only recognized on match expressions"
        | _ -> super#expression e

    val toplevel = true

    method structure_with_regexps l =
      let mapper = ref this in
      let regexps = ref [] in
      let l =
        List.concat
          (List.map
             (function
               | [%stri
                   let [%p? { ppat_desc = Ppat_var { txt = name } }] =
                     [%sedlex.regexp? [%p? p]]] as i ->
                   regexps := i :: !regexps;
                   mapper := !mapper#define_regexp name p;
                   []
               | i -> [!mapper#structure_item i])
             l)
      in
      (l, List.rev !regexps)

    method! structure l =
      if toplevel then (
        let sub = {<toplevel = false>} in
        let l, regexps' = sub#structure_with_regexps (!previous @ l) in
        let parts = List.map partition (get_partitions ()) in
        let tables = List.map table (get_tables ()) in
        regexps := regexps';
        should_set_cookies := true;
        loc_ghoster#structure (tables @ parts) @ l)
      else fst (this#structure_with_regexps l)
  end

let pre_handler cookies =
  previous :=
    match Driver.Cookies.get cookies "sedlex.regexps" Ast_pattern.__ with
      | Some { pexp_desc = Pexp_extension (_, PStr l) } -> l
      | Some _ -> assert false
      | None -> []

let post_handler cookies =
  if !should_set_cookies then (
    let loc = default_loc in
    Driver.Cookies.set cookies "sedlex.regexps"
      (pexp_extension ~loc ({ loc; txt = "regexps" }, PStr !regexps)))

let extensions =
  [
    Extension.declare "sedlex" Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      (fun ~loc:_ ~path:_ expr ->
        let output = mapper#expression expr in
        loc_ghoster#expression output);
  ]

let () =
  Driver.Cookies.add_handler pre_handler;
  Driver.Cookies.add_post_handler post_handler;
  Driver.register_transformation "sedlex" ~impl:mapper#structure
