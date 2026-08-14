(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Vox_logic

type query =
  | Prove
  | Disprove

exception Ill_formed of string

let error fmt = Format.kasprintf (fun message -> raise (Ill_formed message)) fmt

(* SMT-LIB symbols.  A simple symbol is a nonempty sequence of letters,
   digits and [~ ! @ $ % ^ & * _ - + = < > . ? /] that does not start with a
   digit; anything else must be written [|quoted|], which cannot contain [|]
   or [\].  Reserved words are legal only when quoted. *)

let reserved =
  [ "BINARY"; "DECIMAL"; "HEXADECIMAL"; "NUMERAL"; "STRING"
  ; "_"; "!"; "as"; "exists"; "forall"; "let"; "match"; "par" ]

let simple_symbol_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
  | '~' | '!' | '@' | '$' | '%' | '^' | '&' | '*' | '_' | '-' | '+' | '='
  | '<' | '>' | '.' | '?' | '/' ->
    true
  | _ -> false

let symbol name =
  if String.equal name "" then error "empty symbol";
  let simple =
    (match name.[0] with '0' .. '9' -> false | _ -> true)
    && String.for_all simple_symbol_char name
    && not (List.mem name reserved)
  in
  if simple
  then name
  else if String.exists (function '|' | '\\' -> true | _ -> false) name
  then error "symbol %S cannot be represented in SMT-LIB" name
  else "|" ^ name ^ "|"

let sort = function
  | Sort.Bool -> "Bool"
  | Sort.Int -> "Int"
  | Sort.Bitvec width ->
    if width < 1 then error "bitvector sort must have positive width";
    Printf.sprintf "(_ BitVec %d)" width
  | Sort.Uninterpreted name | Sort.Datatype name -> symbol name

(* What the signature declares, in checkable form. *)
type tables =
  { sorts : (string, unit) Hashtbl.t  (* uninterpreted and datatype *)
  ; variables : (string, unit) Hashtbl.t
  ; functions : (string, int) Hashtbl.t  (* arity *)
  ; constructors : (string, Signature.constructor) Hashtbl.t
  }

let check_signature (signature : Signature.t) =
  let tables =
    { sorts = Hashtbl.create 16
    ; variables = Hashtbl.create 16
    ; functions = Hashtbl.create 16
    ; constructors = Hashtbl.create 16
    }
  in
  let declare_sort name =
    if Hashtbl.mem tables.sorts name then error "duplicate sort %s" name;
    Hashtbl.add tables.sorts name ()
  in
  List.iter declare_sort signature.sorts;
  List.iter
    (fun (datatype : Signature.datatype) ->
       declare_sort datatype.datatype_name)
    signature.datatypes;
  let check_sort_declared s =
    ignore (sort s);
    match s with
    | Sort.Uninterpreted name | Sort.Datatype name ->
      if not (Hashtbl.mem tables.sorts name)
      then error "undeclared sort %s" name
    | Sort.Bool | Sort.Int | Sort.Bitvec _ -> ()
  in
  (* Variables, functions, constructors and selectors are all function
     symbols to the solver: one namespace. *)
  let symbols = Hashtbl.create 16 in
  let declare_symbol kind name =
    if Hashtbl.mem symbols name
    then error "duplicate symbol %s (as %s)" name kind;
    Hashtbl.add symbols name ()
  in
  List.iter
    (fun (name, s) ->
       declare_symbol "variable" name;
       check_sort_declared s;
       Hashtbl.add tables.variables name ())
    signature.variables;
  List.iter
    (fun (name, argument_sorts, result_sort) ->
       declare_symbol "function" name;
       List.iter check_sort_declared argument_sorts;
       check_sort_declared result_sort;
       Hashtbl.add tables.functions name (List.length argument_sorts))
    signature.functions;
  List.iter
    (fun (datatype : Signature.datatype) ->
       if datatype.constructors = []
       then error "datatype %s has no constructors" datatype.datatype_name;
       List.iter
         (fun (constructor : Signature.constructor) ->
            declare_symbol "constructor" constructor.constructor_name;
            Hashtbl.add tables.constructors constructor.constructor_name
              constructor;
            List.iter
              (fun (selector, field_sort) ->
                 declare_symbol "selector" selector;
                 check_sort_declared field_sort)
              constructor.fields)
         datatype.constructors)
    signature.datatypes;
  tables

let literal = function
  | Literal.Bool true -> "true"
  | Literal.Bool false -> "false"
  | Literal.Int digits ->
    let body =
      match String.length digits with
      | 0 -> error "empty integer literal"
      | length when digits.[0] = '-' && length > 1 ->
        Some (String.sub digits 1 (length - 1))
      | _ when digits.[0] = '-' -> error "empty integer literal"
      | _ -> None
    in
    let check s =
      if not (String.for_all (function '0' .. '9' -> true | _ -> false) s)
      then error "malformed integer literal %S" digits
    in
    (match body with
     | None -> check digits; digits
     | Some magnitude ->
       check magnitude;
       Printf.sprintf "(- %s)" magnitude)
  | Literal.Bitvec { width; value } ->
    if width < 1 || width > 64
    then error "bitvector literal width %d not between 1 and 64" width;
    let masked =
      if width = 64
      then value
      else Int64.logand value (Int64.sub (Int64.shift_left 1L width) 1L)
    in
    Printf.sprintf "(_ bv%Lu %d)" masked width

let op_name : Op.t -> string = function
  | Not -> "not"
  | And -> "and"
  | Or -> "or"
  | Implies -> "=>"
  | Eq -> "="
  | Distinct -> "distinct"
  | Neg -> "-"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "div"
  | Mod -> "mod"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Bv_neg -> "bvneg"
  | Bv_add -> "bvadd"
  | Bv_sub -> "bvsub"
  | Bv_mul -> "bvmul"
  | Bv_sdiv -> "bvsdiv"
  | Bv_srem -> "bvsrem"
  | Bv_not -> "bvnot"
  | Bv_and -> "bvand"
  | Bv_or -> "bvor"
  | Bv_xor -> "bvxor"
  | Bv_shl -> "bvshl"
  | Bv_lshr -> "bvlshr"
  | Bv_ashr -> "bvashr"
  | Bv_slt -> "bvslt"
  | Bv_sle -> "bvsle"
  | Bv_sgt -> "bvsgt"
  | Bv_sge -> "bvsge"

(* [None] means any arity of at least two. *)
let op_arity : Op.t -> int option = function
  | Not | Neg | Bv_neg | Bv_not -> Some 1
  | And | Or | Eq | Distinct | Add | Mul -> None
  | Implies | Sub | Div | Mod | Lt | Le | Gt | Ge | Bv_add | Bv_sub | Bv_mul
  | Bv_sdiv | Bv_srem | Bv_and | Bv_or | Bv_xor | Bv_shl | Bv_lshr | Bv_ashr
  | Bv_slt | Bv_sle | Bv_sgt | Bv_sge ->
    Some 2

let rec term tables : Term.t -> string = function
  | Var name ->
    if not (Hashtbl.mem tables.variables name)
    then error "undeclared variable %s" name;
    symbol name
  | Const l -> literal l
  | App (op, arguments) ->
    let given = List.length arguments in
    (match op_arity op with
     | Some expected when expected <> given ->
       error "operator %s expects %d argument(s) but was given %d"
         (op_name op) expected given
     | None when given < 2 ->
       error "operator %s expects at least two arguments but was given %d"
         (op_name op) given
     | Some _ | None -> ());
    application tables (op_name op) arguments
  | Call (name, arguments) ->
    (match Hashtbl.find_opt tables.functions name with
     | None -> error "undeclared function %s" name
     | Some arity ->
       if arity <> List.length arguments
       then
         error "function %s expects %d argument(s) but was given %d" name
           arity (List.length arguments));
    application tables (symbol name) arguments
  | Ite (condition, if_true, if_false) ->
    application tables "ite" [condition; if_true; if_false]
  | Construct (constructor, arguments) ->
    let { Signature.constructor_name = _; fields } =
      find_constructor tables constructor
    in
    if List.length fields <> List.length arguments
    then
      error "constructor %s expects %d argument(s) but was given %d"
        constructor (List.length fields) (List.length arguments);
    if arguments = []
    then symbol constructor
    else application tables (symbol constructor) arguments
  | Select (constructor, index, argument) ->
    let { Signature.constructor_name = _; fields } =
      find_constructor tables constructor
    in
    (match List.nth_opt fields index with
     | None ->
       error "constructor %s has no field %d" constructor index
     | Some (selector, _) -> application tables (symbol selector) [argument])
  | Test (constructor, argument) ->
    let (_ : Signature.constructor) = find_constructor tables constructor in
    application tables
      (Printf.sprintf "(_ is %s)" (symbol constructor))
      [argument]

and find_constructor tables name =
  match Hashtbl.find_opt tables.constructors name with
  | Some constructor -> constructor
  | None -> error "undeclared constructor %s" name

and application tables head arguments =
  "(" ^ String.concat " " (head :: List.map (term tables) arguments) ^ ")"

(* Mutually recursive datatypes must share one [declare-datatypes], and a
   group must come after the groups it references: strongly connected
   components of the reference graph, in dependency order. *)
let datatype_groups (datatypes : Signature.datatype list) =
  let index_of =
    let table = Hashtbl.create 16 in
    List.iteri
      (fun i (datatype : Signature.datatype) ->
         Hashtbl.replace table datatype.datatype_name i)
      datatypes;
    table
  in
  let nodes = Array.of_list datatypes in
  let successors i =
    List.concat_map
      (fun (constructor : Signature.constructor) ->
         List.filter_map
           (fun (_, field_sort) ->
              match field_sort with
              | Sort.Datatype name -> Hashtbl.find_opt index_of name
              | _ -> None)
           constructor.fields)
      nodes.(i).constructors
  in
  (* Tarjan.  Components come out with dependencies first. *)
  let unvisited = -1 in
  let number = Array.make (Array.length nodes) unvisited in
  let lowlink = Array.make (Array.length nodes) 0 in
  let on_stack = Array.make (Array.length nodes) false in
  let stack = ref [] in
  let next = ref 0 in
  let groups = ref [] in
  let rec visit i =
    number.(i) <- !next;
    lowlink.(i) <- !next;
    incr next;
    stack := i :: !stack;
    on_stack.(i) <- true;
    List.iter
      (fun j ->
         if number.(j) = unvisited
         then begin
           visit j;
           lowlink.(i) <- min lowlink.(i) lowlink.(j)
         end
         else if on_stack.(j)
         then lowlink.(i) <- min lowlink.(i) number.(j))
      (successors i);
    if lowlink.(i) = number.(i)
    then begin
      let rec pop members =
        match !stack with
        | [] -> members
        | j :: rest ->
          stack := rest;
          on_stack.(j) <- false;
          if j = i then j :: members else pop (j :: members)
      in
      groups := List.map (fun j -> nodes.(j)) (pop []) :: !groups
    end
  in
  Array.iteri (fun i _ -> if number.(i) = unvisited then visit i) nodes;
  List.rev !groups

let render_datatype_group buffer (group : Signature.datatype list) =
  Buffer.add_string buffer "(declare-datatypes (";
  List.iteri
    (fun i (datatype : Signature.datatype) ->
       if i > 0 then Buffer.add_char buffer ' ';
       Buffer.add_string buffer
         (Printf.sprintf "(%s 0)" (symbol datatype.datatype_name)))
    group;
  Buffer.add_string buffer ") (";
  List.iter
    (fun (datatype : Signature.datatype) ->
       Buffer.add_string buffer "\n  (";
       List.iteri
         (fun i (constructor : Signature.constructor) ->
            if i > 0 then Buffer.add_char buffer ' ';
            Buffer.add_char buffer '(';
            Buffer.add_string buffer (symbol constructor.constructor_name);
            List.iter
              (fun (selector, field_sort) ->
                 Buffer.add_string buffer
                   (Printf.sprintf " (%s %s)" (symbol selector)
                      (sort field_sort)))
              constructor.fields;
            Buffer.add_char buffer ')')
         datatype.constructors;
       Buffer.add_char buffer ')')
    group;
  Buffer.add_string buffer "))\n"

let render ?timeout_ms query (obligation : Obligation.t) =
  match
    let signature = obligation.signature in
    let tables = check_signature signature in
    (match
       List.sort_uniq Int.compare
         (List.map
            (fun (hypothesis : Obligation.hypothesis) -> hypothesis.id)
            obligation.hypotheses)
     with
     | ids when List.length ids <> List.length obligation.hypotheses ->
       error "duplicate hypothesis id"
     | _ -> ());
    let buffer = Buffer.create 1024 in
    let line fmt = Format.kasprintf
      (fun s -> Buffer.add_string buffer s; Buffer.add_char buffer '\n') fmt
    in
    (match timeout_ms with
     | Some ms -> line "(set-option :timeout %d)" ms
     | None -> ());
    (match query with
     | Prove -> line "(set-option :produce-unsat-cores true)"
     | Disprove -> ());
    List.iter (fun name -> line "(declare-sort %s 0)" (symbol name))
      signature.sorts;
    List.iter (render_datatype_group buffer)
      (datatype_groups signature.datatypes);
    List.iter
      (fun (name, s) -> line "(declare-const %s %s)" (symbol name) (sort s))
      signature.variables;
    List.iter
      (fun (name, argument_sorts, result_sort) ->
         line "(declare-fun %s (%s) %s)" (symbol name)
           (String.concat " " (List.map sort argument_sorts))
           (sort result_sort))
      signature.functions;
    List.iter
      (fun (hypothesis : Obligation.hypothesis) ->
         let rendered = term tables hypothesis.term in
         match query with
         | Prove -> line "(assert (! %s :named h%d))" rendered hypothesis.id
         | Disprove -> line "(assert %s)" rendered)
      obligation.hypotheses;
    let goal = term tables obligation.goal in
    (match query with
     | Prove -> line "(assert (not %s))" goal
     | Disprove -> line "(assert %s)" goal);
    line "(check-sat)";
    (match query with
     | Prove ->
       line "(get-unsat-core)";
       line "(get-model)"
     | Disprove -> ());
    line "(get-info :reason-unknown)";
    Buffer.contents buffer
  with
  | script -> Ok script
  | exception Ill_formed message -> Error message
