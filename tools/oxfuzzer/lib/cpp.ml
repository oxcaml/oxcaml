(* This backend reads the IR, never the OCaml Parsetree or generated source.
   Explicit C++ temporaries implement the reference evaluation order while the
   OCaml backend deliberately leaves operands unbound. *)
open Ir

let name n = "fuzz_" ^ Name.to_string n

let quoted s =
  let buffer = Buffer.create ((4 * String.length s) + 2) in
  Buffer.add_char buffer '"';
  String.iter (fun c -> Printf.bprintf buffer "\\%03o" (Char.code c)) s;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let number_kind (nty : NumberTy.t) =
  match nty.base with
  | Float -> "N::F64"
  | Float32 -> "N::F32"
  | Int -> "N::I63"
  | Nativeint | Int64 -> "N::I64"
  | Int32 -> "N::I32"
  | Int16 -> "N::I16"
  | Int8 -> "N::I8"

let literal (number : Number.t) =
  let bits =
    match number with
    | Float bits | Int64 bits -> bits
    | Float32 bits | Int32 bits -> Int64.of_int32 bits
    | Int n | Int16 n | Int8 n -> Int64.of_int n
    | Nativeint n -> Int64.of_nativeint n
  in
  Printf.sprintf "raw(UINT64_C(0x%Lx))" bits

let unary_name (op : Un_op.t) =
  match op with
  | Un_op.Neg -> "Neg"
  | Abs -> "Abs"
  | Succ -> "Succ"
  | Pred -> "Pred"
  | Bit_not -> "Bit_not"
  | Sqrt -> "Sqrt"
  | Round -> "Round"
  | Trunc -> "Trunc"
  | Ceil -> "Ceil"
  | Floor -> "Floor"
  | Not -> Misc.fatal_errorf "Cpp: boolean unary operation at numeric type"

let binary_name (op : Bin_op.t) =
  match op with
  | Bin_op.Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Rem -> "Rem"
  | Min -> "Min"
  | Max -> "Max"
  | Min_num -> "Min_num"
  | Max_num -> "Max_num"
  | Bit_and -> "Bit_and"
  | Bit_or -> "Bit_or"
  | Bit_xor -> "Bit_xor"
  | Shift_left -> "Shift_left"
  | Shift_right -> "Shift_right"
  | Shift_right_logical -> "Shift_right_logical"
  | Eq -> "Eq"
  | Ne -> "Ne"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | Compare -> "Compare"
  | Unsigned_compare -> "Unsigned_compare"
  | And | Or -> Misc.fatal_errorf "Cpp: eager boolean operation"

let fresh counter =
  let n = !counter in
  incr counter;
  Printf.sprintf "ref_%d" n

let lambda body = "([&]() -> V {\n" ^ body ^ "}())"

let declaration variable value = "V " ^ variable ^ " = " ^ value ^ ";\n"

let aggregate tag values =
  Printf.sprintf "aggregate(%d, {%s})" tag (String.concat ", " values)

let rec expr st (expression : Expr.t) =
  match expression with
  | Const { number; unboxed = _ } -> literal number
  | Bool_const b -> "boolean(" ^ Bool.to_string b ^ ")"
  | String_const s ->
    Printf.sprintf "string(%s, %d)" (quoted s) (String.length s)
  | String_length e -> "integer(63, (" ^ expr st e ^ ").text.size())"
  | Bounded_string_concat { lhs; rhs; result_name = _; max_length } ->
    operands st [lhs; rhs] (function
      | [lhs; rhs] -> Printf.sprintf "concat(%s, %s, %d)" lhs rhs max_length
      | _ -> Misc.fatal_errorf "Cpp: concatenation arity")
  | Assert_false id -> Printf.sprintf "assertion(%d)" id
  | Let { name = binding; expr = value; body } ->
    lambda
      (declaration (name binding) (expr st value)
      ^ "return " ^ expr st body ^ ";\n")
  | If { condition; if_true; if_false } ->
    Printf.sprintf "(truth(%s) ? %s : %s)" (expr st condition) (expr st if_true)
      (expr st if_false)
  | Read place -> read st place
  | Array_literal values | Unboxed_tuple values ->
    operands st values (aggregate 0)
  | Array_make { dimensions; init_name = _; init } ->
    Printf.sprintf "make_array({%s}, 0, %s)"
      (String.concat ", " (List.map Int.to_string dimensions))
      (expr st init)
  | Record (_, values) -> operands st values (aggregate 0)
  | Record_update (record, source, field, value) ->
    let base = fresh st in
    (* The base is evaluated first. Copied fields are reads too: snapshot each
       in right-to-left field order, interleaved with the replacement. *)
    let fields =
      List.map
        (fun (candidate : Ty.field) ->
          let temporary = fresh st in
          let value =
            if candidate.index = field.index
            then expr st value
            else Printf.sprintf "field(%s, %d)" base candidate.index
          in
          temporary, declaration temporary value)
        record.fields
    in
    lambda
      (declaration base (expr st source)
      ^ String.concat "" (List.rev_map snd fields)
      ^ "return "
      ^ aggregate 0 (List.map fst fields)
      ^ ";\n")
  | Record_convert { expr = source; _ } -> "copy_record(" ^ expr st source ^ ")"
  | Construct { constructor; args; _ } ->
    operands st args (aggregate constructor.constructor_index)
  | Match { scrutinee; cases; _ } ->
    let value = fresh st in
    let cases =
      List.map
        (fun (case : Expr.case) ->
          Printf.sprintf "case %d: {\n" case.constructor.constructor_index
          ^ String.concat ""
              (List.mapi
                 (fun index (binding : Binding.t) ->
                   declaration (name binding.name)
                     (Printf.sprintf "field(%s, %d)" value index))
                 case.bindings)
          ^ "return " ^ expr st case.body ^ ";\n}\n")
        cases
    in
    lambda
      (declaration value (expr st scrutinee)
      ^ "switch (" ^ value ^ ".object->tag) {\n" ^ String.concat "" cases
      ^ "}\nthrow std::logic_error(\"non-exhaustive IR match\");\n")
  | Opaque e -> expr st e
  | Un_op { op = Not; arg; _ } -> "boolean(!truth(" ^ expr st arg ^ "))"
  | Un_op { ty = Number nty; op; arg } ->
    Printf.sprintf "unary(%s, U::%s, %s)" (number_kind nty) (unary_name op)
      (expr st arg)
  | Un_op
      { ty =
          ( Bool | String | Array _ | Record _ | Unboxed_tuple _ | Variant _
          | Or_null _ );
        _
      } ->
    Misc.fatal_errorf "Cpp: invalid unary operation"
  | Bin_op { op = (And | Or) as op; lhs; rhs; _ } ->
    let op = match op with And -> "&&" | _ -> "||" in
    Printf.sprintf "boolean(truth(%s) %s truth(%s))" (expr st lhs) op
      (expr st rhs)
  | Bin_op { ty; op; lhs; rhs } ->
    operands st [lhs; rhs] (function
      | [lhs; rhs] ->
        let prefix =
          match ty with
          | Number nty -> "binary(" ^ number_kind nty ^ ", "
          | Bool -> "compare_bool("
          | String -> "compare_string("
          | Array _ | Record _ | Unboxed_tuple _ | Variant _ | Or_null _ ->
            Misc.fatal_errorf "Cpp: aggregate binary operation"
        in
        Printf.sprintf "%sB::%s, %s, %s)" prefix (binary_name op) lhs rhs
      | _ -> Misc.fatal_errorf "Cpp: binary arity")
  | Convert { expr = value; from; to_ } ->
    Printf.sprintf "convert(%s, %s, %s)" (number_kind from) (number_kind to_)
      (expr st value)
  | Call_toplevel { fun_name; args } ->
    operands st args (fun values ->
        name fun_name ^ "(" ^ String.concat ", " values ^ ")")
  | Raise raise -> raise_expr st raise
  | Try { body; handlers; _ } ->
    lambda
      ("try { return " ^ expr st body ^ "; }\n"
      ^ handlers_code st handlers (fun body -> "return " ^ expr st body ^ ";"))

and operands st expressions finish =
  let values = List.map (fun _ -> fresh st) expressions in
  let evaluations =
    List.map2 (fun value e -> declaration value (expr st e)) values expressions
  in
  lambda
    (String.concat "" (List.rev evaluations) ^ "return " ^ finish values ^ ";\n")

and read st (place : Expr.place) =
  match place with
  | Variable variable -> name variable
  | Field (parent, _, field) ->
    Printf.sprintf "field(%s, %d)" (read st parent) field.index
  | Element (parent, indices) ->
    operands st indices (fun indices ->
        List.fold_left
          (fun value index -> "field(" ^ value ^ ", as_int(" ^ index ^ "))")
          (read st parent) indices)
  | Unboxed_tuple_component { tuple; index; _ } ->
    Printf.sprintf "field(%s, %d)" (read st tuple) index

and raise_expr st ({ exn; payload; notrace = _ } : Expr.raise) =
  Printf.sprintf "raise(%d, %s)" exn.id
    (match payload with None -> "V{}" | Some payload -> expr st payload)

and handlers_code : 'a. int ref -> 'a Handler.t list -> ('a -> string) -> string
    =
 fun st handlers body ->
  let exception_ = fresh st in
  let rec cases = function
    | [] -> "throw;\n"
    | (handler : _ Handler.t) :: rest -> (
      let code = body handler.body in
      match handler.pattern with
      | Catch_all -> code
      | Exn { exn; payload } ->
        let binding =
          match payload with
          | None -> ""
          | Some (binding : Binding.t) ->
            declaration (name binding.name) (exception_ ^ ".payload")
        in
        Printf.sprintf "if (%s.tag == %d) {\n%s%s\n} else {\n%s}\n" exception_
          exn.id binding code (cases rest))
  in
  "catch (const Exception& " ^ exception_ ^ ") {\n" ^ cases handlers ^ "}\n"

let rec statement st (s : Statement.t) =
  match s with
  | Assign (target, value) ->
    let rhs = fresh st in
    "{\n" ^ declaration rhs (expr st value) ^ assignment st target rhs ^ "}\n"
  | Seq statements -> String.concat "" (List.map (statement st) statements)
  | If (condition, yes, no) ->
    "if (truth(" ^ expr st condition ^ ")) {\n" ^ statement st yes
    ^ "} else {\n" ^ statement st no ^ "}\n"
  | Let (binding, value, body) ->
    "{\n"
    ^ declaration (name binding.name) (expr st value)
    ^ statement st body ^ "}\n"
  | Bounded_loop { var; init; bound; stride; body } ->
    let limit = fresh st in
    "{\n"
    ^ declaration (name var) (expr st init)
    ^ declaration limit (expr st bound)
    ^ Printf.sprintf "while (as_int(%s) %s as_int(%s)) {\n" (name var)
        (if stride > 0 then "<=" else ">=")
        limit
    ^ statement st body
    ^ Printf.sprintf "%s = binary(N::I63, B::Add, %s, %s);\n}\n}\n" (name var)
        (name var)
        (literal (Number.Int stride))
  | Raise r -> raise_expr st r ^ ";\n"
  | Try { body; handlers } ->
    "try {\n" ^ statement st body ^ "}\n"
    ^ handlers_code st handlers (statement st)

and assignment st (target : Expr.place) rhs =
  match target with
  | Expr.Variable variable -> name variable ^ " = " ^ rhs ^ ";\n"
  | Field (parent, _, field) ->
    Printf.sprintf "(%s).object->fields.at(%d) = %s;\n" (read st parent)
      field.index rhs
  | Element (parent, indices) ->
    let values = List.map (fun _ -> fresh st) indices in
    let evaluations =
      List.map2 (fun value e -> declaration value (expr st e)) values indices
    in
    let array = fresh st in
    let rec set source = function
      | [] -> Misc.fatal_errorf "Cpp: empty array indices"
      | [index] ->
        Printf.sprintf "%s.object->fields.at(as_int(%s)) = %s;\n" source index
          rhs
      | index :: rest ->
        set ("field(" ^ source ^ ", as_int(" ^ index ^ "))") rest
    in
    String.concat "" (List.rev evaluations)
    ^ declaration array (read st parent)
    ^ set array values
  | Unboxed_tuple_component _ ->
    Misc.fatal_errorf "Cpp: assignment to immutable tuple component"

let rec print_value st (ty : Ty.t) value =
  match ty with
  | Number nty ->
    Printf.sprintf "print_number(%s, %s);\n" (number_kind nty) value
  | Bool ->
    "std::fputs(truth(" ^ value ^ ") ? \"true \" : \"false \", stdout);\n"
  | String -> "print_string(" ^ value ^ ");\n"
  | Array (element, dimensions) ->
    let rec elements value = function
      | [] -> print_value st element value
      | _ :: rest ->
        let item = fresh st in
        "for (const V& " ^ item ^ " : (" ^ value ^ ").object->fields) {\n"
        ^ elements item rest ^ "}\n"
    in
    elements value dimensions
  | Unboxed_tuple types ->
    String.concat ""
      (List.mapi
         (fun index ty ->
           print_value st ty (Printf.sprintf "field(%s, %d)" value index))
         types)
  | Record record ->
    String.concat ""
      (List.map
         (fun (field : Ty.field) ->
           print_value st field.ty
             (Printf.sprintf "field(%s, %d)" value field.index))
         record.fields)
  | Variant variant ->
    print_sum st value
      (List.map
         (fun (constructor : Ty.constructor) ->
           ( constructor.constructor_index,
             Printf.sprintf "Variant_%d_C%d" variant.variant_id
               constructor.constructor_index,
             constructor.args ))
         variant.constructors)
  | Or_null payload -> print_sum st value [0, "Null", []; 1, "This", [payload]]

and print_sum st value constructors =
  "switch ((" ^ value ^ ").object->tag) {\n"
  ^ String.concat ""
      (List.map
         (fun (tag, label, types) ->
           Printf.sprintf "case %d: { std::fputs(%s, stdout);\n" tag
             (quoted (label ^ " "))
           ^ String.concat ""
               (List.mapi
                  (fun index ty ->
                    print_value st ty
                      (Printf.sprintf "field(%s, %d)" value index))
                  types)
           ^ "break; }\n")
         constructors)
  ^ "default: throw std::logic_error(\"invalid sum tag\");\n}\n"

let to_code (program : Program.t) =
  if Sys.word_size <> 64 || Sys.int_size <> 63
  then Misc.fatal_errorf "Cpp: the reference currently requires a 64-bit target";
  let st = ref 0 in
  let functions =
    List.map
      (fun (f : Function.t) ->
        "V " ^ name f.name ^ "("
        ^ String.concat ", "
            (List.map (fun (b : Binding.t) -> "V " ^ name b.name) f.params)
        ^ ") {\n" ^ statement st f.body ^ "return " ^ expr st f.result
        ^ ";\n}\n")
      program.functions
  in
  let declarations = program.toplevel_decls in
  let initialize =
    List.mapi
      (fun index (binding, value) ->
        Printf.sprintf "%s = %s; initialized = %d;\n"
          (name binding.Binding.name)
          (expr st value) (index + 1))
      declarations
  in
  let observations =
    List.rev
      (List.mapi
         (fun index ((binding : Binding.t), _) ->
           Printf.sprintf "if (initialized > %d) {\n" index
           ^ print_value st binding.Binding.ty (name binding.name)
           ^ "}\n")
         declarations)
  in
  let observe_assertions = program.swarm.assertions = Config.May_raise in
  Cpp_runtime.source ^ "\n// Generated program.\nusing namespace reference;\n"
  ^ String.concat "" functions ^ "int main() {\ntry {\n"
  ^ "if (std::fesetround(FE_TONEAREST) != 0) throw \
     std::runtime_error(\"rounding mode\");\n" ^ "size_t initialized = 0;\n"
  ^ String.concat ""
      (List.map
         (fun (binding, _) -> "V " ^ name binding.Binding.name ^ ";\n")
         declarations)
  ^ (if observe_assertions then "try {\n" else "")
  ^ String.concat "" initialize
  ^ statement st program.toplevel_statement
  ^ (if observe_assertions
     then
       "std::fputs(\"returned\\n\", stdout);\n"
       ^ "} catch (const Exception& exn) {\n" ^ "if (exn.tag != -1) throw;\n"
       ^ "std::printf(\"assertion %d:0\\n\", exn.assertion_id);\n}\n"
     else "")
  ^ String.concat "" observations
  ^ "return 0;\n} catch (const Exception& exn) {\n"
  ^ "std::fprintf(stderr, \"uncaught reference exception %d\\n\", exn.tag); \
     return 1;\n" ^ "} catch (const std::exception& exn) {\n"
  ^ "std::fprintf(stderr, \"reference error: %s\\n\", exn.what()); return 2;\n\
     }\n\
     }\n"
