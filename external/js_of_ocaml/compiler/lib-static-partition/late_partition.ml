open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
module C = Code

module Report = struct
  type value =
    { id : int
    ; name : string option
    ; definition : string
    ; literal_digest : string option
    }

  type t =
    { captures : value list
    ; relocated : value list
    ; rounds : int
    ; globally_optimized_blocks : int
    ; eager_blocks : int
    ; lazy_blocks : int
    }

  let value_to_json { id; name; definition; literal_digest } =
    let option = function
      | None -> `Null
      | Some s -> `String s
    in
    `Assoc
      [ "id", `Int id
      ; "name", option name
      ; "definition", `String definition
      ; "literal_digest", option literal_digest
      ]

  let to_json
      { captures
      ; relocated
      ; rounds
      ; globally_optimized_blocks
      ; eager_blocks
      ; lazy_blocks
      } =
    `Assoc
      [ "captures", `List (List.map captures ~f:value_to_json)
      ; "relocated", `List (List.map relocated ~f:value_to_json)
      ; "rounds", `Int rounds
      ; "globally_optimized_blocks", `Int globally_optimized_blocks
      ; "eager_blocks", `Int eager_blocks
      ; "lazy_blocks", `Int lazy_blocks
      ]
end

type t =
  { eager : C.program
  ; lazy_ : C.program
  ; report : Report.t
  }

type many =
  { eager_program : C.program
  ; roots : (string * C.program * Report.t) list
  }

let eager t = t.eager

let lazy_ t = t.lazy_

let report t = t.report

let eager_many t = t.eager_program

let lazy_roots t = List.map t.roots ~f:(fun (id, program, _) -> id, program)

let reports t = List.map t.roots ~f:(fun (id, _, report) -> id, report)

let bridge_name = "jsoo:split:bridge"

type definition =
  { pc : C.Addr.t
  ; position : int
  ; expression : C.expr option
  }

let definition_name = function
  | None -> "control-flow value"
  | Some (C.Constant _) -> "constant"
  | Some (Closure _) -> "closure"
  | Some (Block (_, _, _, Immutable)) -> "immutable block"
  | Some (Block (_, _, _, Maybe_mutable)) -> "mutable block"
  | Some (Field _) -> "field read"
  | Some (Apply _) -> "application"
  | Some (Prim _) -> "primitive"
  | Some (Special _) -> "special"

let movable = function
  | Some (C.Constant _ | Closure _ | Block (_, _, _, Immutable)) -> true
  | _ -> false

let live_program program =
  let program = Deadcode.remove_unused_blocks program in
  let program, _ = Deadcode.f (Pure_fun.f program) program in
  program

let bound_variables (program : C.program) =
  C.Addr.Map.fold
    (fun _ block variables ->
      let variables = ref variables in
      Freevars.iter_block_bound_vars
        (fun variable -> variables := C.Var.Set.add variable !variables)
        block;
      !variables)
    program.blocks
    C.Var.Set.empty

let clear_call_assumptions (program : C.program) =
  { program with
    blocks =
      C.Addr.Map.map
        (fun (block : C.block) ->
          { block with
            body =
              List.map block.body ~f:(function
                | C.Let (variable, Apply call) ->
                    C.Let
                      (variable, Apply { call with exact = false; yielding = May_yield })
                | instruction -> instruction)
          })
        program.blocks
  }

let anchors (program : C.program) =
  C.Addr.Map.fold
    (fun pc (block : C.block) anchors ->
      let anchors, _ =
        List.fold_left
          block.body
          ~init:(anchors, 0)
          ~f:(fun (anchors, position) -> function
          | C.Let (_, Prim (Extern (name, _), [ Pv thunk ])) -> (
              match Parse_bytecode.split_anchor_root name with
              | None -> anchors, position + 1
              | Some id -> (id, pc, position, thunk) :: anchors, position + 1)
          | _ -> anchors, position + 1)
      in
      anchors)
    program.blocks
    []
  |> List.rev

(* The top-level control flow of [program], as a graph for dominator computation.
   Closure bodies are not part of it. Exception handlers are entered through an
   extra node, so that their exception binding is only available on that entry:
   other edges to the same handler (from ordinary branches) do not bind it. The
   extra nodes never enter the program. *)
let eager_execution_graph (program : C.program) =
  let root_blocks =
    C.traverse
      { fold = C.fold_children }
      C.Addr.Set.add
      program.start
      program.blocks
      C.Addr.Set.empty
  in
  let edges = Int.Hashtbl.create 64 in
  let add_edge source destination =
    Int.Hashtbl.replace
      edges
      source
      (destination :: Option.value ~default:[] (Int.Hashtbl.find_opt edges source))
  in
  let next_node = ref program.free_pc in
  let exception_definitions = ref [] in
  C.Addr.Set.iter
    (fun pc ->
      match (C.Addr.Map.find pc program.blocks).branch with
      | Pushtrap ((body, _), exception_, (handler, _)) ->
          let binding = !next_node in
          next_node := binding + 1;
          add_edge pc body;
          add_edge pc binding;
          add_edge binding handler;
          exception_definitions := (exception_, binding) :: !exception_definitions
      | _ -> C.fold_children program.blocks pc (fun child () -> add_edge pc child) ())
    root_blocks;
  let successors node =
    List.rev (Option.value ~default:[] (Int.Hashtbl.find_opt edges node))
  in
  root_blocks, successors, !exception_definitions

let failwithf fmt = Printf.ksprintf failwith fmt

let split_many_exn program =
  let program = Structure.norm program in
  let anchors = anchors program in
  if List.is_empty anchors then failwith "Expected deferred split anchors";
  let ids = List.map anchors ~f:(fun (id, _, _, _) -> id) in
  if StringSet.cardinal (StringSet.of_list ids) <> List.length ids
  then failwithf "Duplicate deferred root IDs: %s" (String.concat ~sep:", " ids);
  let pcs = IntSet.of_list (List.map anchors ~f:(fun (_, pc, _, _) -> pc)) in
  if IntSet.cardinal pcs <> 1
  then failwith "Sibling split anchors must share an eager block";
  let anchor_pc = IntSet.choose pcs in
  let anchor_positions =
    IntSet.of_list (List.map anchors ~f:(fun (_, _, position, _) -> position))
  in
  let publication_position = IntSet.max_elt anchor_positions in
  let _, flow = Global_flow.f ~fast:false program in
  let roots =
    List.map anchors ~f:(fun (id, _, position, thunk) ->
        let thunk, parameters =
          match Global_flow.get_unique_closure flow thunk with
          | Some closure -> closure
          | None -> failwith "Split anchor does not contain a unique closure"
        in
        if List.length parameters <> 1
        then failwith "Split thunk must take one unit argument";
        id, position, thunk)
  in
  let root_blocks, successors, exception_definitions = eager_execution_graph program in
  if not (C.Addr.Set.mem anchor_pc root_blocks)
  then failwith "Split anchor is not in eager top-level control flow";
  let immediate_dominator =
    Dominators.immediate_dominators ~successors ~root:program.start
  in
  let rec path_to_root pc path =
    let path = pc :: path in
    if pc = program.start then path else path_to_root (immediate_dominator pc) path
  in
  let dominating_nodes = path_to_root anchor_pc [] in
  let dominates_anchor = C.Addr.Set.of_list dominating_nodes in
  let dominator_path =
    List.filter dominating_nodes ~f:(fun pc -> C.Addr.Set.mem pc root_blocks)
  in
  let definitions = C.Var.Hashtbl.create 128 in
  C.Addr.Set.iter
    (fun pc ->
      let block = C.Addr.Map.find pc program.blocks in
      List.iter block.params ~f:(fun variable ->
          C.Var.Hashtbl.replace
            definitions
            variable
            { pc; position = -1; expression = None });
      List.iteri block.body ~f:(fun position -> function
        | C.Let (variable, expression) ->
            C.Var.Hashtbl.replace
              definitions
              variable
              { pc; position; expression = Some expression }
        | _ -> ()))
    root_blocks;
  List.iter exception_definitions ~f:(fun (variable, pc) ->
      C.Var.Hashtbl.replace definitions variable { pc; position = -1; expression = None });
  let definition_exn variable =
    match C.Var.Hashtbl.find_opt definitions variable with
    | Some definition
      when C.Addr.Set.mem definition.pc dominates_anchor
           && (definition.pc <> anchor_pc || definition.position < publication_position)
      -> definition
    | _ ->
        failwithf
          "Crossing value %d%s does not dominate the split anchor"
          (C.Var.idx variable)
          (match C.Var.get_name variable with
          | None -> ""
          | Some name -> Printf.sprintf " (%s)" name)
  in
  let free_variables = Freevars.f program in
  let dependencies variable expression =
    match expression with
    | C.Closure (_, (entry, _), _) -> C.Addr.Map.find entry free_variables
    | _ ->
        let variables = ref C.Var.Set.empty in
        Freevars.iter_instr_free_vars
          (fun variable -> variables := C.Var.Set.add variable !variables)
          (C.Let (variable, expression));
        !variables
  in
  let bridge_argument =
    C.Pc
      (match Config.target () with
      | `JavaScript -> C.NativeString (C.Native_string.of_string bridge_name)
      | `Wasm -> C.String bridge_name)
  in
  let bridge = C.Var.fresh_n "split_capture_bridge" in
  let registered = C.Var.fresh_n "split_capture_registered" in
  let make_eager captures =
    let publication =
      if C.Var.Set.is_empty captures
      then []
      else
        [ C.Let
            ( bridge
            , Block (0, Array.of_list (C.Var.Set.elements captures), NotArray, Immutable)
            )
        ; C.Let
            ( registered
            , Prim (Extern ("caml_register_global", None), [ Pv bridge; bridge_argument ])
            )
        ]
    in
    let block = C.Addr.Map.find anchor_pc program.blocks in
    let body =
      List.concat
        (List.mapi block.body ~f:(fun position instruction ->
             if position = publication_position
             then publication
             else if IntSet.mem position anchor_positions
             then []
             else [ instruction ]))
    in
    live_program
      { program with
        blocks = C.Addr.Map.add anchor_pc { block with body } program.blocks
      }
  in
  let plan eager_live (id, position, thunk) =
    let seen = ref C.Var.Set.empty in
    let captures = ref C.Var.Set.empty in
    let relocated = ref C.Var.Set.empty in
    let rec need variable =
      if not (C.Var.Set.mem variable !seen)
      then (
        seen := C.Var.Set.add variable !seen;
        let definition = definition_exn variable in
        if definition.pc = anchor_pc && definition.position >= position
        then failwithf "Deferred root %s captures a later eager definition" id;
        match definition.expression with
        | Some expression
          when (not (C.Var.Set.mem variable eager_live)) && movable definition.expression
          ->
            relocated := C.Var.Set.add variable !relocated;
            C.Var.Set.iter need (dependencies variable expression)
        | _ -> captures := C.Var.Set.add variable !captures)
    in
    need thunk;
    id, thunk, !captures, !relocated
  in
  let rec settle eager_roots rounds =
    let eager = make_eager eager_roots in
    let plans = List.map roots ~f:(plan (bound_variables eager)) in
    let seen = ref C.Var.Set.empty in
    let shared = ref C.Var.Set.empty in
    let expanded =
      List.fold_left plans ~init:eager_roots ~f:(fun acc (_, _, captures, relocated) ->
          shared := C.Var.Set.union !shared (C.Var.Set.inter !seen relocated);
          seen := C.Var.Set.union !seen relocated;
          C.Var.Set.union acc captures)
    in
    let expanded = C.Var.Set.union expanded !shared in
    if C.Var.Set.equal eager_roots expanded
    then plans, rounds
    else settle expanded (rounds + 1)
  in
  let plans, rounds = settle C.Var.Set.empty 1 in
  let captures =
    List.fold_left plans ~init:C.Var.Set.empty ~f:(fun acc (_, _, captures, _) ->
        C.Var.Set.union acc captures)
  in
  (* Intermediate promotion roots may become redundant when their consumers stay
     eager. *)
  let eager = make_eager captures in
  let eager_bound = bound_variables eager in
  let captured = C.Var.Set.elements captures in
  let describe variables =
    List.map (C.Var.Set.elements variables) ~f:(fun variable ->
        let { expression; _ } = definition_exn variable in
        { Report.id = C.Var.idx variable
        ; name = C.Var.get_name variable
        ; definition = definition_name expression
        ; literal_digest =
            (match expression with
            | Some (C.Constant (String literal)) ->
                Some (Digest.to_hex (Digest.string literal))
            | _ -> None)
        })
  in
  let check_closed (program : C.program) =
    let free = C.Addr.Map.find program.start (Freevars.f program) in
    if not (C.Var.Set.is_empty free)
    then
      failwithf
        "Split program has unbound values: %s"
        (String.concat
           ~sep:", "
           (List.map (C.Var.Set.elements free) ~f:(fun variable ->
                string_of_int (C.Var.idx variable))))
  in
  check_closed eager;
  let roots =
    List.map plans ~f:(fun (id, thunk, root_captures, relocated) ->
        if C.Var.Set.mem thunk root_captures
        then failwith "Deferred thunk is also reachable from eager code or another root";
        if not (C.Var.Set.is_empty (C.Var.Set.inter relocated eager_bound))
        then failwith "Relocated values unexpectedly remain eager";
        let imports =
          if C.Var.Set.is_empty root_captures
          then []
          else
            C.Let (bridge, Prim (Extern ("caml_get_global", None), [ bridge_argument ]))
            :: List.filter_map
                 (List.mapi captured ~f:(fun index variable -> index, variable))
                 ~f:(fun (index, variable) ->
                   if C.Var.Set.mem variable root_captures
                   then Some (C.Let (variable, Field (bridge, index, Non_float)))
                   else None)
        in
        let moved =
          List.concat_map dominator_path ~f:(fun pc ->
              List.filter (C.Addr.Map.find pc program.blocks).body ~f:(function
                | C.Let (variable, _) -> C.Var.Set.mem variable relocated
                | _ -> false))
        in
        let unit_argument = C.Var.fresh_n "split_unit" in
        let result = C.Var.fresh_n "split_result" in
        let start = program.free_pc in
        let block : C.block =
          { params = []
          ; body =
              imports
              @ moved
              @ [ C.Let (unit_argument, Constant (Int Targetint.zero))
                ; C.Let
                    ( result
                    , Apply
                        { f = thunk
                        ; args = [ unit_argument ]
                        ; exact = false
                        ; yielding = May_yield
                        } )
                ]
          ; branch = Stop
          }
        in
        let lazy_ =
          live_program
            { C.start
            ; free_pc = start + 1
            ; blocks =
                C.Addr.Map.filter
                  (fun pc _ -> not (C.Addr.Set.mem pc root_blocks))
                  program.blocks
                |> C.Addr.Map.add start block
            }
        in
        check_closed lazy_;
        let report =
          { Report.captures = describe root_captures
          ; relocated = describe relocated
          ; rounds
          ; globally_optimized_blocks = C.Addr.Map.cardinal program.blocks
          ; eager_blocks = C.Addr.Map.cardinal eager.blocks
          ; lazy_blocks = C.Addr.Map.cardinal lazy_.blocks
          }
        in
        id, clear_call_assumptions lazy_, report)
  in
  { eager_program = clear_call_assumptions eager; roots }

let split_exn program =
  let count = List.length (anchors program) in
  if count <> 1
  then failwithf "Expected exactly one deferred split anchor, found %d" count;
  match split_many_exn program with
  | { eager_program; roots = [ (_, lazy_, report) ] } ->
      { eager = eager_program; lazy_; report }
  | _ -> failwith "Expected exactly one deferred root"
