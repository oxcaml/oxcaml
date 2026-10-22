(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
module K = Flambda_kind
module P = Flambda_primitive
module RE = Rebuilt_expr

(* The continuations closing the regions left by a jump from a [Switch] arm or a
   non-tail application (see [closer]), shared by all such jumps of a region and
   bound at the start of its scope. They are accumulated in the result of the
   rebuilding, keyed by the region. *)
type closers =
  (Continuation.t * RE.continuation_handler) Continuation.Map.t Variable.Map.t

let no_closers = Variable.Map.empty

(* Statistics on the heap allocations rebuilt (blocks, boxed numbers and sets of
   closures: the allocations the analysis handles), printed on standard error
   with the reaper debug flag [escape-stats] (one line per compilation unit) or
   [escape-stats-verbose] (also one line per allocation). Lines are made of
   [key=value] fields separated by single spaces, [at] being the last field: it
   holds the compact debug info of the allocation (see
   [Debuginfo.print_compact]) and may be empty. Sizes are in words, including
   the header.

   reaper-escape-stats: unit=U local_allocs=N local_words=W heap_allocs=N
   heap_words=W heap_candidate_allocs=N heap_candidate_words=W

   reaper-escape-alloc: unit=U result=local|heap candidate=true|false kind=block
   tag=T fields=F words=W at=LOC

   reaper-escape-alloc: ... kind=boxed number=float|... words=W at=LOC

   reaper-escape-alloc: ... kind=closure functions=F value_slots=V words=W
   at=LOC

   The [heap_candidate_*] fields count the allocations the analysis considered
   (see [Escape_graph.Relations.allocated_in]) but left on the heap. Allocations
   that were already local in the input, and candidates unboxed by the reaper
   (which no longer exist), are not counted. *)
type stats =
  { local_allocs : int;
    local_words : int;
    heap_allocs : int;
    heap_words : int;
    heap_candidate_allocs : int;
    heap_candidate_words : int
  }

let empty_stats =
  { local_allocs = 0;
    local_words = 0;
    heap_allocs = 0;
    heap_words = 0;
    heap_candidate_allocs = 0;
    heap_candidate_words = 0
  }

type stats_mode =
  | No_stats
  | Summary
  | Verbose

let stats_mode () =
  if Flambda_features.debug_reaper "escape-stats-verbose"
  then Verbose
  else if Flambda_features.debug_reaper "escape-stats"
  then Summary
  else No_stats

type region =
  { region : Variable.t;
    ghost_region : Variable.t;
    inner_conts : Continuation.Set.t;
    (* [false] for the region of the caller of a function returning at mode
       local ([my_region]), which the function must not close. *)
    closable : bool
  }

type 'r t =
  { solution : Escape_solution.t;
    code_ids_directly_called : Name.t -> Code_id.Set.t Or_unknown.t;
    params_of_continuation : Continuation.t -> Bound_parameter.t list;
    (* For each region scope, the continuations defined inside it. *)
    conts_per_scope : Continuation.Set.t Code_id_or_name.Map.t;
    continuation_info : Traverse_acc.continuation_info Continuation.Map.t;
    machine_width : Target_system.Machine_width.t;
    (* The return continuation of the enclosing function, or of the unit:
       applications returning to it are tail calls. *)
    return_continuation : Continuation.t;
    (* The regions of the enclosing region scopes being rebuilt, innermost
       first. *)
    regions : region list;
    (* Access to the closers in the result of the rebuilding. *)
    closers_of : 'r -> closers;
    with_closers : closers -> 'r -> 'r;
    stats_mode : stats_mode
  }

let create ~solution ~code_ids_directly_called ~params_of_continuation
    ~conts_per_scope ~continuation_info ~machine_width ~return_continuation
    ~closers_of ~with_closers =
  { solution;
    code_ids_directly_called;
    params_of_continuation;
    conts_per_scope;
    continuation_info;
    machine_width;
    return_continuation;
    regions = [];
    closers_of;
    with_closers;
    stats_mode = stats_mode ()
  }

(* Descriptions of the allocations for the statistics (see [stats]). *)
type allocation =
  | Block of
      { tag : int;
        fields : int;
        words : int
      }
  | Boxed_number of
      { number : string;
        words : int
      }
  | Closure of
      { functions : int;
        value_slots : int;
        words : int
      }

let words_of_allocation = function
  | Block { words; _ } | Boxed_number { words; _ } | Closure { words; _ } ->
    words

let print_allocation ppf = function
  | Block { tag; fields; words = _ } ->
    Format.fprintf ppf "kind=block tag=%d fields=%d" tag fields
  | Boxed_number { number; words = _ } ->
    Format.fprintf ppf "kind=boxed number=%s" number
  | Closure { functions; value_slots; words = _ } ->
    Format.fprintf ppf "kind=closure functions=%d value_slots=%d" functions
      value_slots

(* Sizes in words of the fields of blocks, on 64-bit targets. *)
let words_of_naked_number (kind : K.Naked_number_kind.t) =
  match kind with
  | Naked_immediate | Naked_float32 | Naked_float | Naked_int8 | Naked_int16
  | Naked_int32 | Naked_int64 | Naked_nativeint | Naked_mask ->
    1
  | Naked_vec128 -> 2
  | Naked_vec256 -> 4
  | Naked_vec512 -> 8

let words_of_kind (kind : K.t) =
  match kind with
  | Value -> 1
  | Naked_number kind -> words_of_naked_number kind
  | Region | Rec_info -> 0

let block_allocation (block_kind : P.Block_kind.t) ~num_fields ~dbg =
  let tag, _ = P.Block_kind.to_shape block_kind in
  let field_words =
    match block_kind with
    | Values _ | Naked_floats -> num_fields
    | Mixed (_, shape) -> K.Mixed_block_shape.size_in_words shape
  in
  ( Block { tag = Tag.to_int tag; fields = num_fields; words = 1 + field_words },
    dbg )

(* Boxed floats are plain blocks, boxed vectors flat blocks, and the other boxed
   numbers custom blocks with a pointer to their operations (see the headers in
   [Cmm_helpers]). *)
let boxed_number_allocation (kind : K.Boxable_number.t) ~dbg =
  let number, words =
    match kind with
    | Naked_float -> "float", 2
    | Naked_float32 -> "float32", 3
    | Naked_int32 -> "int32", 3
    | Naked_int64 -> "int64", 3
    | Naked_nativeint -> "nativeint", 3
    | Naked_vec128 -> "vec128", 3
    | Naked_vec256 -> "vec256", 5
    | Naked_vec512 -> "vec512", 9
    | Naked_mask -> "mask", 2
  in
  Boxed_number { number; words }, dbg

(* The layout of [Slot_offsets]: the function slots, each preceded by an infix
   header except the first, then the value slots. *)
let closure_allocation set_of_closures ~code_metadata =
  let funs =
    Function_declarations.funs (Set_of_closures.function_decls set_of_closures)
  in
  let functions = Function_slot.Map.cardinal funs in
  let function_words, dbg =
    Function_slot.Map.fold
      (fun _ (decl : Function_declarations.code_id_in_function_declaration)
           (words, dbg) ->
        match decl with
        | Deleted { function_slot_size; dbg = dbg' } ->
          ( words + function_slot_size,
            if Debuginfo.is_none dbg then dbg' else dbg )
        | Code_id { code_id; only_full_applications = _ } ->
          let code_metadata = code_metadata code_id in
          ( words + Code_metadata.function_slot_size code_metadata,
            if Debuginfo.is_none dbg
            then Code_metadata.dbg code_metadata
            else dbg ))
      funs (0, Debuginfo.none)
  in
  let value_slots = Set_of_closures.value_slots set_of_closures in
  let value_slot_words =
    Value_slot.Map.fold
      (fun slot _ words -> words + words_of_kind (Value_slot.kind slot))
      value_slots 0
  in
  ( Closure
      { functions;
        value_slots = Value_slot.Map.cardinal value_slots;
        words = 1 + function_words + max 0 (functions - 1) + value_slot_words
      },
    dbg )

let unit_name () =
  Compilation_unit.full_path_as_string (Current_unit.get_cu_exn ())

let record_allocation t stats ~is_local ~candidate allocation =
  match t.stats_mode with
  | No_stats -> stats
  | Summary | Verbose ->
    let allocation, dbg = allocation () in
    let words = words_of_allocation allocation in
    (match t.stats_mode with
    | Verbose ->
      Format.eprintf
        "reaper-escape-alloc: unit=%s result=%s candidate=%b %a words=%d \
         at=%a@."
        (unit_name ())
        (if is_local then "local" else "heap")
        candidate print_allocation allocation words Debuginfo.print_compact dbg
    | No_stats | Summary -> ());
    if is_local
    then
      { stats with
        local_allocs = stats.local_allocs + 1;
        local_words = stats.local_words + words
      }
    else
      let stats =
        { stats with
          heap_allocs = stats.heap_allocs + 1;
          heap_words = stats.heap_words + words
        }
      in
      if candidate
      then
        { stats with
          heap_candidate_allocs = stats.heap_candidate_allocs + 1;
          heap_candidate_words = stats.heap_candidate_words + words
        }
      else stats

let report_stats t stats =
  match t.stats_mode with
  | No_stats -> ()
  | Summary | Verbose ->
    Format.eprintf
      "reaper-escape-stats: unit=%s local_allocs=%d local_words=%d \
       heap_allocs=%d heap_words=%d heap_candidate_allocs=%d \
       heap_candidate_words=%d@."
      (unit_name ()) stats.local_allocs stats.local_words stats.heap_allocs
      stats.heap_words stats.heap_candidate_allocs stats.heap_candidate_words

(* Turn a heap allocation into a stack allocation in the region of the innermost
   region scope when the escape analysis has decided it is local, and record it
   in the statistics. Allocations already local in the input are not ours. *)
let localize t stats ~is_local ~candidate ~allocation
    (alloc_mode : Alloc_mode.For_allocations.t) =
  match alloc_mode, t.regions with
  | Local _, _ -> alloc_mode, stats
  | Heap { alloc_region }, { region; _ } :: _ when is_local ->
    ( Alloc_mode.For_allocations.local ~alloc_region ~region,
      record_allocation t stats ~is_local:true ~candidate allocation )
  | Heap _, _ ->
    alloc_mode, record_allocation t stats ~is_local:false ~candidate allocation

let localize_make_block t stats bv block_kind alloc_mode ~num_fields ~dbg =
  let var = Bound_var.var bv in
  localize t stats
    ~is_local:(Escape_solution.is_local_allocation t.solution var)
    ~candidate:(Escape_solution.is_candidate t.solution var)
    ~allocation:(fun () -> block_allocation block_kind ~num_fields ~dbg)
    alloc_mode

let localize_set_of_closures t stats bvs set_of_closures ~code_metadata
    alloc_mode =
  let vars = List.map Bound_var.var bvs in
  localize t stats
    ~is_local:
      (List.for_all (Escape_solution.is_local_allocation t.solution) vars)
    ~candidate:(List.for_all (Escape_solution.is_candidate t.solution) vars)
    ~allocation:(fun () -> closure_allocation set_of_closures ~code_metadata)
    alloc_mode

let localize_box_number t stats bv (defining_expr : Named.t) =
  match[@ocaml.warning "-fragile-match"] defining_expr with
  | Prim (Unary (Box_number (bn, alloc_mode), contents), dbg) ->
    let var = Bound_var.var bv in
    let alloc_mode', stats =
      localize t stats
        ~is_local:(Escape_solution.is_local_allocation t.solution var)
        ~candidate:(Escape_solution.is_candidate t.solution var)
        ~allocation:(fun () -> boxed_number_allocation bn ~dbg)
        alloc_mode
    in
    if alloc_mode' == alloc_mode
    then defining_expr, stats
    else
      ( Named.create_prim (Unary (Box_number (bn, alloc_mode'), contents)) dbg,
        stats )
  | Prim _ | Simple _ | Set_of_closures _ | Static_consts _ | Rec_info _ ->
    defining_expr, stats

(* The regions to close when jumping to continuation [k] from the current point:
   those of the enclosing region scopes in which [k] is not defined. Nothing is
   closed on the exceptional path (see [Escape_analysis]). *)
let regions_exited_by t k =
  match t.regions with
  | [] -> []
  | _ :: _ ->
    let is_exn_handler =
      match Continuation.Map.find_opt k t.continuation_info with
      | Some { Traverse_acc.is_exn_handler; _ } -> is_exn_handler
      | None ->
        Misc.fatal_errorf
          "[regions_exited_by]: no information about continuation %a"
          Continuation.print k
    in
    if is_exn_handler
    then []
    else
      List.filter
        (fun { inner_conts; closable; _ } ->
          closable && not (Continuation.Set.mem k inner_conts))
        t.regions

let end_regions t regions (body : RE.t) : RE.t =
  let end_region ~ghost region body =
    let end_region : P.t = Unary (End_region { ghost }, Simple.var region) in
    RE.create_let
      (Bound_pattern.singleton
         (Bound_var.create
            (Variable.create "region_end" K.value)
            Flambda_debug_uid.none Name_mode.normal))
      (Named.create_prim end_region Debuginfo.none)
      ~size_of_defining_expr:
        (Code_size.prim ~machine_width:t.machine_width end_region)
      ~body
  in
  (* The regions are innermost first. Restoring the outermost exited checkpoint
     also frees the nested regions; restoring an inner checkpoint afterwards
     would retain allocations that should have been freed. *)
  match List.rev regions with
  | [] -> body
  | { region; ghost_region; _ } :: _ ->
    end_region ~ghost:false region (end_region ~ghost:true ghost_region body)

let begin_regions t { region; ghost_region; _ } (body : RE.t) : RE.t =
  let begin_region ~ghost region body =
    let begin_region : P.t = Variadic (Begin_region { ghost }, []) in
    RE.create_let
      (Bound_pattern.singleton
         (Bound_var.create region Flambda_debug_uid.none Name_mode.normal))
      (Named.create_prim begin_region Debuginfo.none)
      ~size_of_defining_expr:
        (Code_size.prim ~machine_width:t.machine_width begin_region)
      ~body
  in
  begin_region ~ghost:false region (begin_region ~ghost:true ghost_region body)

let end_regions_before_jump t k body =
  end_regions t (regions_exited_by t k) body

(* Calls to functions made to return at mode local (see [Escape_analysis])
   receive their result in the region of the innermost enclosing scope. *)
let set_local_return_mode t apply =
  let returns_local code_id =
    Escape_solution.returns_local t.solution code_id
  in
  (* The callee may be given by the call kind, or be one of the functions the
     callee expression is known to evaluate to (which is how [Rebuild] changes
     the calling convention of known-arity indirect calls). *)
  let callee_code_ids_return_local () =
    match Apply.callee apply with
    | None -> false
    | Some callee ->
      Simple.pattern_match callee
        ~const:(fun _ -> false)
        ~name:(fun name ~coercion:_ ->
          match t.code_ids_directly_called name with
          | Unknown -> false
          | Known code_ids -> Code_id.Set.exists returns_local code_ids)
  in
  let callee_returns_local =
    match Apply.call_kind apply with
    | Function { function_call = Direct code_id; _ } ->
      returns_local code_id || callee_code_ids_return_local ()
    | Function { function_call = Indirect_known_arity _; _ } ->
      callee_code_ids_return_local ()
    | Function { function_call = Indirect_unknown_arity; _ }
    | Method _ | C_call _ | Effect _ ->
      false
  in
  if not callee_returns_local
  then apply
  else
    match t.regions with
    | [] ->
      Misc.fatal_errorf "No region for the local result of application %a"
        Apply.print apply
    | { region; ghost_region; _ } :: _ ->
      let alloc_region =
        Alloc_mode.For_applications.alloc_region (Apply.return_mode apply)
      in
      Apply.with_return_mode apply
        (Alloc_mode.For_applications.maybe_alloc_stack ~alloc_region ~region
           ~ghost_region)

(* A continuation that closes the regions left when jumping to [k], then jumps
   to [k]. Used where an [End_region] cannot simply be inserted before the jump:
   [Switch] arms, and non-tail applications returning to [k]. *)
let closer t closers k =
  match t.regions with
  | [] ->
    Misc.fatal_errorf "[closer]: no region to close before jumping to %a"
      Continuation.print k
  | innermost :: _ -> (
    let region_closers =
      Option.value ~default:Continuation.Map.empty
        (Variable.Map.find_opt innermost.region closers)
    in
    match Continuation.Map.find_opt k region_closers with
    | Some (k_close, _) -> k_close, closers
    | None ->
      let k_close = Continuation.create () in
      let params = t.params_of_continuation k in
      let apply_cont =
        Apply_cont_expr.create k
          ~args:(List.map (fun p -> Simple.var (Bound_parameter.var p)) params)
          ~dbg:Debuginfo.none
      in
      let handler =
        RE.from_expr
          ~expr:(Expr.create_apply_cont apply_cont)
          ~free_names:(Apply_cont_expr.free_names apply_cont)
          ~code_size:(Code_size.apply_cont apply_cont)
      in
      let handler = end_regions t (regions_exited_by t k) handler in
      let handler =
        RE.create_continuation_handler
          (Bound_parameters.create params)
          ~handler ~is_exn_handler:false ~is_cold:false
      in
      ( k_close,
        Variable.Map.add innermost.region
          (Continuation.Map.add k (k_close, handler) region_closers)
          closers ))

let redirect_switch_arms t closers arms =
  Target_ocaml_int.Map.fold
    (fun discriminant arm (arms, closers) ->
      let action = Switch_arm.action arm in
      let k = Apply_cont_expr.continuation action in
      match regions_exited_by t k with
      | [] -> Target_ocaml_int.Map.add discriminant arm arms, closers
      | _ :: _ ->
        let k_close, closers = closer t closers k in
        let arm =
          Switch_arm.create
            ~annotations:(Switch_arm.annotations arm)
            (Apply_cont_expr.with_continuation action k_close)
        in
        Target_ocaml_int.Map.add discriminant arm arms, closers)
    arms
    (Target_ocaml_int.Map.empty, closers)

let rebuild_apply t closers apply ~rebuild =
  let apply = set_local_return_mode t apply in
  let expr : RE.t = rebuild apply in
  match Apply.continuation apply with
  | Never_returns -> expr, closers
  | Return k -> (
    match regions_exited_by t k with
    | [] -> expr, closers
    | regions ->
      let is_tail_call =
        Continuation.equal k t.return_continuation
        && match Apply.position apply with Normal -> true | Nontail -> false
      in
      if is_tail_call
      then
        (* Tail call: close the regions beforehand so that it remains a tail
           call; no local value is passed to it (see
           [Escape_traverse.record_apply]). *)
        end_regions t regions expr, closers
      else
        let k_close, closers = closer t closers k in
        let renaming = Renaming.add_continuation Renaming.empty k k_close in
        ( RE.from_expr
            ~expr:(Expr.apply_renaming expr.expr renaming)
            ~free_names:
              (Name_occurrences.apply_renaming expr.free_names renaming)
            ~code_size:expr.code_size,
          closers ))

(* Rebuild the body of a region scope (a function body or a recursive
   continuation handler), adding a region around it if the escape analysis found
   local allocations in it: [Begin_region] at the start, and [End_region] on
   every exit, which [Rebuild.rebuild_expr] inserts when rebuilding the jumps to
   continuations defined outside the scope. Tail calls are preserved by closing
   the regions before them (no local value may be passed to them, see
   [Escape_traverse.record_apply]). Nothing is done on the exceptional path: the
   exception handler that catches the exception closes the region (as for
   regions introduced by the type checker), or, failing that, an enclosing
   region. *)
let rebuild_region_scope t ~scope ~rebuild_body =
  if not (Escape_solution.scope_needs_region t.solution scope)
  then rebuild_body t
  else
    let region =
      { region = Variable.create "region" K.region;
        ghost_region = Variable.create "ghost_region" K.region;
        inner_conts =
          (match Code_id_or_name.Map.find_opt scope t.conts_per_scope with
          | Some conts -> conts
          | None -> Continuation.Set.empty);
        closable = true
      }
    in
    let body, res = rebuild_body { t with regions = region :: t.regions } in
    let closers = t.closers_of res in
    let body =
      match Variable.Map.find_opt region.region closers with
      | None -> body
      | Some region_closers ->
        Continuation.Map.fold
          (fun _ (k_close, handler) body ->
            RE.create_non_recursive_let_cont k_close handler ~body)
          region_closers body
    in
    let res = t.with_closers (Variable.Map.remove region.region closers) res in
    begin_regions t region body, res

(* A function returning at mode local allocates in its caller's region, which it
   must not close (see [Escape_analysis]). *)
let enter_function t ~code_id ~return_continuation
    ~(my_alloc_mode : Alloc_mode.For_applications.t) =
  let t = { t with return_continuation; regions = [] } in
  if not (Escape_solution.returns_local t.solution code_id)
  then my_alloc_mode, t
  else
    match my_alloc_mode with
    | Maybe_alloc_stack _ ->
      Misc.fatal_errorf
        "Function %a already returns at mode local, but the escape analysis \
         decided to make it do so"
        Code_id.print code_id
    | Not_alloc_stack { alloc_region } ->
      let region = Variable.create "my_region" K.region in
      let ghost_region = Variable.create "my_ghost_region" K.region in
      ( Alloc_mode.For_applications.maybe_alloc_stack ~alloc_region ~region
          ~ghost_region,
        { t with
          regions =
            [ { region;
                ghost_region;
                inner_conts = Continuation.Set.empty;
                closable = false
              } ]
        } )

let rebuild_function_body t ~code_id ~rebuild_body =
  if Escape_solution.returns_local t.solution code_id
  then rebuild_body t
  else
    rebuild_region_scope t
      ~scope:(Code_id_or_name.code_id code_id)
      ~rebuild_body
