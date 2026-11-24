(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(*= OxCaml-Written-By: ai:fable-5.2
    OxCaml-Review-Status: ai:fable-5.2:xhigh *)

open! Flambda.Import
module K = Flambda_kind
module P = Flambda_primitive
module RE = Rebuilt_expr

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

   reaper-escape-local-return: unit=U code=C own_region=true|false

   The last one is printed (in verbose mode) for each function made to return at
   mode local, and tells whether it has a region of its own (see
   [Escape_exclave]).

   The [heap_candidate_*] fields count the allocations the analysis considered
   (see [Escape_graph.Relations.allocated_in]) but left on the heap. Allocations
   that were already local in the input, and allocations removed by the reaper
   (see [allocation_is_kept]), are not counted. The allocations are described as
   they are before rebuilding, which may still remove some of their fields. *)
type stats =
  { local_allocs : int;
    local_words : int;
    heap_allocs : int;
    heap_words : int;
    heap_candidate_allocs : int;
    heap_candidate_words : int
  }

(* What rebuilding accumulates: the statistics, and for each region we
   introduce, the continuations closing the regions left by a jump from a
   [Switch] arm or a non-tail application (see [closer]), keyed by the target of
   the jump. These continuations are shared by all such jumps for which that
   region is the innermost one; they are bound at the start of its scope (see
   [leave_region_scope]), which removes them from the accumulator. *)
type acc =
  { stats : stats;
    closers :
      (Continuation.t * RE.continuation_handler) Continuation.Map.t
      Variable.Map.t
  }

let empty_acc =
  { stats =
      { local_allocs = 0;
        local_words = 0;
        heap_allocs = 0;
        heap_words = 0;
        heap_candidate_allocs = 0;
        heap_candidate_words = 0
      };
    closers = Variable.Map.empty
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

type region_scope =
  | Scope of Code_id_or_name.t  (** The region spans the region scope. *)
  | Callers_region of { returned : Code_id_or_name.Set.t }
      (** The region of the caller of a function returning at mode local
          ([my_region]), which the function must not close. [returned] are the
          local allocations that the function may return. *)
  | Before_exclave of
      { scope : Code_id_or_name.t;
        plan : Escape_exclave.t
      }
      (** The region of a function returning at mode local, for its temporaries:
          it is closed before what the function returns is allocated (see
          [Escape_exclave]). *)

type region =
  { region : Variable.t;
    ghost_region : Variable.t;
    (* The ghost region is only needed by the applications whose result is
       allocated in the region (see [set_local_return_mode]); as Simplify would,
       we do not open it otherwise. *)
    has_ghost_region : bool;
    scope : region_scope
  }

type enabled =
  { graph : Escape_graph.t;
    solution : Escape_solution.t;
    code_ids_directly_called : Name.t -> Code_id.Set.t Or_unknown.t;
    allocation_is_kept : Variable.t -> bool;
    get_code_metadata : Code_id.t -> Code_metadata.t;
    continuation_info : Traverse_acc.Continuation_info.t Continuation.Map.t;
    machine_width : Target_system.Machine_width.t;
    (* The return continuation of the enclosing function, if any: applications
       returning to it are tail calls. *)
    return_continuation : Continuation.t option;
    (* The regions of the enclosing region scopes being rebuilt, innermost
       first. *)
    regions : region list;
    (* For a function returning at mode local that has a region of its own:
       where to close it, and the region. *)
    exclave : (Escape_exclave.t * region) option;
    (* The continuation whose handler is being rebuilt, if any. *)
    block : Continuation.t option;
    stats_mode : stats_mode
  }

(* When the analysis has not been run, every function below leaves what it is
   given unchanged. *)
type t =
  | Disabled
  | Enabled of enabled

let create graph ~is_unboxed ~allocation_is_kept ~code_ids_directly_called
    ~get_code_metadata ~continuation_info ~machine_width =
  match Escape_graph.solved graph with
  | None -> Disabled
  | Some db ->
    Enabled
      { graph;
        solution = Escape_solution.create db ~is_unboxed;
        code_ids_directly_called;
        allocation_is_kept;
        get_code_metadata;
        continuation_info;
        machine_width;
        return_continuation = None;
        regions = [];
        exclave = None;
        block = None;
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
let closure_allocation t (set_of_closures : Rev_expr.rev_set_of_closures) =
  let funs = Function_declarations.funs set_of_closures.function_decls in
  let functions = Function_slot.Map.cardinal funs in
  let function_words =
    Function_slot.Map.fold
      (fun _ (decl : Function_declarations.code_id_in_function_declaration)
           words ->
        match decl with
        | Deleted { function_slot_size; dbg = _ } -> words + function_slot_size
        | Code_id { code_id; only_full_applications = _ } ->
          words + Code_metadata.function_slot_size (t.get_code_metadata code_id))
      funs 0
  in
  let value_slot_words =
    Value_slot.Map.fold
      (fun slot _ words -> words + words_of_kind (Value_slot.kind slot))
      set_of_closures.value_slots 0
  in
  ( Closure
      { functions;
        value_slots = Value_slot.Map.cardinal set_of_closures.value_slots;
        words = 1 + function_words + max 0 (functions - 1) + value_slot_words
      },
    set_of_closures.dbg )

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

let report_stats t { stats; closers = _ } =
  match t with
  | Disabled -> ()
  | Enabled { stats_mode = No_stats; _ } -> ()
  | Enabled { stats_mode = Summary | Verbose; _ } ->
    Format.eprintf
      "reaper-escape-stats: unit=%s local_allocs=%d local_words=%d \
       heap_allocs=%d heap_words=%d heap_candidate_allocs=%d \
       heap_candidate_words=%d@."
      (unit_name ()) stats.local_allocs stats.local_words stats.heap_allocs
      stats.heap_words stats.heap_candidate_allocs stats.heap_candidate_words

(* A function returning at mode local allocates in the region of its caller
   whenever it has no region of its own open (see [Escape_exclave]). The local
   allocations that it makes there without returning them (the residual
   allocations) are only freed when its caller's region is closed. Unless the
   option [-X reaper-escape-localize-residual=1] is given, they are left on the
   heap instead, when that is possible: a heap block must not point at the local
   stack, so the ones that may contain a local allocation stay local. *)
let localize_residual =
  Oxcaml_args.Extra_options.bool __LOC__ "reaper-escape-localize-residual"

let is_residual t ~returned vars =
  (not (localize_residual ()))
  && List.for_all
       (fun var ->
         let alloc = Code_id_or_name.var var in
         (not (Code_id_or_name.Set.mem alloc returned))
         && Code_id_or_name.Set.is_empty
              (Escape_solution.local_contents t.solution alloc))
       vars

(* The mode for a heap allocation bound to [vars]: a stack allocation in the
   region of the innermost region scope when the escape analysis has decided it
   is local. The allocation is recorded in the statistics if the reaper keeps
   it. Allocations already local in the input are not ours. *)
let localize_alloc_mode t acc vars ~allocation
    (alloc_mode : Alloc_mode.For_allocations.t) =
  match alloc_mode with
  | Local _ -> alloc_mode, acc
  | Heap { alloc_region } -> (
    let is_local =
      List.for_all (Escape_solution.is_local_allocation t.solution) vars
    in
    let record ~is_local =
      if List.exists t.allocation_is_kept vars
      then
        { acc with
          stats =
            record_allocation t acc.stats ~is_local
              ~candidate:
                (List.for_all (Escape_solution.is_candidate t.solution) vars)
              allocation
        }
      else acc
    in
    let is_local =
      is_local
      &&
      match t.regions with
      | { scope = Callers_region { returned }; _ } :: _ ->
        not (is_residual t ~returned vars)
      | [] | { scope = Scope _ | Before_exclave _; _ } :: _ -> true
    in
    match t.regions with
    | { region; _ } :: _ when is_local ->
      ( Alloc_mode.For_allocations.local ~alloc_region ~region,
        record ~is_local:true )
    | [] | _ :: _ -> alloc_mode, record ~is_local:false)

let localize t acc (bound_pattern : Bound_pattern.t)
    (defining_expr : Rev_expr.rev_named) : Rev_expr.rev_named * acc =
  match t with
  | Disabled -> defining_expr, acc
  | Enabled t -> (
    match[@ocaml.warning "-fragile-match"] bound_pattern, defining_expr with
    | ( Singleton bv,
        Named
          (Prim
             ( Variadic (Make_block (block_kind, mutability, alloc_mode), fields),
               dbg )) ) ->
      let alloc_mode', acc =
        localize_alloc_mode t acc
          [Bound_var.var bv]
          ~allocation:(fun () ->
            block_allocation block_kind ~num_fields:(List.length fields) ~dbg)
          alloc_mode
      in
      if alloc_mode' == alloc_mode
      then defining_expr, acc
      else
        ( Named
            (Named.create_prim
               (Variadic
                  (Make_block (block_kind, mutability, alloc_mode'), fields))
               dbg),
          acc )
    | Singleton bv, Named (Prim (Unary (Box_number (bn, alloc_mode), arg), dbg))
      ->
      let alloc_mode', acc =
        localize_alloc_mode t acc
          [Bound_var.var bv]
          ~allocation:(fun () -> boxed_number_allocation bn ~dbg)
          alloc_mode
      in
      if alloc_mode' == alloc_mode
      then defining_expr, acc
      else
        ( Named
            (Named.create_prim (Unary (Box_number (bn, alloc_mode'), arg)) dbg),
          acc )
    | Set_of_closures bvs, Set_of_closures (set_of_closures, alloc_mode) ->
      let alloc_mode', acc =
        localize_alloc_mode t acc
          (List.map Bound_var.var bvs)
          ~allocation:(fun () -> closure_allocation t set_of_closures)
          alloc_mode
      in
      if alloc_mode' == alloc_mode
      then defining_expr, acc
      else Set_of_closures (set_of_closures, alloc_mode'), acc
    | _ -> defining_expr, acc)

let adjust_code_metadata t code_id code_metadata =
  match t with
  | Disabled -> code_metadata
  | Enabled t ->
    Escape_solution.adjust_code_metadata t.solution code_id code_metadata

(* The regions to close when jumping to continuation [k] from the current point:
   those of the enclosing region scopes in which [k] is not defined. Nothing is
   closed on the exceptional path (see [Escape_analysis]). *)
let regions_exited_by t k =
  match t.regions with
  | [] -> []
  | _ :: _ ->
    let is_exn_handler =
      match Continuation.Map.find_opt k t.continuation_info with
      | Some { is_exn_handler; _ } -> is_exn_handler
      | None ->
        Misc.fatal_errorf
          "[regions_exited_by]: no information about continuation %a"
          Continuation.print k
    in
    if is_exn_handler
    then []
    else
      List.filter
        (fun { scope; _ } ->
          match scope with
          | Callers_region _ -> false
          | Scope scope ->
            not (Escape_graph.scope_contains_continuation t.graph ~scope k)
          | Before_exclave { scope; plan } ->
            (not (Escape_graph.scope_contains_continuation t.graph ~scope k))
            || Escape_exclave.closed_before_jump plan k)
        t.regions

let end_regions t regions (body : RE.t) : RE.t =
  let end_region ~ghost region body =
    let end_region : P.t = Unary (End_region { ghost }, Simple.var region) in
    RE.create_let
      (Bound_pattern.singleton
         (Bound_var.create
            (Variable.create "unit" K.value)
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
  | { region; ghost_region; has_ghost_region; _ } :: _ ->
    end_region ~ghost:false region
      (if has_ghost_region
       then end_region ~ghost:true ghost_region body
       else body)

(* As in [Lambda_to_flambda], a region is given the enclosing one as its
   parent. *)
let begin_regions t ({ region; ghost_region; has_ghost_region; _ } as opened)
    (body : RE.t) : RE.t =
  let rec parent regions =
    match regions with
    | [] -> None
    | region :: regions -> (
      if region != opened
      then parent regions
      else match regions with [] -> None | parent :: _ -> Some parent)
  in
  let parent = parent t.regions in
  let begin_region ~ghost region body =
    let parent =
      Option.map
        (fun { region; ghost_region; _ } ->
          Simple.var (if ghost then ghost_region else region))
        parent
    in
    let begin_region : P.t =
      Variadic (Begin_region { ghost }, Option.to_list parent)
    in
    RE.create_let
      (Bound_pattern.singleton
         (Bound_var.create region Flambda_debug_uid.none Name_mode.normal))
      (Named.create_prim begin_region Debuginfo.none)
      ~size_of_defining_expr:
        (Code_size.prim ~machine_width:t.machine_width begin_region)
      ~body
  in
  begin_region ~ghost:false region
    (if has_ghost_region
     then begin_region ~ghost:true ghost_region body
     else body)

let end_regions_before_jump t k body =
  match t with
  | Disabled -> body
  | Enabled t -> end_regions t (regions_exited_by t k) body

(* Calls to functions made to return at mode local (see [Escape_analysis])
   receive their result in the region of the innermost enclosing scope. So do
   the unknown-arity applications whose result is a partial application treated
   as a local allocation: with a global return mode, the generic application
   functions would free it before returning. *)
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
    | Function { function_call = Indirect_unknown_arity; _ } -> (
      match Apply.continuation apply with
      | Never_returns -> false
      | Return k -> (
        match Continuation.Map.find_opt k t.continuation_info with
        | Some { params; _ } ->
          List.exists (Escape_solution.is_local_allocation t.solution) params
        | None ->
          Misc.fatal_errorf
            "[set_local_return_mode]: no information about continuation %a"
            Continuation.print k))
    | Method _ | C_call _ | Effect _ -> false
  in
  if not callee_returns_local
  then apply
  else
    match t.regions with
    | [] ->
      Misc.fatal_errorf "No region for the local result of application %a"
        Apply.print apply
    | { has_ghost_region = false; _ } :: _ ->
      Misc.fatal_errorf "No ghost region for the local result of application %a"
        Apply.print apply
    | { region; ghost_region; has_ghost_region = true; _ } :: _ ->
      let alloc_region =
        Alloc_mode.For_applications.alloc_region (Apply.return_mode apply)
      in
      Apply.with_return_mode apply
        (Alloc_mode.For_applications.maybe_alloc_stack ~alloc_region ~region
           ~ghost_region)

(* A continuation that closes the regions left when jumping to [k], then jumps
   to [k]. Used where an [End_region] cannot simply be inserted before the jump:
   [Switch] arms, and non-tail applications returning to [k]. *)
let closer t acc ~params_of_continuation k =
  match t.regions with
  | [] ->
    Misc.fatal_errorf "[closer]: no region to close before jumping to %a"
      Continuation.print k
  | innermost :: _ -> (
    let closers =
      match Variable.Map.find_opt innermost.region acc.closers with
      | Some closers -> closers
      | None -> Continuation.Map.empty
    in
    match Continuation.Map.find_opt k closers with
    | Some (k_close, _) -> k_close, acc
    | None ->
      let k_close = Continuation.create () in
      (* Named as in [Lambda_to_flambda]. *)
      let params =
        List.map
          (fun param ->
            Bound_parameter.create
              (Variable.create "region_return"
                 (Flambda_kind.With_subkind.kind (Bound_parameter.kind param)))
              (Bound_parameter.kind param)
              Flambda_debug_uid.none)
          (params_of_continuation k)
      in
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
      let closers = Continuation.Map.add k (k_close, handler) closers in
      ( k_close,
        { acc with
          closers = Variable.Map.add innermost.region closers acc.closers
        } ))

let redirect_switch_arms t acc ~params_of_continuation arms =
  match t with
  | Disabled | Enabled { regions = []; _ } -> arms, acc
  | Enabled ({ regions = _ :: _; _ } as t) ->
    Target_ocaml_int.Map.fold
      (fun discr arm (arms, acc) ->
        let action = Switch_arm.action arm in
        let k = Apply_cont_expr.continuation action in
        match regions_exited_by t k with
        | [] -> arms, acc
        | _ :: _ ->
          let k_close, acc = closer t acc ~params_of_continuation k in
          let arm =
            Switch_arm.create
              ~annotations:(Switch_arm.annotations arm)
              (Apply_cont_expr.with_continuation action k_close)
          in
          Target_ocaml_int.Map.add discr arm arms, acc)
      arms (arms, acc)

let rebuild_apply_in_regions t acc ~params_of_continuation apply ~rebuild :
    RE.t * acc =
  let apply = set_local_return_mode t apply in
  let expr : RE.t = rebuild apply in
  match Apply.continuation apply with
  | Never_returns -> expr, acc
  | Return k -> (
    match regions_exited_by t k with
    | [] -> expr, acc
    | _ :: _ as regions ->
      let is_tail_call =
        Option.equal Continuation.equal (Some k) t.return_continuation
        && match Apply.position apply with Normal -> true | Nontail -> false
      in
      if is_tail_call
      then
        (* Tail call: close the regions beforehand so that it remains a tail
           call; no local value is passed to it (see
           [Escape_traverse.record_apply]). *)
        end_regions t regions expr, acc
      else
        let k_close, acc = closer t acc ~params_of_continuation k in
        let renaming = Renaming.add_continuation Renaming.empty k k_close in
        ( RE.from_expr
            ~expr:(Expr.apply_renaming expr.expr renaming)
            ~free_names:
              (Name_occurrences.apply_renaming expr.free_names renaming)
            ~code_size:expr.code_size,
          acc ))

let without_region t region =
  { t with
    regions =
      List.filter
        (fun { region = region'; _ } ->
          not (Variable.equal region.region region'))
        t.regions
  }

let rebuild_apply t acc ~params_of_continuation apply ~rebuild : RE.t * acc =
  match t with
  | Disabled -> rebuild apply, acc
  | Enabled t -> (
    match t.exclave with
    | Some (plan, region)
      when Escape_exclave.closed_before_apply plan t.block
           && List.memq region t.regions ->
      (* The callee allocates what it returns in the region of our caller. *)
      let expr, acc =
        rebuild_apply_in_regions (without_region t region) acc
          ~params_of_continuation apply ~rebuild
      in
      end_regions t [region] expr, acc
    | Some _ | None ->
      rebuild_apply_in_regions t acc ~params_of_continuation apply ~rebuild)

(* A region scope (a function body or the handler of a recursive continuation)
   gets a region if the escape analysis found local allocations in it:
   [Begin_region] at the start, and [End_region] on every exit, which
   [Rebuild.rebuild_expr] inserts when rebuilding the jumps to continuations
   defined outside the scope. Tail calls are preserved by closing the regions
   before them (no local value may be passed to them, see
   [Escape_traverse.record_apply]). Nothing is done on the exceptional path: the
   exception handler that catches the exception closes the region (as for
   regions introduced by the type checker), or, failing that, an enclosing
   region. *)
let enter_region_scope t ~scope =
  if not (Escape_solution.scope_needs_region t.solution scope)
  then t
  else
    let region =
      { region = Variable.create "region" K.region;
        ghost_region = Variable.create "ghost_region" K.region;
        has_ghost_region =
          Escape_solution.scope_has_local_call_result t.solution scope;
        scope = Scope scope
      }
    in
    { t with regions = region :: t.regions }

(* Open the region around the body of its scope, after binding the continuations
   closing it (see [closer]). *)
let open_region t acc region (body : RE.t) =
  let body, acc =
    match Variable.Map.find_opt region.region acc.closers with
    | None -> body, acc
    | Some closers ->
      ( Continuation.Map.fold
          (fun _ (k_close, handler) body ->
            RE.create_non_recursive_let_cont k_close handler ~body)
          closers body,
        { acc with closers = Variable.Map.remove region.region acc.closers } )
  in
  begin_regions t region body, acc

(* [t] must be the result of [enter_region_scope] for [scope]. *)
let leave_region_scope t acc ~scope (body : RE.t) =
  if not (Escape_solution.scope_needs_region t.solution scope)
  then body, acc
  else
    match t.regions with
    | ({ scope = Scope scope'; _ } as region) :: _
      when Code_id_or_name.equal scope scope' ->
      open_region t acc region body
    | [] | { scope = Scope _ | Callers_region _ | Before_exclave _; _ } :: _ ->
      Misc.fatal_errorf
        "[leave_region_scope]: the innermost region is not the one of %a"
        Code_id_or_name.print scope

let recursive_scope t cont =
  Option.map Code_id_or_name.var (Escape_graph.recursive_scope t.graph cont)

(* The handlers of the zone of a function returning at mode local run after its
   region has been closed (see [Escape_exclave]). *)
let enter_handler t cont =
  match t with
  | Disabled -> t
  | Enabled t -> (
    let t = { t with block = Some cont } in
    let t =
      match t.exclave with
      | Some (plan, region) when Escape_exclave.in_zone plan cont ->
        without_region t region
      | Some _ | None -> t
    in
    match recursive_scope t cont with
    | None -> Enabled t
    | Some scope -> Enabled (enter_region_scope t ~scope))

let leave_handler t acc cont body =
  match t with
  | Disabled -> body, acc
  | Enabled t -> (
    let body, acc =
      match recursive_scope t cont with
      | None -> body, acc
      | Some scope -> leave_region_scope t acc ~scope body
    in
    match t.exclave with
    | Some (plan, region) when Escape_exclave.closed_on_entry plan cont ->
      end_regions t [region] body, acc
    | Some _ | None -> body, acc)

(* A function returning at mode local allocates what it returns in its caller's
   region, which it must not close (see [Escape_analysis]); it may have a region
   of its own for the rest (see [Escape_exclave]). *)
let enter_function t ~code_id ~return_continuation
    ~(my_alloc_mode : Alloc_mode.For_applications.t) body =
  match t with
  | Disabled -> my_alloc_mode, t
  | Enabled t -> (
    let t =
      { t with
        return_continuation = Some return_continuation;
        regions = [];
        exclave = None;
        block = None
      }
    in
    let scope = Code_id_or_name.code_id code_id in
    if not (Escape_solution.returns_local t.solution code_id)
    then my_alloc_mode, Enabled (enter_region_scope t ~scope)
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
        let callers_region =
          { region;
            ghost_region;
            has_ghost_region = true;
            scope =
              Callers_region
                { returned = Escape_solution.local_returned t.solution code_id }
          }
        in
        let plan =
          Escape_exclave.compute t.solution
            ~continuation_info:t.continuation_info ~code_id ~return_continuation
            body
        in
        (match t.stats_mode with
        | Verbose ->
          Format.eprintf
            "reaper-escape-local-return: unit=%s code=%s own_region=%b@."
            (unit_name ()) (Code_id.name code_id) (Option.is_some plan)
        | No_stats | Summary -> ());
        let t =
          match plan with
          | None -> { t with regions = [callers_region] }
          | Some plan ->
            let own_region =
              { region = Variable.create "region" K.region;
                ghost_region = Variable.create "ghost_region" K.region;
                has_ghost_region = Escape_exclave.receives_call_results plan;
                scope = Before_exclave { scope; plan }
              }
            in
            { t with
              regions = [own_region; callers_region];
              exclave = Some (plan, own_region)
            }
        in
        ( Alloc_mode.For_applications.maybe_alloc_stack ~alloc_region ~region
            ~ghost_region,
          Enabled t ))

let leave_function t acc ~code_id body =
  match t with
  | Disabled -> body, acc
  | Enabled t -> (
    if not (Escape_solution.returns_local t.solution code_id)
    then leave_region_scope t acc ~scope:(Code_id_or_name.code_id code_id) body
    else
      match t.exclave with
      | None -> body, acc
      | Some (_, region) -> open_region t acc region body)
