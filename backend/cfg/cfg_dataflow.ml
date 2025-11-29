[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Oxcaml_utils.Doubly_linked_list
include Cfg_dataflow_intf.S

module type Dataflow_direction_S = sig
  module Transfer_domain : Domain_S

  type transfer_image

  val join_result :
    old_value:Transfer_domain.t ->
    transfer_result:transfer_image ->
    predecessor:Cfg.basic_block ->
    current:Cfg.basic_block ->
    Transfer_domain.t

  type instr_domain

  (* For a given block gives a sequence of all successor labels (taking the
     dataflow direction into account). *)
  val edges_out : Cfg.basic_block -> Label.t Seq.t

  type context

  type error

  exception Dataflow_aborted of error

  val transfer_block :
    update_instr:(InstructionId.t -> instr_domain -> unit) ->
    update_body:(Label.t -> Transfer_domain.t -> unit) ->
    Transfer_domain.t ->
    Cfg.basic_block ->
    context ->
    transfer_image

  val get_result : Transfer_domain.t -> instr_domain

  val init_block :
    Cfg.t -> Cfg.basic_block -> instr_domain control -> Transfer_domain.t
end

module type Dataflow_S = sig
  type instr_domain

  type context

  type error

  val run :
    Cfg.t ->
    ?max_iteration:int ->
    init:instr_domain control ->
    map:('a, instr_domain) map ->
    context ->
    ('a, error) Dataflow_result.t
end

module Make_dataflow (D : Dataflow_direction_S) :
  Dataflow_S
    with type instr_domain = D.instr_domain
     and type error = D.error
     and type context = D.context = struct
  module Transfer_domain = D.Transfer_domain

  module WorkSet : sig
    type t

    type element = Label.t

    val create : priorities:int Label.Tbl.t -> t

    val add : t -> element -> unit

    val is_empty : t -> bool

    val remove_and_return : t -> element
  end = struct
    module WorkSetElement = struct
      type t =
        { priority : int;
          label : Label.t
        }

      let compare t1 t2 =
        match Int.compare t1.priority t2.priority with
        | 0 -> Label.compare t1.label t2.label
        | c -> c
    end

    module WorkSet = Set.Make (WorkSetElement)

    type t =
      { priorities : int Label.Tbl.t;
        mutable work_set : WorkSet.t
      }

    type element = Label.t

    let create ~priorities = { priorities; work_set = WorkSet.empty }

    let add t label =
      let priority = Label.Tbl.find t.priorities label in
      t.work_set <- WorkSet.add { label; priority } t.work_set

    let is_empty t = WorkSet.is_empty t.work_set

    let choose t = WorkSet.max_elt t.work_set

    let remove_and_return t =
      let element = choose t in
      t.work_set <- WorkSet.remove element t.work_set;
      element.label
  end

  type work_state =
    { cfg : Cfg.t;
      queue : WorkSet.t;
      map_block : Transfer_domain.t Label.Tbl.t;
      map_instr : D.instr_domain InstructionId.Tbl.t option;
      map_body : Transfer_domain.t Label.Tbl.t option
    }

  type instr_domain = D.instr_domain

  type context = D.context

  type error = D.error

  type priority_helper =
    { label : Label.t;
      index : int;
      mutable lowlink : int;
      mutable on_stack : bool
    }

  let compute_priorities (cfg : Cfg.t) =
    (* This algorithm is based on Tarjan's strongly connected components
       algorithm explained in "DEPTH-FIRST SEARCH AND LINEAR GRAPH ALGORITHMS*"
       by Robert Tarjan, chapter 4.

       We assign priorities to the nodes based on order they are popped from the
       stack. With that, for two strongly connected components C1 and C2 with an
       edge from C1 to C2 all nodes from C1 will have higher priorities than
       nodes in C2. That is a good order for computing dataflow on the DAG of
       strongly connected components.

       Nodes in a single strongly connected component are added to the stack in
       pre-order and when popping the order is be reversed. But we compute
       dataflow in order of decreasing priority so it will go through them in
       the original pre-order. That seems to be a good heuristic for strongly
       connected components because for a simple cycle that is the best
       ordering. *)
    let stack = Stack.create () in
    let mapping = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
    let priorities = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
    let priority = ref 0 in
    let rec pop_until v =
      let w_values = Stack.pop stack in
      w_values.on_stack <- false;
      incr priority;
      let w = w_values.label in
      assert (not (Label.Tbl.mem priorities w));
      Label.Tbl.add priorities w !priority;
      if not (Label.equal v w) then pop_until v
    in
    let i = ref 0 in
    let int_min (i1 : int) (i2 : int) : int = if i1 < i2 then i1 else i2 in
    let rec strong_connect v =
      assert (not (Label.Tbl.mem mapping v));
      incr i;
      let v_values = { label = v; index = !i; lowlink = !i; on_stack = true } in
      Label.Tbl.add mapping v v_values;
      Stack.push v_values stack;
      let block = Cfg.get_block_exn cfg v in
      Seq.iter
        (fun w ->
          match Label.Tbl.find_opt mapping w with
          | None ->
            let w_values = strong_connect w in
            v_values.lowlink <- int_min v_values.lowlink w_values.lowlink
          | Some w_values ->
            if w_values.on_stack
            then v_values.lowlink <- int_min v_values.lowlink w_values.index)
        (D.edges_out block);
      if v_values.lowlink = v_values.index then pop_until v;
      v_values
    in
    Cfg.iter_blocks cfg ~f:(fun label _block ->
        if not (Label.Tbl.mem mapping label)
        then
          let (_ : priority_helper) = strong_connect label in
          assert (Stack.is_empty stack));
    assert (Label.Tbl.length priorities = Label.Tbl.length cfg.blocks);
    priorities

  let update_instr : work_state -> InstructionId.t -> instr_domain -> unit =
   fun t instr_id value ->
    match t.map_instr with
    | None -> ()
    | Some map_instr -> InstructionId.Tbl.replace map_instr instr_id value

  let update_body : work_state -> Label.t -> Transfer_domain.t -> unit =
   fun t label value ->
    match t.map_body with
    | None -> ()
    | Some map_body -> Label.Tbl.replace map_body label value

  let create :
      Cfg.t ->
      init:instr_domain control ->
      store_instr:bool ->
      store_body:bool ->
      work_state =
   fun cfg ~init ~store_instr ~store_body ->
    let priorities = compute_priorities cfg in
    let queue = WorkSet.create ~priorities in
    let map_block = Label.Tbl.create (Label.Tbl.length cfg.Cfg.blocks) in
    let map_instr =
      if store_instr
      then
        let map_instr =
          (* CR-soon xclerc for xclerc: review the `16` constant. *)
          InstructionId.Tbl.create (Label.Tbl.length cfg.Cfg.blocks * 16)
        in
        Some map_instr
      else None
    in
    let map_body =
      if store_body
      then (
        let map_body = Label.Tbl.create (Label.Tbl.length cfg.Cfg.blocks) in
        Cfg.iter_blocks cfg ~f:(fun label _block ->
            Label.Tbl.replace map_body label D.Transfer_domain.bot);
        Some map_body)
      else None
    in
    let t = { cfg; queue; map_block; map_instr; map_body } in
    Cfg.iter_blocks cfg ~f:(fun label block ->
        let value = D.init_block cfg block init in
        Label.Tbl.replace map_block label value;
        WorkSet.add t.queue label);
    t

  let get_res_block t = t.map_block

  let get_res_instr_exn t = Option.get t.map_instr

  let get_res_body_exn t = Option.get t.map_body

  let run ~max_iteration work_state context =
    let iteration = ref 0 in
    while
      (not (WorkSet.is_empty work_state.queue)) && !iteration < max_iteration
    do
      incr iteration;
      let element = WorkSet.remove_and_return work_state.queue in
      let current_block = Cfg.get_block_exn work_state.cfg element in
      let current_value =
        Label.Tbl.find work_state.map_block current_block.start
      in
      let transfer_result =
        D.transfer_block ~update_instr:(update_instr work_state)
          ~update_body:(update_body work_state) current_value current_block
          context
      in
      Seq.iter
        (fun successor ->
          let successor_block = Cfg.get_block_exn work_state.cfg successor in
          let successor_value =
            Label.Tbl.find work_state.map_block successor_block.start
          in
          let new_value =
            D.join_result ~old_value:successor_value ~transfer_result
              ~predecessor:current_block ~current:successor_block
          in
          if not (Transfer_domain.less_equal new_value successor_value)
          then (
            Label.Tbl.replace work_state.map_block successor new_value;
            WorkSet.add work_state.queue successor))
        (D.edges_out current_block);
      ()
    done;
    if WorkSet.is_empty work_state.queue then Ok () else Error ()

  let run (type a) cfg ?(max_iteration = max_int) ~init
      ~(map : (a, instr_domain) map) context : (a, D.error) Dataflow_result.t =
    let store_instr =
      match map with Instr | Both -> true | Block | Body -> false
    in
    let store_body =
      match map with Block | Both | Instr -> false | Body -> true
    in
    let work_state = create cfg ~init ~store_instr ~store_body in
    let get_result (type a) work_state (map : (a, instr_domain) map) : a =
      let get_res_block work_state : instr_domain Hashtbl.Make(Label.T).t =
        Label.Tbl.map (get_res_block work_state) D.get_result
      in
      let get_res_instr work_state :
          instr_domain Hashtbl.Make(InstructionId.T).t =
        get_res_instr_exn work_state
      in
      let get_res_body work_state : instr_domain Hashtbl.Make(Label.T).t =
        Label.Tbl.map (get_res_body_exn work_state) D.get_result
      in
      match map with
      | Body -> get_res_body work_state
      | Block -> get_res_block work_state
      | Instr -> get_res_instr_exn work_state
      | Both -> get_res_instr work_state, get_res_block work_state
    in
    try
      match run ~max_iteration work_state context with
      | Ok () -> Ok (get_result work_state map)
      | Error () -> Max_iterations_reached
    with D.Dataflow_aborted error -> Aborted (get_result work_state map, error)
end

module Forward (D : Domain_S) (T : Forward_transfer with type d = D.t) :
  S with type domain = D.t and type error = T.error and type context = T.context =
struct
  module Direction :
    Dataflow_direction_S
      with type Transfer_domain.t = D.t
       and type instr_domain = D.t
       and type error = T.error
       and type context = T.context = struct
    module Transfer_domain : Domain_S with type t = D.t = struct
      include D
    end

    type transfer_image = T.output

    type instr_domain = D.t

    let edges_out : Cfg.basic_block -> Label.t Seq.t =
     fun block ->
      (* CR-soon azewierzejew for xclerc: Add something to [Cfg] interface to
         make this function (and the one in [Backward]) more efficient. *)
      Cfg.successor_labels ~normal:true ~exn:true block |> Label.Set.to_seq

    let join_result :
        old_value:Transfer_domain.t ->
        transfer_result:transfer_image ->
        predecessor:Cfg.basic_block ->
        current:Cfg.basic_block ->
        Transfer_domain.t =
     fun ~old_value ~transfer_result ~predecessor:_ ~current ->
      if current.is_trap_handler
      then D.join old_value transfer_result.exceptional
      else D.join old_value transfer_result.normal

    type context = T.context

    type error = T.error

    exception Dataflow_aborted of T.error

    let unwrap_transfer_result value =
      match value with
      | Ok value -> value
      | Error error -> raise (Dataflow_aborted error)

    let transfer_block :
        update_instr:(InstructionId.t -> instr_domain -> unit) ->
        update_body:(Label.t -> instr_domain -> unit) ->
        Transfer_domain.t ->
        Cfg.basic_block ->
        context ->
        transfer_image =
     fun ~update_instr ~update_body value block context ->
      let value =
        DLL.fold_left block.body ~init:value
          ~f:(fun value (instr : Cfg.basic Cfg.instruction) ->
            let value = T.basic value instr context |> unwrap_transfer_result in
            update_instr instr.id value;
            value)
      in
      update_body block.start value;
      let value =
        T.terminator value block.terminator context |> unwrap_transfer_result
      in
      (* CR-someday gyorsh: record both normal and exceptional results *)
      update_instr block.terminator.id value.normal;
      value

    let get_result (v : D.t) = v

    let init_block (cfg : Cfg.t) (block : Cfg.basic_block) init =
      if Label.equal block.start cfg.entry_label
      then init.normal
      else if block.is_trap_handler
      then init.exceptional
      else D.bot
  end

  type domain = D.t

  type context = T.context

  type error = T.error

  type init = domain control

  module Dataflow_impl = Make_dataflow (Direction)

  let run :
      type a.
      Cfg.t ->
      ?max_iteration:int ->
      init:init ->
      map:(a, domain) map ->
      context ->
      (a, T.error) Dataflow_result.t =
   fun cfg ?max_iteration ~init ~map context ->
    Dataflow_impl.run cfg ?max_iteration ~init ~map context
end

module Backward (D : Domain_S) (T : Backward_transfer with type d = D.t) :
  S with type domain = D.t and type error = T.error and type context = T.context =
struct
  type transfer_domain = D.t control

  module Direction :
    Dataflow_direction_S
      with type Transfer_domain.t = transfer_domain
       and type instr_domain = D.t
       and type error = T.error
       and type context = T.context = struct
    module Transfer_domain : Domain_S with type t = transfer_domain = struct
      type t = transfer_domain

      let bot = { normal = D.bot; exceptional = D.bot }

      let less_equal t1 t2 =
        D.less_equal t1.normal t2.normal
        && D.less_equal t1.exceptional t2.exceptional

      let join { normal = n1; exceptional = e1 }
          { normal = n2; exceptional = e2 } =
        { normal = D.join n1 n2; exceptional = D.join e1 e2 }
    end

    type transfer_image = D.t

    type instr_domain = D.t

    let edges_out : Cfg.basic_block -> Label.t Seq.t =
     fun block -> Cfg.predecessor_labels block |> List.to_seq

    let join_result :
        old_value:Transfer_domain.t ->
        transfer_result:transfer_image ->
        predecessor:Cfg.basic_block ->
        current:Cfg.basic_block ->
        Transfer_domain.t =
     fun ~old_value ~transfer_result ~predecessor ~current:_ ->
      if predecessor.is_trap_handler
      then
        { old_value with
          exceptional = D.join old_value.exceptional transfer_result
        }
      else { old_value with normal = D.join old_value.normal transfer_result }

    type context = T.context

    type error = T.error

    exception Dataflow_aborted of T.error

    let unwrap_transfer_result value =
      match value with
      | Ok value -> value
      | Error error -> raise (Dataflow_aborted error)

    let transfer_block :
        update_instr:(InstructionId.t -> instr_domain -> unit) ->
        update_body:(Label.t -> transfer_domain -> unit) ->
        Transfer_domain.t ->
        Cfg.basic_block ->
        context ->
        transfer_image =
     fun ~update_instr ~update_body { normal; exceptional } block context ->
      let transfer (instr : _ Cfg.instruction) value =
        let value = unwrap_transfer_result value in
        update_instr instr.id value;
        value
      in
      let value =
        transfer block.terminator
          (T.terminator { normal; exceptional } block.terminator context)
      in
      let value =
        DLL.fold_right block.body ~init:value ~f:(fun instr value ->
            transfer instr (T.basic value instr context))
      in
      let value =
        if block.is_trap_handler
        then T.exception_ value context |> unwrap_transfer_result
        else value
      in
      update_body block.start { normal = value; exceptional = D.bot };
      value

    let get_result (v : Transfer_domain.t) = v.normal

    let init_block _cfg b (init : instr_domain control) =
      { normal = init.normal;
        exceptional =
          (if Cfg.can_raise_interproc b then init.exceptional else D.bot)
      }
  end

  type domain = D.t

  type context = T.context

  type error = T.error

  type init = domain control

  module Dataflow_impl = Make_dataflow (Direction)

  let run :
      type a.
      Cfg.t ->
      ?max_iteration:int ->
      init:init ->
      map:(a, domain) map ->
      context ->
      (a, T.error) Dataflow_result.t =
   fun cfg ?max_iteration ~init ~map context ->
    Dataflow_impl.run cfg ?max_iteration ~init ~map context
end
