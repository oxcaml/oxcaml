(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Graph = Global_flow_graph

type continuation_info =
  { is_exn_handler : bool;
    params : Variable.t list;
    arity : Flambda_kind.With_subkind.t list
  }

module Env = struct
  type cont_kind = Normal of Variable.t list

  type should_preserve_direct_calls =
    | Yes
    | No
    | Auto

  type t =
    { parent : Rev_expr.rev_expr_holed;
      conts : cont_kind Continuation.Map.t;
      current_code_id : Code_id.t option;
      should_preserve_direct_calls : should_preserve_direct_calls;
      le_monde_exterieur : Name.t;
      all_constants : Name.t
    }
end

type code_dep =
  { arity : [`Complex] Flambda_arity.t;
    params : Variable.t list;
    my_closure : Variable.t;
    return : Variable.t list; (* Dummy variable representing return value *)
    exn : Variable.t; (* Dummy variable representing exn return value *)
    is_tupled : bool;
    known_arity_call_witness : Code_id_or_name.t;
    unknown_arity_call_witnesses :
      Code_id_or_name.t list (* One element for each (complex) parameter *)
  }

type apply_dep =
  { function_containing_apply_expr : Code_id.t option;
    apply_code_id : Code_id.t;
    apply_closure : Simple.t option;
    apply_call_witness : Code_id_or_name.t
  }

type closure_dep =
  { let_bound_name_of_the_closure : Name.t;
    closure_code_id : Code_id.t;
    only_full_applications : bool
  }

type t =
  { mutable code : code_dep Code_id.Map.t;
    mutable apply_deps : apply_dep list;
    mutable set_of_closures_dep : closure_dep list;
    deps : Graph.graph;
    mutable kinds : Flambda_kind.t Name.Map.t;
    mutable fixed_arity_conts : Continuation.Set.t;
    mutable continuation_info : continuation_info Continuation.Map.t
  }

let code_deps t = t.code

let create () =
  { code = Code_id.Map.empty;
    apply_deps = [];
    set_of_closures_dep = [];
    deps = Graph.create ();
    kinds = Name.Map.empty;
    fixed_arity_conts = Continuation.Set.empty;
    continuation_info = Continuation.Map.empty
  }

let kinds t = t.kinds

let kind name k t = t.kinds <- Name.Map.add name k t.kinds

let bound_parameter_kind (bp : Bound_parameter.t) t =
  let kind = Flambda_kind.With_subkind.kind (Bound_parameter.kind bp) in
  let name = Name.var (Bound_parameter.var bp) in
  t.kinds <- Name.Map.add name kind t.kinds

(* CR-someday ncourant: it would be great if we kept constants and symbols from
   external compilation units in the graph as well, making effectively all
   simples be representable. In this case, we should however be *very* careful
   with coercions... *)
let simple_to_node t ~all_constants simple =
  Simple.pattern_match' simple
    ~const:(fun _ -> Code_id_or_name.name all_constants)
    ~var:(fun v ~coercion:_ -> Code_id_or_name.var v)
    ~symbol:(fun s ~coercion:_ ->
      if not (Compilation_unit.is_current (Symbol.compilation_unit s))
      then Graph.add_any_source t.deps (Code_id_or_name.symbol s);
      Code_id_or_name.symbol s)

let alias_kind name simple t =
  let kind =
    Simple.pattern_match simple
      ~name:(fun name ~coercion:_ ->
        (* Symbols are always values and might not be in t.kinds *)
        if Name.is_symbol name
        then Flambda_kind.value
        else
          match Name.Map.find_opt name t.kinds with
          | Some k -> k
          | None -> Misc.fatal_errorf "Unbound name %a" Name.print name)
      ~const:(fun const ->
        match Int_ids.Const.descr const with
        | Naked_immediate _ -> Flambda_kind.naked_immediate
        | Tagged_immediate _ | Null -> Flambda_kind.value
        | Naked_float _ -> Flambda_kind.naked_float
        | Naked_float32 _ -> Flambda_kind.naked_float32
        | Naked_int8 _ -> Flambda_kind.naked_int8
        | Naked_int16 _ -> Flambda_kind.naked_int16
        | Naked_int32 _ -> Flambda_kind.naked_int32
        | Naked_int64 _ -> Flambda_kind.naked_int64
        | Naked_nativeint _ -> Flambda_kind.naked_nativeint
        | Naked_vec128 _ -> Flambda_kind.naked_vec128
        | Naked_vec256 _ -> Flambda_kind.naked_vec256
        | Naked_vec512 _ -> Flambda_kind.naked_vec512)
  in
  t.kinds <- Name.Map.add name kind t.kinds

let add_code code_id dep t = t.code <- Code_id.Map.add code_id dep t.code

let find_code t code_id = Code_id.Map.find code_id t.code

let add_alias t ~to_ ~from = Graph.add_alias t.deps ~to_ ~from

let add_use_dep t ~to_ ~from = Graph.add_use_dep t.deps ~to_ ~from

let add_accessor_dep t ~to_ relation ~base =
  Graph.add_accessor_dep t.deps ~to_ relation ~base

let add_constructor_dep t ~base relation ~from =
  Graph.add_constructor_dep t.deps ~base relation ~from

let add_coaccessor_dep t ~to_ relation ~base =
  Graph.add_coaccessor_dep t.deps ~to_ relation ~base

let add_coconstructor_dep t ~base relation ~from =
  Graph.add_coconstructor_dep t.deps ~base relation ~from

let add_propagate_dep t ~if_used ~to_ ~from =
  Graph.add_propagate_dep t.deps ~if_used ~to_ ~from

let add_alias_if_any_source_dep t ~if_any_source ~to_ ~from =
  Graph.add_alias_if_any_source_dep t.deps ~if_any_source ~to_ ~from

let add_any_source t x = Graph.add_any_source t.deps x

let add_any_usage t x = Graph.add_any_usage t.deps x

let add_code_id_my_closure t code_id my_closure =
  Graph.add_code_id_my_closure t.deps code_id my_closure

let add_cond_any_usage t ~(denv : Env.t) simple =
  let node = simple_to_node t ~all_constants:denv.all_constants simple in
  match denv.current_code_id with
  | None -> add_any_usage t node
  | Some code_id ->
    (* CR ncourant: this always makes [node] any_source, we should improve
       that. *)
    add_use_dep t ~to_:(Code_id_or_name.code_id code_id) ~from:node

let add_cond_any_source t ~(denv : Env.t) v =
  match denv.current_code_id with
  | None -> add_any_source t v
  | Some code_id ->
    add_propagate_dep t
      ~if_used:(Code_id_or_name.code_id code_id)
      ~from:(Code_id_or_name.name denv.le_monde_exterieur)
      ~to_:v

let cond_alias t ~(denv : Env.t) ~from ~to_ =
  match denv.current_code_id with
  | None -> add_alias t ~from ~to_
  | Some code_id ->
    add_propagate_dep t ~if_used:(Code_id_or_name.code_id code_id) ~from ~to_

let fixed_arity_continuation t k =
  t.fixed_arity_conts <- Continuation.Set.add k t.fixed_arity_conts

let fixed_arity_continuations t = t.fixed_arity_conts

let continuation_info t k info =
  t.continuation_info <- Continuation.Map.add k info t.continuation_info

let get_continuation_info t = t.continuation_info

let add_apply apply t = t.apply_deps <- apply :: t.apply_deps

let add_set_of_closures_dep let_bound_name_of_the_closure closure_code_id
    ~only_full_applications t =
  t.set_of_closures_dep
    <- { let_bound_name_of_the_closure;
         closure_code_id;
         only_full_applications
       }
       :: t.set_of_closures_dep

(*= Encoding of sets of closures and apply

   Let us consider, for now, a set of closures that is only directly called.
   Assume that it has a value slot x, two function slots f and g, with
   associated code_ids p (param a, return s) and q (param b, return t).
   We will name the respective witnesses n and m, and ignore exception returns
   to make the diagram simpler.

   We will create a widget looking like this:
    ┌──╔═══╗─────[g]────>╔═══╗──┐
   [f] ║ f ║             ║ g ║ [g]
    └─>╚═══╝<────[f]─────╚═══╝<─┘
        │ │     ╔═══╗     │ │
        │ └[x]─>║ x ║<─[x]┘ │
      [wit]     ╚═══╝     [wit]
        │                   │
        v                   v
      ╔═══╗               ╔═══╗
      ║ n ║               ║ m ║
      ╚═══╝               ╚═══╝
       ││║          ╔═══╗  ││║          ╔═══╗
       ││╚[param0]═>║ a ║  ││╚[param0]═>║ b ║
       ││           ╚═══╝  ││           ╚═══╝
       ││           ╔═══╗  ││           ╔═══╗
       │└[return0]─>║ s ║  │└[return0]─>║ t ║
       │            ╚═══╝  │            ╚═══╝
       │            ╔═══╗  │            ╔═══╗
       └─[code_id]─>║ p ║  └─[code_id]─>║ q ║
                    ╚═══╝               ╚═══╝

   For indirect calls, we have a series of call witnesses for each
   complex parameter, each with coconstructors for each part of the
   complex parameter, the code_id, and returning a value with the next call
   witness.
*)

let create_known_arity_call_witness t code_id ~params ~returns ~exn =
  let witness =
    Variable.create
      (Format.asprintf "known_arity_witness_%s" (Code_id.name code_id))
      Flambda_kind.rec_info
    (* dummy kind to make sure the rest of the code breaks if this is ever
       used *)
  in
  let witness = Code_id_or_name.var witness in
  List.iteri
    (fun i v ->
      add_coconstructor_dep t ~base:witness (Cofield.param i)
        ~from:(Code_id_or_name.var v))
    params;
  List.iteri
    (fun i v ->
      add_constructor_dep t ~base:witness (Field.apply (Normal i))
        ~from:(Code_id_or_name.var v))
    returns;
  add_constructor_dep t ~base:witness (Field.apply Exn)
    ~from:(Code_id_or_name.var exn);
  add_constructor_dep t ~base:witness Field.code_id_of_call_witness
    ~from:(Code_id_or_name.code_id code_id);
  witness

let make_known_arity_apply_widget t ~(denv : Env.t) ~params ~returns ~exn =
  let witness =
    Code_id_or_name.var
      (Variable.create "known_arity_apply" Flambda_kind.rec_info)
  in
  List.iteri
    (fun i v ->
      add_coaccessor_dep t ~base:witness (Cofield.param i)
        ~to_:(simple_to_node t ~all_constants:denv.all_constants v))
    params;
  List.iteri
    (fun i v ->
      add_accessor_dep t ~base:witness (Field.apply (Normal i))
        ~to_:(Code_id_or_name.var v))
    returns;
  add_accessor_dep t ~base:witness (Field.apply Exn)
    ~to_:(Code_id_or_name.var exn);
  let called =
    Code_id_or_name.var (Variable.create "called" Flambda_kind.rec_info)
  in
  add_accessor_dep t ~base:witness Field.code_id_of_call_witness ~to_:called;
  add_any_usage t called;
  let apply =
    Code_id_or_name.var (Variable.create "apply" Flambda_kind.rec_info)
  in
  cond_alias t ~denv ~from:apply ~to_:witness;
  apply

let create_unknown_arity_call_witnesses t code_id ~is_tupled ~arity ~params
    ~returns ~exn =
  if is_tupled
  then (
    let witness =
      Variable.create
        (Format.asprintf "unknown_arity_witness_tupled_%s"
           (Code_id.name code_id))
        Flambda_kind.rec_info
    in
    let witness = Code_id_or_name.var witness in
    List.iteri
      (fun i v ->
        add_constructor_dep t ~base:witness (Field.apply (Normal i))
          ~from:(Code_id_or_name.var v))
      returns;
    add_constructor_dep t ~base:witness (Field.apply Exn)
      ~from:(Code_id_or_name.var exn);
    add_constructor_dep t ~base:witness Field.code_id_of_call_witness
      ~from:(Code_id_or_name.code_id code_id);
    let untuple_var =
      Code_id_or_name.var (Variable.create "untuple_var" Flambda_kind.value)
    in
    add_coconstructor_dep t ~base:witness (Cofield.param 0) ~from:untuple_var;
    (* CR ncourant: this should be changed if we ever allow non-value tuples *)
    List.iteri
      (fun i v ->
        add_accessor_dep t ~to_:(Code_id_or_name.var v)
          (Field.block i Flambda_kind.value)
          ~base:untuple_var)
      params;
    [witness])
  else
    let rec add_deps params_and_witnesses =
      match params_and_witnesses with
      | [] -> Misc.fatal_error "add_deps: no params"
      | (first, witness) :: rest -> (
        List.iteri
          (fun i arg ->
            add_coconstructor_dep t ~from:(Code_id_or_name.var arg)
              (Cofield.param i) ~base:witness)
          first;
        add_constructor_dep t ~base:witness Field.code_id_of_call_witness
          ~from:(Code_id_or_name.code_id code_id);
        match rest with
        | [] ->
          add_constructor_dep t ~base:witness (Field.apply Exn)
            ~from:(Code_id_or_name.var exn);
          List.iteri
            (fun i return_arg ->
              add_constructor_dep t
                ~from:(Code_id_or_name.var return_arg)
                (Field.apply (Normal i)) ~base:witness)
            returns
        | (_, next_witness) :: _ ->
          let v =
            Code_id_or_name.var
              (Variable.create "partial_apply" Flambda_kind.value)
          in
          add_constructor_dep t ~from:v (Field.apply (Normal 0)) ~base:witness;
          add_constructor_dep t ~from:next_witness
            (Field.code_of_closure Unknown_arity_code_pointer)
            ~base:v;
          add_deps rest)
    in
    let params = Flambda_arity.group_by_parameter arity params in
    let witnesses =
      List.mapi
        (fun i _ ->
          Code_id_or_name.var
            (Variable.create
               (Format.asprintf "unknown_arity_witness_%d_%s" i
                  (Code_id.name code_id))
               Flambda_kind.rec_info))
        params
    in
    add_deps (List.combine params witnesses);
    witnesses

let make_unknown_arity_apply_widget t ~(denv : Env.t) ~arity ~params ~returns
    ~exn =
  let called =
    Code_id_or_name.var (Variable.create "called" Flambda_kind.rec_info)
  in
  add_any_usage t called;
  let rec add_deps params_and_witnesses =
    match params_and_witnesses with
    | [] -> Misc.fatal_error "add_deps: no params"
    | (first, witness) :: rest -> (
      List.iteri
        (fun i v ->
          add_coaccessor_dep t ~base:witness (Cofield.param i)
            ~to_:(simple_to_node t ~all_constants:denv.all_constants v))
        first;
      add_accessor_dep t ~base:witness (Field.apply Exn)
        ~to_:(Code_id_or_name.var exn);
      add_accessor_dep t ~base:witness Field.code_id_of_call_witness ~to_:called;
      match rest with
      | [] ->
        List.iteri
          (fun i v ->
            add_accessor_dep t ~base:witness (Field.apply (Normal i))
              ~to_:(Code_id_or_name.var v))
          returns
      | (_, next_witness) :: _ ->
        let v =
          Code_id_or_name.var
            (Variable.create "partial_apply" Flambda_kind.value)
        in
        add_accessor_dep t ~base:witness (Field.apply (Normal 0)) ~to_:v;
        add_accessor_dep t ~base:v
          (Field.code_of_closure Unknown_arity_code_pointer)
          ~to_:next_witness;
        add_deps rest)
  in
  let params = Flambda_arity.group_by_parameter arity params in
  let witnesses =
    List.mapi
      (fun i _ ->
        Code_id_or_name.var
          (Variable.create
             (Format.asprintf "unknown_arity_apply_%d" i)
             Flambda_kind.rec_info))
      params
  in
  add_deps (List.combine params witnesses);
  let apply =
    Code_id_or_name.var (Variable.create "apply" Flambda_kind.rec_info)
  in
  cond_alias t ~denv ~from:apply ~to_:(List.hd witnesses);
  apply

let record_set_of_closure_deps t =
  List.iter
    (fun { let_bound_name_of_the_closure = name;
           closure_code_id = code_id;
           only_full_applications = _
         } ->
      (* CR ncourant: use only_full_applications; not done here to avoid
         conflicts in code that will be rewritten for unbox-fv-closures
         anyway. *)
      match find_code t code_id with
      | exception Not_found ->
        assert (
          not
            (Compilation_unit.is_current (Code_id.get_compilation_unit code_id)));
        (* The code comes from another compilation unit; so we don't know what
           happens once it is applied. As such, it must escape the whole
           block. *)
        let witness =
          Code_id_or_name.var
            (Variable.create
               (Format.asprintf "external_code_id_witness_%s"
                  (Code_id.name code_id))
               Flambda_kind.value)
        in
        add_any_source t witness;
        add_constructor_dep t ~from:witness
          (Field.code_of_closure Known_arity_code_pointer)
          ~base:(Code_id_or_name.name name);
        add_constructor_dep t ~from:witness
          (Field.code_of_closure Unknown_arity_code_pointer)
          ~base:(Code_id_or_name.name name);
        add_constructor_dep t ~base:witness Field.code_id_of_call_witness
          ~from:(Code_id_or_name.name name)
      | code_dep ->
        add_propagate_dep t
          ~to_:(Code_id_or_name.var code_dep.my_closure)
          ~from:(Code_id_or_name.name name)
          ~if_used:(Code_id_or_name.code_id code_id);
        add_constructor_dep t ~from:code_dep.known_arity_call_witness
          (Field.code_of_closure Known_arity_code_pointer)
          ~base:(Code_id_or_name.name name);
        add_constructor_dep t
          ~from:(List.hd code_dep.unknown_arity_call_witnesses)
          (Field.code_of_closure Unknown_arity_code_pointer)
          ~base:(Code_id_or_name.name name))
    t.set_of_closures_dep

let deps t ~all_constants =
  List.iter
    (fun { function_containing_apply_expr;
           apply_code_id;
           apply_closure;
           apply_call_witness
         } ->
      let code_dep = find_code t apply_code_id in
      add_alias t ~from:code_dep.known_arity_call_witness
        ~to_:apply_call_witness;
      match apply_closure with
      | None -> ()
      | Some closure -> (
        match function_containing_apply_expr with
        | None ->
          add_alias t
            ~to_:(Code_id_or_name.var code_dep.my_closure)
            ~from:(simple_to_node t ~all_constants closure)
        | Some code_id ->
          add_propagate_dep t
            ~to_:(Code_id_or_name.var code_dep.my_closure)
            ~from:(simple_to_node t ~all_constants closure)
            ~if_used:(Code_id_or_name.code_id code_id)))
    t.apply_deps;
  record_set_of_closure_deps t;
  t.deps

let simple_to_node t ~denv s =
  simple_to_node t ~all_constants:denv.Env.all_constants s
