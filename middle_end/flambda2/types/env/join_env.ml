(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2013--2025 OCamlPro SAS                                    *)
(*   Copyright 2014--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Implement the join of typing envs, or more precisely of typing env levels.

   Most of the code here is actually concerned with the join of *aliases*
   specifically (keeping track of how names change between the different
   environments), and delegates the actual join of types to the
   [Meet_and_n_way_join] module.

   The join involves multiple environments that are known under different names.
   Within this file, we standardise on the following names:

   - The {b source environment} is the initial value (before the join) of the
   environment that we will extend. This is also called the "definition typing
   env" in join_levels.ml; in the context of [Simplify], this is the environment
   we would use to simplify the handler when not doing a join. This is different
   from the "env at fork" in that the source environment is expected to already
   have definitions for the params (and extra params) of the current handler.

   - The {b target environment} is the final value (after the join) of the
   source environment. This is also called the "handler env" in [Simplify]. This
   environment does not exist until the join is completed, but it is still
   helpful to refer to things that will exist there (those either also exist in
   the source environment, or are existential variables added during the join).

   - The {b joined environments} are each of the individual environments that we
   are joining. In the context of [Simplify], these are the environments at each
   use. The joined environments are uniquely identified (within the current
   join) by an {!Index.t}.

   {1:assumptions Assumptions}

   We make the following assumptions on the input environments.

   {2:scope_of_names Scope of variables and symbols}

   We assume that any name (variable or symbol) defined in the source
   environment is also be defined in all the joined environments.

   Any name defined in the source environment is also necessarily defined in the
   target environment by definition.

   {2:lifted_constants Lifted constants}

   We further assume that any symbol defined in one of the joined environments
   is also defined in the source environment (and hence the target environment).
   In the context of [Simplify], this means that we expect lifted constants from
   the joined environments to have already been inserted into the source
   environment with a suitable type.

   In practice, this means that any of the symbols we manipulate can be assumed
   to exist in both the source environment and in the target environemnt (but
   not in the joined environments, as they could be lifted constants from
   another branch).

   {2:coherent_binding_times Coherent binding times}

   We assume that {b the relative order of variables defined in the source
   environment is preserved across all the joined environments}.

   More precisely, if [a] is defined before (resp. strictly before) [b] in the
   source environment, then [a] is also defined before (resp. strictly before)
   [b] in all of the joined environments. In the context of [Simplify], this
   means that the continuation parameters must be added in the same order in the
   handler and at all uses.

   Note that this assumption does not impose any restriction on the relative
   binding times of variables that don't exist in the source environment, even
   if they exist in all the joined environments.

   This assumption is used in [get_possible_canonical_in_source_env] and allows
   an efficient (linear) implementation of this function. *)

module K = Flambda_kind
module TG = Type_grammar
module TE = Typing_env
module ME = Meet_env
module TEE = Typing_env_extension
module TEL = Typing_env_level
module ET = Expand_head.Expanded_type

module Symbol_projection = struct
  include Symbol_projection
  include Container_types.Make (Symbol_projection)
end

(* {1 Prelude: iterators} *)

(* We start off with some utilities for using leapfrog iterators that will be
   useful to compute intersections below.

   We use a local module to encapsulate the use of imperative iterators. *)

(* CR bclement: These should be in [Flambda_algorithms]. *)

module Iterator_utils : sig
  (* Given two maps [m1] and [m2], calls [f name (find m1 name) (find m2 name)]
     for each [name] in the intersection of [m1] and [m2]. *)
  val fold_binary_join :
    f:(Name.t -> 'a -> 'b -> 'c -> 'c) ->
    init:'c ->
    'a Name.Map.t ->
    'b Name.Map.t ->
    'c

  type ('a, 'b) incremental_join_entry

  val fold_incremental_join_entry :
    f:('a -> 'b -> 'c -> 'c) -> init:'c -> ('a, 'b) incremental_join_entry -> 'c

  type 'a incremental =
    { previous : 'a;
      diff : 'a;
      current : 'a
    }

  type ('a, 'b) folder = { fold : 'c. ('a -> 'b -> 'c -> 'c) -> 'c -> 'c }

  (* Compute an incremental join using the semi-naive algorithm from Datalog.

     Given a set of incremental inputs [Ci = Pi + Δi] (where [Pi], [Δi] and [Ci]
     are the [previous], [diff], and [current] fields of the {!incremental} type
     above, and [+] is [Name.Map.union (fun _ _ v -> Some v)]), fold over the
     entries in [join(C1, ..., Cn)] {b except for those that are also in
     [join(P1, ..., Pn)]}.

     {b Note}: The equality [Ci = Pi + Δi] must be ensured by the caller. *)
  val fold_incremental_join :
    f:(Name.t -> ('a, 'b) incremental_join_entry -> 'c -> 'c) ->
    init:'c ->
    ('a, 'b Name.Map.t incremental) folder ->
    'c
end = struct
  module Name_map_iterator = Leapfrog.Map (Name)
  module Name_map_join_iterator = Leapfrog.Join (Name_map_iterator)

  let create_iterator ~init ~dummy =
    let send_map, recv_map = Channel.create init in
    let send_val, recv_val = Channel.create dummy in
    let iterator = Name_map_iterator.create recv_map send_val in
    send_map, iterator, recv_val

  let naive_iterator ~init ~dummy =
    let _send, iterator, recv = create_iterator ~init ~dummy in
    iterator, recv

  let join_iterators = Name_map_join_iterator.create

  let[@inline] fold_iterator ~f ~init iterator =
    let rec loop iterator acc =
      match Name_map_join_iterator.current iterator with
      | None -> acc
      | Some name ->
        Name_map_join_iterator.accept iterator;
        let acc = (f [@inlined hint]) name acc in
        Name_map_join_iterator.advance iterator;
        loop iterator acc
    in
    Name_map_join_iterator.init iterator;
    loop iterator init

  let fold_binary_join ~f ~init a b =
    (* CR bclement: create an [Name.Map.iterator], get its initial value, and
       initialise the [Name_map_iterator] (and the [Name_map_join_iterator])
       from it to avoid double lookups. *)
    match Name.Map.choose_opt a, Name.Map.choose_opt b with
    | None, _ | _, None -> init
    | Some (_, dummy_a), Some (_, dummy_b) ->
      let iterator_a, recv_a = naive_iterator ~init:a ~dummy:dummy_a in
      let iterator_b, recv_b = naive_iterator ~init:b ~dummy:dummy_b in
      let iterator = join_iterators [iterator_a; iterator_b] in
      fold_iterator iterator ~init ~f:(fun name acc ->
          f name (Channel.recv recv_a) (Channel.recv recv_b) acc)

  type ('a, 'b) incremental_join_entry = ('a * 'b Channel.receiver) list

  let fold_incremental_join_entry ~f ~init incremental_join_entry =
    List.fold_left
      (fun acc (index, receiver) -> f index (Channel.recv receiver) acc)
      init incremental_join_entry

  type 'a incremental =
    { previous : 'a;
      diff : 'a;
      current : 'a
    }

  type ('a, 'b) folder = { fold : 'c. ('a -> 'b -> 'c -> 'c) -> 'c -> 'c }

  exception Join_is_empty

  let fold_incremental_join ~f ~init { fold } =
    (* If $Ci = Pi + Δi$ (where $Ci$, $Pi$ and $Δi$ are the [current],
       [previous], and [diff] fields, respectively), then we have:

       $$join(C1, ..., Cn) = join(P1 + Δ1, ..., Pn + Δn)$$

       By multilinearity: *)
    (*
     * join(C1, ..., Cn) =
     *   join(P1, ..., Pn) +
     *   join(Δ1, P2, ..., Pn) +       \
     *   join(C1, Δ2, P3, ..., Pn) +    | n incremental joins
     *   ... +                          |
     *   join(C1, ..., C{n-1}, Δn)     /
     *)
    (* We are interested in computing the join {b incrementally}, so we want to
       ignore the $join(P0, ..., Pn)$ part and only compute the new joined
       equations that involve at least one of the $Δi$.

       This can be done by initializing all join inputs to their previous ($Pi$)
       value, then for each input $i$:

       - Perform a join with $Δi$;

       - Set the input to $Ci$ for the following joins.

       In total, there are $n + 1$ joins, including the join of the previous
       values that we don't want to compute and $n$ incremental joins involving
       one of the $Δi$.

       We can simplify the joins by noticing the following:

       - We can remove any join where $Δi$ is empty

       - Suppose that the first $p$ inputs have an empty $Pi$ (we can always
       sort these first). Then the result of the first $p$ joins is necessarily
       empty, since it involves an empty $Pi$. Note that these are the first $p$
       join {b including the previous join}, so only the first $p - 1$
       incremental joins.

       This means that for any $i$ such that {b either} $Δi$ or $Pi$ is empty,
       the $i$-th input to the [join] is invariant and always equal to $Ci$ (if
       $Pi$ is empty, then all the non-empty joins use either $Ci$ or $Δi$; if
       $Δi$ is empty, then all the non-empty joins use either $Ci$ or $Pi$). For
       these inputs, we can simply initialize the input to $Ci$.

       There is one caveat: usually we are skipping the first join since all
       inputs are equal to their $Pi$ values. But if there is at least one of
       the inputs that has an empty $Pi$ and a non-empty $Δi$, we have already
       skipped this join by initializing that input to $Ci = Δi$ instead, and so
       we must perform join with the initial inputs. *)
    try
      let senders, iterators, receivers, perform_initial_join =
        fold
          (fun index { previous; diff; current }
               (senders, iterators, receivers, perform_initial_join) ->
            let perform_initial_join =
              perform_initial_join
              || (Name.Map.is_empty previous && not (Name.Map.is_empty diff))
            in
            (* CR bclement: we should be able to initialise the iterator with
               this value (see [fold_binary_join]). *)
            match Name.Map.choose_opt current with
            | None -> raise Join_is_empty
            | Some (_, dummy) ->
              if Name.Map.is_empty diff || Name.Map.is_empty previous
              then
                let iterator, receiver = naive_iterator ~init:current ~dummy in
                ( senders,
                  iterator :: iterators,
                  (index, receiver) :: receivers,
                  perform_initial_join )
              else
                let sender, iterator, receiver =
                  create_iterator ~init:previous ~dummy
                in
                ( (sender, diff, current) :: senders,
                  iterator :: iterators,
                  (index, receiver) :: receivers,
                  perform_initial_join ))
          ([], [], [], false)
      in
      let iterator = join_iterators iterators in
      let[@inline] f name acc = f name receivers acc in
      let acc =
        (* If any of the inputs has an empty $Pi$ and a non-empty $Δi$, then the
           initial join is not $join(P1, ..., Pn)$ but a join involving this
           $Δi$ and it must not be skipped. *)
        if perform_initial_join then fold_iterator ~f ~init iterator else init
      in
      List.fold_left
        (fun acc (sender, diff, current) ->
          Channel.send sender diff;
          let acc = fold_iterator ~f ~init:acc iterator in
          Channel.send sender current;
          acc)
        acc senders
    with Join_is_empty -> init
end

open Iterator_utils

(* {1:type_safe_wrappers Type-safe wrappers}

   Since we are dealing with many environments with distinct set of bound names,
   we introduce small wrappers around the [Variable], [Name], [Simple] and
   [Type_grammar] modules depending on the environment they live in. *)

module Index : sig
  include Container_types.S

  (* Fold over the list with a distinct index for each element.

     This is the only way to create [Index.t] values and is called when starting
     a new join. *)
  val fold_list : (t -> 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
end = struct
  include Numeric_types.Int

  let fold_list f xs init =
    let _, acc =
      List.fold_left
        (fun (index, acc) x -> index + 1, f index x acc)
        (0, init) xs
    in
    acc
end

module Id_in_env (Id : Container_types.S) () : sig
  include
    Container_types.S
      with type t = private Id.t
       and type Set.t = private Id.Set.t
       and type +!'a Map.t = private 'a Id.Map.t

  val create : Id.t -> t

  val create_set : Id.Set.t -> Set.t

  val create_map : 'a Id.Map.t -> 'a Map.t
end = struct
  include Id

  let create thing = thing

  let create_set s = s

  let create_map m = m
end

module Int_ids_in_env () = struct
  module Variable = Id_in_env (Variable) ()
  module Symbol = Id_in_env (Symbol) ()

  module Name : sig
    include module type of Id_in_env (Name) ()

    val var : Variable.t -> t

    val symbol : Symbol.t -> t

    val pattern_match :
      t -> var:(Variable.t -> 'a) -> symbol:(Symbol.t -> 'a) -> 'a
  end = struct
    include Id_in_env (Name) ()

    let var (var : Variable.t) : t =
      create (Name.var (var :> Int_ids.Variable.t))

    let symbol (symbol : Symbol.t) =
      create (Name.symbol (symbol :> Int_ids.Symbol.t))

    let[@inline] pattern_match (t : t) ~var:when_var ~symbol:when_symbol =
      Name.pattern_match
        (t :> Name.t)
        ~var:(fun var -> (when_var [@inlined hint]) (Variable.create var))
        ~symbol:(fun symbol ->
          (when_symbol [@inlined hint]) (Symbol.create symbol))
  end

  (* CR bclement: In practice, we consider that these must be canonicals in the
     corresponding environment, so this could be renamed to [Canonical] (and
     [Canonical_in_target_env] etc. below) for clarity. *)
  module Simple : sig
    include module type of Id_in_env (Simple) ()

    val const : Reg_width_const.t -> t

    val name : ?coercion:Coercion.t -> Name.t -> t

    val symbol : ?coercion:Coercion.t -> Symbol.t -> t

    val var : ?coercion:Coercion.t -> Variable.t -> t

    val coercion : t -> Coercion.t

    val without_coercion : t -> t

    val apply_coercion_exn : t -> Coercion.t -> t

    val pattern_match :
      t ->
      name:(Name.t -> coercion:Coercion.t -> 'a) ->
      const:(Reg_width_const.t -> 'a) ->
      'a

    val pattern_match' :
      t ->
      var:(Variable.t -> coercion:Coercion.t -> 'a) ->
      symbol:(Symbol.t -> coercion:Coercion.t -> 'a) ->
      const:(Reg_width_const.t -> 'a) ->
      'a
  end = struct
    include Id_in_env (Simple) ()

    let const const = create (Simple.const const)

    let name ?(coercion = Coercion.id) (name : Name.t) =
      let simple_without_coercion = Simple.name (name :> Int_ids.Name.t) in
      let simple = Simple.with_coercion simple_without_coercion coercion in
      create simple

    let symbol ?coercion symbol = name ?coercion (Name.symbol symbol)

    let var ?coercion var = name ?coercion (Name.var var)

    let coercion t = Simple.coercion (t : t :> Simple.t)

    let without_coercion t =
      create (Simple.without_coercion (t : t :> Simple.t))

    let apply_coercion_exn t coercion =
      create (Simple.apply_coercion_exn (t : t :> Simple.t) coercion)

    let[@inline always] pattern_match (t : t) ~name:when_name ~const =
      Simple.pattern_match
        (t :> Simple.t)
        ~name:(fun name ~coercion ->
          (when_name [@inlined hint]) (Name.create name) ~coercion)
        ~const

    let[@inline always] pattern_match' (t : t) ~var:when_var ~symbol:when_symbol
        ~const =
      Simple.pattern_match'
        (t :> Simple.t)
        ~var:(fun var ~coercion ->
          (when_var [@inlined hint]) (Variable.create var) ~coercion)
        ~symbol:(fun symbol ~coercion ->
          (when_symbol [@inlined hint]) (Symbol.create symbol) ~coercion)
        ~const
  end
end

module Int_ids_in_source_env = Int_ids_in_env ()
module Variable_in_source_env = Int_ids_in_source_env.Variable
module Name_in_source_env = Int_ids_in_source_env.Name
module Symbol_in_source_env = Int_ids_in_source_env.Symbol
module Simple_in_source_env = Int_ids_in_source_env.Simple

module Int_ids_from_source_env () = struct
  module Int_ids_in_env = Int_ids_in_env ()

  module Variable = struct
    include Int_ids_in_env.Variable

    (* See {!section-scope_of_names} *)
    let from_source_env (var : Variable_in_source_env.t) =
      create (var :> Variable.t)
  end

  module Symbol = Int_ids_in_env.Symbol

  module Name = struct
    include Int_ids_in_env.Name

    (* See {!section-scope_of_names} *)
    let from_source_env (name : Name_in_source_env.t) = create (name :> Name.t)

    let from_source_env_map (type a) map =
      create_map (map : a Name_in_source_env.Map.t :> a Name.Map.t)
  end

  module Simple = struct
    include Int_ids_in_env.Simple

    (* See {!section-scope_of_names} *)
    let from_source_env (simple : Simple_in_source_env.t) =
      create (simple :> Simple.t)
  end
end

module Int_ids_in_target_env = Int_ids_from_source_env ()
module Variable_in_target_env = Int_ids_in_target_env.Variable
module Symbol_in_target_env = Int_ids_in_target_env.Symbol
module Name_in_target_env = Int_ids_in_target_env.Name
module Simple_in_target_env = Int_ids_in_target_env.Simple
module Int_ids_in_one_joined_env = Int_ids_from_source_env ()
module Variable_in_one_joined_env = Int_ids_in_one_joined_env.Variable

module Symbol_in_one_joined_env = struct
  include Int_ids_in_one_joined_env.Symbol

  (* See {!section-lifted_constants} *)
  let in_source_env symbol =
    Symbol_in_source_env.create (symbol : t :> Symbol.t)
end

module Name_in_one_joined_env = Int_ids_in_one_joined_env.Name
module Simple_in_one_joined_env = Int_ids_in_one_joined_env.Simple

module Type_in_env () : sig
  type t = private TG.t

  val kind : t -> K.t

  val create : TG.t -> t

  val create_equations : TG.t Name.Map.t -> t Name.Map.t
end = struct
  type t = TG.t

  let kind = TG.kind

  let create ty = ty

  let create_equations equations = equations
end

module Type_in_target_env : sig
  include module type of Type_in_env ()

  val alias_type_of : K.t -> Simple_in_target_env.t -> t
end = struct
  include Type_in_env ()

  let alias_type_of kind simple =
    create (TG.alias_type_of kind (simple : Simple_in_target_env.t :> Simple.t))
end

module Type_in_one_joined_env : sig
  include module type of Type_in_env ()

  val alias_type_of : K.t -> Simple_in_one_joined_env.t -> t
end = struct
  include Type_in_env ()

  let alias_type_of kind simple =
    create
      (TG.alias_type_of kind (simple : Simple_in_one_joined_env.t :> Simple.t))
end

(* {1:environments Environments} *)

module Simples_in_joined_envs : sig
  include Container_types.S with type t = Simple_in_one_joined_env.t Index.Map.t

  val of_list : (Index.t * Simple.t) list -> t

  val choose_a_suitable_name : t -> string
end = struct
  module T0 = struct
    type t = Simple_in_one_joined_env.t Index.Map.t

    let print = Index.Map.print Simple_in_one_joined_env.print

    let hash map =
      Index.Map.fold
        (fun index simple hash ->
          Hashtbl.hash
            (hash, Index.hash index, Simple_in_one_joined_env.hash simple))
        map (Hashtbl.hash 0)

    let equal = Index.Map.equal Simple_in_one_joined_env.equal

    let compare = Index.Map.compare Simple_in_one_joined_env.compare
  end

  include T0
  include Container_types.Make (T0)

  let of_list simples =
    List.fold_left
      (fun simples (index, simple) ->
        Index.Map.add index (Simple_in_one_joined_env.create simple) simples)
      Index.Map.empty simples

  let choose_a_suitable_name t =
    let shared_name =
      try
        Index.Map.fold
          (fun _ simple raw_name ->
            Simple.pattern_match' simple
              ~const:(fun _ -> raw_name)
              ~symbol:(fun _ ~coercion:_ -> raw_name)
              ~var:(fun var ~coercion:_ ->
                let var_name = Variable.raw_name var in
                match raw_name with
                | None -> Some var_name
                | Some raw_name when String.equal raw_name var_name ->
                  Some raw_name
                | Some _ -> raise Not_found))
          (t : t :> Simple.t Index.Map.t)
          None
      with Not_found -> None
    in
    match shared_name with Some raw_name -> raw_name | None -> "join_var"
end

module Source_env : sig
  type t

  val create : TE.t -> t

  val machine_width : t -> Target_system.Machine_width.t

  val exists_in_source_env : t -> Variable.t -> Variable_in_source_env.t option

  type candidate_canonical_in_source_env =
    | No_simples_in_joined_envs  (** The provided set of simples was empty. *)
    | No_canonical_in_source_env
        (** There is no [simple] in the source environment that is equal to this
            specific set of simples in each joined environment. *)
    | Canonical_in_all_joined_envs of Simple_in_one_joined_env.t
        (** This [simple] is canonical in all the joined environments.

            It may or may not be defined in the source environment. *)
    | Latest_bound_source_var of Variable_in_source_env.t * Coercion.t
        (** This variable is the one with the latest binding time amongst the
            variables in joined environments that exist in the source
            environment.

            If there is any simple in the source environment that is equal to
            the provided set of simples in each joined environments, it can only
            be this variable because of our assumption on binding times being
            coherent (see {!section-coherent_binding_times}). *)

  val candidate_canonical_in_source_env :
    t -> Simples_in_joined_envs.t -> candidate_canonical_in_source_env
end = struct
  type t = { source_env : TE.t } [@@unboxed]

  let create source_env = { source_env }

  let machine_width { source_env; _ } = TE.machine_width source_env

  let exists_in_source_env { source_env } var =
    if TE.mem ~min_name_mode:Name_mode.in_types source_env (Name.var var)
    then Some (Variable_in_source_env.create var)
    else None

  let total_compare_binding_times { source_env } var1 var2 =
    TE.stable_compare_simples source_env
      (Simple.var (var1 : Variable_in_source_env.t :> Variable.t))
      (Simple.var (var2 : Variable_in_source_env.t :> Variable.t))

  type candidate_canonical_in_source_env =
    | No_simples_in_joined_envs
    | No_canonical_in_source_env
    | Canonical_in_all_joined_envs of Simple_in_one_joined_env.t
    | Latest_bound_source_var of Variable_in_source_env.t * Coercion.t

  let candidate_canonical_in_source_env t canonicals_in_joined_envs =
    Index.Map.fold
      (fun _index canonical possible_canonical_in_source_env ->
        let[@inline] pattern_match_local_simple simple ~local_simple ~source_var
            =
          Simple_in_one_joined_env.pattern_match' simple
            ~const:(fun _ -> local_simple simple)
            ~symbol:(fun _ ~coercion:_ -> local_simple simple)
            ~var:(fun var ~coercion ->
              match
                exists_in_source_env t
                  (var : Variable_in_one_joined_env.t :> Variable.t)
              with
              | None -> local_simple simple
              | Some var -> (source_var [@inlined hint]) var ~coercion)
        in
        let maybe_this_source_var () =
          pattern_match_local_simple canonical
            ~local_simple:(fun _ -> No_canonical_in_source_env)
            ~source_var:(fun var ~coercion ->
              Latest_bound_source_var (var, coercion))
        in
        let latest_source_var_with var ~coercion =
          pattern_match_local_simple canonical
            ~local_simple:(fun _ -> Latest_bound_source_var (var, coercion))
            ~source_var:(fun var0 ~coercion:coercion0 ->
              let c = total_compare_binding_times t var var0 in
              if c < 0
              then Latest_bound_source_var (var0, coercion0)
              else (
                if not (c > 0 || Variable_in_source_env.equal var0 var)
                then
                  Misc.fatal_errorf "Non-total extension of binding times order";
                Latest_bound_source_var (var, coercion)))
        in
        match possible_canonical_in_source_env with
        | No_simples_in_joined_envs -> Canonical_in_all_joined_envs canonical
        | No_canonical_in_source_env -> maybe_this_source_var ()
        | Latest_bound_source_var (var, coercion) ->
          latest_source_var_with var ~coercion
        | Canonical_in_all_joined_envs shared_simple ->
          if Simple_in_one_joined_env.equal canonical shared_simple
          then possible_canonical_in_source_env
          else
            pattern_match_local_simple shared_simple
              ~local_simple:(fun _ -> maybe_this_source_var ())
              ~source_var:(fun var ~coercion ->
                latest_source_var_with var ~coercion))
      canonicals_in_joined_envs No_simples_in_joined_envs
end

module Bindings_in_target_env : sig
  (* This module is only concerned with providing a consistent name to represent
     a set of simples in the joined environments.

     Names in the target environment are either names that exist in the source
     environment, or local variables that are created in the target environment
     but do not exist in the source environment.

     We currently maintain two types of relations between names in the joined
     environment and names in the target environment:

     - Imported variables represent a specific variable in all the joined
     environments where it exists.

     - Existentials represent a specific set of simples in the joined
     environments.

     {b Warning}: Each local variable is defined either as an imported variable
     or as an existential, but imported variables and existentials are not
     necessarily represented by local variables in the target environment: there
     might already be a suitable name in the source environment to represent
     this imported variable or existential.

     For instance, consider that we are doing the join of [x: (= a)] in env 0
     and [x: (= b)] in env 1, where [x] exists in the source environment but not
     [a] and [b]. Then we can use [x] to represent [((0 a) (1 b))], we do not
     have to create a local variable. Note that in the case of imported
     variables, this effectively mean that we can rename variables as we import
     them. *)

  type t

  val from_source_env : Source_env.t -> t

  val source_env : t -> Source_env.t

  val exists_in_target_env : t -> Variable.t -> Variable_in_target_env.t option

  (* Must only be called from the toplevel join, before creating any local
     variable. *)
  val add_alias_between_names_in_source_env :
    t -> K.t -> Name_in_source_env.t -> Simple_in_source_env.t -> t

  (* Record the [name_in_source_env] as the canonical name for this set of
     simples in joined environments. If there was already a [name_in_source_env]
     representing that set of simple, an alias is recorded in the
     [alias_types_in_target_env] instead.

     Must only be called from the toplevel join, before creating any local
     variable. *)
  val add_existential_for_these_simples :
    t ->
    name_in_source_env:Name_in_source_env.t ->
    Simples_in_joined_envs.t ->
    K.t ->
    t

  (* Record the [name_in_source_env] as the canonical name for this variable
     (whenever it appears in any joined environment). If there was already a
     [name_in_source_env] representing the variable, an alias is recorded in the
     [alias_types_in_target_env] instead.

     Must only be called from the toplevel join, before creating any local
     variable. *)
  val add_imported_var :
    t ->
    name_in_source_env:Name_in_source_env.t ->
    coercion_to_name_in_source_env:Coercion.t ->
    Variable_in_one_joined_env.t ->
    K.t ->
    t

  (* Return the (unique across the whole join) name to be used to represent an
     imported variable.

     This is usually the provided variable itself, unless a name for it was
     recorded by [add_imported_var]. *)
  val import_from_all_envs :
    t -> Variable_in_one_joined_env.t -> K.t -> Simple_in_target_env.t * t

  (* Return the (unique across the whole join) name to be used to represent this
     set of simples in joined environments.

     This is either a name that has been recorded with
     [add_existential_for_these_simples], or an existential local variable
     created to represent it. *)
  val existential_for_these_simples :
    t -> Simples_in_joined_envs.t -> K.t -> Simple_in_target_env.t * t

  (* Return the canonical name of an imported variable in the target env.

     If the variable has not been used (neither [add_imported_var] nor
     [import_from_all_envs] has been called on it), return [None]. *)
  val is_imported_from_all_joined_envs :
    t -> Variable_in_one_joined_env.t -> Simple_in_target_env.t option

  (* Return the canonical name of a set of simples in joined environments.

     If this set of simples has not been used (neither
     [add_existential_for_these_simples] nor [existential_for_these_simples] has
     been called on it), returns [None]. *)
  val has_existential_for_these_simples :
    t -> Simples_in_joined_envs.t -> Simple_in_target_env.t option

  type definition_in_joined_envs =
    | Imported_var of Variable_in_one_joined_env.t * K.t
    | These_canonicals of Simple_in_one_joined_env.t Index.Map.t * K.t

  val alias_types_in_target_env :
    t -> Type_in_target_env.t Name_in_source_env.Map.t

  (* Assuming that [since] derives from [t], returns the definitions of local
     variables that have been added to [t] after [since]. *)
  val new_bindings :
    t -> since:t -> definition_in_joined_envs Name_in_target_env.Map.t

  val fold_created_variables :
    (Variable_in_target_env.t -> K.t -> 'a -> 'a) -> t -> 'a -> 'a

  (* These are for the join of env extensions, see [prepare_nested_join]. *)

  val replay_definition_of_aliases_in_target_env :
    t ->
    Index.t ->
    Type_in_one_joined_env.t Name.Map.t ->
    Type_in_one_joined_env.t Name.Map.t

  val definition_of_local_variables_in_one_joined_env :
    t -> Index.t -> Type_in_one_joined_env.t Variable_in_target_env.Map.t
end = struct
  type coercion_to_canonical_in_target_env = Coercion.t

  type definition_in_joined_envs =
    | Imported_var of Variable_in_one_joined_env.t * K.t
    | These_canonicals of Simple_in_one_joined_env.t Index.Map.t * K.t

  type t =
    { source_env : Source_env.t;
      alias_types_in_target_env : Type_in_target_env.t Name_in_source_env.Map.t;
      (* Aliases shared across all the joined environments. *)
      existential_for_these_simples :
        Simple_in_target_env.t Simples_in_joined_envs.Map.t;
      (* Maps a set of [simples] in joined environments to the (unique across
         the whole join) name used to represent this exact set of simples in the
         target environment. *)
      imported_variables :
        Simple_in_target_env.t Variable_in_one_joined_env.Map.t;
      (* Maps a local variable (a variable that exists in at least one joined
         environment) to its (unique across the whole join) name in the target
         environment.

         This same name is used for all the joined environments where the
         variable exists.

         {b Note}: This is not necessarily the variable itself. For instance, if
         there is a variable [x] in the source environment that gets demoted to
         a local variable [y] in all joined environments, then [x] might be used
         as a name for [y]. *)
      created_variables : K.t Variable_in_target_env.Map.t;
      (* This contains all the local variables (existentials or imported) that
         exist in the target environment but not in the source environment. *)
      definitions_in_joined_envs :
        definition_in_joined_envs Name_in_target_env.Map.t;
      (* Reverse map from local variables to their definition. *)
      aliases_of_names_in_joined_envs :
        coercion_to_canonical_in_target_env Name_in_target_env.Map.t
        Name_in_one_joined_env.Map.t
        Index.Map.t;
      (* For each joined environment and each name in the joined environment,
         record the set of names in the target environment it is equal to (and
         the corresponding coercions).

         This is used to implement [replay_definitions_of_aliases_in_target_env]
         in the join of env extensions, see [prepare_nested_join]. *)
      equations_for_local_vars :
        Type_in_one_joined_env.t Variable_in_target_env.Map.t Index.Map.t
          (* Environment extensions to use in each of the joined environments to
             replay the definition of local variables.

             This is used to implement
             [definitions_of_local_variables_in_one_joined_env] in the join of
             env extensions, see [prepare_nested_join]. *)
    }

  let from_source_env source_env =
    { source_env;
      alias_types_in_target_env = Name_in_source_env.Map.empty;
      existential_for_these_simples = Simples_in_joined_envs.Map.empty;
      imported_variables = Variable_in_one_joined_env.Map.empty;
      aliases_of_names_in_joined_envs = Index.Map.empty;
      definitions_in_joined_envs = Name_in_target_env.Map.empty;
      equations_for_local_vars = Index.Map.empty;
      created_variables = Variable_in_target_env.Map.empty
    }

  let add_alias t kind ~canonical_element:canonical_element_with_coercion
      ~name_to_be_demoted ~coercion_to_name_to_be_demoted =
    let canonical_element =
      canonical_element_with_coercion |> Simple_in_target_env.without_coercion
    in
    let coercion_from_canonical_element_to_name_to_be_demoted =
      Coercion.compose_exn
        (Simple_in_target_env.coercion canonical_element_with_coercion)
        ~then_:coercion_to_name_to_be_demoted
    in
    if
      Simple_in_target_env.equal canonical_element
        (Simple_in_target_env.from_source_env
           (Simple_in_source_env.name name_to_be_demoted))
    then
      if Coercion.is_id coercion_from_canonical_element_to_name_to_be_demoted
      then t
      else
        Misc.fatal_errorf
          "Cannot add alias of %a to itself with a non-identity coercion@ %a"
          Simple_in_target_env.print canonical_element Coercion.print
          (Coercion.inverse
             coercion_from_canonical_element_to_name_to_be_demoted)
    else
      let alias_types_in_target_env =
        Name_in_source_env.Map.add name_to_be_demoted
          (Type_in_target_env.alias_type_of kind
             (Simple_in_target_env.apply_coercion_exn canonical_element
                coercion_from_canonical_element_to_name_to_be_demoted))
          t.alias_types_in_target_env
      in
      { t with alias_types_in_target_env }

  let alias_types_in_target_env t = t.alias_types_in_target_env

  let new_bindings t ~since =
    Name_in_target_env.Map.diff_shared
      (fun _ new_definition _old_definition -> Some new_definition)
      t.definitions_in_joined_envs since.definitions_in_joined_envs

  let source_env { source_env; _ } = source_env

  let is_local_variable { created_variables; _ } name =
    Name_in_target_env.pattern_match name
      ~symbol:(fun _ -> None)
      ~var:(fun var ->
        if Variable_in_target_env.Map.mem var created_variables
        then Some var
        else None)

  let exists_in_target_env t var =
    match Source_env.exists_in_source_env (source_env t) var with
    | Some var_in_source_env ->
      Some (Variable_in_target_env.from_source_env var_in_source_env)
    | None ->
      let var = Variable_in_target_env.create var in
      if Variable_in_target_env.Map.mem var t.created_variables
      then Some var
      else None

  let add_alias_between_names_in_source_env t kind name canonical =
    assert (not (Name_in_source_env.Map.mem name t.alias_types_in_target_env));
    assert (
      not
        (Name_in_target_env.Map.mem
           (Name_in_target_env.from_source_env name)
           t.definitions_in_joined_envs));
    add_alias t kind ~name_to_be_demoted:name
      ~coercion_to_name_to_be_demoted:Coercion.id
      ~canonical_element:(Simple_in_target_env.from_source_env canonical)

  let update_aliases_of_names_in_joined_envs ~f simples aliases_in_target_env =
    Index.Map.fold
      (fun index simple aliases_in_target_env ->
        Simple_in_one_joined_env.pattern_match simple
          ~const:(fun _ -> aliases_in_target_env)
          ~name:(fun name ~coercion ->
            Index.Map.update index
              (fun aliases ->
                let aliases =
                  Name_in_one_joined_env.Map.update name
                    (fun aliases ->
                      let aliases =
                        f coercion
                          (Option.value ~default:Name_in_target_env.Map.empty
                             aliases)
                      in
                      Some aliases)
                    (Option.value ~default:Name_in_one_joined_env.Map.empty
                       aliases)
                in
                Some aliases)
              aliases_in_target_env))
      simples aliases_in_target_env

  let record_definition_for t ~name_in_target_env
      ~coercion_to_name_in_target_env definition =
    (* name_in_target_env ~ coercion_to_name_in_target_env(definition) *)
    let definitions_in_joined_envs =
      Name_in_target_env.Map.add name_in_target_env definition
        t.definitions_in_joined_envs
    in
    let canonical_in_target_env =
      Simple_in_target_env.name
        ~coercion:(Coercion.inverse coercion_to_name_in_target_env)
        name_in_target_env
    in
    match definition with
    | Imported_var (imported_var, _kind) ->
      let imported_variables =
        Variable_in_one_joined_env.Map.add imported_var canonical_in_target_env
          t.imported_variables
      in
      ( canonical_in_target_env,
        { t with imported_variables; definitions_in_joined_envs } )
    | These_canonicals (simples, kind) ->
      let existential_for_these_simples =
        Simples_in_joined_envs.Map.add simples canonical_in_target_env
          t.existential_for_these_simples
      in
      (* The following is some bookkeeping so that we know how to replay the
         definition of existential variables during nested joins (i.e. joins of
         env extensions); see {!section-extensions}. *)
      let equations_for_local_vars =
        (* If the variable is a fresh variable, record it so that we can replay
           its definition during the join of env extensions. *)
        match is_local_variable t name_in_target_env with
        | None -> t.equations_for_local_vars
        | Some var ->
          Index.Map.update_many
            (fun _index existentials simple ->
              let ty =
                Type_in_one_joined_env.alias_type_of kind
                  (Simple_in_one_joined_env.apply_coercion_exn simple
                     coercion_to_name_in_target_env)
              in
              let existentials_in_one_joined_env =
                Variable_in_target_env.Map.add var ty
                  (Option.value ~default:Variable_in_target_env.Map.empty
                     existentials)
              in
              Some existentials_in_one_joined_env)
            t.equations_for_local_vars simples
      in
      let aliases_of_names_in_joined_envs =
        update_aliases_of_names_in_joined_envs simples
          t.aliases_of_names_in_joined_envs ~f:(fun coercion aliases ->
            (* name_in_target_env ~ coercion_to_name_in_target_env(definition) *)
            (* definition ~ coercion(name_in_joined_env) *)
            let coercion_from_joined_to_target =
              Coercion.compose_exn coercion
                ~then_:coercion_to_name_in_target_env
            in
            Name_in_target_env.Map.add name_in_target_env
              coercion_from_joined_to_target aliases)
      in
      ( canonical_in_target_env,
        { t with
          equations_for_local_vars;
          existential_for_these_simples;
          aliases_of_names_in_joined_envs;
          definitions_in_joined_envs
        } )

  let create_name_for_definition t definition =
    let var, kind =
      match definition with
      | Imported_var (var, kind) ->
        let var = (var : Variable_in_one_joined_env.t :> Variable.t) in
        Variable_in_target_env.create var, kind
      | These_canonicals (simples, kind) ->
        let name = Simples_in_joined_envs.choose_a_suitable_name simples in
        Variable_in_target_env.create (Variable.create name kind), kind
    in
    let created_variables =
      Variable_in_target_env.Map.add var kind t.created_variables
    in
    let t = { t with created_variables } in
    let name_in_target_env = Name_in_target_env.var var in
    record_definition_for t ~name_in_target_env
      ~coercion_to_name_in_target_env:Coercion.id definition

  let is_imported_from_all_joined_envs t var =
    Variable_in_one_joined_env.Map.find_opt var t.imported_variables

  let has_existential_for_these_simples t simples =
    Simples_in_joined_envs.Map.find_opt simples t.existential_for_these_simples

  let existing_canonical_for t definition =
    match definition with
    | Imported_var (imported_var, _kind) ->
      is_imported_from_all_joined_envs t imported_var
    | These_canonicals (simples, _kind) ->
      has_existential_for_these_simples t simples

  let add_name_for_definition t ~name_in_source_env
      ~coercion_to_name_in_source_env definition =
    (* name_in_source_env ~ coercion_to_name_in_source_env(definition) *)
    let name_in_target_env =
      Name_in_target_env.from_source_env name_in_source_env
    in
    match existing_canonical_for t definition with
    | None ->
      let _, t =
        record_definition_for t ~name_in_target_env
          ~coercion_to_name_in_target_env:coercion_to_name_in_source_env
          definition
      in
      t
    | Some existing_canonical ->
      (* This can happen if multiple variables in the source environment are
         demoted to the same local variable in all environments, in which case
         we record an alias between these names instead.

         Note that we might add aliases in the "wrong" order w.r.t binding times
         here, but it is not a problem -- we only care that we have a single
         definition for each name during the join. We don't have to use the
         proper binding time ordering; the typing env will take care of giving
         the proper orientation to aliases after the join is complete. *)
      let kind =
        match definition with
        | Imported_var (_, kind) | These_canonicals (_, kind) -> kind
      in
      add_alias t kind ~canonical_element:existing_canonical
        ~name_to_be_demoted:name_in_source_env
        ~coercion_to_name_to_be_demoted:coercion_to_name_in_source_env

  let add_existential_for_these_simples t ~name_in_source_env simples kind =
    add_name_for_definition t ~name_in_source_env
      ~coercion_to_name_in_source_env:Coercion.id
      (These_canonicals (simples, kind))

  let add_imported_var t ~name_in_source_env ~coercion_to_name_in_source_env
      imported_var kind =
    add_name_for_definition t ~name_in_source_env
      ~coercion_to_name_in_source_env
      (Imported_var (imported_var, kind))

  let get_or_create_canonical_for t definition =
    match existing_canonical_for t definition with
    | None -> create_name_for_definition t definition
    | Some existing_canonical -> existing_canonical, t

  let existential_for_these_simples t simples kind =
    get_or_create_canonical_for t (These_canonicals (simples, kind))

  let import_from_all_envs t imported_var kind =
    get_or_create_canonical_for t (Imported_var (imported_var, kind))

  let replay_definition_of_aliases_in_target_env t index equations =
    match Index.Map.find_opt index t.aliases_of_names_in_joined_envs with
    | None -> equations
    | Some aliases_in_target_env ->
      fold_binary_join equations
        (aliases_in_target_env
          : Coercion.t Name_in_target_env.Map.t Name_in_one_joined_env.Map.t
          :> Coercion.t Name_in_target_env.Map.t Name.Map.t)
        ~init:equations
        ~f:(fun[@inline] name ty aliases equations ->
          let kind = Type_in_one_joined_env.kind ty in
          let name = Name_in_one_joined_env.create name in
          Name_in_target_env.Map.fold
            (fun alias coercion equations ->
              (* alias = coercion(name) *)
              let ty =
                Type_in_one_joined_env.alias_type_of kind
                  (Simple_in_one_joined_env.name ~coercion name)
              in
              Name.Map.add (alias : Name_in_target_env.t :> Name.t) ty equations)
            aliases equations)

  let definition_of_local_variables_in_one_joined_env t index =
    match Index.Map.find_opt index t.equations_for_local_vars with
    | None -> Variable_in_target_env.Map.empty
    | Some existentials -> existentials

  let fold_created_variables f t acc =
    (* CR bclement: iterate in order consistent with the recorded binding
       times. *)
    Variable_in_target_env.Map.fold f t.created_variables acc
end

module Joined_envs : sig
  type t

  (* We use an [incremental] type for equations because the join of env
     extensions needs to know about the equations that exist outside of the join
     extension.

     The [previous] field correspond to the equations added at higher scopes
     (one layer of env extensions removed), and is empty for a top-level
     join. *)
  val create :
    (TE.t * Type_in_one_joined_env.t Name.Map.t incremental) Index.Map.t -> t

  val get_nth_joined_env : t -> Index.t -> TE.t

  val equations_in_nth_joined_env :
    t -> Index.t -> Type_in_one_joined_env.t Name.Map.t

  val keys : t -> Index.Set.t

  val alias_types_of :
    t ->
    K.t ->
    Variable_in_one_joined_env.t ->
    Type_in_one_joined_env.t Index.Map.t

  val expand_heads :
    t ->
    Type_in_one_joined_env.t Index.Map.t ->
    Type_in_one_joined_env.t Index.Map.t

  val equal_in_all_joined_envs :
    t -> Simple_in_one_joined_env.t -> Simples_in_joined_envs.t -> bool
end = struct
  type t =
    { envs_and_equations :
        (TE.t * Type_in_one_joined_env.t Name.Map.t incremental) Index.Map.t
    }
  [@@unboxed]

  let create envs_and_equations = { envs_and_equations }

  let envs_and_equations = function
    | { envs_and_equations } -> envs_and_equations

  let get_nth_joined_env t index =
    match Index.Map.find_opt index (envs_and_equations t) with
    | Some (one_joined_env, _) -> one_joined_env
    | None ->
      Misc.fatal_errorf "Join does not include environment %a" Index.print index

  let equations_in_nth_joined_env t index =
    match Index.Map.find_opt index (envs_and_equations t) with
    | None ->
      Misc.fatal_errorf "Join does not include environment %a" Index.print index
    | Some (_, { current; _ }) -> current

  let keys t = Index.Map.keys (envs_and_equations t)

  let get_canonical_simple_ignoring_name_mode typing_env simple =
    Simple_in_one_joined_env.create
      (TE.get_canonical_simple_ignoring_name_mode typing_env
         (simple : Simple_in_one_joined_env.t :> Simple.t))

  let equal_in_all_joined_envs t simple simples_in_joined_envs =
    Index.Map.for_all
      (fun index canonical ->
        (* Avoid env lookup when not necessary *)
        Simple_in_one_joined_env.equal canonical simple
        || Simple_in_one_joined_env.equal canonical
             (get_canonical_simple_ignoring_name_mode
                (get_nth_joined_env t index)
                simple))
      simples_in_joined_envs

  let alias_types_of t kind var =
    Index.Map.filter_map
      (fun _index (env, _) ->
        if
          TE.mem ~min_name_mode:Name_mode.in_types env
            (Name.var (var : Variable_in_one_joined_env.t :> Variable.t))
        then
          let canonical =
            get_canonical_simple_ignoring_name_mode env
              (Simple_in_one_joined_env.var var)
          in
          Some (Type_in_one_joined_env.alias_type_of kind canonical)
        else None)
      (envs_and_equations t)

  let expand_heads t types =
    Index.Map.mapi
      (fun index ty ->
        let typing_env = get_nth_joined_env t index in
        Type_in_one_joined_env.create
          (ET.to_type
             (Expand_head.expand_head typing_env
                (ty : Type_in_one_joined_env.t :> TG.t))))
      types
end

(** {1 Public interface} *)

type env_id = Index.t

type 'a join_arg = env_id * 'a

type t =
  { joined_envs : Joined_envs.t;
    bindings : Bindings_in_target_env.t
  }

type n_way_join_type = t -> TG.t join_arg list -> TG.t Or_unknown.t * t

let joined_env t index = Joined_envs.get_nth_joined_env t.joined_envs index

let machine_width t =
  Source_env.machine_width (Bindings_in_target_env.source_env t.bindings)

type canonical_in_target_env =
  | Canonical_in_source_env of Simple_in_source_env.t
  | Import_from_all_joined_envs of Variable_in_one_joined_env.t * Coercion.t
  | Existential_for_these_simples

let get_canonical_in_target_env ~bindings ~joined_envs canonicals_in_joined_envs
    =
  let source_env = Bindings_in_target_env.source_env bindings in
  match
    Source_env.candidate_canonical_in_source_env source_env
      canonicals_in_joined_envs
  with
  | No_simples_in_joined_envs | No_canonical_in_source_env ->
    Existential_for_these_simples
  | Canonical_in_all_joined_envs simple ->
    Simple_in_one_joined_env.pattern_match' simple
      ~const:(fun const ->
        Canonical_in_source_env (Simple_in_source_env.const const))
      ~symbol:(fun symbol ~coercion ->
        Canonical_in_source_env
          (Simple_in_source_env.symbol ~coercion
             (Symbol_in_one_joined_env.in_source_env symbol)))
      ~var:(fun var ~coercion ->
        match
          Source_env.exists_in_source_env source_env
            (var : Variable_in_one_joined_env.t :> Variable.t)
        with
        | Some var ->
          Canonical_in_source_env (Simple_in_source_env.var ~coercion var)
        | None -> Import_from_all_joined_envs (var, coercion))
  | Latest_bound_source_var (var, coercion) ->
    let latest_simple = Simple_in_source_env.var var ~coercion in
    if
      Joined_envs.equal_in_all_joined_envs joined_envs
        (Simple_in_one_joined_env.from_source_env latest_simple)
        canonicals_in_joined_envs
    then Canonical_in_source_env latest_simple
    else Existential_for_these_simples

let fold_incremental_join ~f ~init equations_to_join =
  fold_incremental_join ~f ~init
    { fold =
        (fun[@inline] f init ->
          Index.Map.fold
            (fun index (env, maps) -> f (index, env) maps)
            equations_to_join init)
    }

type types_in_joined_envs =
  | Equals_in_all_envs of Simples_in_joined_envs.t * K.t
  | No_alias_in_some_env of Type_in_one_joined_env.t Index.Map.t

let get_types_in_joined_envs join_entry : _ Or_bottom.t =
  let kind, canonicals, concrete_equations =
    fold_incremental_join_entry join_entry
      ~init:(None, Index.Map.empty, Index.Map.empty)
      ~f:(fun (index, env) ty (kind, canonicals, concrete_equations) ->
        let kind =
          match kind with
          | None -> Type_in_one_joined_env.kind ty
          | Some kind ->
            if not (K.equal kind (Type_in_one_joined_env.kind ty))
            then Misc.fatal_errorf "Incompatible kinds for variable during join";
            kind
        in
        match TG.get_alias_opt (ty : Type_in_one_joined_env.t :> TG.t) with
        | None ->
          let concrete_equations = Index.Map.add index ty concrete_equations in
          Some kind, canonicals, concrete_equations
        | Some simple ->
          let canonical =
            Simple_in_one_joined_env.create
              (TE.get_canonical_simple_ignoring_name_mode env simple)
          in
          let canonicals = Index.Map.add index canonical canonicals in
          Some kind, canonicals, concrete_equations)
  in
  match kind with
  | None ->
    assert (
      Index.Map.is_empty canonicals && Index.Map.is_empty concrete_equations);
    Bottom
  | Some kind ->
    if Index.Map.is_empty concrete_equations
    then Ok (Equals_in_all_envs (canonicals, kind))
    else
      (* CR-someday bclement: We could create a fresh (unique) existential here,
         which would allow to preserve more information about identity in
         subsequent joins, but it's not clear it would be useful. *)
      let alias_equations =
        Index.Map.map
          (fun simple -> Type_in_one_joined_env.alias_type_of kind simple)
          canonicals
      in
      Ok
        (No_alias_in_some_env
           (Index.Map.disjoint_union alias_equations concrete_equations))

(* Wrapper around [fold_incremental_join] so that we only fold over equations
   for names that exist in the target env (see [prepare_nested_join]). *)
let fold_incremental_join_in_target_env equations_to_join ~exists_in_target_env
    ~init ~f =
  fold_incremental_join equations_to_join ~init ~f:(fun name join_entry acc ->
      Name.pattern_match name
        ~var:(fun var ->
          match exists_in_target_env var with
          | None -> acc
          | Some var_in_target_env ->
            f (Name_in_target_env.var var_in_target_env) join_entry acc)
        ~symbol:(fun symbol ->
          (* See {!section-lifted_constants} *)
          let symbol = Symbol_in_target_env.create symbol in
          let name = Name_in_target_env.symbol symbol in
          f name join_entry acc))

let fold_incremental_join_in_source_env equations_to_join ~exists_in_source_env
    ~init ~f =
  fold_incremental_join equations_to_join ~init ~f:(fun name join_entry acc ->
      Name.pattern_match name
        ~var:(fun var ->
          match exists_in_source_env var with
          | None -> acc
          | Some var_in_target_env ->
            f (Name_in_source_env.var var_in_target_env) join_entry acc)
        ~symbol:(fun symbol ->
          (* See {!section-lifted_constants} *)
          let symbol = Symbol_in_source_env.create symbol in
          let name = Name_in_source_env.symbol symbol in
          f name join_entry acc))

(* This function is responsible for splitting the [equations_to_join] between
   those that are demotions in all joined environments, that are replayed in the
   target environment by adding to the bindings, and the rest, that are expanded
   to equations on concrete types that will be joined later.

   Note that we only care about names that have new types in all of the joined
   environments, otherwise the join can never be more precise than what we had
   initially. We also only care about names that exist in the target
   environments; other names will be imported automatically during the join of
   types and only if they are reachable.

   {b Note}: This function is only used during a top-level join. For nested
   joins, it would be incorrect to record aliases into the bindings (since they
   are only valid during the env extension, and the bindings must be valid for
   the whole join); [join_aliases_in_env_extension] is used instead. *)
let join_aliases_into_bindings ~joined_envs ~bindings equations_to_join =
  fold_incremental_join_in_source_env equations_to_join
    ~exists_in_source_env:
      (Source_env.exists_in_source_env
         (Bindings_in_target_env.source_env bindings))
    ~init:(Name_in_target_env.Map.empty, bindings)
    ~f:(fun name join_entry (equations_to_join, bindings) ->
      match get_types_in_joined_envs join_entry with
      | Bottom -> Misc.fatal_error "Unexpected bottom during join"
      | Ok (No_alias_in_some_env types) ->
        let equations_to_join =
          Name_in_target_env.Map.add
            (Name_in_target_env.from_source_env name)
            types equations_to_join
        in
        equations_to_join, bindings
      | Ok (Equals_in_all_envs (canonicals, kind)) -> (
        match get_canonical_in_target_env ~bindings ~joined_envs canonicals with
        | Canonical_in_source_env canonical ->
          let bindings =
            Bindings_in_target_env.add_alias_between_names_in_source_env
              bindings kind name canonical
          in
          equations_to_join, bindings
        | Import_from_all_joined_envs (var, coercion) ->
          (* name = coercion(var) *)
          let bindings =
            Bindings_in_target_env.add_imported_var bindings
              ~name_in_source_env:name ~coercion_to_name_in_source_env:coercion
              var kind
          in
          equations_to_join, bindings
        | Existential_for_these_simples ->
          let bindings =
            Bindings_in_target_env.add_existential_for_these_simples bindings
              ~name_in_source_env:name canonicals kind
          in
          equations_to_join, bindings))

let n_way_join_round ~(n_way_join_type : n_way_join_type) t equations_to_join
    types_in_target_env =
  Name_in_target_env.Map.fold
    (fun name types (types_in_target_env, t) ->
      if
        Flambda_features.check_light_invariants ()
        && Name_in_target_env.Map.mem name types_in_target_env
      then
        Misc.fatal_errorf
          "Processing join of %a but we already have a type for it."
          Name_in_target_env.print name;
      match
        n_way_join_type t
          (Index.Map.bindings (Joined_envs.expand_heads t.joined_envs types)
            : (Index.t * Type_in_one_joined_env.t) list
            :> (Index.t * TG.t) list)
      with
      | Unknown, t -> types_in_target_env, t
      | Known ty, t ->
        let ty = Type_in_target_env.create ty in
        Name_in_target_env.Map.add name ty types_in_target_env, t)
    equations_to_join (types_in_target_env, t)

(** {2:n-way-join Cut and n-way join} *)

let get_canonical_simple_if_exists ~bindings ~joined_envs simples =
  match get_canonical_in_target_env ~bindings ~joined_envs simples with
  | Canonical_in_source_env simple ->
    Some (Simple_in_target_env.from_source_env simple)
  | Import_from_all_joined_envs (var, coercion) -> (
    match
      Bindings_in_target_env.is_imported_from_all_joined_envs bindings var
    with
    | Some simple ->
      Some (Simple_in_target_env.apply_coercion_exn simple coercion)
    | None -> None)
  | Existential_for_these_simples ->
    Bindings_in_target_env.has_existential_for_these_simples bindings simples

let n_way_join_symbol_projections t symbol_projections_to_join =
  let joined_projections =
    Index.Map.fold
      (fun index symbol_projections acc ->
        let typing_env = Joined_envs.get_nth_joined_env t.joined_envs index in
        Variable_in_one_joined_env.Map.fold
          (fun var symbol_projection acc ->
            let canonical =
              TE.get_canonical_simple_ignoring_name_mode typing_env
                (Simple.var (var : Variable_in_one_joined_env.t :> Variable.t))
            in
            let canonical = Simple_in_one_joined_env.create canonical in
            Symbol_projection.Map.update symbol_projection
              (fun joined_projections ->
                let joined_projections =
                  Option.value joined_projections ~default:Index.Map.empty
                in
                Some (Index.Map.add index canonical joined_projections))
              acc)
          symbol_projections acc)
      symbol_projections_to_join Symbol_projection.Map.empty
  in
  let all_indices = Joined_envs.keys t.joined_envs in
  Symbol_projection.Map.fold
    (fun symbol_projection simples symbol_projections ->
      if not (Index.Set.subset all_indices (Index.Map.keys simples))
      then symbol_projections
      else
        match
          get_canonical_simple_if_exists ~bindings:t.bindings
            ~joined_envs:t.joined_envs simples
        with
        | Some simple ->
          Simple_in_target_env.pattern_match' simple
            ~var:(fun var ~coercion ->
              if Coercion.is_id coercion
              then
                Variable_in_target_env.Map.add var symbol_projection
                  symbol_projections
              else symbol_projections)
            ~symbol:(fun _ ~coercion:_ -> symbol_projections)
            ~const:(fun _ -> symbol_projections)
        | None -> symbol_projections)
    joined_projections Variable_in_target_env.Map.empty

let cut_for_join typing_env ~cut_after =
  let level = TE.cut typing_env ~cut_after in
  let equations =
    Type_in_one_joined_env.create_equations (TEL.equations level)
  in
  let incremental_equations =
    { previous = Name.Map.empty; diff = equations; current = equations }
  in
  let symbol_projections =
    Variable_in_one_joined_env.create_map (TEL.symbol_projections level)
  in
  incremental_equations, symbol_projections

let cut_and_n_way_join0 ~n_way_join_type ~meet_type ~cut_after source_env
    joined_envs equations_to_join symbol_projections_to_join =
  try
    let empty_bindings =
      Bindings_in_target_env.from_source_env
        (Source_env.create (ME.typing_env source_env))
    in
    let joined_envs = Joined_envs.create equations_to_join in
    let concrete_equations_to_join, bindings =
      join_aliases_into_bindings ~joined_envs ~bindings:empty_bindings
        equations_to_join
    in
    let equations_for_bindings bindings ~since =
      let new_bindings = Bindings_in_target_env.new_bindings bindings ~since in
      Name_in_target_env.Map.map
        (fun (definition : Bindings_in_target_env.definition_in_joined_envs) ->
          match definition with
          | Imported_var (var, kind) ->
            Joined_envs.alias_types_of joined_envs kind var
          | These_canonicals (simples, kind) ->
            Index.Map.map
              (fun simple -> Type_in_one_joined_env.alias_type_of kind simple)
              simples)
        new_bindings
    in
    let equations_to_join =
      Name_in_target_env.Map.disjoint_union concrete_equations_to_join
        (equations_for_bindings bindings ~since:empty_bindings)
    in
    let rec loop t equations_to_join concrete_types_in_target_env =
      let bindings_before_this_round = t.bindings in
      let types_in_target_env, t =
        n_way_join_round ~n_way_join_type t equations_to_join
          concrete_types_in_target_env
      in
      let new_equations_to_join =
        equations_for_bindings t.bindings ~since:bindings_before_this_round
      in
      if Name_in_target_env.Map.is_empty new_equations_to_join
      then
        ( (* We compute symbol projections last so that we can pick up
             existential variables, but there is no need to create existential
             variables from symbol projections since they would not be
             accessible.

             CR-someday bclement: perform CSE for symbol projections? *)
          types_in_target_env,
          n_way_join_symbol_projections t symbol_projections_to_join,
          t.bindings )
      else loop t new_equations_to_join types_in_target_env
    in
    let equations, symbol_projections, bindings =
      loop { joined_envs; bindings } equations_to_join
        (Name_in_target_env.from_source_env_map
           (Bindings_in_target_env.alias_types_in_target_env bindings))
    in
    let target_env =
      Bindings_in_target_env.fold_created_variables
        (fun var kind target_env ->
          ME.add_variable_definition target_env
            (var : Variable_in_target_env.t :> Variable.t)
            kind Name_mode.in_types)
        bindings source_env
    in
    let target_env =
      ME.add_env_extension ~meet_type target_env
        (TEE.from_map
           (equations
             : Type_in_target_env.t Name_in_target_env.Map.t
             :> TG.t Name.Map.t))
    in
    let target_env =
      Variable_in_target_env.Map.fold
        (fun var symbol_projection target_env ->
          ME.add_symbol_projection target_env
            (var :> Variable.t)
            symbol_projection)
        symbol_projections target_env
    in
    ( target_env,
      Bindings_in_target_env.new_bindings bindings ~since:empty_bindings )
  with Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "\n@[<v 2>%tContext is:%t cut and join of levels:@ %a@]\n"
      Flambda_colours.error Flambda_colours.pop
      (Index.Map.print (fun ppf env -> TEL.print ppf (TE.cut ~cut_after env)))
      joined_envs;
    Printexc.raise_with_backtrace Misc.Fatal_error bt

(* Join analysis *)

module Analysis = struct
  type 'a t =
    { definitions_in_joined_envs :
        Bindings_in_target_env.definition_in_joined_envs
        Name_in_target_env.Map.t;
      external_ids : 'a Index.Map.t
    }

  let print ppf { definitions_in_joined_envs; _ } =
    Name_in_target_env.Map.print
      (fun ppf (def : Bindings_in_target_env.definition_in_joined_envs) ->
        match def with
        | Imported_var (var, _) ->
          Format.fprintf ppf "@[<hov 1>(imported@ %a)@]"
            Variable_in_one_joined_env.print var
        | These_canonicals (simples, _) ->
          Index.Map.print Simple_in_one_joined_env.print ppf simples)
      ppf definitions_in_joined_envs

  let create ~external_ids definitions_in_joined_envs =
    { definitions_in_joined_envs; external_ids }

  module Variable_refined_at_join = struct
    type 'a t =
      { canonicals_in_joined_envs : Simple_in_one_joined_env.t Index.Map.t;
        kind : K.t;
        external_ids : 'a Index.Map.t
      }

    let fold_values_at_uses f t init =
      Index.Map.fold
        (fun index simple acc ->
          match Index.Map.find_opt index t.external_ids with
          | None -> Misc.fatal_error "Missing environment for use"
          | Some external_id ->
            Simple_in_one_joined_env.pattern_match simple
              ~const:(fun const -> f external_id (Or_unknown.Known const) acc)
              ~name:(fun _ ~coercion:_ -> f external_id Or_unknown.Unknown acc))
        t.canonicals_in_joined_envs init
  end

  type 'a simple_refined_at_join =
    | Not_refined_at_join
    | Invariant_in_all_uses of Simple.t
    | Variable_refined_at_these_uses of 'a Variable_refined_at_join.t

  let simple_refined_at_join t env simple =
    let simple = TE.get_canonical_simple_ignoring_name_mode env simple in
    Simple.pattern_match' simple
      ~const:(fun _ -> Invariant_in_all_uses simple)
      ~symbol:(fun _ ~coercion:_ -> Invariant_in_all_uses simple)
      ~var:(fun var ~coercion:_ ->
        match
          Name_in_target_env.Map.find_opt
            (Name_in_target_env.create (Name.var var))
            t.definitions_in_joined_envs
        with
        | None ->
          (* CR bclement: This is not entirely true -- variables in the source
             env could have been refined at some (but not all!) of the uses, in
             which case we won't have a [definition_in_join_env].

             This could be fixed by storing a [definition_in_join_env] in the
             [Latest_bound_source_var] / [Canonical_in_source_env] case in
             [join_aliases_into_bindings]. *)
          Not_refined_at_join
        | Some (Imported_var (var, _)) ->
          Invariant_in_all_uses
            (Simple.var (var : Variable_in_one_joined_env.t :> Variable.t))
        | Some (These_canonicals (canonicals_in_joined_envs, kind)) ->
          Variable_refined_at_these_uses
            { Variable_refined_at_join.canonicals_in_joined_envs;
              kind;
              external_ids = t.external_ids
            })
end

let cut_and_n_way_join ~n_way_join_type ~meet_type ~cut_after source_env
    joined_envs =
  let joined_envs, equations_to_join, symbol_projections_to_join =
    Index.fold_list
      (fun index typing_env
           (joined_envs, equations_to_join, symbol_projections_to_join) ->
        let equations, symbol_projections =
          cut_for_join typing_env ~cut_after
        in
        ( Index.Map.add index typing_env joined_envs,
          Index.Map.add index (typing_env, equations) equations_to_join,
          Index.Map.add index symbol_projections symbol_projections_to_join ))
      joined_envs
      (Index.Map.empty, Index.Map.empty, Index.Map.empty)
  in
  let target_env, _ =
    cut_and_n_way_join0 ~n_way_join_type ~meet_type ~cut_after source_env
      joined_envs equations_to_join symbol_projections_to_join
  in
  target_env

let cut_and_n_way_join_with_analysis ~n_way_join_type ~meet_type ~cut_after
    source_env joined_envs =
  let external_ids, joined_envs, equations_to_join, symbol_projections_to_join =
    Index.fold_list
      (fun index (external_id, typing_env)
           ( external_ids,
             joined_envs,
             equations_to_join,
             symbol_projections_to_join ) ->
        let equations, symbol_projections =
          cut_for_join typing_env ~cut_after
        in
        ( Index.Map.add index external_id external_ids,
          Index.Map.add index typing_env joined_envs,
          Index.Map.add index (typing_env, equations) equations_to_join,
          Index.Map.add index symbol_projections symbol_projections_to_join ))
      joined_envs
      (Index.Map.empty, Index.Map.empty, Index.Map.empty, Index.Map.empty)
  in
  let source_env = ME.create source_env in
  let target_env, bindings =
    cut_and_n_way_join0 ~n_way_join_type ~meet_type ~cut_after source_env
      joined_envs equations_to_join symbol_projections_to_join
  in
  let target_env = ME.typing_env target_env in
  let join_analysis = Analysis.create ~external_ids bindings in
  target_env, join_analysis

let n_way_join_canonicals ~bindings ~joined_envs kind simples =
  match get_canonical_in_target_env ~bindings ~joined_envs simples with
  | Canonical_in_source_env simple ->
    Simple_in_target_env.from_source_env simple, bindings
  | Import_from_all_joined_envs (var, coercion) ->
    let simple, bindings =
      Bindings_in_target_env.import_from_all_envs bindings var kind
    in
    let simple = Simple_in_target_env.apply_coercion_exn simple coercion in
    simple, bindings
  | Existential_for_these_simples ->
    Bindings_in_target_env.existential_for_these_simples bindings simples kind

let n_way_join_simples t kind simples : _ Or_bottom.t * t =
  match simples with
  | [] -> Bottom, t
  | _ :: _ ->
    let canonicals_in_joined_envs = Simples_in_joined_envs.of_list simples in
    (* CR-someday bclement: somehow mark the local variable as used, so that it
       can be re-processed in the current env extension if applicable (if a
       local variable is created while processing an env extension, we currently
       lose any equation that the extension had for that variable). *)
    let canonical_in_target_env, bindings =
      n_way_join_canonicals ~bindings:t.bindings ~joined_envs:t.joined_envs kind
        canonicals_in_joined_envs
    in
    ( Ok (canonical_in_target_env : Simple_in_target_env.t :> Simple.t),
      { t with bindings } )

(** {2:extensions Join of extensions} *)

let prepare_nested_join ~meet_type ~joined_envs ~bindings extensions =
  let joined_envs_and_extensions =
    List.fold_left
      (fun joined_envs_and_extensions (index, extension) ->
        let parent_env = Joined_envs.get_nth_joined_env joined_envs index in
        (* The extension is not guaranteed to still be in canonical form, but we
           need the equations to be in canonical form to known which variables
           are actually touched by the extension, so we add it once then cut it.

           Note: we need to cut it as a level, because the meets from
           [add_env_extension_strict] could add perform nested joins which could
           add new variables. *)
        assert (not (TE.is_bottom parent_env));
        let cut_after = TE.current_scope parent_env in
        let typing_env = TE.increment_scope parent_env in
        match
          ME.add_env_extension_strict ~meet_type (ME.create typing_env)
            extension
        with
        | Bottom ->
          (* We can reach bottom here if the extension was created in a more
             generic context, but is added in a context where it is no longer
             reachable. *)
          joined_envs_and_extensions
        | Ok env ->
          let level = ME.cut env ~cut_after in
          let extension = TEL.as_extension_without_bindings level in
          Index.Map.add index
            (ME.typing_env env, extension)
            joined_envs_and_extensions)
      Index.Map.empty extensions
  in
  Index.Map.mapi
    (fun index (env, diff_ext) ->
      let previous_equations =
        Joined_envs.equations_in_nth_joined_env joined_envs index
      in
      let diff_equations =
        Type_in_one_joined_env.create_equations (TEE.to_map diff_ext)
      in
      (* The call below to [replay_definition_of_aliases_in_target_env] is only
         relevant when doing a nested join (join of env extensions); for a
         toplevel join, [join_aliases] is empty and this does nothing.

         Consider that we first perform the following join (assuming that [x]
         and [y] exist in the source env and all other variables are local to
         their joined env) of:

         x: (= a) ; y: (= a)

         and

         x: (= c)

         and that we later perform in the same context the join of nested
         extensions:

         a: (= d)

         and

         y: (= c)

         We'd like to determine that the join of the extensions is:

         x: (= y)

         If we simply use the incremental join algorithm without taking
         demotions into account, we'll find the join of [y: (= a)] (from the
         outer scope in the left environment) and [y: (= c)] (from the nested
         scope in the right environment) but we don't have a way to determine
         that [x] and [y] are equal without reprocessing the equations on [x]
         (in the outer scope, the canonicals for [x] were [(a, c)] so we
         couldn't even find it from the canonicals of [y] in the inner scope,
         which are [(d, c)]).

         We do this by keeping track of the aliases of [a] in the joined env
         ([x] and [y]), and adding back the corresponding demotions (only for
         the variables that actually have an equation in the extension) to the
         first extension, yielding:

         x: (= a) ; y: (= a) ; a: (= d)

         This will interact with the equation [y: (= c)] from the extension
         scope in the right environment, and with the equation [x: (= c)] from
         the parent scope in the right environment, from which we can deduce the
         equality between [x] and [y].

         Note that if we instead have:

         x: (Block 0 (= a)) ; y: (Block 0 (= a))

         and

         x: (Block 0 (= c))

         at the toplevel and

         a: (= d)

         and

         y: (Block 0 (= c))

         in the extensions, we will create a single existential variable [ac] at
         the toplevel.

         When performing the join of the extensions, we will add the equation
         [ac: (= a)] to the left extension, but we also need to add an equation
         [ac: (= c)] to the outer scope in the right env (see the call to
         [defining_equations_of_existentials] below) in order to reprocess
         [ac]. *)
      let diff_equations =
        Bindings_in_target_env.replay_definition_of_aliases_in_target_env
          bindings index diff_equations
      in
      (* We call [union diff previous] rather than [union previous diff] because
         we want maximum sharing with [diff] (see the computation of
         [previous_equations] below). *)
      let current_equations =
        Name.Map.union_sharing
          (fun _ diff_ty _previous_ty -> Some diff_ty)
          diff_equations previous_equations
      in
      (* Drop variables from the previous level if they get a more precise type
         in the current level (otherwise they would appear in both $Pi$ and $Δi$
         and be processed twice -- see [incremental_join]). *)
      let previous_equations =
        Name.Map.diff_shared
          (fun _ _current_ty _diff_ty -> None)
          current_equations diff_equations
      in
      (* This call is only relevant if we are doing a nested join (join of env
         extensions); for a toplevel join, we don't have existential variables.

         When doing a nested join, we need to make sure that any existential
         variables created at an earlier level are tracked in the previous level
         so that they can correctly interact with equations added by
         [replay_definition_of_aliases_in_target_env] to the [diff_equations] of
         another joined env (see the call to
         [replay_definition_of_aliases_in_target_env] above). *)
      (* CR bclement: it would be more efficient to do an union of iterators to
         avoid re-processing all the existentials every time. *)
      let previous_equations =
        let defining_equations_of_existential_vars =
          Bindings_in_target_env.definition_of_local_variables_in_one_joined_env
            bindings index
        in
        Name.Map.union
          (fun _ previous _defining_eqn ->
            (* Sometimes we might have already added the defining equation of an
               existential due to [replay_definition_of_aliases_in_target_env],
               which is fine. *)
            Some previous)
          previous_equations
          (Name.var_map
             (defining_equations_of_existential_vars
               : Type_in_one_joined_env.t Variable_in_target_env.Map.t
               :> Type_in_one_joined_env.t Variable.Map.t))
      in
      let incremental_equations =
        { previous = previous_equations;
          diff = diff_equations;
          current = current_equations
        }
      in
      env, incremental_equations)
    joined_envs_and_extensions

(* This is similar to [join_aliases_into_bindings], except that when doing a
   join of env extensions, we cannot just accumulate aliases into the [bindings]
   because any demotion we process is local *just* to the current env extension,
   and stops being valid once we leave the extension.

   Instead, we just accumulate the (local) alias equations directly. *)
let join_aliases_in_env_extension ~joined_envs ~bindings equations_to_join =
  fold_incremental_join_in_target_env equations_to_join
    ~exists_in_target_env:(Bindings_in_target_env.exists_in_target_env bindings)
    ~init:(Name_in_target_env.Map.empty, Name_in_target_env.Map.empty, bindings)
    ~f:(fun
        name
        join_entry
        (equations_in_target_env, equations_to_join, bindings)
      ->
      match get_types_in_joined_envs join_entry with
      | Bottom -> Misc.fatal_error "Unexpected bottom during join"
      | Ok (No_alias_in_some_env types) ->
        let equations_to_join =
          Name_in_target_env.Map.add name types equations_to_join
        in
        equations_in_target_env, equations_to_join, bindings
      | Ok (Equals_in_all_envs (canonicals, kind)) ->
        (* CR-someday bclement: If this creates new variables, they will not be
           processed inside the env extension (see also the comment in
           [n_way_join_simples]). *)
        let canonical, bindings =
          n_way_join_canonicals ~bindings ~joined_envs kind canonicals
        in
        let equations_in_target_env =
          Name_in_target_env.Map.add name
            (Type_in_target_env.alias_type_of kind canonical)
            equations_in_target_env
        in
        equations_in_target_env, equations_to_join, bindings)

let n_way_join_env_extension ~n_way_join_type ~meet_type t extensions :
    _ Or_bottom.t =
  let joined_equations =
    try
      prepare_nested_join ~meet_type ~bindings:t.bindings
        ~joined_envs:t.joined_envs extensions
    with Misc.Fatal_error ->
      let bt = Printexc.get_raw_backtrace () in
      Format.eprintf
        "\n@[<v 2>%tContext is:%t preparing join of env extensions:@ %a@]\n"
        Flambda_colours.error Flambda_colours.pop
        (Index.Map.print TEE.print)
        (Index.Map.of_list extensions);
      Printexc.raise_with_backtrace Misc.Fatal_error bt
  in
  if Index.Map.is_empty joined_equations
  then Bottom
  else
    try
      let joined_envs = Joined_envs.create joined_equations in
      let alias_types_in_target_env, concrete_types_to_join, bindings =
        join_aliases_in_env_extension ~joined_envs ~bindings:t.bindings
          joined_equations
      in
      (* CR-someday bclement: if we create new existential variables during the
         join of env extensions, we might need additional rounds for
         completeness (see comment in [n_way_join_simples]) -- in practice one
         round should be plenty. *)
      let equations, { bindings; _ } =
        n_way_join_round ~n_way_join_type { joined_envs; bindings }
          concrete_types_to_join alias_types_in_target_env
      in
      Ok
        ( TEE.from_map
            (equations
              : Type_in_target_env.t Name_in_target_env.Map.t
              :> TG.t Name.Map.t),
          { t with bindings } )
    with Misc.Fatal_error ->
      (* Note that we display the env extensions in their current canonical
         form, which might differ from their form as recorded in the input
         types. *)
      let bt = Printexc.get_raw_backtrace () in
      Format.eprintf "\n@[<v 2>%tContext is:%t join of env extensions:@ %a@]\n"
        Flambda_colours.error Flambda_colours.pop
        (Index.Map.print (fun ppf (_, extension) ->
             TEE.print ppf
               (TEE.from_map
                  (extension.current
                    : Type_in_one_joined_env.t Name.Map.t
                    :> TG.t Name.Map.t))))
        joined_equations;
      Printexc.raise_with_backtrace Misc.Fatal_error bt
