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
module MTC = More_type_creators
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
      | Null -> acc
      | This name ->
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
  end = struct
    include Id_in_env (Name) ()

    let var (var : Variable.t) : t =
      create (Name.var (var :> Int_ids.Variable.t))

    let symbol (symbol : Symbol.t) =
      create (Name.symbol (symbol :> Int_ids.Symbol.t))
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
  module Name = Int_ids_in_env.Name

  module Simple = struct
    include Int_ids_in_env.Simple

    (* See {!section-scope_of_names} *)
    let from_source_env (simple : Simple_in_source_env.t) =
      create (simple :> Simple.t)
  end
end

module Int_ids_in_target_env = Int_ids_from_source_env ()
module Variable_in_target_env = Int_ids_in_target_env.Variable
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

  val exists_at_name_mode :
    min_name_mode:Name_mode.t ->
    t ->
    Variable.t ->
    Variable_in_source_env.t option

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
    if TE.mem source_env (Name.var var)
    then Some (Variable_in_source_env.create var)
    else None

  let exists_at_name_mode ~min_name_mode { source_env } var =
    if TE.mem ~min_name_mode source_env (Name.var var)
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

  (* Mark a variable as imported in the target env, so that it gets redefined in
     the target env. *)
  val import_from_all_envs : t -> Variable_in_one_joined_env.t -> K.t -> t

  (* Return the (unique across the whole join) name to be used to represent this
     set of simples in joined environments. *)
  val existential_for_these_simples :
    t -> Simples_in_joined_envs.t -> K.t -> Variable_in_target_env.t * t

  (* Assuming that [t] derives from [since], extract the created variables from
     [t], adding them to [since]. Any information about the created variables
     besides their kind (in particular, their [definition_in_joined_env]) is
     forgotten, and they won't appear in the [new_bindings]. *)
  val forget_definition_of_created_variables : t -> since:t -> t

  val fold_imported_variables :
    (Variable_in_one_joined_env.t -> K.t -> 'a -> 'a) -> t -> 'a -> 'a

  val fold_existential_variables :
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

  type t =
    { source_env : Source_env.t;
      existential_for_these_simples :
        Variable_in_target_env.t Simples_in_joined_envs.Map.t;
          (* Maps a set of [simples] in joined environments to the (unique
             across the whole join) name used to represent this exact set of
             simples in the target environment. *)
      imported_variables : K.t Variable_in_one_joined_env.Map.t;
          (* Set of variables that have been imported from at least one of the
             joined environments into the target environment. *)
      existential_variables : K.t Variable_in_target_env.Map.t;
          (* This contains all the existential variables, created during the
             join, that exist in the target environment but not in the source
             environment or in any of the joined environments. *)
      aliases_of_names_in_joined_envs :
        coercion_to_canonical_in_target_env Name_in_target_env.Map.t
        Name_in_one_joined_env.Map.t
        Index.Map.t;
          (* For each joined environment and each name in the joined
             environment, record the set of names in the target environment it
             is equal to (and the corresponding coercions).

             This is used to implement
             [replay_definitions_of_aliases_in_target_env] in the join of env
             extensions, see [prepare_nested_join]. *)
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
      existential_for_these_simples = Simples_in_joined_envs.Map.empty;
      imported_variables = Variable_in_one_joined_env.Map.empty;
      aliases_of_names_in_joined_envs = Index.Map.empty;
      equations_for_local_vars = Index.Map.empty;
      existential_variables = Variable_in_target_env.Map.empty
    }

  let forget_definition_of_created_variables t ~since =
    (* We still need to record the fact that we created those variables in order
       to add them to the target environment at the end of the join. *)
    { since with
      existential_variables = t.existential_variables;
      imported_variables = t.imported_variables
    }

  let source_env { source_env; _ } = source_env

  let exists_in_target_env t var =
    match Source_env.exists_in_source_env (source_env t) var with
    | Some var_in_source_env ->
      Some (Variable_in_target_env.from_source_env var_in_source_env)
    | None ->
      let var = Variable_in_target_env.create var in
      if
        Variable_in_target_env.Map.mem var t.existential_variables
        || Variable_in_one_joined_env.Map.mem
             (Variable_in_one_joined_env.create (var :> Variable.t))
             t.imported_variables
      then Some var
      else None

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

  let has_existential_for_these_simples t simples =
    Simples_in_joined_envs.Map.find_opt simples t.existential_for_these_simples

  let existential_for_these_simples t simples kind =
    match has_existential_for_these_simples t simples with
    | Some existing_canonical -> existing_canonical, t
    | None ->
      let var =
        let name = Simples_in_joined_envs.choose_a_suitable_name simples in
        Variable_in_target_env.create (Variable.create name kind)
      in
      let existential_variables =
        Variable_in_target_env.Map.add var kind t.existential_variables
      in
      let t = { t with existential_variables } in
      let existential_for_these_simples =
        Simples_in_joined_envs.Map.add simples var
          t.existential_for_these_simples
      in
      (* The following is some bookkeeping so that we know how to replay the
         definition of existential variables during nested joins (i.e. joins of
         env extensions); see {!section-extensions}. *)
      let equations_for_local_vars =
        (* If the variable is a fresh variable, record it so that we can replay
           its definition during the join of env extensions. *)
        Index.Map.update_many
          (fun _index existentials simple ->
            let ty = Type_in_one_joined_env.alias_type_of kind simple in
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
            (* definition ~ coercion(name_in_joined_env) *)
            Name_in_target_env.Map.add
              (Name_in_target_env.var var)
              coercion aliases)
      in
      ( var,
        { t with
          equations_for_local_vars;
          existential_for_these_simples;
          aliases_of_names_in_joined_envs
        } )

  let import_from_all_envs t imported_var kind =
    if Variable_in_one_joined_env.Map.mem imported_var t.imported_variables
    then t
    else
      let imported_variables =
        Variable_in_one_joined_env.Map.add imported_var kind
          t.imported_variables
      in
      { t with imported_variables }

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

  let fold_imported_variables f t acc =
    (* CR bclement: iterate in order consistent with the recorded binding
       times. *)
    Variable_in_one_joined_env.Map.fold f t.imported_variables acc

  let fold_existential_variables f t acc =
    (* CR bclement: iterate in order consistent with the recorded binding
       times. *)
    Variable_in_target_env.Map.fold f t.existential_variables acc
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

  val exists_in_all_joined_envs : t -> _ Index.Map.t -> bool

  val find_simples_in_joined_envs :
    t -> Simples_in_joined_envs.t -> K.t -> Type_in_one_joined_env.t Index.Map.t

  val find_imported_var :
    t ->
    Variable_in_one_joined_env.t ->
    K.t ->
    Type_in_one_joined_env.t Index.Map.t

  val equal_in_all_joined_envs :
    t -> Simple_in_one_joined_env.t -> Simples_in_joined_envs.t -> bool

  val get_canonical_simples_ignoring_name_mode :
    t -> (Index.t * Simple.t) list -> Simples_in_joined_envs.t
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

  let exists_in_all_joined_envs t m =
    Index.Map.subset_domain (envs_and_equations t) m

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

  let find_simples_in_joined_envs t simples kind =
    Index.Map.mapi
      (fun index simple ->
        Simple_in_one_joined_env.pattern_match simple
          ~const:(fun const -> ET.create_const const |> ET.to_type)
          ~name:(fun name ~coercion ->
            let env = get_nth_joined_env t index in
            let ty = TE.find env (name :> Name.t) (Some kind) in
            TG.apply_coercion ty coercion)
        |> Type_in_one_joined_env.create)
      simples

  let find_imported_var t var kind =
    let erased_var = (var : Variable_in_one_joined_env.t :> Variable.t) in
    if Flambda_features.check_light_invariants ()
    then
      if not (Current_unit.is_current (Variable.compilation_unit erased_var))
      then
        Misc.fatal_errorf
          "Cannot re-define variable %a defined in another compilation unit \
           into the target environment of join"
          Variable.print erased_var;
    Index.Map.filter_map
      (fun _index (env, _) ->
        let name = Name.var erased_var in
        if TE.mem env name
        then Some (Type_in_one_joined_env.create (TE.find env name (Some kind)))
        else None)
      (envs_and_equations t)

  let get_canonical_simples_ignoring_name_mode t simples =
    List.fold_left
      (fun acc (index, simple) ->
        let env = get_nth_joined_env t index in
        let canonical =
          Simple_in_one_joined_env.create
            (TE.get_canonical_simple_ignoring_name_mode env simple)
        in
        Index.Map.add index canonical acc)
      Index.Map.empty simples
end

module Aliases_of_existentials = struct
  type t =
    { aliases_of_variables :
        Variable_in_target_env.Set.t Variable_in_target_env.Map.t
    }

  let empty = { aliases_of_variables = Variable_in_target_env.Map.empty }

  let aliases_of_existential_var t var =
    try Variable_in_target_env.Map.find var t.aliases_of_variables
    with Not_found -> Variable_in_target_env.Set.empty

  let add t ~(demoted_var : Variable_in_target_env.t)
      ~(existential_var : Variable_in_target_env.t) =
    let aliases_of_canonical_element =
      aliases_of_existential_var t existential_var
    in
    let aliases_of_existential_var =
      Variable_in_target_env.Set.add demoted_var aliases_of_canonical_element
    in
    let aliases_of_variables =
      Variable_in_target_env.Map.add existential_var aliases_of_existential_var
        t.aliases_of_variables
    in
    { aliases_of_variables }
end

(** {1 Public interface} *)

type env_id = Index.t

type 'a join_arg = env_id * 'a

type t =
  { joined_envs : Joined_envs.t;
    types_in_target_env : Type_in_target_env.t Name_in_target_env.Map.t;
    types_in_joined_envs :
      Type_in_one_joined_env.t Index.Map.t Variable_in_target_env.Map.t;
    aliases_of_existentials : Aliases_of_existentials.t;
    definitions_of_existentials :
      (Simples_in_joined_envs.t * K.t) Variable_in_target_env.Map.t;
    bindings : Bindings_in_target_env.t
  }

let create ~joined_envs ~bindings =
  { joined_envs;
    types_in_target_env = Name_in_target_env.Map.empty;
    types_in_joined_envs = Variable_in_target_env.Map.empty;
    aliases_of_existentials = Aliases_of_existentials.empty;
    definitions_of_existentials = Variable_in_target_env.Map.empty;
    bindings
  }

let new_definitions_of_existentials t ~since =
  Variable_in_target_env.Map.diff_shared
    (fun _ new_definition _old_definition -> Some new_definition)
    t.definitions_of_existentials since.definitions_of_existentials

let new_equations_in_joined_envs t ~since =
  Variable_in_target_env.Map.diff_shared
    (fun _ new_types _old_types -> Some new_types)
    t.types_in_joined_envs since.types_in_joined_envs

let existential_for_these_simples env canonicals kind =
  let existential_var, bindings =
    Bindings_in_target_env.existential_for_these_simples env.bindings canonicals
      kind
  in
  let definitions_of_existentials =
    if
      Variable_in_target_env.Map.mem existential_var
        env.definitions_of_existentials
    then env.definitions_of_existentials
    else
      Variable_in_target_env.Map.add existential_var (canonicals, kind)
        env.definitions_of_existentials
  in
  existential_var, { env with bindings; definitions_of_existentials }

let import_from_all_envs env var kind =
  let bindings =
    Bindings_in_target_env.import_from_all_envs env.bindings var kind
  in
  let imported_var =
    Variable_in_target_env.create
      (var : Variable_in_one_joined_env.t :> Variable.t)
  in
  let types_in_joined_envs =
    if Variable_in_target_env.Map.mem imported_var env.types_in_joined_envs
    then env.types_in_joined_envs
    else
      Variable_in_target_env.Map.add imported_var
        (Joined_envs.find_imported_var env.joined_envs var kind)
        env.types_in_joined_envs
  in
  { env with bindings; types_in_joined_envs }

type n_way_join_type = t -> TG.t join_arg list -> TG.t Or_unknown.t * t

let joined_env t index = Joined_envs.get_nth_joined_env t.joined_envs index

let machine_width t =
  Source_env.machine_width (Bindings_in_target_env.source_env t.bindings)

type canonical_in_target_env =
  | Canonical_in_source_env of Simple_in_source_env.t
  | Import_from_all_joined_envs of Variable_in_one_joined_env.t * Coercion.t
  | Existential_for_these_simples

let get_canonical_in_target_env env canonicals_in_joined_envs =
  let source_env = Bindings_in_target_env.source_env env.bindings in
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
      Joined_envs.equal_in_all_joined_envs env.joined_envs
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
          | Some var_in_target_env -> f var_in_target_env join_entry acc)
        ~symbol:(fun _symbol ->
          (* If [name] is that of a lifted constant symbol generated during one
             of the levels, then ignore it. [Simplify_expr] will already have
             made its type suitable for the [source_env] and inserted it into
             that environment.

             This should not be necessary, but if we don't ignore the join of
             types for lifted constants, and one of them happen to be a
             moderately large mutually recursive set of closures, we end up
             computing a potentially very expensive but useless meet of closure
             types (between the type from [make_suitable_for_environment] and
             the one we are computing during the join).

             It's quite brittle to depend on the set of known lifted constants,
             however, so we just never propagate types on symbols for now. This
             is fine, because if [name] is a symbol that is not a lifted
             constant, it was defined before the fork and already has an
             equation in the [source_env]. While it is possible that its type
             could be refined by all of the branches, it is unlikely, so we are
             fine with dropping the equation. *)
          acc))

let add_equation env name ty =
  assert (not (Name_in_target_env.Map.mem name env.types_in_target_env));
  let types_in_target_env =
    Name_in_target_env.Map.add name ty env.types_in_target_env
  in
  { env with types_in_target_env }

(* This function is responsible for splitting the [equations_to_join] between
   those that are demotions in all joined environments, that are replayed in the
   target environment in the [types_in_target_env], and the rest, that are
   expanded to equations on concrete types to be joined later.

   Note that we only care about names that have new types in all of the joined
   environments, otherwise the join can never be more precise than what we had
   initially. We also only care about names that exist in the target
   environment; other names will be imported automatically during the join of
   types and only if they are reachable. *)
let join_aliases_into_bindings env equations_to_join =
  fold_incremental_join_in_target_env equations_to_join
    ~exists_in_target_env:
      (Bindings_in_target_env.exists_in_target_env env.bindings) ~init:env
    ~f:(fun var join_entry env ->
      match get_types_in_joined_envs join_entry with
      | Bottom -> Misc.fatal_error "Unexpected bottom during join"
      | Ok (No_alias_in_some_env types) ->
        let types_in_joined_envs =
          Variable_in_target_env.Map.add var types env.types_in_joined_envs
        in
        { env with types_in_joined_envs }
      | Ok (Equals_in_all_envs (canonicals, kind)) -> (
        let[@local] add_equals_in_target_env env canonical =
          add_equation env
            (Name_in_target_env.var var)
            (Type_in_target_env.alias_type_of kind canonical)
        in
        match get_canonical_in_target_env env canonicals with
        | Canonical_in_source_env canonical ->
          add_equals_in_target_env env
            (Simple_in_target_env.from_source_env canonical)
        | Import_from_all_joined_envs (imported_var, coercion) ->
          let env = import_from_all_envs env imported_var kind in
          let imported_var =
            Variable_in_target_env.create (imported_var :> Variable.t)
          in
          let simple = Simple_in_target_env.var ~coercion imported_var in
          add_equals_in_target_env env simple
        | Existential_for_these_simples ->
          let existential_var, env =
            existential_for_these_simples env canonicals kind
          in
          let aliases_of_existentials =
            Aliases_of_existentials.add env.aliases_of_existentials
              ~demoted_var:var ~existential_var
          in
          let env = { env with aliases_of_existentials } in
          let simple = Simple_in_target_env.var existential_var in
          add_equals_in_target_env env simple))

let rec add_inverse_relation_to_env_extension ?(seen = Name.Set.empty)
    env_extension name relation ~scrutinee =
  let empty_descr : TG.Head_of_kind_naked_immediate.descr =
    { naked_immediates = Unknown; inverse_relations = TG.Relation.Map.empty }
  in
  let[@inline] updated_type_from_descr
      (descr : TG.Head_of_kind_naked_immediate.descr) =
    let inverse_relations =
      TG.Relation.Map.update relation
        (function
          | None -> Some (Name.Set.singleton scrutinee)
          | Some existing_args -> Some (Name.Set.add scrutinee existing_args))
        descr.inverse_relations
    in
    TG.create_from_head_naked_immediate
      (TG.Head_of_kind_naked_immediate.from_descr_non_empty
         { descr with inverse_relations })
  in
  match Name.Map.find_opt name (TEE.to_map env_extension) with
  | None ->
    TEE.add_or_replace_equation env_extension name
      (updated_type_from_descr empty_descr)
  | Some existing_ty -> (
    match TG.descr existing_ty with
    | Naked_immediate Bottom ->
      (* If we already know that we are bottom, we don't need to store anything
         more precise. *)
      env_extension
    | Naked_immediate Unknown ->
      (* This should not happen, as we would usually only only store non-obvious
         types in extensions -- but it's also harmless. *)
      TEE.add_or_replace_equation env_extension name
        (updated_type_from_descr empty_descr)
    | Naked_immediate (Ok (No_alias head)) ->
      (* There is a concrete type for this name in the extension; augment it
         with the reverse relation. *)
      let descr = TG.Head_of_kind_naked_immediate.descr head in
      TEE.add_or_replace_equation env_extension name
        (updated_type_from_descr descr)
    | Naked_immediate (Ok (Equals simple)) ->
      (* Usually we expect that the name we are adding an alias for would be
         canonical in the env extension, but it could (rarely) happen that it is
         not the case. We simply follow the aliases until we either find one
         that has a concrete type in the extension, or until we detect a
         loop. *)
      Simple.pattern_match simple
        ~name:(fun name' ~coercion:_ ->
          if Name.Set.mem name' seen
          then
            (* There is an alias loop in the env extension -- it is fine to
               break the loop to store the non-alias type anywhere, so we might
               as well do it when we detect the loop. *)
            TEE.add_or_replace_equation env_extension name
              (updated_type_from_descr empty_descr)
          else
            add_inverse_relation_to_env_extension ~seen:(Name.Set.add name seen)
              env_extension name' relation ~scrutinee)
        ~const:(fun _ ->
          (* We do not store reverse relations on constants as that would be
             both expensive and of dubious use. *)
          env_extension)
    | Value _ | Naked_float32 _ | Naked_float _ | Naked_int8 _ | Naked_int16 _
    | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ | Naked_vec128 _
    | Naked_vec256 _ | Naked_vec512 _ | Naked_mask _ | Rec_info _ | Region _ ->
      Misc.fatal_error "Kind mismatch for output of relation: expected %a")

let add_to_inverse_relations inverse_relations var relation ~scrutinee =
  Variable.Map.union_total_shared
    (fun _ inv_rels1 inv_rels2 ->
      TG.Relation.Map.union_total_shared
        (fun _ names1 names2 -> Name.Set.union names1 names2)
        inv_rels1 inv_rels2)
    inverse_relations
    (Variable.Map.singleton var
       (TG.Relation.Map.singleton relation (Name.Set.singleton scrutinee)))

let recover_inverse_relations ~exists_in_all_joined_envs inverse_relations name
    ty =
  (* We can only recover inverse relations if the type we are recovering from is
     valid in all the joined environments.

     If we have a type [x : Variant (is_int = y)] for [x], but [x] only exists
     in a subset of the joined environments, then the equation [y = %is_int x]
     is only valid in those environments -- in particular, if [y] exists in more
     environments than [x], it is unsound to include that equation in the target
     environment.

     We avoid this situation by only recovering relations if the type we are
     recovering from exists in all the joined environments -- this ensures that
     the variables mentioned in the type cannot exist in more environments than
     the type itself.

     CR-someday bclement: We could be more precise here by recovering relations
     if they are valid in all the environments where the involved variables are
     defined, but it is not clear if that would actually be useful. *)
  assert exists_in_all_joined_envs;
  match TG.descr ty with
  | Value (Ok (No_alias { is_null = Not_null; non_null = Ok head })) -> (
    match head with
    | Variant { immediates = Known imm_ty; get_tag = Some get_tag_var; _ }
      when TG.is_obviously_bottom imm_ty ->
      (* If we have no immediates, we can add the inverse relation on [get_tag]
         at the toplevel. *)
      let inverse_relations =
        add_to_inverse_relations inverse_relations get_tag_var
          TG.Relation.get_tag ~scrutinee:name
      in
      ty, inverse_relations
    | Variant
        { is_int;
          get_tag;
          immediates = (Known _ | Unknown) as immediates;
          blocks;
          extensions;
          is_unique
        } ->
      (* In the general case, we must store the [Get_tag] equation inside the
         "block" env extension. This is because storing a [Get_tag] reverse
         relation on a naked immediate allows us to perform a reduction to learn
         that the target of the relation is a block, which is not valid if it
         could be an immediate. *)
      let inverse_relations =
        match is_int with
        | None -> inverse_relations
        | Some is_int_var ->
          add_to_inverse_relations inverse_relations is_int_var
            TG.Relation.is_int ~scrutinee:name
      in
      let ty =
        match get_tag with
        | None -> ty
        | Some get_tag_var ->
          let when_immediate, when_block =
            match extensions with
            | No_extensions -> TEE.empty, TEE.empty
            | Ext { when_immediate; when_block } -> when_immediate, when_block
          in
          let when_block =
            add_inverse_relation_to_env_extension when_block
              (Name.var get_tag_var) TG.Relation.get_tag ~scrutinee:name
          in
          let head' =
            TG.Head_of_kind_value_non_null.create_variant ~is_unique ~blocks
              ~immediates
              ~extensions:(Ext { when_immediate; when_block })
              ~is_int ~get_tag
          in
          TG.create_from_head_value { is_null = Not_null; non_null = Ok head' }
      in
      ty, inverse_relations
    | Mutable_block _
    | Boxed_float32 (_, _)
    | Boxed_float (_, _)
    | Boxed_int32 (_, _)
    | Boxed_int64 (_, _)
    | Boxed_nativeint (_, _)
    | Boxed_vec128 (_, _)
    | Boxed_vec256 (_, _)
    | Boxed_vec512 (_, _)
    | Boxed_mask (_, _)
    | Closures _ | String _ | Array _ ->
      ty, inverse_relations)
  | Value (Ok (No_alias { is_null = Maybe_null { is_null }; non_null = _ })) ->
    (* CR bclement: if we are possibly null, we can't recover inverse relations
       from the non-null case because we don't have an appropriate env extension
       to place them in.

       We can't store them directly in the env for the same reason we can't do
       it for [Get_tag], see the comment for the [Variant] case. *)
    let inverse_relations =
      match is_null with
      | None -> inverse_relations
      | Some is_null_var ->
        add_to_inverse_relations inverse_relations is_null_var
          TG.Relation.is_null ~scrutinee:name
    in
    ty, inverse_relations
  | Value
      ( Ok
          ( Equals _
          | No_alias { is_null = Not_null; non_null = Unknown | Bottom } )
      | Unknown | Bottom )
  | Naked_immediate _ | Naked_float32 _ | Naked_float _ | Naked_int8 _
  | Naked_int16 _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
  | Naked_vec128 _ | Naked_vec256 _ | Naked_vec512 _ | Naked_mask _ | Rec_info _
  | Region _ ->
    ty, inverse_relations

let n_way_join_round ~(n_way_join_type : n_way_join_type) t equations_to_join
    inverse_relations =
  Variable_in_target_env.Map.fold
    (fun var types (inverse_relations, t) ->
      let name = Name_in_target_env.var var in
      if
        Flambda_features.check_light_invariants ()
        && Name_in_target_env.Map.mem name t.types_in_target_env
      then
        Misc.fatal_errorf
          "Processing join of %a but we already have a type for it."
          Name_in_target_env.print name;
      let heads =
        Index.Map.bindings
          (types : Type_in_one_joined_env.t Index.Map.t :> TG.t Index.Map.t)
      in
      match n_way_join_type t heads with
      | Unknown, t -> inverse_relations, t
      | Known ty, t ->
        let exists_in_all_joined_envs =
          Joined_envs.exists_in_all_joined_envs t.joined_envs types
        in
        let ty, inverse_relations =
          if exists_in_all_joined_envs
          then
            recover_inverse_relations ~exists_in_all_joined_envs
              inverse_relations
              (name :> Name.t)
              ty
          else ty, inverse_relations
        in
        let ty = Type_in_target_env.create ty in
        inverse_relations, add_equation t name ty)
    equations_to_join (inverse_relations, t)

(** {2:n-way-join Cut and n-way join} *)

let n_way_join_symbol_projections t symbol_projections_to_join =
  (* Recall that being a symbol projection is a property of the *variable*
     itself, not of the canonicals -- so we can only propagate a symbol
     projection when the same symbol projection is associated with the same
     variable in all joined environments. *)
  let joined_projections =
    Index.Map.fold
      (fun index symbol_projections acc ->
        Variable_in_one_joined_env.Map.fold
          (fun var symbol_projection symbol_projections_to_join ->
            match
              Source_env.exists_at_name_mode ~min_name_mode:Name_mode.normal
                (Bindings_in_target_env.source_env t.bindings)
                (var :> Variable.t)
            with
            | None -> symbol_projections_to_join
            | Some var ->
              Variable_in_source_env.Map.update var
                (fun joined_projections ->
                  let joined_projections =
                    Option.value joined_projections ~default:Index.Map.empty
                  in
                  Some
                    (Index.Map.add index symbol_projection joined_projections))
                symbol_projections_to_join)
          symbol_projections acc)
      symbol_projections_to_join Variable_in_source_env.Map.empty
  in
  let all_indices = Joined_envs.keys t.joined_envs in
  Variable_in_source_env.Map.fold
    (fun var joined_projections symbol_projections ->
      if not (Index.Set.subset all_indices (Index.Map.keys joined_projections))
      then symbol_projections
      else
        match Index.Map.choose joined_projections with
        | _, unique_projection
          when Index.Map.for_all
                 (fun _ projection ->
                   Symbol_projection.equal projection unique_projection)
                 joined_projections ->
          Variable_in_target_env.Map.add
            (Variable_in_target_env.from_source_env var)
            unique_projection symbol_projections
        | _ | (exception Not_found) ->
          (* This can only happen if:

             - The same variable is bound to different symbol projections in
             different input environments; or

             - We are joining zero environments

             We don't expect either of these to happen, but still return
             [symbol_projections] in this case as it is harmless. *)
          symbol_projections)
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

let move_inverse_relation ~from ~to_ inverse_relations =
  match Variable.Map.find from inverse_relations with
  | exception Not_found -> inverse_relations
  | names ->
    let inverse_relations = Variable.Map.remove from inverse_relations in
    Variable.Map.union_total_shared
      (fun _ rels1 rels2 ->
        TG.Relation.Map.union_total_shared
          (fun _ names1 names2 -> Name.Set.union names1 names2)
          rels1 rels2)
      (Variable.Map.singleton to_ names)
      inverse_relations

let move_equation ~from ~to_ equations =
  let from = Name.var from in
  let to_ = Name.var to_ in
  if Name.Map.mem to_ equations
  then
    Misc.fatal_errorf
      "Cannot move equation from %a to %a: there is already an equation: %a"
      Name.print from Name.print to_ TG.print
      (Name.Map.find to_ equations);
  match Name.Map.find from equations with
  | exception Not_found -> equations
  | ty ->
    let equations = Name.Map.remove from equations in
    Name.Map.add to_ ty equations

let move_definition ~from ~to_ definitions =
  let from = Variable_in_target_env.create from in
  let to_ = Variable_in_target_env.create to_ in
  match Variable_in_target_env.Map.find from definitions with
  | exception Not_found -> definitions
  | defn ->
    let definitions = Variable_in_target_env.Map.remove from definitions in
    Variable_in_target_env.Map.add to_ defn definitions

let alias_equations_for_existential kind ~canonical_element ~demoted_aliases
    equations =
  let ty = TG.alias_type_of kind canonical_element in
  let equations =
    Variable_in_target_env.Set.fold
      (fun alias equations ->
        Name.Map.add (Name.var (alias :> Variable.t)) ty equations)
      demoted_aliases equations
  in
  ty, equations

let define_or_eliminate_variables_and_add_equations ~meet_expanded_head env
    source_env inverse_relations =
  let target_env =
    Bindings_in_target_env.fold_imported_variables
      (fun var kind target_env ->
        ME.add_variable_definition target_env
          (var :> Variable.t)
          kind Name_mode.in_types)
      env.bindings source_env
  in
  let names_in_inverse_relations =
    Variable.Map.fold
      (fun var relations names_in_inverse_relations ->
        TG.Relation.Map.fold
          (fun _ names names_in_inverse_relations ->
            Name.Set.union names names_in_inverse_relations)
          relations
          (Name.Set.add (Name.var var) names_in_inverse_relations))
      inverse_relations Name.Set.empty
  in
  let equations = (env.types_in_target_env :> TG.t Name.Map.t) in
  let free_vars_in_equations =
    Name.Map.fold
      (fun _ ty free_names_in_equations ->
        Name_occurrences.with_only_variables (TG.free_names ty)
        |> Name_occurrences.union free_names_in_equations)
      equations Name_occurrences.empty
  in
  let unique_occurence_is_in_equations var =
    (not (Name.Set.mem (Name.var var) names_in_inverse_relations))
    &&
    match Name_occurrences.count_variable free_vars_in_equations var with
    | Zero | One -> true
    | More_than_one -> false
  in
  let definitions = env.definitions_of_existentials in
  let target_env, to_expand, equations, inverse_relations, definitions =
    Bindings_in_target_env.fold_existential_variables
      (fun var kind
           (target_env, to_expand, equations, inverse_relations, definitions) ->
        let aliases_of_var =
          Aliases_of_existentials.aliases_of_existential_var
            env.aliases_of_existentials var
        in
        let var = (var :> Variable.t) in
        match Variable_in_target_env.Set.choose_opt aliases_of_var with
        | None ->
          (* Project out variables with a single occurrence. This is important
             to cut out the size of the resulting environments.

             If the variable appears anywhere in the [inverse_relations] map, we
             don't remove it: it means that it is the result of a primitive
             (%is_int, %is_null, %get_tag, ...) that we are likely to later do a
             switch on, and we need the variable to exist in the analysis for
             the match-in-match transform. *)
          if unique_occurence_is_in_equations var
          then
            let ty, equations =
              match Name.Map.find (Name.var var) equations with
              | exception Not_found -> MTC.unknown kind, equations
              | ty -> ty, Name.Map.remove (Name.var var) equations
            in
            let to_expand = Variable.Map.add var ty to_expand in
            let definitions =
              Variable_in_target_env.Map.remove
                (Variable_in_target_env.create var)
                definitions
            in
            target_env, to_expand, equations, inverse_relations, definitions
          else
            let target_env =
              ME.add_variable_definition target_env var kind Name_mode.in_types
            in
            target_env, to_expand, equations, inverse_relations, definitions
        | Some canon_var ->
          let demoted_aliases =
            Variable_in_target_env.Set.remove canon_var aliases_of_var
          in
          let canon_var = (canon_var :> Variable.t) in
          let definitions =
            move_definition definitions ~from:var ~to_:canon_var
          in
          let equations = move_equation equations ~from:var ~to_:canon_var in
          let inverse_relations =
            move_inverse_relation inverse_relations ~from:var ~to_:canon_var
          in
          let ty, equations =
            alias_equations_for_existential kind equations
              ~canonical_element:(Simple.var canon_var) ~demoted_aliases
          in
          let to_expand = Variable.Map.add var ty to_expand in
          target_env, to_expand, equations, inverse_relations, definitions)
      env.bindings
      (target_env, Variable.Map.empty, equations, inverse_relations, definitions)
  in
  let to_project = Variable.Map.keys to_expand in
  let rec expand var =
    match Variable.Map.find var to_expand with
    | exception Not_found -> assert false
    | ty -> (
      match TG.get_alias_opt ty with
      | None -> TG.project_variables_out ~to_project ~expand ty
      | Some simple ->
        Simple.pattern_match' simple
          ~const:(fun _ -> ty)
          ~symbol:(fun _ ~coercion:_ -> ty)
          ~var:(fun var ~coercion ->
            if Variable.Set.mem var to_project
            then TG.apply_coercion (expand var) coercion
            else ty))
  in
  let equations =
    Name.Map.map (TG.project_variables_out ~to_project ~expand) equations
  in
  let equations_for_inverse_relations =
    Variable.Map.map
      (fun inverse_relations ->
        TG.Head_of_kind_naked_immediate.create_inverse_relations
          inverse_relations
        |> TG.create_from_head_naked_immediate
        |> TG.project_variables_out ~to_project ~expand)
      inverse_relations
    |> Name.var_map
  in
  let target_env =
    ME.add_env_extension ~meet_expanded_head target_env (TEE.from_map equations)
  in
  let target_env =
    ME.add_env_extension ~meet_expanded_head target_env
      (TEE.from_map equations_for_inverse_relations)
  in
  target_env, definitions

let cut_and_n_way_join0 ~n_way_join_type ~meet_expanded_head ~cut_after
    source_env source_tenv joined_envs equations_to_join
    symbol_projections_to_join =
  try
    let empty_bindings =
      Bindings_in_target_env.from_source_env (Source_env.create source_tenv)
    in
    let joined_envs = Joined_envs.create equations_to_join in
    let empty_env = create ~joined_envs ~bindings:empty_bindings in
    let env = join_aliases_into_bindings empty_env equations_to_join in
    let rec n_way_join_delayed_equations env_this_round inverse_relations
        equations_this_round =
      if Variable_in_target_env.Map.is_empty equations_this_round
      then inverse_relations, env_this_round
      else
        let inverse_relations, env_next_round =
          n_way_join_round ~n_way_join_type env_this_round equations_this_round
            inverse_relations
        in
        n_way_join_delayed_equations env_next_round inverse_relations
          (new_equations_in_joined_envs env_next_round ~since:env_this_round)
    in
    let rec n_way_join_loop env_this_round inverse_relations
        existentials_this_round =
      if Variable_in_target_env.Map.is_empty existentials_this_round
      then inverse_relations, env_this_round
      else
        let equations_in_joined_envs =
          Variable_in_target_env.Map.map
            (fun (simples, kind) ->
              Joined_envs.find_simples_in_joined_envs env_this_round.joined_envs
                simples kind)
            existentials_this_round
        in
        let inverse_relations, env_next_round =
          n_way_join_delayed_equations env_this_round inverse_relations
            equations_in_joined_envs
        in
        n_way_join_loop env_next_round inverse_relations
          (new_definitions_of_existentials env_next_round ~since:env_this_round)
    in
    let inverse_relations, env =
      n_way_join_delayed_equations env Variable.Map.empty
        (new_equations_in_joined_envs env ~since:empty_env)
    in
    let inverse_relations, env =
      n_way_join_loop env inverse_relations
        (new_definitions_of_existentials env ~since:empty_env)
    in
    let target_env, definitions =
      define_or_eliminate_variables_and_add_equations ~meet_expanded_head env
        source_env inverse_relations
    in
    let target_env =
      Variable_in_target_env.Map.fold
        (fun var symbol_projection target_env ->
          ME.add_symbol_projection target_env
            (var :> Variable.t)
            symbol_projection)
        (n_way_join_symbol_projections env symbol_projections_to_join)
        target_env
    in
    target_env, definitions
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
        (Simples_in_joined_envs.t * K.t) Variable_in_target_env.Map.t;
      canonical_definitions_at_normal_mode :
        (Simples_in_joined_envs.t * K.t) Variable_in_target_env.Map.t;
      external_ids : 'a Index.Map.t
    }

  let print ppf { definitions_in_joined_envs; _ } =
    Variable_in_target_env.Map.print
      (fun ppf (simples, _) ->
        Index.Map.print Simple_in_one_joined_env.print ppf simples)
      ppf definitions_in_joined_envs

  let create ~external_ids ~joined_envs definitions_in_joined_envs =
    let canonical_definitions_at_normal_mode =
      Variable_in_target_env.Map.filter_map
        (fun _name (simples, kind) ->
          let exists_at_normal_name_mode_in_all_envs_it_is_defined_in =
            Index.Map.for_all
              (fun env_id simple ->
                let typing_env =
                  match Index.Map.find_opt env_id joined_envs with
                  | Some typing_env -> typing_env
                  | None ->
                    Misc.fatal_errorf "Join does not include environment %a"
                      Index.print env_id
                in
                TE.mem_simple ~min_name_mode:Name_mode.normal typing_env simple)
              (simples : Simples_in_joined_envs.t :> Simple.t Index.Map.t)
          in
          if exists_at_normal_name_mode_in_all_envs_it_is_defined_in
          then Some (simples, kind)
          else None)
        definitions_in_joined_envs
    in
    { definitions_in_joined_envs;
      canonical_definitions_at_normal_mode;
      external_ids
    }

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
          Variable_in_target_env.Map.find_opt
            (Variable_in_target_env.create var)
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
        | Some (canonicals_in_joined_envs, kind) ->
          Variable_refined_at_these_uses
            { Variable_refined_at_join.canonicals_in_joined_envs;
              kind;
              external_ids = t.external_ids
            })

  module Simples_at_join = struct
    type 'a t =
      { canonicals_in_joined_envs : Simple_in_one_joined_env.t Index.Map.t;
        external_ids : 'a Index.Map.t
      }

    type definition_at_use = At_normal_mode of Simple.t [@@unboxed]

    let fold_definitions_at_uses f t init =
      Index.Map.fold
        (fun index simple acc ->
          match Index.Map.find_opt index t.external_ids with
          | None -> Misc.fatal_error "Missing environment for use"
          | Some external_id ->
            f external_id
              (At_normal_mode (simple : Simple_in_one_joined_env.t :> Simple.t))
              acc)
        t.canonicals_in_joined_envs init
  end

  let fold_variables_created_at_join ~f t ~init =
    Variable_in_target_env.Map.fold
      (fun var (canonicals_in_joined_envs, kind) acc ->
        (f [@inlined hint])
          (Name.var (var :> Variable.t))
          { Simples_at_join.canonicals_in_joined_envs;
            external_ids = t.external_ids
          }
          kind acc)
      t.canonical_definitions_at_normal_mode init
end

let cut_and_n_way_join ~n_way_join_type ~meet_expanded_head ~cut_after
    source_env source_tenv joined_envs =
  let joined_envs, equations_to_join, symbol_projections_to_join =
    Index.fold_list
      (fun index typing_env
           ((joined_envs, equations_to_join, symbol_projections_to_join) as acc)
         ->
        (* Skip bottom environments -- we should have detected the impossibility
           and replaced them with an invalid earlier, but if we did not, they
           won't bring anything but subtleties to the join. *)
        if TE.is_bottom typing_env
        then acc
        else
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
    cut_and_n_way_join0 ~n_way_join_type ~meet_expanded_head ~cut_after
      source_env source_tenv joined_envs equations_to_join
      symbol_projections_to_join
  in
  target_env

let cut_and_n_way_join_with_analysis ~n_way_join_type ~meet_expanded_head
    ~cut_after source_tenv joined_envs =
  let external_ids, joined_envs, equations_to_join, symbol_projections_to_join =
    Index.fold_list
      (fun index (external_id, typing_env)
           (( external_ids,
              joined_envs,
              equations_to_join,
              symbol_projections_to_join ) as acc) ->
        (* Skip bottom environments -- we should have detected the impossibility
           and replaced them with an invalid earlier, but if we did not, they
           won't bring anything but subtleties to the join. *)
        if TE.is_bottom typing_env
        then acc
        else
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
  let source_env = ME.create source_tenv in
  let target_env, bindings =
    cut_and_n_way_join0 ~n_way_join_type ~meet_expanded_head ~cut_after
      source_env source_tenv joined_envs equations_to_join
      symbol_projections_to_join
  in
  let target_env = ME.final_typing_env ~meet_expanded_head target_env in
  let join_analysis = Analysis.create ~external_ids ~joined_envs bindings in
  target_env, join_analysis

let n_way_join_canonicals env kind simples =
  match get_canonical_in_target_env env simples with
  | Canonical_in_source_env simple ->
    Simple_in_target_env.from_source_env simple, env
  | Import_from_all_joined_envs (imported_var, coercion) ->
    let env = import_from_all_envs env imported_var kind in
    let imported_var =
      Variable_in_target_env.create (imported_var :> Variable.t)
    in
    let simple = Simple_in_target_env.var ~coercion imported_var in
    simple, env
  | Existential_for_these_simples ->
    let existential_var, env = existential_for_these_simples env simples kind in
    Simple_in_target_env.var existential_var, env

let n_way_join_simples t kind simples : _ Or_bottom.t * t =
  match simples with
  | [] -> Bottom, t
  | _ :: _ ->
    let canonicals_in_joined_envs =
      Joined_envs.get_canonical_simples_ignoring_name_mode t.joined_envs simples
    in
    (* CR-someday bclement: somehow mark the local variable as used, so that it
       can be re-processed in the current env extension if applicable (if a
       local variable is created while processing an env extension, we currently
       lose any equation that the extension had for that variable). *)
    let canonical_in_target_env, t =
      n_way_join_canonicals t kind canonicals_in_joined_envs
    in
    Ok (canonical_in_target_env : Simple_in_target_env.t :> Simple.t), t

(** {2:extensions Join of extensions} *)

let prepare_nested_join ~meet_expanded_head ~joined_envs ~bindings extensions =
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
          ME.use_meet_env_strict ~meet_expanded_head typing_env
            ~f:(fun meet_env ->
              ME.add_env_extension ~meet_expanded_head meet_env extension)
        with
        | Bottom ->
          (* We can reach bottom here if the extension was created in a more
             generic context, but is added in a context where it is no longer
             reachable. *)
          joined_envs_and_extensions
        | Ok env ->
          let level = TE.cut env ~cut_after in
          Index.Map.add index (env, level) joined_envs_and_extensions)
      Index.Map.empty extensions
  in
  Index.Map.mapi
    (fun index (env, diff_level) ->
      let previous_equations =
        Joined_envs.equations_in_nth_joined_env joined_envs index
      in
      let diff_equations =
        (* Note that we forget the potential newly created variables here, but
           they could end up in the [Bindings_in_target_env] and cause issue if
           they are ever used in the parent environment.

           This is fine, however, because we drop any possible information about
           these variables by calling [forget_definition_of_created_variables]
           in [n_way_join_env_extension]. *)
        Type_in_one_joined_env.create_equations (TEL.equations diff_level)
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
        Name.Map.union_left_biased diff_equations previous_equations
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
        (* Sometimes we might have already added the defining equation of an
           existential due to [replay_definition_of_aliases_in_target_env],
           which is fine. *)
        Name.Map.union_left_biased previous_equations
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

let n_way_join_env_extension ~n_way_join_type ~meet_expanded_head
    env_before_extension extensions : _ Or_bottom.t =
  let joined_equations =
    try
      prepare_nested_join ~meet_expanded_head
        ~bindings:env_before_extension.bindings
        ~joined_envs:env_before_extension.joined_envs extensions
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
      let env_in_extension =
        create ~joined_envs ~bindings:env_before_extension.bindings
      in
      let env_in_extension =
        join_aliases_into_bindings env_in_extension joined_equations
      in
      (* CR-someday bclement: if we create new existential variables during the
         join of env extensions, we might need additional rounds for
         completeness (see comment in [n_way_join_simples]) -- in practice one
         round should be plenty. *)
      let _env_extension_for_inverse_relations, env_in_extension =
        n_way_join_round ~n_way_join_type env_in_extension
          env_in_extension.types_in_joined_envs Variable.Map.empty
      in
      (* It is possible for the call to [add_env_extension] in
         [prepare_nested_join] above to create new variables, which do not exist
         in the parent environments. These variables must not leak into the
         [bindings]: since they don't exist in the parent joined environments,
         we won't be able to find a type for them in the target environment
         outside of the extension.

         For now, we avoid this problem by simply forgetting about the
         definition of new variables (in the target env) during the join of
         extensions. This means that in some cases we might create the same
         variable twice (e.g. we might create a variable to represent {0, 1}
         inside an env extension and then another one outside of the env
         extension), but not incorrect, only slighly inefficient. *)
      let bindings =
        Bindings_in_target_env.forget_definition_of_created_variables
          env_in_extension.bindings ~since:env_before_extension.bindings
      in
      Ok
        ( TEE.from_map (env_in_extension.types_in_target_env :> TG.t Name.Map.t),
          { env_before_extension with bindings } )
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
