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

(* This file performs an analysis where, for each variable, we track how it is
   used and what sources it can have. It then uses the results of this analysis
   to compute the transformations we want to do: removing unused arguments of
   constructors, continuations, and even functions when possible, unboxing
   values which do not escape, and changing the representation of some values.

   # Core analysis

   At its core, we have a graph with a given number of nodes (corresponding to
   variables in the input program), and three types of arrows between nodes:

   - [alias], which means that a variable can flow to another variable,

   - [constructor], which means that a variable can flow to a given field of
   another variable,

   - and [accessor], which means that a given field of a variable can flow to
   another variable.

   Some nodes are also labelled as having unknown sources (for variables outside
   the compilation unit, for instance), or unknown usages (for arguments to
   functions outside the compilation unit, for instance).

   The analysis then tracks, for each node in the graph, what sources that node
   can have (unknown sources, or for each field, what is the source of each of
   its fields), and how it could be used (unknown usages, or for each field, how
   that field could be used).

   With this presentation, sources and usages are symmetrical, up to reversing
   the direction of arrows and exchanging constructors and accessors, so let us
   focus on usages for the moment. The result we want is for each node $x$, the
   sequences of field accesses for which the result has unknown usages. We can
   specify this as a language $Lₓ$ of words over the alphabet of fields. Then,
   the result we want is the least fixed point of the equations:

   - for each node $x$ with unknown usages, $ε ∈ Lₓ$,

   - for each alias [let t = s], $Lₜ ⊆ Lₛ$ (the source has at least as many uses
   as the target)

   - for each accessor [let t = s.f], $f ⋅ Lₜ ⊆ Lₛ$ (each access to the target
   corresponds to an access to the source after taking the field $f$)

   - for each constructor [let t = { f = s; ... }], $f⁻¹ ⋅ Lₜ ⊆ Lₛ$ (each access
   to the target that starts with $f$ corresponds to an access to the sources
   after removing the initial $f$), where $f⁻¹ ⋅ Lₜ$ is the Brzozowski
   derivative of $Lₜ$ with respect to $f$, i.e. { u / f ⋅ u ∈ Lₜ }.

   Given all these operations preserve regular languages, it is easy to see the
   output will be a regular language as well. In practice, we will represent the
   result by saying that for each variable, its usages are either unknown, or
   that for each field, the usages for that field correspond to the union of the
   usages of a given set of variables, essentially specifying the language $Lₓ$
   by its Brzozowski derivatives.

   # Implementation of the core analysis

   We still consider only usages, as sources are symmetrical. One problem we
   have is, in an alias chain, the $Lₓ$ will be repeated a large number of
   times, leading to quadratic behaviour. To avoid this, we only represent
   explicitely $Lₓ$ when $x$ is the source of an accessor. For other variables,
   we represent it implicitely with a set $Uₓ$ of usages of $x$, corresponding
   to accessors only, and such that $Lₓ = ⋃_{t ∈ Uₓ} Lₜ$. However, if a variable
   has unknown usages, we do not represent $Uₓ$, as it would be useless once the
   variable has unknown usages.

   In practice we have several Datalog relations to represent this: first,
   [any_usage x] means that [x] has unknown usages. Then, [usages x y] means
   that [y] is a point where [x] is accessed, corresponding to $y ∈ Uₓ$.

   # Encoding of function calls

   [CR ncourant: move here the explanation about how my_closure is handled.]

   Closures have a special field, [Call_witness], which represents what
   happens when the closure is called. Schematically, if we have a function
   named [f], with a code_id [p], which has a parameter [x] and returns a result
   [r], and that this function flows to a point where there is an application
   where it is named [g], it takes as argument [y] and names the result [s], the
   graph will look like the following: *)
(*
 *    ╔═══╗                    ╔═══╗
 *    ║ f ║───────────────────>║ g ║
 *    ╚═══╝                    ╚═══╝
 *      ^                        │
 *      │                        │
 *    [wit]                    [wit]
 *      │                        │
 *      │                        v
 *    ╔═══╗                    ╔═══╗
 *    ║   ║                    ║   ║
 *    ╚═══╝                    ╚═══╝
 *     ^^^                      ││║
 *     ││║          ╔═══╗       ││║          ╔═══╗
 *     ││╚[param0]══║ a ║       ││╚[param0]═>║ b ║
 *     ││           ╚═══╝       ││           ╚═══╝
 *     ││           ╔═══╗       ││           ╔═══╗
 *     │└[return0]──║ r ║       │└[return0]─>║ s ║
 *     │            ╚═══╝       │            ╚═══╝
 *     │            ╔═══╗       │            ╔═══╗
 *     └─[code_id]──║ p ║       └─[code_id]─>║ T ║
 *                  ╚═══╝                    ╚═══╝
 *  ╰──────────╮╭──────────╯ ╰─────────╮╭───────────╯
 *      (co)constructors         (co)accessors
 *)
(* On the left side of the graph, the construction of the [Call_witness]
   field of the closure (named [wit] in the graph, for compactness) is done. On
   the right side, the access to that field representing the application is
   done. When the function is applied, we need to do three things:

   - Mark the code_id [p] as used. This is easy, since it corresponds exactly to
   what happens with standard block constructors and accessors.

   - Add an alias [let s = r]. Again, this is easy and corresponds precisely to
   what happens with constructors and accessors.

   - Add an alias [let a = b]. This is the direction opposite to what would
   happen with constructors and accessors. Thus, for parameters, we use
   coconstructors and coaccessors that put this alias in the opposite direction.

   # Local fields

   Fields that are value slots or function slots originating from the current
   compilation unit are said to be *local*. Local fields are special, because we
   know all the places they are used: any constructor or accessor to a local
   field in a different compilation unit comes necessarily from inlining such a
   use from the current compilation unit, or type-based changed from the types
   exported by the current compilation unit. As such, we can perform a much more
   precise analysis on them.

   Thus, let us assume we have a block $a$, with a local field $f$ containing a
   value $u$, and a block $b$, from which we read a value $v$ from the field
   $f$. We must add an alias [let v = u] if there is a way $a$ could possibly
   flow to $b$.

   Let us consider three cases:

   - $a$ has known usages. Then, $b$ is in the usages of $a$, making the usual
   mechanism of handling usages for fields of constructors add the alias [let v
   = u].

   - $b$ has known sources. Then, $a$ is in the sources of $b$, making the usual
   mechanism of handling sources for the result of accessors add the alias [let
   v = u].

   - Both $a$ has unknown usages and $b$ has unknown sources. Then, neither of
   the above will apply, and we need to do something. Fortunately, when this
   happens, we cannot do any better than assuming $a$ will flow to $b$, so it is
   enough to mark all local fields stored in blocks that have unknown usages,
   and all local fields read from block that have unknown sources, and add
   aliases between both.

   Still, there is a subtlety in the other cases we must take care not to
   overlook. The usual mechanism of handling usages for fields of constructors
   (and likewise for the sources of the result of accessors) only add an alias
   if the result has known usages (resp. sources) as an optimisation. However,
   this optimisation is incorrect in the case of local fields! Remember the
   above example, and assume that $a$ has unknown usages, $b$ has known sources
   (including $a$), and that $u$ has unknown sources. If we used the
   optimisation we use for other fields, then we would mark that the field $f$
   of $a$ has unknown sources, therefore $v$ has unknown sources, without adding
   an alias [let v = u]. While this is correct for the sources of $v$, this
   misses the usages of $u$! Indeed, $u$ is correctly marked as being in a local
   field $f$ of a block with unknown usages, but no block with unknown sources
   reads from a local field $f$, missing $v$ as uses of $u$. Fortunately,
   disabling this optimisation for local fields is enough to restore
   correctness: in that case, we simply add an alias [let v = u], which
   correctly accounts for the usages of $u$.
*)

(* Disable [not-principal] warning in this file. We often write code that looks
   like [let@ [x; y] = a in b] where the list constructor is an [hlist], and [a]
   is used to determine the type of that [hlist]. Unfortunately, due to how
   [let@] is expansed, this is not principal. *)
[@@@ocaml.warning "-not-principal"]

open Global_flow_graph.Relations
open! Datalog_helpers.Syntax
open Datalog_helpers

module Relations = struct
  type 'a atom = [> `Atom of Datalog.atom] as 'a

  type 'a term = 'a Datalog.Term.t

  (** [usages] and [sources] are dual. They build the same relation from
      [accessor] and [rev_constructor]. [any_usage] and [any_source] are the
      tops. *)

  (** [usages x y] y is an alias of x.

      For performance reasons, we don't want to represent [usages x y] when x is
      top ([any_usage x] is valid). If x is top the any_usage predicate subsumes
      this property.

      We avoid building this relation in that case, but it is possible to have
      both [usages x y] and [any_usage x] depending on the resolution order.

      [usages x x] is used to represent the actual use of x. *)
  let usages = rel2 "usages" Cols.[n; n]

  (** [any_usage x] x is used in an uncontrolled way *)
  let any_usage = any_usage

  (** [sources x y] y is a source of x.

      For performance reasons, we don't want to represent [sources x y] when x
      is top ([any_source x] is valid). If x is top the any_source predicate
      subsumes this property.

      We avoid building this relation in that case, but it is possible to have
      both [sources x y] and [any_source x] depending on the resolution order.

      [sources x x] is used to represent the actual source of x. *)
  let sources = rel2 "sources" Cols.[n; n]

  (** [any_source x] the special extern value 'any_source' is a source of x it
      represents the top for the sources. It can be produced for instance by an
      argument from an escaping function or the result of non axiomatized
      primitives and external symbols. Right now functions coming from other
      files are considered unknown *)
  let any_source = any_source

  (* Reverse relations *)
  let rev_alias =
    let tbl = nrel "rev_alias" Cols.[n; n] in
    fun ~from ~to_ -> tbl % [from; to_]

  let rev_use =
    let tbl = nrel "rev_use" Cols.[n; n] in
    fun ~from ~to_ -> tbl % [from; to_]

  let rev_constructor =
    let tbl = nrel "rev_constructor" Cols.[n; f; n] in
    fun ~from relation ~base -> tbl % [from; relation; base]

  let rev_accessor =
    let tbl = nrel "rev_accessor" Cols.[n; f; n] in
    fun ~base relation ~to_ -> tbl % [base; relation; to_]

  let rev_parameter =
    let tbl = nrel "rev_parameter" Cols.[n; cf; n] in
    fun ~to_ relation ~base -> tbl % [to_; relation; base]

  let rev_argument =
    let tbl = nrel "rev_argument" Cols.[n; cf; n] in
    fun ~base relation ~from -> tbl % [base; relation; from]

  (* Local fields are value and function slots that originate from the current
     compilation unit. As such, all sources and usages from these fields will
     necessarily correspond to either code in the current compilation unit, or a
     resimplified version of it.

     The consequence of this is that we can consider them not to have
     [any_usage], nor to have [any_source], even if the block containing them
     has [any_usage] or [any_source]. Instead, we need to add an alias from [x]
     to [y] if [x] if stored in a field of [source], [y] is read from the same
     field of [usage], and [source] might flow to [usage]. *)

  let reading_field = rel2 "reading_field" Cols.[f; n]

  let escaping_field = rel2 "escaping_field" Cols.[f; n]

  let nontop_usages x y = `Only_if ([~~(any_usage x)], usages x y)

  let nontop_sources x y = `Only_if ([~~(any_source x)], sources x y)

  (* [has_usage x] means that [x] must continue to exist at runtime, that is,
     either it has [any_usage], or some field of [x] is itself [has_usage]. *)
  let has_usage = rel1 "has_usage" Cols.[n]

  (* [has_source x] means that [x] can have been created at runtime.
     Unfortunately due to limitations of the datalog engine, it means that
     either [x] is [any_source], or that at least one field of [x] (instead of
     all fields of [x], which would be the exact answer) is [has_source]. *)
  let has_source = rel1 "has_source" Cols.[n]

  let field_of_constructor_is_used_tbl =
    Datalog.create_relation ~name:"field_of_constructor_is_used" Cols.[n; f]

  let field_of_constructor_is_used constr field =
    field_of_constructor_is_used_tbl % [constr; field]

  let field_of_constructor_is_used_top =
    rel2 "field_of_constructor_is_used_top" Cols.[n; f]

  let field_of_constructor_is_used_as =
    rel3 "field_of_constructor_is_used_as" Cols.[n; f; n]

  let multiple_allocation_points = rel1 "multiple_allocations_points" Cols.[n]

  let dominated_by_allocation_point =
    rel2 "dominated_by_allocation_point" Cols.[n; n]

  let allocation_point_dominator = rel2 "allocation_point_dominator" Cols.[n; n]
end

open! Relations

(* The program is abstracted as a series of relations concerning the reading and
   writing of fields of values.

   There are 5 different relations:

   - [alias to_ from] corresponds to [let to_ = from]

   - [accessor to_ relation base] corresponds to [let to_ = base.relation]

   - [constructor base relation from] corresponds to constructing a block [let
   base = { relation = from }]

   - [propagate if_used to_ from] means [alias to_ from], but only if [is_used]
   is used

   - [use to_ from] corresponds to [let to_ = f(from)], creating an arbitrary
   result [to_] and consuming [from].

   We perform an analysis that computes the ways each value can be used: either
   entirely, not at all, or, for each of its fields, how that field might be
   used. We also perform a reverse analysis that computes where each value can
   come from: either an arbitrary source (for use and values coming from outside
   the compilation unit), or a given constructor. *)

module Datalog_schedule = struct
  (* Group rules by priority. Rules with (let$) are executed first, then the
     rules with (let$$) are executed. *)
  let with_priority p x f = p, ( let$ ) x f

  let ( let$ ) x f = with_priority 0 x f

  let ( let$$ ) x f = with_priority 1 x f

  let make_schedule l =
    Schedule.fixpoint
      (List.init 2 (fun i ->
           Schedule.saturate
             (List.filter_map (fun (p, r) -> if i = p then Some r else None) l)))

  let reverse_rules =
    (* Reverse relations, because datalog does not implement a more efficient
       representation yet. Datalog iterates on the first key of a relation
       first, those reversed relations allows to select a different key. Of
       these, only [alias] has both priorities, because it is the only of those
       relations that is extended after graph construction. *)
    [ (let$ [to_; from] = ["to_"; "from"] in
       [alias ~to_ ~from] ==> rev_alias ~from ~to_);
      (let$$ [to_; from] = ["to_"; "from"] in
       [alias ~to_ ~from] ==> rev_alias ~from ~to_);
      (let$ [to_; from] = ["to_"; "from"] in
       [use ~to_ ~from] ==> rev_use ~from ~to_);
      (let$ [to_; relation; base] = ["to_"; "relation"; "base"] in
       [accessor ~to_ relation ~base] ==> rev_accessor ~base relation ~to_);
      (let$ [base; relation; from] = ["base"; "relation"; "from"] in
       [constructor ~base relation ~from]
       ==> rev_constructor ~from relation ~base);
      (let$ [from; relation; base] = ["from"; "relation"; "base"] in
       [argument ~from relation ~base] ==> rev_argument ~base relation ~from);
      (let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
       [parameter ~base relation ~to_] ==> rev_parameter ~to_ relation ~base) ]

  let alias_rules =
    [ (* The [propagate] relation is part of the input of the solver, with the
         intended meaning of this rule, that is, an alias if [is_used] is
         used. *)
      (let$ [if_used; to_; from] = ["if_used"; "to_"; "from"] in
       [any_usage if_used; propagate ~if_used ~to_ ~from] ==> alias ~to_ ~from);
      (* Likewise, [alias_if_any_source] means an alias if [is_any_source] has
         any source. *)
      (let$ [if_any_source; to_; from] = ["if_any_source"; "to_"; "from"] in
       [any_source if_any_source; alias_if_any_source ~if_any_source ~to_ ~from]
       ==> alias ~to_ ~from);
      (let$$ [base; base_use; relation; from; to_] =
         ["base"; "base_use"; "relation"; "from"; "to_"]
       in
       [ constructor ~base relation ~from;
         nontop_usages base base_use;
         rev_accessor ~base:base_use relation ~to_ ]
       ==> alias ~to_ ~from);
      (let$$ [base; base_use; relation; from; to_] =
         ["base"; "base_use"; "relation"; "from"; "to_"]
       in
       [ when1 Field.is_local relation;
         constructor ~base relation ~from;
         usages base base_use;
         rev_accessor ~base:base_use relation ~to_ ]
       ==> alias ~to_ ~from);
      (let$$ [base; base_use; relation; to_; from] =
         ["base"; "base_use"; "relation"; "to_"; "from"]
       in
       [ parameter ~base relation ~to_;
         nontop_usages base base_use;
         rev_argument ~base:base_use relation ~from ]
       ==> alias ~to_ ~from);
      (let$$ [base; base_source; relation; to_; from] =
         ["base"; "base_source"; "relation"; "to_"; "from"]
       in
       [ rev_accessor ~base relation ~to_;
         nontop_sources base base_source;
         constructor ~base:base_source relation ~from ]
       ==> alias ~to_ ~from);
      (let$$ [base; base_source; relation; to_; from] =
         ["base"; "base_source"; "relation"; "to_"; "from"]
       in
       [ when1 Field.is_local relation;
         rev_accessor ~base relation ~to_;
         sources base base_source;
         constructor ~base:base_source relation ~from ]
       ==> alias ~to_ ~from);
      (let$ [relation; from; to_] = ["relation"; "from"; "to_"] in
       [escaping_field relation from; reading_field relation to_]
       ==> alias ~to_ ~from);
      (let$$ [base; base_source; relation; from; to_] =
         ["base"; "base_source"; "relation"; "from"; "to_"]
       in
       [ rev_argument ~base relation ~from;
         nontop_sources base base_source;
         parameter ~base:base_source relation ~to_ ]
       ==> alias ~to_ ~from) ]

  let has_usage_rules =
    [ (let$ [x] = ["x"] in
       [any_usage x] ==> has_usage x);
      (let$ [to_; from] = ["to_"; "from"] in
       [has_usage to_; alias ~to_ ~from] ==> has_usage from);
      (let$$ [to_; relation; base] = ["to_"; "relation"; "base"] in
       [has_usage to_; accessor ~to_ relation ~base] ==> has_usage base);
      (let$$ [from; relation; base] = ["from"; "relation"; "base"] in
       [has_source from; argument ~from relation ~base] ==> has_usage base) ]

  let has_source_rules =
    [ (let$ [x] = ["x"] in
       [any_source x] ==> has_source x);
      (let$ [from; to_] = ["from"; "to_"] in
       [has_source from; rev_alias ~from ~to_] ==> has_source to_);
      (let$$ [from; relation; base] = ["from"; "relation"; "base"] in
       [has_source from; rev_constructor ~from relation ~base]
       ==> has_source base);
      (let$$ [to_; relation; base] = ["to_"; "relation"; "base"] in
       [has_usage to_; rev_parameter ~to_ relation ~base] ==> has_source base)
    ]

  let any_usage_rules =
    [ (let$ [to_; from] = ["to_"; "from"] in
       [has_usage to_; use ~to_ ~from] ==> any_usage from);
      (let$ [to_; from] = ["to_"; "from"] in
       [any_usage to_; alias ~to_ ~from] ==> any_usage from);
      (let$ [base; relation; from] = ["base"; "relation"; "from"] in
       [ any_usage base;
         constructor ~base relation ~from;
         unless1 Field.is_local relation ]
       ==> any_usage from);
      (let$ [base; relation; from] = ["base"; "relation"; "from"] in
       [any_source base; rev_argument ~base relation ~from] ==> any_usage from)
    ]

  let any_source_rules =
    [ (let$ [x] = ["x"] in
       [zero_alloc_source x] ==> any_source x);
      (let$ [from; to_] = ["from"; "to_"] in
       [rev_alias ~from ~to_; any_source from] ==> any_source to_);
      (let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
       [any_usage base; parameter ~base relation ~to_] ==> any_source to_);
      (let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
       [ any_source base;
         rev_accessor ~base relation ~to_;
         unless1 Field.is_local relation ]
       ==> any_source to_);
      (let$ [from; to_] = ["from"; "to_"] in
       [has_source from; rev_use ~from ~to_] ==> any_source to_) ]

  let usages_rules =
    [ (let$$ [to_; relation; base] = ["to_"; "relation"; "base"] in
       [accessor ~to_ relation ~base] ==> nontop_usages base base);
      (let$$ [from; relation; base] = ["from"; "relation"; "base"] in
       [argument ~from relation ~base] ==> nontop_usages base base);
      (let$$ [to_; from; usage] = ["to_"; "from"; "usage"] in
       [nontop_usages to_ usage; alias ~to_ ~from] ==> nontop_usages from usage)
    ]

  let sources_rules =
    [ (let$$ [from; relation; base] = ["from"; "relation"; "base"] in
       [rev_constructor ~from relation ~base] ==> nontop_sources base base);
      (let$$ [to_; relation; base] = ["to_"; "relation"; "base"] in
       [rev_parameter ~to_ relation ~base] ==> nontop_sources base base);
      (let$$ [from; to_; source] = ["from"; "to_"; "source"] in
       [nontop_sources from source; rev_alias ~from ~to_]
       ==> nontop_sources to_ source) ]

  let local_rules =
    [ (let$ [base; relation; from] = ["base"; "relation"; "from"] in
       [ any_usage base;
         constructor ~base relation ~from;
         when1 Field.is_local relation ]
       ==> escaping_field relation from);
      (let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
       [ any_source base;
         rev_accessor ~base relation ~to_;
         when1 Field.is_local relation ]
       ==> reading_field relation to_) ]

  let zero_alloc_rules =
    [ (let$ [from; to_] = ["from"; "to_"] in
       [rev_alias ~from ~to_; zero_alloc_source from] ==> zero_alloc_source to_);
      (let$ [base; relation; to_] = ["base"; "relation"; "to_"] in
       [zero_alloc_source base; rev_accessor ~base relation ~to_]
       ==> zero_alloc_source to_);
      (let$ [from; to_] = ["from"; "to_"] in
       [zero_alloc_source from; rev_use ~from ~to_] ==> zero_alloc_source to_)
    ]

  let schedule =
    List.concat
      [ reverse_rules;
        alias_rules;
        has_usage_rules;
        has_source_rules;
        any_usage_rules;
        any_source_rules;
        usages_rules;
        sources_rules;
        local_rules;
        zero_alloc_rules ]
    |> make_schedule
end

let datalog_schedule = Datalog_schedule.schedule

type usages = Usages of unit Code_id_or_name.Map.t [@@unboxed]

(** Computes all usages of a set of variables (input). Sets are represented as
    unit maps for convenience with datalog. Usages is represented as a set of
    variables: those are the variables where the input variables flow with live
    accessor.

    [follow_known_arity_calls] specifies that if the set of variables
    corresponds to a closure that is called by an known arity call, we should
    look at the [my_closure] value of the corresponding code_id as well. This is
    only necessary if the set of variables can correspond to a closure *and* the
    set of variables contains variables that are not the allocation point of the
    set of closures.

    The reason for this is that for a given closure that is called, the [usages]
    do not usually include the uses of the closure inside the code of the
    closure itself. However, when we allocate a set of closures, we include an
    alias between the allocated closures and their [my_closure] variable inside
    the corresponding code. As such, the usages at an allocation point are
    always representative of all the uses, and as such, do not require to follow
    the calls.

    Function slots are considered as aliases for this analysis. *)
let add_usages_through_function_slots :
    follow_known_arity_calls:bool -> Datalog.database -> usages -> usages =
  let open! Fixit in
  let stmt =
    let@ follow_known_arity_calls =
      paramc "follow_known_arity_calls" One.cols One.of_bool
    in
    let@ in_ = param "in_" Cols.[n] in
    let@ out = fix1' in_ in
    [ (let$ [x; apply_witness; call_witness; code_id; my_closure_of_code_id; y] =
         [ "x";
           "apply_witness";
           "call_witness";
           "code_id";
           "my_closure_of_code_id";
           "y" ]
       in
       [ One.flag follow_known_arity_calls;
         out % [x];
         rev_accessor ~base:x
           !!Field.known_arity_call_witness
           ~to_:apply_witness;
         sources apply_witness call_witness;
         constructor ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:code_id;
         code_id_my_closure ~code_id ~my_closure:my_closure_of_code_id;
         usages my_closure_of_code_id y;
         has_usage y ]
       ==> out % [y]);
      (let$ [x; field; y; z] = ["x"; "field"; "y"; "z"] in
       [ out % [x];
         rev_accessor ~base:x field ~to_:y;
         when1 Field.is_function_slot field;
         usages y z;
         has_usage z ]
       ==> out % [z]) ]
  in
  fun ~follow_known_arity_calls db (Usages s) ->
    Usages (run stmt db follow_known_arity_calls s)

let get_direct_usages : Datalog.database -> unit Code_id_or_name.Map.t -> usages
    =
  let open! Fixit in
  run
    (let+ usages =
       let@ in_ = param "in_" Cols.[n] in
       let@ out = fix1' (empty Cols.[n]) in
       [ (let$ [x; y] = ["x"; "y"] in
          [in_ % [x]; usages x y; has_usage y] ==> out % [y]) ]
     in
     Usages usages)

let get_one_field_usage :
    Datalog.database -> Field.t -> usages -> _ Or_unknown_or_bottom.t =
  let open! Fixit in
  run
    (let@ in_field = param1s "in_field" Cols.f in
     let@ in_ = paramc "in_" Cols.[n] (fun (Usages s) -> s) in
     let+ [used_as_top; used_as_vars] =
       let@ [used_as_top; used_as_vars] =
         seq' [empty One.cols; empty Cols.[n]]
       in
       [ (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ in_ % [x];
            in_field % [field];
            rev_accessor ~base:x field ~to_:y;
            any_usage y ]
          ==> One.flag used_as_top);
         (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ ~~(One.flag used_as_top);
            in_ % [x];
            in_field % [field];
            rev_accessor ~base:x field ~to_:y;
            has_usage y ]
          ==> used_as_vars % [y]) ]
     in
     if One.to_bool used_as_top
     then Or_unknown_or_bottom.Unknown
     else if Code_id_or_name.Map.is_empty used_as_vars
     then Or_unknown_or_bottom.Bottom
     else Or_unknown_or_bottom.Ok used_as_vars)

(** For an usage set, compute the way its fields are used. As function slots are
    transparent for [get_usages], functions slot usages are ignored here. *)
let get_fields :
    Datalog.database ->
    usages ->
    unit Code_id_or_name.Map.t Or_unknown.t Field.Map.t =
  let open! Fixit in
  run
    (let@ in_ = paramc "in_" Cols.[n] (fun (Usages s) -> s) in
     let+ [out1; out2] =
       let@ [out1; out2] = seq' [empty Cols.[f]; empty Cols.[f; n]] in
       [ (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ in_ % [x];
            rev_accessor ~base:x field ~to_:y;
            any_usage y;
            unless1 Field.is_function_slot field ]
          ==> out1 % [field]);
         (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ in_ % [x];
            rev_accessor ~base:x field ~to_:y;
            has_usage y;
            ~~(out1 % [field]);
            unless1 Field.is_function_slot field ]
          ==> out2 % [field; y]) ]
     in
     Field.Map.merge
       (fun k x y ->
         match x, y with
         | None, None ->
           Misc.fatal_errorf
             "Field %a appeared in neither output in [get_fields_usage]"
             Field.print k
         | Some _, Some _ ->
           Misc.fatal_errorf "Got two results for field %a" Field.print k
         | Some (), None -> Some Or_unknown.Unknown
         | None, Some m -> Some (Or_unknown.Known m))
       out1 out2)

let get_one_field_usage_of_constructors :
    Datalog.database ->
    unit Code_id_or_name.Map.t ->
    Field.t ->
    _ Or_unknown_or_bottom.t =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let@ fieldt = param1s "field" Cols.f in
     let+ [out1; out2] =
       let@ [out1; out2] = seq' [empty One.cols; empty Cols.[n]] in
       [ (let$ [x; field] = ["x"; "field"] in
          [in_ % [x]; fieldt % [field]; field_of_constructor_is_used_top x field]
          ==> One.flag out1);
         (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ in_ % [x];
            fieldt % [field];
            field_of_constructor_is_used_as x field y;
            ~~(One.flag out1) ]
          ==> out2 % [y]) ]
     in
     if One.to_bool out1
     then Or_unknown_or_bottom.Unknown
     else if Code_id_or_name.Map.is_empty out2
     then Or_unknown_or_bottom.Bottom
     else Or_unknown_or_bottom.Ok out2)

let get_fields_usage_of_constructors :
    Datalog.database ->
    unit Code_id_or_name.Map.t ->
    unit Code_id_or_name.Map.t Or_unknown.t Field.Map.t =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let+ [out1; out2] =
       let@ [out1; out2] = seq' [empty Cols.[f]; empty Cols.[f; n]] in
       [ (let$ [x; field] = ["x"; "field"] in
          [ in_ % [x];
            field_of_constructor_is_used_top x field;
            unless1 Field.is_function_slot field ]
          ==> out1 % [field]);
         (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ in_ % [x];
            field_of_constructor_is_used_as x field y;
            ~~(out1 % [field]);
            unless1 Field.is_function_slot field ]
          ==> out2 % [field; y]) ]
     in
     Field.Map.merge
       (fun k x y ->
         match x, y with
         | None, None ->
           Misc.fatal_errorf
             "Field %a appeared in neither output in \
              [get_fields_usage_of_constructors]"
             Field.print k
         | Some _, Some _ ->
           Misc.fatal_errorf "Got two results for field %a" Field.print k
         | Some (), None -> Some Or_unknown.Unknown
         | None, Some m -> Some (Or_unknown.Known m))
       out1 out2)

type set_of_closures_def =
  | Not_a_set_of_closures
  | Set_of_closures of (Function_slot.t * Code_id_or_name.t) list

let get_set_of_closures_def :
    Datalog.database -> Code_id_or_name.t -> set_of_closures_def =
  let q =
    query
      (let^$ [x], [relation; y] = ["x"], ["relation"; "y"] in
       [ constructor ~base:x relation ~from:y;
         when1 Field.is_function_slot relation ]
       =>? [relation; y])
  in
  fun db v ->
    let l =
      Cursor.fold_with_parameters q [v] db ~init:[] ~f:(fun [f; y] l ->
          (Field.must_be_function_slot f, y) :: l)
    in
    match l with [] -> Not_a_set_of_closures | _ :: _ -> Set_of_closures l

let any_usage_query =
  let^? [x], [] = ["x"], [] in
  [any_usage x]

let has_usage_query =
  let^? [x], [] = ["x"], [] in
  [has_usage x]

let has_use db x = has_usage_query [x] db

(* CR pchambart: field_used should rename to mean that this is the specific
   field of a given variable. *)
let field_used =
  let field_of_constructor_is_used_query =
    let^? [x; f], [] = ["x"; "f"], [] in
    [field_of_constructor_is_used x f]
  in
  fun db x field -> field_of_constructor_is_used_query [x; field] db

let any_source_query =
  let^? [x], [] = ["x"], [] in
  [any_source x]

let has_source_query =
  let^? [x], [] = ["x"], [] in
  [has_source x]

let not_local_field_has_source =
  let field_source_query =
    let^? [x; f], [s; v] = ["x"; "f"], ["s"; "v"] in
    [sources x s; constructor ~base:s f ~from:v; has_source v]
  in
  fun db x field -> any_source_query [x] db || field_source_query [x; field] db

let post_processing_rules =
  saturate_in_order
    [ (let$ [base; relation; from] = ["base"; "relation"; "from"] in
       [ constructor ~base relation ~from;
         any_usage base;
         unless1 Field.is_local relation ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_top base relation ]);
      (let$ [base; relation; from; usage; v] =
         ["base"; "relation"; "from"; "usage"; "v"]
       in
       [ constructor ~base relation ~from;
         usages base usage;
         rev_accessor ~base:usage relation ~to_:v;
         any_usage v ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_top base relation ]);
      (let$ [base; relation; from; usage; v] =
         ["base"; "relation"; "from"; "usage"; "v"]
       in
       [ constructor ~base relation ~from;
         usages base usage;
         rev_accessor ~base:usage relation ~to_:v;
         has_usage v ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_as base relation v ]);
      (let$ [base; relation; from; x] = ["base"; "relation"; "from"; "x"] in
       [ constructor ~base relation ~from;
         any_usage base;
         reading_field relation x;
         any_usage x ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_top base relation ]);
      (let$ [base; relation; from; x] = ["base"; "relation"; "from"; "x"] in
       [ constructor ~base relation ~from;
         any_usage base;
         reading_field relation x;
         has_usage x ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_as base relation x ]);
      (let$ [usage; base; relation; from; v] =
         ["usage"; "base"; "relation"; "from"; "v"]
       in
       [ constructor ~base relation ~from;
         sources usage base;
         when1 Field.is_local relation;
         any_usage base;
         rev_accessor ~base:usage relation ~to_:v;
         has_usage v ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_as base relation v ]);
      (let$ [usage; base; relation; from; v] =
         ["usage"; "base"; "relation"; "from"; "v"]
       in
       [ constructor ~base relation ~from;
         sources usage base;
         when1 Field.is_local relation;
         any_usage base;
         rev_accessor ~base:usage relation ~to_:v;
         any_usage v ]
       ==> and_
             [ field_of_constructor_is_used base relation;
               field_of_constructor_is_used_top base relation ]);
      (* CR ncourant: this marks any [Apply] field as
         [field_of_constructor_is_used], as long as the function is called.
         Shouldn't that be gated behind a [cannot_change_calling_convetion]? *)
      (* (let$ [base; relation; from; coderel; call_witness] = ["base";
         "relation"; "from"; "coderel"; "call_witness"] in [ constructor base
         relation from; when1 is_apply_field relation; constructor base coderel
         call_witness; any_usage indirect_call_witness; when1 is_code_field
         coderel ] ==> field_of_constructor_is_used base relation); *)
      (* CR ncourant: should this be reenabled? I think this is no longer
         necessary because we remove unused arguments of continuations,
         including return continuations. *)
      (let$ [x] = ["x"] in
       [any_source x] ==> multiple_allocation_points x);
      (let$ [x; y; z] = ["x"; "y"; "z"] in
       [ sources x y;
         has_source y;
         sources x z;
         has_source z;
         distinct Cols.n y z ]
       ==> multiple_allocation_points x);
      (* [allocation_point_dominator x y] is the same as
         [dominated_by_allocation_point y x], which is that [y] is the
         allocation point dominator of [x]. *)
      (let$ [x; y] = ["x"; "y"] in
       [sources x y; has_source y; ~~(multiple_allocation_points x)]
       ==> and_
             [allocation_point_dominator x y; dominated_by_allocation_point y x])
    ]

let has_source_query db x = has_source_query [x] db

let perform_analysis db ~stats =
  let db =
    Profile.record_call ~accumulate:true "analysis" (fun () ->
        Datalog.Schedule.run ~stats datalog_schedule db)
  in
  let db =
    Profile.record_call ~accumulate:true "compute_field_usages" (fun () ->
        List.fold_left
          (fun db rule -> Datalog.Schedule.run ~stats rule db)
          db post_processing_rules)
  in
  db

type keep_or_delete =
  | Keep
  | Delete

let unknown_code_id_actually_directly_called_query =
  let^? [closure], [known_arity_call_witness] =
    ["closure"], ["known_arity_call_witness"]
  in
  [ rev_accessor ~base:closure
      !!Field.known_arity_call_witness
      ~to_:known_arity_call_witness;
    any_source known_arity_call_witness ]

let code_id_actually_directly_called_query =
  query
    (let^$ [closure], [apply_widget; call_witness; codeid] =
       ["closure"], ["apply_widget"; "call_witness"; "codeid"]
     in
     [ rev_accessor ~base:closure
         !!Field.known_arity_call_witness
         ~to_:apply_widget;
       sources apply_widget call_witness;
       has_source call_witness;
       constructor ~base:call_witness
         !!Field.code_id_of_call_witness
         ~from:codeid ]
     =>? [codeid])

let code_id_actually_directly_called db closure =
  let closure = Code_id_or_name.name closure in
  if unknown_code_id_actually_directly_called_query [closure] db
  then Or_unknown.Unknown
  else
    Or_unknown.Known
      (Datalog.Cursor.fold_with_parameters
         code_id_actually_directly_called_query [closure] db
         ~init:Code_id.Set.empty ~f:(fun [codeid] acc ->
           let codeid =
             Code_id_or_name.pattern_match' codeid
               ~code_id:(fun code_id -> code_id)
               ~name:(fun name ->
                 Misc.fatal_errorf
                   "code_id_actually_directly_called found a name: %a"
                   Name.print name)
           in
           Code_id.Set.add codeid acc))

type sources =
  | Any_source
  | Sources of unit Code_id_or_name.Map.t

let get_direct_sources :
    Datalog.database -> unit Code_id_or_name.Map.t -> sources =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let+ [any; out] =
       let@ [any; out] = fix' [empty One.cols; empty Cols.[n]] in
       [ (let$ [x] = ["x"] in
          [in_ % [x]; any_source x] ==> One.flag any);
         (let$ [x; y] = ["x"; "y"] in
          [~~(One.flag any); in_ % [x]; sources x y; has_source y] ==> out % [y])
       ]
     in
     if One.to_bool any then Any_source else Sources out)

let get_field_sources :
    Datalog.database -> unit Code_id_or_name.Map.t -> Field.t -> sources =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let@ in_field = param1s "in_field" Cols.f in
     let+ [any; out] =
       let@ [any; out] = fix' [empty One.cols; empty Cols.[n]] in
       [ (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ ~~(One.flag any);
            in_ % [x];
            in_field % [field];
            constructor ~base:x field ~from:y;
            any_source y ]
          ==> One.flag any);
         (let$ [x; field; y; z] = ["x"; "field"; "y"; "z"] in
          [ ~~(One.flag any);
            in_ % [x];
            in_field % [field];
            constructor ~base:x field ~from:y;
            sources y z;
            has_source z ]
          ==> out % [z]) ]
     in
     if One.to_bool any then Any_source else Sources out)

let cofield_has_use :
    Datalog.database -> unit Code_id_or_name.Map.t -> Cofield.t -> bool =
  let open! Fixit in
  run
    (let@ in_ = param "in_" Cols.[n] in
     let@ in_field = param1s "in_field" Cols.cf in
     let+ out =
       let@ out = fix1' (empty One.cols) in
       [ (let$ [x; field; y] = ["x"; "field"; "y"] in
          [ in_ % [x];
            in_field % [field];
            parameter ~base:x field ~to_:y;
            has_usage y ]
          ==> One.flag out) ]
     in
     One.to_bool out)

let rec arguments_used_by_call db ep callee_sources grouped_args =
  match grouped_args with
  | [] -> []
  | first_arg_group :: grouped_args_rest -> (
    match callee_sources with
    | Any_source -> List.map (List.map (fun x -> x, Keep)) grouped_args
    | Sources callee_sources -> (
      let witness_sources =
        get_field_sources db callee_sources (Field.call_witness ep)
      in
      match witness_sources with
      | Any_source -> List.map (List.map (fun x -> x, Keep)) grouped_args
      | Sources witness_sources ->
        let first_arg_group =
          List.mapi
            (fun i x ->
              ( x,
                if cofield_has_use db witness_sources (Cofield.param i)
                then Keep
                else Delete ))
            first_arg_group
        in
        let grouped_args_rest =
          match grouped_args_rest with
          | [] -> [] (* Avoid computing sources of result if no more args *)
          | _ :: _ ->
            arguments_used_by_call db ep
              (get_field_sources db witness_sources
                 (Field.normal_return_of_call 0))
              grouped_args_rest
        in
        first_arg_group :: grouped_args_rest))

let arguments_used_by_known_arity_call db callee args =
  List.flatten
    (arguments_used_by_call db Field.Known_arity_code_pointer
       (get_direct_sources db (Code_id_or_name.Map.singleton callee ()))
       [args])

let arguments_used_by_unknown_arity_call db callee args =
  arguments_used_by_call db Field.Unknown_arity_code_pointer
    (get_direct_sources db (Code_id_or_name.Map.singleton callee ()))
    args

type single_field_source =
  | No_source
  | One of Code_id_or_name.t
  | Many

let get_single_field_source =
  let q_any_source =
    let^? [block; field], [source] = ["block"; "field"], ["source"] in
    [constructor ~base:block field ~from:source; any_source source]
  in
  let q_source =
    query
      (let^$ [block; field], [field_source; source] =
         ["block"; "field"], ["field_source"; "source"]
       in
       [ constructor ~base:block field ~from:field_source;
         sources field_source source;
         has_source source ]
       =>? [source])
  in
  fun db block field ->
    if q_any_source [block; field] db
    then Many
    else
      Cursor.fold_with_parameters q_source [block; field] db ~init:No_source
        ~f:(fun [source] acc ->
          match acc with No_source -> One source | One _ | Many -> Many)

let get_allocation_point =
  let dom =
    query
      (let^$ [x], [y] = ["x"], ["y"] in
       [allocation_point_dominator x y] =>? [y])
  in
  fun db x ->
    Cursor.fold_with_parameters dom [x] db ~init:None ~f:(fun [y] acc ->
        assert (Option.is_none acc);
        Some y)

let any_usage db x = any_usage_query [x] db

let any_source db x = any_source_query [x] db

let get_usages db x : _ Or_unknown_or_bottom.t =
  if not (has_usage_query [x] db)
  then Bottom
  else if any_usage_query [x] db
  then Unknown
  else Ok (get_direct_usages db (Code_id_or_name.Map.singleton x ()))

let get_single_source db x : _ Or_unknown_or_bottom.t =
  if not (has_source_query db x)
  then Bottom
  else
    match get_allocation_point db x with
    | None -> Unknown
    | Some source -> Ok source
