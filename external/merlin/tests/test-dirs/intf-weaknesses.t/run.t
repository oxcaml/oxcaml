The `intf-weaknesses` query suggests mode, modality, and kind annotations
to add to an OCaml .mli to expose OxCaml functionality.

[strengthen foo.mli] (helpers.sh) prints the implementation, then foo.mli
with every suggested edit applied, then re-checks the unmodified .ml against
it with the batch compiler. A "CONFORMANCE ERROR" line is a bug: the query
suggested something that does not typecheck. Values whose analysis was
abandoned by a failed moregen re-run are printed too, so a block cannot hide
one by suggesting nothing.

  $ . ./helpers.sh

  $ printf 'S .\nB .\nFLG -extension-universe beta\n' > .merlin

The query resolves a unit's interface through its compiled artifacts, so the
fixtures are compiled first. [dep] and [hof_intf] lead: other units depend on
them. The fixtures depend on nothing else, so the query only ever consults
artifacts built here plus the compiler's own stdlib.

[abstract_kind.ml] defines a record over a field whose kind is abstract, and
records with fields of unknown-representability kinds are gated on layouts
>= beta, so the whole directory is compiled (and re-checked) under that
extension universe.

  $ $OCAMLC -bin-annot-cms -w -a -extension-universe beta -c \
  >   dep.mli dep.ml hof_intf.ml \
  >   abstract_kind.mli abstract_kind.ml \
  >   alias_arity.mli alias_arity.ml \
  >   arrows.mli arrows.ml \
  >   bounds.mli bounds.ml \
  >   default_many.mli default_many.ml \
  >   default_portable.mli default_portable.ml \
  >   default_stateless.mli default_stateless.ml \
  >   edit_precision.mli edit_precision.ml \
  >   functor_pairing.mli functor_pairing.ml \
  >   hof_once.mli hof_once.ml \
  >   indirections.mli indirections.ml \
  >   kabbrev.mli kabbrev.ml \
  >   kinds.mli kinds.ml \
  >   module_modality.mli module_modality.ml \
  >   portability_lock.mli portability_lock.ml \
  >   rec_modules.mli rec_modules.ml \
  >   sealed_nested.mli sealed_nested.ml \
  >   shadowing.mli shadowing.ml \
  >   sibling_dep.mli sibling_dep.ml \
  >   value_aliases.mli value_aliases.ml \
  >   values.mli values.ml

Modes and modalities on plain functions.

  $ strengthen arrows.mli
  === arrows.ml (implementation) ===
  (* The payload types are our own: [elt] immutable, [cell] mutable, so any crossing in the
     outputs is declared in the mli, not incidental. *)
  type elt = { fixed : unit }
  type arrow = elt -> elt
  
  type flag =
    | On
    | Off
  
  type cell = { mutable contents : elt }
  
  (* A polymorphic mutable box: instantiating [_ box] with an inferred arrow keeps mode
     variables in the stored function's type, unlike a monomorphic field. *)
  type 'a box = { mutable v : 'a }
  
  let return_first (a : elt) (_ : elt) = a
  let apply f x = f x
  let id x = x
  
  let choose f x y =
    match f with
    | On -> x
    | Off -> y
  ;;
  
  let unstaged (_ : elt) (_ : elt) (_ : elt) = ()
  
  (* Returns a 1-ary closure. *)
  let staged_1ary (_ : elt) =
    let g (_ : elt) = () in
    g
  ;;
  
  (* Returns a 2-ary closure. Arity comes from the type's spelled spine, so the staging
     does not cap the closure's params. *)
  let staged_2ary (_ : elt) =
    let g (_ : elt) (_ : elt) = () in
    g
  ;;
  
  (* This is treated as a 3-ary function by the strengthener. *)
  let staged_via_fun_syntax = fun (_ : elt) -> fun (_ : elt) -> fun (_ : elt) -> ()
  
  (* Point-free alias of [unstaged]: arity comes from the type, not the binding's syntax,
     so the suggestions match [unstaged]'s. *)
  let point_free = unstaged
  
  let call_twice f x = f (f x)
  let store c x = c.contents <- x
  
  (* Legacy toplevel mutable capturing a function. *)
  let cache = { v = (fun (x : elt) -> x) }
  let retain f = cache.v <- f
  
  let use (f : arrow) x = f x
  
  let apply_once (type b) f x =
    let _ : b = f x in
    ()
  ;;
  
  let apply_twice (type b) f x =
    let _ : b = f x in
    let _ : b = f x in
    ()
  ;;
  
  let churn_local f =
    let _ = f (f { contents = { fixed = () } }) in
    ()
  ;;
  
  let churn_nonlocal f =
    let _ = f (f { contents = { fixed = () } }) in
    ()
  ;;
  
  (* Four levels of argument nesting: each level's arrows get their own edits. *)
  let nest3 f = f (fun g -> g (fun (x : elt) -> x))
  
  (* [f]'s result flows into [g], so their modes are linked through [r]; [relay_pinned]'s
     mli annotates [g]'s parameter. *)
  let relay f g =
    let r = { contents = { fixed = () } } in
    g (f r)
  ;;
  
  let relay_pinned f g =
    let r = { contents = { fixed = () } } in
    g (f r)
  ;;
  
  (* The same function [f] reaches depth 4 of both arguments (monomorphic by usage, so its
     arrow's mode variables are shared, not legacy-pinned); [weave_pinned]'s mli annotates
     depth 4 inside [u]'s type only. *)
  let weave u v =
    let f c = { contents = c.contents } in
    u (fun g -> g f);
    v (fun h -> h f)
  ;;
  
  let weave_pinned u v =
    let f c = { contents = c.contents } in
    u (fun g -> g f);
    v (fun h -> h f)
  ;;
  
  (* [app_pinned]'s mli marks [f]'s parameter [@ local], which frees [x], a different
     top-level argument, to be borrowed. *)
  let app f x = f x
  let app_pinned f x = f x
  === arrows.mli (strengthened) ===
  @@ stateless
  
  type elt : immutable_data
  type arrow = elt -> elt
  
  type flag =
    | On
    | Off
  
  type cell : mutable_data
  
  val return_first : elt -> elt @ local -> elt
  val apply : (elt -> elt) @ local once -> elt -> elt
  val id : 'a -> 'a
  val choose : flag -> 'a -> 'a -> 'a
  val unstaged : elt @ local -> elt @ local -> elt @ local -> unit
  val staged_1ary : elt @ local -> elt @ local -> unit
  val staged_2ary : elt @ local -> elt @ local -> elt @ local -> unit
  val staged_via_fun_syntax : elt @ local -> elt @ local -> elt @ local -> unit
  val point_free : elt @ local -> elt @ local -> elt @ local -> unit
  val call_twice : (elt -> elt) @ local -> elt -> elt
  val store : cell @ local write -> elt -> unit
  val retain : (elt @ unique -> elt @ local) -> unit @@ stateful
  val use : arrow @ local once -> elt -> elt
  val apply_once : ('a -> 'b @ immutable local once) @ local once -> 'a -> unit
  val apply_twice : ('a -> 'b @ immutable local once) @ local -> 'a -> unit
  val churn_local : (cell @ local -> cell @ local) @ local -> unit
  val churn_nonlocal : (cell -> cell) @ local -> unit
  val nest3 : ((((elt -> elt) @ stateless -> elt) @ local once -> elt) @ stateless -> elt) @ local once -> elt
  val relay : (cell @ unique -> cell) @ local once -> (cell -> unit) @ local once -> unit
  val relay_pinned : (cell @ unique -> cell @ contended) @ local once -> (cell @ contended local -> unit) @ local once -> unit
  
  val weave
    :  ((((cell @ local read -> cell @ unique) @ stateless -> cell) @ local once -> cell) @ stateless -> unit @ local) @ local once
    -> ((((cell @ local read -> cell @ unique) @ stateless -> cell) @ local once -> cell) @ stateless -> unit) @ local once
    -> unit
  
  val weave_pinned
    :  ((((cell @ local read -> cell @ unique) @ stateless -> cell) @ local once -> cell) @ stateless -> unit @ local) @ local once
    -> ((((cell @ local read -> cell @ unique) @ stateless -> cell) @ local once -> cell) @ stateless -> unit) @ local once
    -> unit
  
  val app : ('a -> 'b) @ local once -> 'a -> 'b
  val app_pinned : ('a @ local -> 'b) @ local once -> 'a @ local -> 'b

# Arity is taken from the interface's spelled arrow spine, which is what keeps the
# block above conformant; the [alias_arity] block below pins that choice. The
# underlying hazard remains: the unit's inclusion check constrains the
# implementation's mode variables in interface order, so when two exported
# declarations share one type expression (an alias, a re-export block) whichever is
# checked first pins the shared variables. Strengthening the stronger of the two is
# then unsound, and item order decides which one breaks. Two candidate fixes if it
# resurfaces: generalize the include-sharing rule to suppress (or consistently
# propagate) suggestions for values whose solver variables another exported item
# shares, or re-run the unit's inclusion check against the strengthened signature
# and drop what breaks. The first is the analysis-side fix. [value_aliases] pins the
# behaviour currently relied on.

A declaration shared via [include Hof_intf.S] gets no suggestions:
strengthening it against one unit could break another that includes it.

# CR-someday ggray: Suggest edits to the declaring module type, regardless of whether it's
# an .mli or in an .ml file.

  $ strengthen hof_once.mli
  === hof_once.ml (implementation) ===
  let app (type b) f x =
    let _ : b = f x in
    ()
  ;;
  === hof_once.mli (strengthened) ===
  include Hof_intf.S

Values over abstract records: kind suggestions, borrows exactly where the
argument does not escape, and modes on [make_fn]'s hidden-function return.
[Nested.double] pins that nested-signature values strengthen like top-level
ones.

  $ strengthen values.mli
  === values.ml (implementation) ===
  type fn = int -> int
  
  let identity (x : int) = x
  let make_fn () = identity
  
  type t =
    { id : int
    ; label : string
    }
  
  let create id label = { id; label }
  let id t = t.id
  let label t = t.label
  let relabel t label = { t with label }
  let default = { id = 0; label = "default" }
  
  type queue = { mutable items : float list }
  
  type metrics =
    { mutable count : int
    ; mutable total : float
    ; samples : queue
    }
  
  let create_metrics () = { count = 0; total = 0.; samples = { items = [] } }
  
  let record m x =
    m.count <- m.count + 1;
    m.total <- m.total +. x;
    m.samples.items <- x :: m.samples.items
  ;;
  
  let mean m = if m.count = 0 then 0. else m.total /. float_of_int m.count
  
  let rec rev_map f acc = function
    | [] -> acc
    | x :: rest -> rev_map f (f x :: acc) rest
  ;;
  
  let summarize m ~f = rev_map f [] m.samples.items
  
  (* Values inside a nested signature strengthen like top-level ones. *)
  module Nested = struct
    let double x = x * 2
  end
  === values.mli (strengthened) ===
  @@ stateless
  
  type fn : value non_float mod aliased immutable
  
  val make_fn : unit -> fn @ stateless
  
  type t : immutable_data
  
  val create : int -> string -> t
  val id : t @ local -> int
  val label : t -> string
  val relabel : t @ local -> string -> t
  val default : t
  
  type metrics : mutable_data
  
  val create_metrics : unit -> metrics @ unique
  val record : metrics @ local -> float -> unit @@ portable stateful
  val mean : metrics @ local read -> float @@ portable stateful
  val summarize : metrics @ local read -> f:(float -> string) @ local -> string list
  
  module Nested : sig
    val double : int -> int
  end @@ portable stateful

[create] reads a legacy toplevel through the portability lock. The portable
majority hoists to a floating clause and [create] is exempted with
[@@ nonportable]; [iter]'s stronger per-item [@@ stateless] stays written.

  $ strengthen portability_lock.mli
  === portability_lock.ml (implementation) ===
  type 'a u = (string * 'a) list
  type 'a t = { mutable entries : 'a u }
  
  (* Legacy toplevel state: [create] reads it through the portability lock, which keeps
     [create] unportable while the other values stay portable. *)
  let total_registries = ref 0
  
  let create () =
    incr total_registries;
    { entries = [] }
  ;;
  
  let rec find_entry name = function
    | [] -> None
    | (key, data) :: rest ->
      if String.equal key name then Some data else find_entry name rest
  ;;
  
  let register t ~name value =
    match find_entry name t.entries with
    | Some _ -> Error "duplicate registration"
    | None ->
      t.entries <- (name, value) :: t.entries;
      Ok ()
  ;;
  
  let find t name = find_entry name t.entries
  
  let rec iter_data f = function
    | [] -> ()
    | (_, data) :: rest ->
      f data;
      iter_data f rest
  ;;
  
  let iter t ~f = iter_data f t.entries
  === portability_lock.mli (strengthened) ===
  @@ portable
  
  type 'a t : mutable_data with 'a @@ forkable unyielding many
  type 'a u = (string * 'a) list
  
  val create : unit -> 'a t @ unique @@ nonportable
  val register : 'a t @ local -> name:string -> 'a -> (unit, string) result @ unique
  val find : 'a t @ local -> string @ local -> 'a option
  val iter : 'a t @ local -> f:('a -> unit @ local) @ local -> unit @@ stateless

Closed kind suggestions for abstract types.

  $ strengthen kinds.mli
  === kinds.ml (implementation) ===
  type id = int
  
  type point =
    { x : float
    ; y : float
    }
  
  type counter = { mutable count : int }
  
  (* This type has an explicit kind annotation in the signature, but it is weaker than it
     can be. *)
  type can_be_strengthened = Foo [@@warning "-37"]
  === kinds.mli (strengthened) ===
  type id : immediate
  type point : immutable_data
  type counter : mutable_data
  type can_be_strengthened : mutable_data

Kind suggestions resolve through recursive modules.

  $ strengthen rec_modules.mli
  === rec_modules.ml (implementation) ===
  module rec Tree : sig
    type t =
      | Leaf
      | Node of Forest.t
  end = struct
    type t =
      | Leaf
      | Node of Forest.t
  end
  
  and Forest : sig
    type t = { trees : Tree.t list }
  end = struct
    type t = { trees : Tree.t list }
  end
  
  type tree = Tree.t
  type forest = Forest.t
  === rec_modules.mli (strengthened) ===
  type tree : immutable_data
  type forest : immutable_data

A shadowed value pairs with its last definition; only it earns
[@@ stateless].

  $ strengthen shadowing.mli
  === shadowing.ml (implementation) ===
  type state = { mutable used : unit }
  
  let state = { used = () }
  
  (* Shadowed: the first [v] writes a legacy toplevel, so only pairing with the last
     definition can suggest [@@ stateless]. *)
  let v x =
    state.used <- ();
    x
  ;;
  
  let _ = v
  let v x = x
  === shadowing.mli (strengthened) ===
  @@ stateless
  
  val v : 'a -> 'a

Bounds only ever mention the declaration's own type parameters: bounds
over other types — hidden, exposed, aliased, annotated ([both]), or behind
a functor application — are discharged, while parameter bounds survive
with the owning field's modality as their mask.

  $ strengthen bounds.mli
  === bounds.ml (implementation) ===
  (* Every way a bound can involve a type other than the declaration's own parameters:
     hidden outright, exposed, exposed through an alias, mixed with a parameter, annotated
     on both sides of the veil, and reached through a functor application.
     [box]/[pair]/[masked]/[residue] are the bounds that do survive, on the parameters
     themselves. *)
  module Hidden : sig
    type u
    type 'a tracked : immutable_data with 'a
  end = struct
    type u = int
    type 'a tracked : immutable_data with 'a
  end
  
  module Exposed : sig
    type u
  end = struct
    type u = int
  end
  
  (* The functor's result is sealed so the applied path stays abstract. *)
  module Make_table (Key : sig
      type u
    end) : sig
    type 'a t : immutable_data with Key.u with 'a
  end = struct
    type 'a t = { entries : (Key.u * 'a) list }
  end
  
  type 'a opaque_arg
  type 'a tracked : immutable_data with 'a
  
  (* CR-someday ggray: strengthen ml-side module ascriptions too; [Hidden]'s cap limits the
     suggestions for these declarations. *)
  type public_u = Hidden.u
  type from_hidden = { a : Hidden.u }
  type from_exposed = { b : Exposed.u }
  type from_alias = { c : public_u }
  
  type partial =
    { hidden : Hidden.u
    ; pending : float opaque_arg
    }
  
  type 'a both =
    { pub : 'a tracked
    ; priv : 'a Hidden.tracked
    }
  
  type 'a from_functor = { table : 'a Make_table(Hidden).t }
  
  module Int_u = struct
    type u = int
  end
  
  type 'a from_functor_immutable = { table : 'a Make_table(Int_u).t }
  type 'a box = { value : 'a }
  
  type ('a, 'b) pair =
    { left : 'a
    ; right : 'b
    }
  
  type 'a masked = { value : 'a @@ portable }
  type 'a residue = { payload : 'a @@ aliased global }
  type 'a in_mutable = { mutable slot : 'a }
  === bounds.mli (strengthened) ===
  module Exposed : sig
    type u
  end
  
  type 'a opaque_arg
  type 'a tracked : immutable_data with 'a
  type public_u
  type from_hidden : value non_float
  type from_exposed : value non_float
  type from_alias : value non_float
  type partial : value non_float
  type 'a both : immutable_data with 'a
  type 'a from_functor : value non_float
  type 'a from_functor_immutable : immutable_data with 'a
  type 'a box : immutable_data with 'a
  type ('a, 'b) pair : immutable_data with 'a with 'b
  type 'a masked : immutable_data with 'a @@ portable
  type 'a residue : immutable_data with 'a @@ forkable unyielding
  type 'a in_mutable : mutable_data with 'a @@ forkable unyielding many

A bound on a sibling unit's type is judged by that unit's mli alone:
unannotated [Dep.weak] leaves only the layout, annotated [Dep.strong]
discharges fully.

# We could theoretically give [weak] the kind `immutable_data with Dep.weak`. In most
# cases though, this would be pretty useless, so it's unclear to me whether we should.

  $ strengthen sibling_dep.mli
  === sibling_dep.ml (implementation) ===
  type weak =
    { dep : Dep.weak
    ; tag : int
    }
  
  type strong =
    { dep : Dep.strong
    ; tag : int
    }
  === sibling_dep.mli (strengthened) ===
  type weak : value non_float
  type strong : immutable_data

A bound over [M.t], whose kind is the abstract [kind_ k], is discharged
like any other non-parameter bound.

  $ strengthen abstract_kind.mli
  === abstract_kind.ml (implementation) ===
  module M : sig
    kind_ k
  
    type t : k
  end = struct
    kind_ k = value
  
    type t = int
  end
  
  type u = { field : M.t }
  
  type v : M.k mod portable
  === abstract_kind.mli (strengthened) ===
  module M : sig
    kind_ k
  
    type t : k
  end
  
  type u : value non_float
  type v : M.k

Edits must land exactly at their anchors: trailing comments and
attributes, several declarations on one line, [constraint] clauses, and
[and]-groups. The tail pins existing annotations: an annotated kind is
skipped, and an existing modality is REPLACED with the merged set but
never upgraded on its own axis ([id_writing]).

  $ strengthen edit_precision.mli
  === edit_precision.ml (implementation) ===
  type a = int
  type b = int
  type c = int
  type d = int
  type 'a e = int constraint 'a = int * int
  
  let double x = x * 2
  
  type f = int
  
  let quad x = x * 4
  
  type g = { left : int }
  and h = { right : g option }
  
  type t = int
  
  let scale x = x * 3
  let id_portable x = x
  let id_writing x = x
  === edit_precision.mli (strengthened) ===
  @@ portable
  
  type a : immediate (* trailing comment *)
  
  type b : immediate
  (** floating documentation after the declaration *)
  
  type c : immediate   type d : immediate
  
  type 'a e : immediate constraint 'a = int * int
  
  val double : int -> int (* trailing comment *)
  
  type f : immediate [@@deprecated "gone"]
  
  val quad : int -> int [@@deprecated "gone"]
  
  type g : immutable_data
  and h : immutable_data
  
  type t : value
  
  val scale : int -> int @@ many
  val id_portable : int -> int @@ portable stateless
  val id_writing : int -> int @@ writing

A signature-level [@@ stateless] default already claims everything [id]
could earn, so nothing is suggested.

  $ strengthen default_stateless.mli
  === default_stateless.ml (implementation) ===
  let id x = x
  === default_stateless.mli (strengthened) ===
  @@ stateless
  
  val id : int -> int

A module-level modality distributes to the members: [Foo]'s [@@ stateless] already claims
[f], so no suggestion may re-state it, even though [Foo] itself carries state.

  $ strengthen module_modality.mli
  === module_modality.ml (implementation) ===
  type t = { fixed : unit }
  type cell = { mutable contents : t }
  
  module Foo @ stateful = struct
    let f (x : t) = x
  
    (* Unexported state keeps [Foo] itself genuinely stateful. *)
    let cache = { contents = { fixed = () } }
    let _set x = cache.contents <- x
  end
  === module_modality.mli (strengthened) ===
  type t : immutable_data
  
  module (Foo @@ stateless) : sig
    val f : t -> t
  end

Functor-heavy shapes (toward the core/map.ml families). Values in functor
results earn arrow modes as well as modalities: the pairing walk equates
the functor parameters ([Key]) the way [Includemod] does, so [consume]'s
unused argument claims every non-crossing axis ([Key.t] is fully abstract)
and its return claims [@ static] -- with or without an application body.

  $ strengthen functor_pairing.mli
  === functor_pairing.ml (implementation) ===
  module Make (Key : sig
      type t
    end) =
  struct
    let find (k : Key.t) = k
    let consume (_ : Key.t) = ()
  end
  
  (* The functor's body is itself an application, the way core/map.ml builds [Make] from
     [Make_using_comparator]. *)
  module Make_applied (Key : sig
      type t
    end) =
    Make (Key)
  === functor_pairing.mli (strengthened) ===
  (* Values in a functor's result strengthen fully: modalities and arrow modes alike,
     whether the functor body is a struct or an application. *)
  
  module Make (Key : sig
      type t
    end) : sig
    val find : Key.t -> Key.t @@ stateless
    val consume : Key.t @ immutable local once -> unit @@ stateless
  end
  
  module Make_applied (Key : sig
      type t
    end) : sig
    val find : Key.t -> Key.t @@ stateless
    val consume : Key.t @ immutable local once -> unit @@ stateless
  end

Nested modules: an unsealed nested module strengthens fully; an inline
ascription in the .ml is a seal, and the interface cannot claim more than
the seal grants, so the sealed [Tree] correctly gets nothing.

[Shadowed.Tree.t] is immutable data, so atoms on axes it crosses are
implied away rather than printed: [id]'s row claims only [@@ stateless].
Crossing sees the nested record declaration because the walk extends the
environment with the implementation's items at every signature level, the
way [Includemod.signatures] does.

  $ strengthen sealed_nested.mli
  === sealed_nested.ml (implementation) ===
  (* Shapes from core/map.ml: a nested module sealed by an inline ascription, and a
     same-named module shadowed at two levels. *)
  
  module Shadowed = struct
    module Tree = struct
      type t = { leaf : unit }
  
      let id (x : t) = x
    end
  end
  
  module Tree : sig
    type t
  
    val id : t -> t
  end = struct
    type t = { leaf : unit }
  
    let id (x : t) = x
  end
  === sealed_nested.mli (strengthened) ===
  module Shadowed : sig
    module Tree : sig
      @@ stateless
  
      type t : immutable_data
  
      val id : t -> t
    end
  end
  
  module Tree : sig
    type t
  
    val id : t -> t
  end

Module-type indirections, expanded through the environment: a seal by a
named signature caps claims at the seal ([Sealed.id] earns [SMALL]'s
[@@ stateless] and nothing beyond it); an alias and an include keep the
implementation's unsealed claims, masked by [t]'s crossing.

  $ strengthen indirections.mli
  === indirections.ml (implementation) ===
  (* Module-type indirections from core/map.ml: a seal by a named signature, a module alias,
     and an include; the pairing walk expands each through the environment. *)
  
  module type SMALL = sig
    type t : immutable_data
  
    val id : t -> t @@ stateless
  end
  
  module Base0 = struct
    type t = { leaf : unit }
  
    let id (x : t) = x
  end
  
  (* Sealed by name: claims are capped at [SMALL], though the interface may still be weaker
     than the seal. *)
  module Sealed : SMALL = Base0
  
  (* An alias keeps [Base0]'s unsealed signature. *)
  module Alias = Base0
  
  (* The include-then-extend shape of core/map.ml's [Tree0]. *)
  module Combined = struct
    include Base0
  
    let use (_ : t) = ()
  end
  === indirections.mli (strengthened) ===
  module Sealed : sig
    @@ stateless
  
    type t : immutable_data
  
    val id : t -> t
  end
  
  module Alias : sig
    @@ stateless
  
    type t : immutable_data
  
    val id : t -> t
  end
  
  module Combined : sig
    @@ stateless
  
    type t : immutable_data
  
    val id : t -> t
    val use : t @ local -> unit
  end

Arity is the interface's spelled arrow spine, not the implementation's:
[spelled] and [aliased] share one implementation, but [aliased]'s spine
stops at [step]. Its argument therefore cannot borrow -- it escapes into
the returned closure -- and the alias's right-hand side is never annotated.

  $ strengthen alias_arity.mli
  === alias_arity.ml (implementation) ===
  (* Arity is the declared type's spelled arrow spine: [spelled] and [aliased] have
     identical implementations, but [aliased]'s row hides the second arrow behind [step],
     capping the spine at one. *)
  
  type t = { fixed : unit }
  type step = t -> unit
  
  let spelled (_ : t) (_ : t) = ()
  let aliased (_ : t) (_ : t) = ()
  === alias_arity.mli (strengthened) ===
  @@ stateless
  
  type t : immutable_data
  type step = t -> unit
  
  val spelled : t @ local -> t @ local -> unit
  val aliased : t -> step @ stateless

Aliases ([let ( <*> ) = apply], hashtbl.ml's re-export blocks) share one
type expression, and its live mode variables, with the original binding.
Every sharer must strengthen exactly like the original, and none may be
abandoned.

  $ strengthen value_aliases.mli
  === value_aliases.ml (implementation) ===
  (* Aliases share one type expression -- and its live mode variables -- with the original
     binding: applicative.ml's [let ( <*> ) = apply] and hashtbl.ml's re-export blocks
     ([let mem = mem]). *)
  
  type t = { fixed : unit }
  
  let apply (f : (t -> t) @ local) x = f x
  let ( <*> ) = apply
  
  (* The re-export block shape of hashtbl.ml. *)
  module Export = struct
    let apply = apply
  end
  === value_aliases.mli (strengthened) ===
  @@ stateless
  
  type t : immutable_data
  
  val apply : (t -> t) @ local once -> t -> t
  val ( <*> ) : (t -> t) @ local once -> t -> t
  
  module Export : sig
    val apply : (t -> t) @ local once -> t -> t
  end

A weaker signature-level default ([@@ portable]) does not cap what [id]
can earn on other axes.

  $ strengthen default_portable.mli
  === default_portable.ml (implementation) ===
  let id x = x
  === default_portable.mli (strengthened) ===
  @@ portable stateless
  
  val id : int -> int

A signature-level default claims an axis for every item, so a suggestion
must leave that axis unwritten: re-stating [many] is redundant, and writing
the weaker [once] would REVOKE the default for this item. With hoisting,
[scale]'s portable claim extends the existing clause in place and [many] is
untouched.

  $ strengthen default_many.mli
  === default_many.ml (implementation) ===
  let scale x = x * 3
  === default_many.mli (strengthened) ===
  @@ many portable
  
  val scale : int -> int

A kind abbreviation in the interface pairs with the implementation's
([Subst.add_jkind]), so [use] and [use2] strengthen identically.

  $ strengthen kabbrev.mli
  === kabbrev.ml (implementation) ===
  kind_ k = immutable_data
  
  let use (type a : immutable_data) (_ : a) = ()
  let use2 (type a : immutable_data) (_ : a) = ()
  === kabbrev.mli (strengthened) ===
  kind_ k = immutable_data
  
  val use : ('a : k). 'a @ local -> unit @@ stateless
  val use2 : ('a : immutable_data). 'a @ local -> unit @@ stateless
