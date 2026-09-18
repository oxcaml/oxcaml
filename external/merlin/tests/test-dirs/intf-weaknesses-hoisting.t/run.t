Modality hoisting: when more than half of a signature's value items support
the same modality, the suggestion should move to a floating [@@ ...] clause on
the signature, and the minority items should be exempted with explicit weak
atoms ([nonportable], [stateful], ...). The rewrite ships as ONE code action
per interface — applying the clause without the exemptions would claim
capabilities the minority does not have, so it is all items or nothing.

Each strengthened interface below was verified by hand against the batch
compiler, and the conformance check re-checks every block mechanically. The
rows are pinned by hand because a hoist can weaken an item invisibly: an
exemption that forgets an implied axis still typechecks, so nothing but these
expectations catches it.

  $ . ./helpers.sh

  $ printf 'S .\nB .\n' > .merlin

  $ $OCAMLC -bin-annot-cms -w -a -c \
  >   hof_intf.ml \
  >   hoist_extend.mli hoist_extend.ml \
  >   hoist_half.mli hoist_half.ml \
  >   hoist_include.mli hoist_include.ml \
  >   hoist_majority.mli hoist_majority.ml \
  >   hoist_nested.mli hoist_nested.ml \
  >   hoist_stateless.mli hoist_stateless.ml \
  >   hoist_written.mli hoist_written.ml

More than half (three of four) portable: the claim hoists, the dissenter is
exempted.

  $ strengthen hoist_majority.mli
  === hoist_majority.ml (implementation) ===
  (* Three of four values are provably portable; [bump] reads the toplevel ref through the
     portability lock. A portable majority should hoist the claim to a floating clause and
     exempt [bump] explicitly. *)
  let counter = ref 0
  let double x = x * 2
  let triple x = x * 3
  let quad x = x * 4
  let bump x = x + !counter
  === hoist_majority.mli (strengthened) ===
  @@ portable
  
  val double : int -> int
  val triple : int -> int
  val quad : int -> int
  val bump : int -> int @@ nonportable

Exactly half is not a majority: suggestions stay per-item.

  $ strengthen hoist_half.mli
  === hoist_half.ml (implementation) ===
  (* Exactly half the values are portable: two of four. A hoist must be strictly better than
     half, so suggestions stay per-item. *)
  let counter = ref 0
  let double x = x * 2
  let triple x = x * 3
  let bump x = x + !counter
  let jump x = x - !counter
  === hoist_half.mli (strengthened) ===
  val double : int -> int @@ portable
  val triple : int -> int @@ portable
  val bump : int -> int
  val jump : int -> int

An item that already writes the hoisted atom counts toward the majority, and
its written atom survives the hoist even though the clause makes it redundant:
a strengthened signature never deletes user-written modalities.

  $ strengthen hoist_written.mli
  === hoist_written.ml (implementation) ===
  (* [ok] already writes [@@ portable]; with two more provably portable values that makes
     three of four, so the claim hoists. The written atom must survive the hoist: a
     strengthened signature never deletes user-written modalities. *)
  let counter = ref 0
  let ok x = x * 1
  let double x = x * 2
  let triple x = x * 3
  let bump x = x + !counter
  === hoist_written.mli (strengthened) ===
  @@ portable
  
  val ok : int -> int @@ portable
  val double : int -> int
  val triple : int -> int
  val bump : int -> int @@ nonportable

An existing floating clause on another axis is extended in place — one clause,
and the axis it already writes is untouched.

  $ strengthen hoist_extend.mli
  === hoist_extend.ml (implementation) ===
  (* The signature already floats [@@ many]; the portable majority extends that clause
     rather than adding a second one, and never touches the axis it already writes. *)
  let counter = ref 0
  let double x = x * 2
  let triple x = x * 3
  let quad x = x * 4
  let bump x = x + !counter
  === hoist_extend.mli (strengthened) ===
  @@ many portable
  
  val double : int -> int
  val triple : int -> int
  val quad : int -> int
  val bump : int -> int @@ nonportable

An outer floating clause composes onto module declarations, and composition
only strengthens: a nested member cannot weaken it from inside. The exemption
lands on [M]'s declaration, and [M]'s portable member then re-claims
individually.

  $ strengthen hoist_nested.mli
  === hoist_nested.ml (implementation) ===
  (* An outer floating clause composes onto module declarations, and composition only
     strengthens: a nested member cannot weaken it from inside (a written [nonportable] is
     absorbed), so the exemption lands on [M]'s declaration, after which [M]'s portable
     member re-claims individually. *)
  let counter = ref 0
  let double x = x * 2
  let triple x = x * 3
  
  module M = struct
    let bump x = x + !counter
    let ok x = x * 4
  end
  === hoist_nested.mli (strengthened) ===
  @@ portable
  
  val double : int -> int
  val triple : int -> int
  
  module M : sig
    val bump : int -> int
    val ok : int -> int @@ portable
  end @@ nonportable

An include re-exports a shared declaration: it cannot be individually
exempted, and a floating clause would silently apply to it. Any include
blocks hoisting at its level; suggestions stay per-item despite the majority.

  $ strengthen hoist_include.mli
  === hoist_include.ml (implementation) ===
  (* [include Hof_intf.S] re-exports a shared declaration: it cannot be individually
     exempted, and a floating clause would silently apply to it. Any include therefore
     blocks hoisting at its level; suggestions stay per-item despite the majority. *)
  let app f x =
    let _ = f x in
    ()
  ;;
  
  let double x = x * 2
  let triple x = x * 3
  let quad x = x * 4
  === hoist_include.mli (strengthened) ===
  include Hof_intf.S
  
  val double : int -> int @@ portable
  val triple : int -> int @@ portable
  val quad : int -> int @@ portable

A stateless majority hoists [@@ stateless] alone, since it delivers
[portable] by implication. Exemptions mind implications too: bare
[@@ stateful] is right for [bump] (nonportable anyway), while [plus1] must
re-claim [@@ portable] or the implied [nonportable] would silently weaken
it.

  $ strengthen hoist_stateless.mli
  === hoist_stateless.ml (implementation) ===
  (* Identity-shaped functions close over nothing and earn [stateless], which delivers
     [portable] to clients by implication, so the hoisted clause is [@@ stateless] alone.
     Exemptions must mind implications too: written [stateful] implies [nonportable], fine
     for [bump] (a ref-reader, nonportable anyway) but a silent weakening for [plus1]
     (portable; it closes over the stdlib [+], stateful only until the stdlib is annotated),
     whose exemption must therefore re-claim [portable]. *)
  let counter = ref 0
  let id x = x
  let same x = x
  let pick b x y = if b then x else y
  let plus1 x = x + 1
  let bump x = x + !counter
  === hoist_stateless.mli (strengthened) ===
  @@ stateless
  
  val id : 'a -> 'a
  val same : 'a -> 'a
  val pick : bool -> 'a -> 'a -> 'a
  val plus1 : int -> int @@ portable stateful
  val bump : int -> int @@ stateful
