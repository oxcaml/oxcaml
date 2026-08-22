open Axis_lattice

(* Direct exhaustive test for [Axis_lattice.meet_right_adjoint]
   (typing/axis_lattice.ml), the soundness-critical right adjoint of "meet with
   a mask" that [apply_modality_r] relies on for middle-modality mode crossing.

   [meet_right_adjoint ~expected ~mask] is meant to be the largest lattice
   element whose meet with [mask] stays below [expected]. Concretely it must
   satisfy the Galois connection (adjunction)

   meet c mask <= expected <=> c <= meet_right_adjoint ~expected ~mask

   for every lattice element [c]. This test verifies that connection directly:

   - exhaustively per axis (the per-axis domains are small: 2, 3 or 4 levels),
   with [c] ranging over the entire product lattice; - against a brute-force
   reference (the join of the whole down-set { c | meet c mask <= expected }) on
   sampled whole-lattice masks/expecteds; - plus the review's derived
   properties: monotonicity in [expected], antitonicity in [mask], and the
   co_sub/join mode-crossing acceptance rule accept(src -> tgt) <=> src <= tgt
   join bound. *)

type sample =
  { areality : Mode.Regionality.Const.t;
    linearity : Mode.Linearity.Const.t;
    uniqueness : Mode.Uniqueness.Const.t;
    portability : Mode.Portability.Const.t;
    contention : Mode.Contention.Const.t;
    forkable : Mode.Forkable.Const.t;
    yielding : Mode.Yielding.Const.t;
    statefulness : Mode.Statefulness.Const.t;
    visibility : Mode.Visibility.Const.t;
    staticity : Mode.Staticity.const;
    externality : Jkind_axis.Externality.t
  }

let sample_of_lattice x =
  { areality = areality x;
    linearity = linearity x;
    uniqueness = uniqueness x;
    portability = portability x;
    contention = contention x;
    forkable = forkable x;
    yielding = yielding x;
    statefulness = statefulness x;
    visibility = visibility x;
    staticity = staticity x;
    externality = externality x
  }

let lattice_of_sample sample =
  create ~areality:sample.areality ~linearity:sample.linearity
    ~uniqueness:sample.uniqueness ~portability:sample.portability
    ~contention:sample.contention ~forkable:sample.forkable
    ~yielding:sample.yielding ~statefulness:sample.statefulness
    ~visibility:sample.visibility ~staticity:sample.staticity
    ~externality:sample.externality

let bot_sample = sample_of_lattice bot

let fail fmt = Format.kasprintf failwith fmt

(* All single-axis lattice elements for one axis: [update] applied to each of
   [values] over the [bot] sample, leaving every other axis at [bot]. *)
let elems_of update values =
  List.map (fun v -> lattice_of_sample (update bot_sample v)) values

let all_axes : (string * t list) list =
  [ ( "areality",
      elems_of
        (fun s areality -> { s with areality })
        Mode.Regionality.Const.[Global; Regional; Local] );
    ( "uniqueness",
      elems_of
        (fun s uniqueness -> { s with uniqueness })
        Mode.Uniqueness.Const.[Aliased; Unique] );
    ( "linearity",
      elems_of
        (fun s linearity -> { s with linearity })
        Mode.Linearity.Const.[Many; Once] );
    ( "contention",
      elems_of
        (fun s contention -> { s with contention })
        Mode.Contention.Const.[Contended; Corrupted; Shared; Uncontended] );
    ( "portability",
      elems_of
        (fun s portability -> { s with portability })
        Mode.Portability.Const.[Portable; Shareable; Corruptible; Nonportable] );
    ( "forkable",
      elems_of
        (fun s forkable -> { s with forkable })
        Mode.Forkable.Const.[Forkable; Unforkable] );
    ( "yielding",
      elems_of
        (fun s yielding -> { s with yielding })
        Mode.Yielding.Const.[Unyielding; Yielding] );
    ( "statefulness",
      elems_of
        (fun s statefulness -> { s with statefulness })
        Mode.Statefulness.Const.[Stateless; Writing; Reading; Stateful] );
    ( "visibility",
      elems_of
        (fun s visibility -> { s with visibility })
        Mode.Visibility.Const.[Immutable; Read; Write; Read_write] );
    ( "staticity",
      elems_of
        (fun s staticity -> { s with staticity })
        Mode.Staticity.[Dynamic; Static] );
    ( "externality",
      elems_of
        (fun s externality -> { s with externality })
        Jkind_axis.Externality.[External; External64; Internal] ) ]

(* The whole product lattice: one level per axis, all combinations. Single-axis
   elements keep the other axes at [bot], and disjoint bit-slots make [join] the
   "combine" operation, so folding [join] over one choice per axis enumerates
   every element exactly once. *)
let all_elements =
  List.fold_left
    (fun acc (_name, elems) ->
      List.concat_map (fun e -> List.map (fun ae -> join e ae) elems) acc)
    [bot] all_axes

(* Core check for one (mask, expected): the function must agree with its
   brute-force reference and satisfy the Galois biconditional for every [c]. *)
let check_pair ~where ~expected ~mask =
  let radj = meet_right_adjoint ~expected ~mask in
  let reference =
    List.fold_left
      (fun acc c ->
        let in_downset = leq (meet c mask) expected in
        let below_radj = leq c radj in
        if in_downset <> below_radj
        then
          fail
            "%s: adjunction biconditional failed: mask=%s expected=%s c=%s \
             (meet c mask <= expected = %b, c <= radj = %b)"
            where (to_string mask) (to_string expected) (to_string c) in_downset
            below_radj;
        if in_downset then join acc c else acc)
      bot all_elements
  in
  if not (equal radj reference)
  then
    fail
      "%s: meet_right_adjoint <> reference: mask=%s expected=%s radj=%s ref=%s"
      where (to_string mask) (to_string expected) (to_string radj)
      (to_string reference);
  (* The reference is the join of the down-set; assert it is itself in the
     down-set. This confirms the down-set is join-closed, i.e. the join really
     is the maximum (= right adjoint) rather than merely an upper bound. *)
  if not (leq (meet radj mask) expected)
  then
    fail "%s: right adjoint not in down-set: mask=%s expected=%s radj=%s" where
      (to_string mask) (to_string expected) (to_string radj)

(* Extremal masks/expecteds pin down the corners of the adjunction. *)
let check_extremes () =
  List.iter
    (fun e ->
      (* mask=bot: meet c bot = bot <= expected always, so the adjoint is
         top. *)
      if not (equal (meet_right_adjoint ~expected:e ~mask:bot) top)
      then fail "extremes: mask=bot should give top (expected=%s)" (to_string e);
      (* mask=top: meet c top = c, so the adjoint is exactly [expected]. *)
      if not (equal (meet_right_adjoint ~expected:e ~mask:top) e)
      then
        fail "extremes: mask=top should give expected (expected=%s)"
          (to_string e);
      (* expected=top: every c is in the down-set, so the adjoint is top. *)
      if not (equal (meet_right_adjoint ~expected:top ~mask:e) top)
      then fail "extremes: expected=top should give top (mask=%s)" (to_string e))
    all_elements

(* Exhaustive over single-axis masks/expecteds (all other axes at bot), with [c]
   ranging over the entire product lattice inside [check_pair]. This covers
   every axis's whole domain including the diamond sibling middles. *)
let check_per_axis () =
  List.iter
    (fun (name, elems) ->
      List.iter
        (fun mask ->
          List.iter
            (fun expected -> check_pair ~where:name ~expected ~mask)
            elems)
        elems)
    all_axes

(* Deterministic stride sample of the whole lattice. *)
let sample stride = List.filteri (fun i _ -> i mod stride = 0) all_elements

(* Whole-lattice masks/expecteds (multi-axis), to catch any cross-axis slot
   contamination the per-axis pass cannot see. *)
let check_sampled_pairs () =
  let s = sample 6143 in
  List.iter
    (fun mask ->
      List.iter (fun expected -> check_pair ~where:"sampled" ~expected ~mask) s)
    s

(* Monotone in [expected], antitone in [mask]. Verified per axis (small, clear
   reporting) and on a whole-lattice sample. *)
let check_monotonicity elems ~where =
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          if leq a b
          then begin
            (* expected grows => adjoint grows *)
            List.iter
              (fun mask ->
                let ra = meet_right_adjoint ~expected:a ~mask in
                let rb = meet_right_adjoint ~expected:b ~mask in
                if not (leq ra rb)
                then
                  fail
                    "%s: not monotone in expected: a=%s b=%s mask=%s ra=%s \
                     rb=%s"
                    where (to_string a) (to_string b) (to_string mask)
                    (to_string ra) (to_string rb))
              elems;
            (* mask grows => adjoint shrinks *)
            List.iter
              (fun expected ->
                let ra = meet_right_adjoint ~expected ~mask:a in
                let rb = meet_right_adjoint ~expected ~mask:b in
                if not (leq rb ra)
                then
                  fail
                    "%s: not antitone in mask: a=%s b=%s expected=%s ra=%s \
                     rb=%s"
                    where (to_string a) (to_string b) (to_string expected)
                    (to_string ra) (to_string rb))
              elems
          end)
        elems)
    elems

(* Mode-crossing acceptance rule at the lattice level: a value [src] crosses to
   [tgt] under crossing bound [bound] iff [src <= tgt join bound]. This is the
   co_sub/join Galois connection (co_sub is the left adjoint of join, used on
   the crossing side), the sibling of [meet_right_adjoint]: co_sub src bound <=
   tgt <=> src <= join bound tgt. *)
let check_acceptance_rule () =
  let s = sample 6143 in
  List.iter
    (fun src ->
      List.iter
        (fun tgt ->
          List.iter
            (fun bound ->
              let accept = leq src (join tgt bound) in
              let via_co_sub = leq (co_sub src bound) tgt in
              if accept <> via_co_sub
              then
                fail
                  "acceptance rule mismatch: src=%s tgt=%s bound=%s (src <= \
                   tgt join bound = %b, co_sub src bound <= tgt = %b)"
                  (to_string src) (to_string tgt) (to_string bound) accept
                  via_co_sub)
            s)
        s)
    s

let () =
  (* Sanity: [bot] really is the all-level-0 element. *)
  if not (equal bot (lattice_of_sample bot_sample))
  then fail "bot sample does not round-trip to bot";
  check_extremes ();
  check_per_axis ();
  check_sampled_pairs ();
  List.iter (fun (name, elems) -> check_monotonicity elems ~where:name) all_axes;
  check_monotonicity (sample 6143) ~where:"sampled";
  check_acceptance_rule ()
