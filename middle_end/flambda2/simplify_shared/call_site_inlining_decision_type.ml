(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: We need to emit [Warnings.Inlining_impossible] as required.

   When in fallback-inlining mode: if we want to follow Closure we should not
   complain about function declarations with e.g. [@inline always] if the
   function contains other functions and therefore cannot be inlined. We should
   however contain at call sites if inlining is requested but cannot be done for
   this reason. I think this will probably all happen without any specific code
   once [Inlining_impossible] handling is implemented for the
   non-fallback-inlining cases. *)

(** How the result of a speculative inlining was judged (see
    [Flambda_features.Inlining.speculative_inlining_criterion]). *)
type speculative_criterion =
  | Threshold of { evaluated_to : float }
  | Ratio of
      { adjusted_size : float;
            (** The size of the inlined body less the call-site credit and the
                bonus for removed operations. *)
        bonus : float;
        ratio : float;  (** [adjusted_size] over [original_size]. *)
        max_ratio : float
      }

type t =
  | Missing_code
  | Definition_says_not_to_inline
  | In_a_stub
  | Doing_speculative_inlining of { charged_code_size : Code_size.t }
  | Argument_types_not_useful
  | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded
  | Never_inlined_attribute
  | Speculative_inlining_budget_exhausted of
      { remaining_budget : float;
        code_size : Code_size.t;
        max_code_size : float
      }
  | Speculative_inlining_aborted of
      { budget : float;
        threshold_is_remaining_budget : bool
      }
  | Speculatively_not_inline of
      { cost_metrics : Cost_metrics.t;
        cost_metrics_of_lifted_constants : Cost_metrics.t;
        original_size : Code_size.t;
        call_site_credit : float;
        criterion : speculative_criterion;
        threshold : float;
        threshold_is_remaining_budget : bool;
        is_a_functor : bool
      }
  | Attribute_always
  | Replay_history_says_must_inline of t
  | Begin_unrolling of int
  | Continue_unrolling
  | Definition_says_inline of { was_inline_always : bool }
  | Speculatively_inline of
      { cost_metrics : Cost_metrics.t;
        cost_metrics_of_lifted_constants : Cost_metrics.t;
        original_size : Code_size.t;
        call_site_credit : float;
        criterion : speculative_criterion;
        threshold : float;
        threshold_is_remaining_budget : bool;
        is_a_functor : bool
      }
  | Jsir_inlining_disabled

let [@ocamlformat "disable"] print_criterion ppf criterion =
  match criterion with
  | Threshold { evaluated_to } ->
    Format.fprintf ppf "@[<hov 1>(Threshold@ (evaluated_to@ %f))@]" evaluated_to
  | Ratio { adjusted_size; bonus; ratio; max_ratio } ->
    Format.fprintf ppf
      "@[<hov 1>(Ratio@ \
        @[<hov 1>(adjusted_size@ %f)@]@ \
        @[<hov 1>(bonus@ %f)@]@ \
        @[<hov 1>(ratio@ %f)@]@ \
        @[<hov 1>(max_ratio@ %f)@])@]"
      adjusted_size bonus ratio max_ratio

let [@ocamlformat "disable"] print_speculation ppf name ~cost_metrics
    ~cost_metrics_of_lifted_constants ~original_size ~call_site_credit
    ~criterion ~threshold ~threshold_is_remaining_budget ~is_a_functor =
  Format.fprintf ppf
    "@[<hov 1>(%s@ \
      @[<hov 1>(cost_metrics@ %a)@]@ \
      @[<hov 1>(cost_metrics_of_lifted_constants@ %a)@]@ \
      @[<hov 1>(original_size@ %a)@]@ \
      @[<hov 1>(call_site_credit@ %f)@]@ \
      @[<hov 1>(criterion@ %a)@]@ \
      @[<hov 1>(threshold@ %f)@]@ \
      @[<hov 1>(threshold_is_remaining_budget@ %b)@]@ \
      @[<hov 1>(is_a_functor@ %b)@]\
      )@]"
    name
    Cost_metrics.print cost_metrics
    Cost_metrics.print cost_metrics_of_lifted_constants
    Code_size.print original_size
    call_site_credit
    print_criterion criterion
    threshold
    threshold_is_remaining_budget
    is_a_functor

let [@ocamlformat "disable"] rec print ppf t =
  match t with
  | Missing_code -> Format.fprintf ppf "Missing_code"
  | Definition_says_not_to_inline ->
    Format.fprintf ppf "Definition_says_not_to_inline"
  | In_a_stub -> Format.fprintf ppf "In_a_stub"
  | Doing_speculative_inlining { charged_code_size } ->
    Format.fprintf ppf
      "@[<hov 1>(Doing_speculative_inlining@ \
        @[<hov 1>(charged_code_size@ %a)@])\
        @]"
      Code_size.print charged_code_size
  | Argument_types_not_useful ->
    Format.fprintf ppf "Argument_types_not_useful"
  | Unrolling_depth_exceeded ->
    Format.fprintf ppf "Unrolling_depth_exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf ppf "Max_inlining_depth_exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf ppf "Recursion_depth_exceeded"
  | Never_inlined_attribute ->
    Format.fprintf ppf "Never_inlined_attribute"
  | Speculative_inlining_budget_exhausted
      { remaining_budget; code_size; max_code_size } ->
    Format.fprintf ppf
      "@[<hov 1>(Speculative_inlining_budget_exhausted@ \
        @[<hov 1>(remaining_budget@ %f)@]@ \
        @[<hov 1>(code_size@ %a)@]@ \
        @[<hov 1>(max_code_size@ %f)@]\
        )@]"
      remaining_budget
      Code_size.print code_size
      max_code_size
  | Attribute_always ->
    Format.fprintf ppf "Attribute_always"
  | Replay_history_says_must_inline t' ->
    Format.fprintf ppf "Replay_history_says_must_inline(%a)" print t'
  | Definition_says_inline { was_inline_always } ->
    Format.fprintf ppf
      "@[<hov 1>(Definition_says_inline@ \
        @[<hov 1>(was_inline_always@ %b)@])\
        @]"
      was_inline_always
  | Begin_unrolling unroll_to ->
    Format.fprintf ppf
      "@[<hov 1>(Begin_unrolling@ \
        @[<hov 1>(unroll_to@ %d)@]\
        )@]"
      unroll_to
  | Continue_unrolling ->
    Format.fprintf ppf "Continue_unrolling"
  | Speculative_inlining_aborted { budget; threshold_is_remaining_budget } ->
    Format.fprintf ppf
      "@[<hov 1>(Speculative_inlining_aborted@ \
        @[<hov 1>(budget@ %f)@]@ \
        @[<hov 1>(threshold_is_remaining_budget@ %b)@]\
        )@]"
      budget
      threshold_is_remaining_budget
  | Speculatively_not_inline { cost_metrics; cost_metrics_of_lifted_constants;
                                original_size; call_site_credit; criterion;
                                threshold; threshold_is_remaining_budget;
                                is_a_functor; } ->
    print_speculation ppf "Speculatively_not_inline" ~cost_metrics
      ~cost_metrics_of_lifted_constants ~original_size ~call_site_credit
      ~criterion ~threshold ~threshold_is_remaining_budget ~is_a_functor
  | Speculatively_inline { cost_metrics; cost_metrics_of_lifted_constants;
                            original_size; call_site_credit; criterion;
                            threshold; threshold_is_remaining_budget;
                            is_a_functor; } ->
    print_speculation ppf "Speculatively_inline" ~cost_metrics
      ~cost_metrics_of_lifted_constants ~original_size ~call_site_credit
      ~criterion ~threshold ~threshold_is_remaining_budget ~is_a_functor
  | Jsir_inlining_disabled -> Format.fprintf ppf "Jsir_inlining_disabled"

type can_inline =
  | Do_not_inline of { erase_attribute_if_ignored : bool }
  | Inline of
      { unroll_to : int option;
        was_inline_always : bool
      }

let rec can_inline (t : t) : can_inline =
  match t with
  | Missing_code | In_a_stub | Doing_speculative_inlining _
  | Max_inlining_depth_exceeded | Recursion_depth_exceeded
  | Speculative_inlining_budget_exhausted _ | Speculative_inlining_aborted _
  | Speculatively_not_inline _ | Definition_says_not_to_inline
  | Argument_types_not_useful ->
    (* If there's an [@inlined] attribute on this, something's gone wrong *)
    Do_not_inline { erase_attribute_if_ignored = false }
  | Never_inlined_attribute ->
    (* If there's an [@inlined] attribute on this, something's gone wrong *)
    Do_not_inline { erase_attribute_if_ignored = false }
  | Unrolling_depth_exceeded ->
    (* If there's an [@unrolled] attribute on this, then we'll ignore the
       attribute when we stop unrolling, which is fine *)
    Do_not_inline { erase_attribute_if_ignored = true }
  | Begin_unrolling unroll_to ->
    Inline { unroll_to = Some unroll_to; was_inline_always = false }
  | Continue_unrolling ->
    let was_inline_always =
      (* This could be [true] since the user asked to unroll this far, but the
         warning would be confusing. We should use something more informative
         than a [bool] here to describe what warning should be raised if we
         don't inline. *)
      false
    in
    Inline { unroll_to = None; was_inline_always }
  | Definition_says_inline { was_inline_always } ->
    Inline { unroll_to = None; was_inline_always }
  | Speculatively_inline _ ->
    Inline { unroll_to = None; was_inline_always = false }
  | Attribute_always -> Inline { unroll_to = None; was_inline_always = true }
  | Replay_history_says_must_inline t' -> can_inline t'
  | Jsir_inlining_disabled ->
    Do_not_inline { erase_attribute_if_ignored = false }

let report_speculation fmt ~inlined ~cost_metrics
    ~cost_metrics_of_lifted_constants ~original_size ~call_site_credit
    ~criterion ~threshold ~threshold_is_remaining_budget ~is_a_functor =
  let what = if is_a_functor then "functor" else "function" in
  let outcome = if inlined then "inlined" else "not inlined" in
  let comparison = if inlined then "<=" else ">" in
  let budget =
    if threshold_is_remaining_budget then "remaining budget" else "threshold"
  in
  let budget_word =
    if threshold_is_remaining_budget then "remaining budget" else "budget"
  in
  match criterion with
  | Threshold { evaluated_to } ->
    Format.fprintf fmt
      "the@ %s@ was@ %s@ after@ speculation@ as@ its@ cost@ metrics@ were=%a@ \
       (of@ which@ lifted@ constants:@ %a;@ size@ before@ inlining@ %a;@ \
       call-site@ credit@ %f),@ which@ was@ evaluated@ to@ %f@ %s@ %s@ %f"
      what outcome Cost_metrics.print cost_metrics Cost_metrics.print
      cost_metrics_of_lifted_constants Code_size.print original_size
      call_site_credit evaluated_to comparison budget threshold
  | Ratio { adjusted_size; bonus; ratio; max_ratio } ->
    Format.fprintf fmt
      "the@ %s@ was@ %s@ after@ speculation:@ size@ before@ inlining@ %a,@ \
       cost@ metrics@ after@ speculation=%a@ (of@ which@ lifted@ constants:@ \
       %a),@ call-site@ credit@ %f,@ bonus@ for@ removed@ operations@ %f,@ \
       adjusted@ size@ %f,@ ratio@ %f@ %s@ maximum@ ratio@ %f@ (%s@ %f)"
      what outcome Code_size.print original_size Cost_metrics.print cost_metrics
      Cost_metrics.print cost_metrics_of_lifted_constants call_site_credit bonus
      adjusted_size ratio comparison max_ratio budget_word threshold

(* CR mshinwell/gbury: tidy up by using Format.pp_print_text *)
let rec report_reason fmt t =
  match (t : t) with
  | Missing_code ->
    Format.fprintf fmt
      "the@ code@ could@ not@ be@ found@ (is@ a@ .cmx@ file@ missing?)"
  | Definition_says_not_to_inline ->
    Format.fprintf fmt
      "this@ function@ was@ deemed@ at@ the@ point@ of@ its@ definition@ to@ \
       never@ be@ inlinable"
  | In_a_stub ->
    Format.fprintf fmt
      "this@ function@ is@ being@ called@ inside@ of@ a@ stub;@ inlining@ is@ \
       not@ performed@ inside@ stubs@ (until@ they@ are@ inlined)"
  | Doing_speculative_inlining { charged_code_size } ->
    if Code_size.equal charged_code_size Code_size.zero
    then Format.fprintf fmt "speculative@ inlining@ is@ in@ progress"
    else
      Format.fprintf fmt
        "speculative@ inlining@ is@ in@ progress;@ an@ estimate@ of@ the@ \
         callee's@ code@ size@ (%a)@ was@ charged@ to@ the@ enclosing@ \
         speculation"
        Code_size.print charged_code_size
  | Argument_types_not_useful ->
    Format.fprintf fmt
      "there@ was@ no@ useful@ information@ about@ the@ arguments"
  | Unrolling_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ unrolling@ depth@ has@ been@ exceeded"
  | Max_inlining_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ inlining@ depth@ has@ been@ exceeded"
  | Recursion_depth_exceeded ->
    Format.fprintf fmt "the@ maximum@ recursion@ depth@ has@ been@ exceeded"
  | Never_inlined_attribute ->
    Format.fprintf fmt "the@ call@ has@ an@ attribute@ forbidding@ inlining"
  | Attribute_always ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@inline always]@ attribute"
  | Replay_history_says_must_inline t' ->
    (* CR gbury: We could decide not to include in the inlining report inlining
       decisions that were made during replays (e.g. continuation
       specialization), or alternatively to store the initial inlining decision
       so that we can report it each time. *)
    Format.fprintf fmt
      "the@ call@ was@ inlined@ during@ the@ first@ pass@ on@ the@ current@ \
       continuation@ handler@ with@ the@ following@ reason:@ @[<hov 2>%a@]"
      report_reason t'
  | Begin_unrolling n ->
    Format.fprintf fmt "the@ call@ has@ an@ [@@unroll %d]@ attribute" n
  | Continue_unrolling ->
    Format.fprintf fmt "this@ function@ is@ being@ unrolled"
  | Definition_says_inline { was_inline_always = _ } ->
    Format.fprintf fmt
      "this@ function@ was@ decided@ to@ be@ always@ inlined@ at@ its@ \
       definition@ site (annotated@ by@ [@inlined always]@ or@ determined@ to@ \
       be@ small@ enough)"
  | Speculative_inlining_budget_exhausted
      { remaining_budget; code_size; max_code_size } ->
    Format.fprintf fmt
      "the@ function@ was@ not@ speculated@ upon@ as@ its@ code@ size@ (%a)@ \
       exceeds@ the@ maximum@ (%f)@ allowed@ by@ the@ remaining@ speculative@ \
       inlining@ budget@ (%f)@ of@ the@ enclosing@ inlined@ body"
      Code_size.print code_size max_code_size remaining_budget
  | Speculative_inlining_aborted { budget; threshold_is_remaining_budget } ->
    Format.fprintf fmt
      "the@ speculation@ was@ aborted@ because@ the@ %s@ (%f)@ was@ exhausted@ \
       while@ simplifying@ the@ inlined@ body"
      (if threshold_is_remaining_budget then "remaining budget" else "budget")
      budget
  | Speculatively_not_inline
      { cost_metrics;
        cost_metrics_of_lifted_constants;
        original_size;
        call_site_credit;
        criterion;
        threshold;
        threshold_is_remaining_budget;
        is_a_functor
      } ->
    report_speculation fmt ~inlined:false ~cost_metrics
      ~cost_metrics_of_lifted_constants ~original_size ~call_site_credit
      ~criterion ~threshold ~threshold_is_remaining_budget ~is_a_functor
  | Speculatively_inline
      { cost_metrics;
        cost_metrics_of_lifted_constants;
        original_size;
        call_site_credit;
        criterion;
        threshold;
        threshold_is_remaining_budget;
        is_a_functor
      } ->
    report_speculation fmt ~inlined:true ~cost_metrics
      ~cost_metrics_of_lifted_constants ~original_size ~call_site_credit
      ~criterion ~threshold ~threshold_is_remaining_budget ~is_a_functor
  | Jsir_inlining_disabled ->
    Format.fprintf fmt
      "function@ inlining@ is@ disabled@ for@ Js_of_ocaml@ translation"

let charged_code_size (t : t) =
  match t with
  | Doing_speculative_inlining { charged_code_size } -> charged_code_size
  | Missing_code | Definition_says_not_to_inline | In_a_stub
  | Argument_types_not_useful | Unrolling_depth_exceeded
  | Max_inlining_depth_exceeded | Recursion_depth_exceeded
  | Never_inlined_attribute | Speculative_inlining_budget_exhausted _
  | Speculative_inlining_aborted _ | Speculatively_not_inline _
  | Attribute_always | Replay_history_says_must_inline _ | Begin_unrolling _
  | Continue_unrolling | Definition_says_inline _ | Speculatively_inline _
  | Jsir_inlining_disabled ->
    Code_size.zero

let rec speculative_inlining_threshold (t : t) =
  match t with
  | Speculatively_inline { threshold; _ } -> Some threshold
  | Replay_history_says_must_inline t -> speculative_inlining_threshold t
  | Missing_code | Definition_says_not_to_inline | In_a_stub
  | Doing_speculative_inlining _ | Argument_types_not_useful
  | Unrolling_depth_exceeded | Max_inlining_depth_exceeded
  | Recursion_depth_exceeded | Never_inlined_attribute
  | Speculative_inlining_budget_exhausted _ | Speculative_inlining_aborted _
  | Speculatively_not_inline _ | Attribute_always | Begin_unrolling _
  | Continue_unrolling | Definition_says_inline _ | Jsir_inlining_disabled ->
    None

let report fmt t =
  Format.fprintf fmt
    "@[<v>The function call %s been inlined@ because @[<hov>%a@]@]"
    (match can_inline t with Inline _ -> "has" | Do_not_inline _ -> "has not")
    report_reason t
