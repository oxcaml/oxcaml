(** Breadcrumbs mark assumptions we plan to revisit.

    Code depending on these assumptions should destruct these values. Then, upon
    breaking an assumption, delete the breadcrumb to be forced by the compiler
    to update its dependencies.

    Breadcrumbs can also be depended on in tests; grep [testsuite/] for
    examples. *)

(** Mark to revisit once we have [kind_of] (internal ticket 2912), e.g.
    {[
      type ('a : any) t : (kind_of 'a) & (kind_of 'a) = #('a * 'a)
    ]}

    Some users of this breadcrumb may really care about [layout_of], but we
    expect these features to arrive ~together. *)
val until_kind_of : unit
