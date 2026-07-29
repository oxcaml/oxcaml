(** Breadcrumbs mark assumptions we plan to revisit.

    Code depending on these assumptions should destruct these values. Then, upon
    breaking an assumption, delete the breadcrumb, and the compiler will force
    its references to be updated.

    Breadcrumbs can also be depended on in tests; grep [testsuite/] for
    examples.

    When considering adding new breadcrumbs, be judicious! Often, it's best to
    structure the code such that the future change will be caught by a type
    error anyway. For example, future constructors added to a variant are
    naturally handled by the match exhaustiveness check, and
    invariants/assumptions can often be localized to a new module's
    implementation.

    Some cases in which breadcrumbs *can* be the right choice are:
    - When we have non-local invariants (which, of course, should be avoided
      anyway).
    - When we will want to *produce* different data in the future while
      consuming the same data.
    - When we expect a test to update with some future feature, and want to
      ensure that we come back to check that it does. *)

(** Marks code to revisit once we have [kind_of] (internal ticket 2912), e.g.
    {[
      type ('a : any) t : (kind_of 'a) & (kind_of 'a) = #('a * 'a)
    ]}

    Some users of this breadcrumb may really care about [layout_of], but we
    expect these features to arrive ~together. *)
val until_kind_of : unit
