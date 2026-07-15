(* TEST flags = "-extension layouts_alpha"; expect;
*)

(* REGRESSION PIN for the deletion-wave-2 round_up re-route None-path (STAGE5F-NOTES.md,
   "deletion-wave-2: round_up ikind re-route").

   [Jkind.round_up] must return [None] for an ABSTRACT-base jkind with surviving
   with-bounds (base [k] is an abstract [kind_]): legacy [Ignore_best normalize]
   conservatively keeps the with-bound unfolded over an unknown base, and
   [Ctype.nondep_type_decl]'s covariant-erase fallback (ctype.ml) relies on that [None] to
   raise [Nondep_cannot_erase] and report a clean "cannot be eliminated" error at functor
   application.

   The naive ikind re-route ([Solver.round_up] always [Some], collapsing the abstract base
   atom to top) produced a non-supertype and CRASHED with "Fatal error: nondep_supertype
   not included in original module type" (typemod.ml). This test pins the CLEAN error --
   it also falsifies the earlier "round_up None path is unreachable from surface syntax"
   claim (it is reached by functor + abstract [kind_] + with-bound). It must keep giving
   the clean error, never the fatal. *)

module F (X : sig
    type t
  end) =
struct
  kind_ k

  type t : k with X.t
end

module M = F (struct
    type t
  end)

[%%expect
  {|
module F :
  functor (X : sig type t end) ->
    sig
      kind_ k
      type t
        : k
            mod global many stateless immutable dynamic external_
            with X.t
            with k
    end
Lines 10-12, characters 11-6:
10 | ...........F (struct
11 |     type t
12 |   end)
Error: This functor has type
       "functor (X : sig type t end) ->
         sig
           kind_ k
           type t
             : k
                 mod global many stateless immutable dynamic external_
                 with X.t
                 with k
         end"
       The parameter cannot be eliminated in the result type.
       Please bind the argument to a module identifier.
|}]
