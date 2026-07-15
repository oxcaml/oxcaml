(* TEST flags = "-extension layouts_alpha"; expect;
*)

(* REGRESSION PIN for the Stage C fold-stop engine-divergence (STAGE5F-NOTES.md, "Stage C
   fold-stop (C(i)): SECOND engine-divergence finding (-principal)").

   The Stage C(i) fold-stop stored the RAW decl jkind and ran the annotation sub-check
   [Ikind.sub_jkind_l] on it (typedecl.ml, normalize_decl_jkind). On this existential +
   `with (type:...) abstract` shape that flipped a verdict UNDER -principal: the raw
   inferred kind `value non_float mod portable` failed the annotation sub-check that the
   [Require_best]-normalized jkind passed (while non-principal stayed green). So fold-stop
   was NOT shipped; normalize_decl_jkind keeps folding via [Base_and_axes.normalize].

   The expect harness validates this [%%expect] under BOTH non-principal (run-expect) and
   -principal (check-program-output), so this test PINS the -principal behavior
   specifically. It must keep PASSING with the fold in place. If someone re-attempts
   fold-stop and this still passes under -principal with the annotation sub-check reading
   raw with_bounds, that's the signal the ikind engine now reproduces the fold on this
   shape. *)

type existential_abstract : value mod portable with (type : value mod portable) abstract =
  | P : ('a : value mod portable). 'a abstract t2 -> existential_abstract

and 'a t2 =
  | P :
      { contents : 'a
      ; other : ('b : value mod portable) option
      }
      -> 'a t2

and 'a abstract : value mod portable

[%%expect
  {|
type existential_abstract =
    P : ('a : value mod portable). 'a abstract t2 -> existential_abstract
and 'a t2 =
    P : 'a ('b : value mod portable). { contents : 'a; other : 'b option;
    } -> 'a t2
and 'a abstract : value mod portable
|}]
