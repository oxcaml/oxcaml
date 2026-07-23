(* TEST
 flags = "-extension layout_poly_alpha -extension layouts_beta -i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Applying a functor parameter's lpoly val instantiates its layout
   variable [x] through flexible instance copies, which is reachable today
   (translation of the instantiation is not, hence [-stop-after typing]).

   At [x := bits8 & bits16], the [('b : x addressable)] bound is exactly
   the whole-marked product: [ok] is accepted, and [rejected] passes a
   component-marked product - a different, incomparable kind - and fails.
   The instance copies preserve the bound's exact meaning; see the
   invariants on [Jkind_types.Layout]. *)

type p_plain : bits8 & bits16
type p_whole : (bits8 & bits16) addressable
type p_components : bits8 addressable & bits16 addressable

module Use (M : sig
  val f :
    layout_ x.
    ('a : x) ('b : x addressable). 'a -> 'b -> unit
end @ static) = struct
  let ok (a : p_plain) (b : p_whole) = M.f a b

  let rejected (a : p_plain) (b : p_components) =
    M.f a b
end
