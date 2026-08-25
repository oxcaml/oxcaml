(* [Basic_transparent.t = P.t] is P-dependent, so [Intf(P_int).S] and
   [Intf(P_int_alt).S] differ; ascription fails. *)

module R : Bundle2.Intf(P_int_alt).S = Bundle2.Make (P_int) ()
