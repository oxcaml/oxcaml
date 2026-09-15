(* Parameters: P *)

(* Alias-only module: doesn't reference [P] in its body, so when compiled with
   [-no-alias-deps] the runtime layout collapses to [Rp_unit] (no
   [Rp_argument_block P]).  Phase-1 of functorize sees [cmi_params = [P]];
   phase-2 must agree by reading the cmi's declared params rather than the
   cmo's collapsed runtime layout. *)
module Message = Message
