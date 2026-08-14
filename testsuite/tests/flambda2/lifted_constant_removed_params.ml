(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags += " -O3";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

(* Regression test for a bug in the placement of lifted constants relative to
   the [let]s introduced by the flow analysis to rebind alias-removed
   continuation parameters (see [Simplify_let_cont_expr.add_lets_around_handler]).

   The alias analysis removes continuation parameters whose canonical
   dominator is another simple, rebinding them inside the handler with
   [let param = dominator]. At unit toplevel, lifted constants placed around
   the same handler can reference such removed parameters. They were
   previously placed outside the rebinding [let]s, leaving the reference to
   the removed parameter unbound in the rebuilt term, which tripped the
   fatal error "The alias analysis marked the param ... as removed, but the
   free_names indicate it is actually used". Whether the crash manifested
   was sensitive to name-stamp ordering (e.g. the source file name), since
   that determines which member of an alias class becomes the canonical
   dominator.

   This file was minimized (by chamelon) from real code; the exact shape,
   including the seemingly redundant matches and opaque calls, is load
   bearing. *)

[@@@ocaml.warning "-6-20-26-27-32-37-39-60"]

external __dummy2__ : unit -> 'a = "%opaque"

external __ignore__ : 'a -> unit = "%ignore"

module Comparison_result = struct
  type config = { assume_text : bool }

  type t =
    | Binary_different of
        { prev_is_binary : bool
        ; next_is_binary : bool
        }

  let is_binary (s : _) =
    let (_ : int) =
      if (__dummy2__ ()) ((__dummy2__ ()) 0) then (__dummy2__ ()) ~length:0 else 0
    in
    let rec go i = __dummy2__ () && (__dummy2__ ()) ((__dummy2__ ()) __dummy2__) in
    go 0

  let create (config : _) ~prev:(_ : _) ~next:(_ : _) ~compare_assuming_text:_ =
    let prev_is_binary, next_is_binary =
      match config.assume_text with
      | true -> __dummy2__ ()
      | false -> is_binary "", __dummy2__ ()
    in
    if prev_is_binary || __dummy2__ ()
    then Binary_different { next_is_binary; prev_is_binary }
    else __dummy2__ ()
end

module Compare_core = struct
  module type S_stub = sig end

  module Make (Patdiff_core_arg : S_stub) = struct
    let diff_strings (config : _) =
      Comparison_result.create config __dummy2__ __dummy2__ __dummy2__
  end

  module Patdiff_core_stub : S_stub = struct end
  module Without_unix = Make (Patdiff_core_stub)
end

let (_ : Comparison_result.t) =
  let config = { Comparison_result.assume_text = false } in
  Compare_core.Without_unix.diff_strings config
