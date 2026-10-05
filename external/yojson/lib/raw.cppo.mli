(**
   Ints, floats and strings literals are systematically preserved using
   [`Intlit], [`Floatlit] and [`Stringlit].
*)

#define INTLIT
#define FLOATLIT
#define STRINGLIT

#include "type.ml"

#include "monomorphic.mli"

#include "write.mli"

#include "write2.mli"

#include "read.mli"

(** This module provides combinators for extracting fields from JSON values. *)
module Util : sig
  #include "util.mli"
end

#undef INTLIT
#undef FLOATLIT
#undef STRINGLIT
