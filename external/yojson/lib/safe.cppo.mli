(**
   This module supports a slight superset of the standard JSON nodes.
   Arbitrary integers are supported and represented as a decimal string 
   using [`Intlit] when they cannot be represented using OCaml's int type
   (31 or 63 bits depending on the platform).

   This module is recommended for intensive use or OCaml-friendly use of
   JSON.
*)

#define INT
#define INTLIT
#define FLOAT
#define STRING

#include "type.ml"

#include "monomorphic.mli"

#include "safe_to_basic.mli"

#include "write.mli"

#include "write2.mli"

#include "read.mli"

(** This module provides combinators for extracting fields from JSON values. *)
module Util : sig
  #include "util.mli"
end

#undef INT
#undef INTLIT
#undef FLOAT
#undef STRING
