(**
   This module supports standard JSON nodes only.
   Arbitrary integers are not supported as they must all fit within the
   standard OCaml int type (31 or 63 bits depending on the platform).

   The main advantage of this module is its simplicity.
*)

#define INT
#define FLOAT
#define STRING

#include "type.ml"

#include "write.mli"

#include "monomorphic.mli"

#include "write2.mli"

#include "read.mli"

(** This module provides combinators for extracting fields from JSON values. *)
module Util : sig
  #include "util.mli"
end

#undef INT
#undef FLOAT
#undef STRING
