#define INT
#define FLOAT
#define STRING

#include "type.ml"

#include "write.ml"

module Pretty = struct
  #include "prettyprint.ml"
end

#include "monomorphic.ml"

#include "write2.ml"

#include "read.ml"

module Util = struct
  #include "util.ml"
end

#undef INT
#undef FLOAT
#undef STRING
