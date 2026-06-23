#define INT
#define INTLIT
#define FLOAT
#define FLOATLIT
#define STRING
#define STRINGLIT

#include "type.ml"

#include "write.ml"

#include "monomorphic.ml"

module Pretty = struct
#include "prettyprint.ml"
end

#include "write2.ml"

#undef INT
#undef INTLIT
#undef FLOAT
#undef FLOATLIT
#undef STRING
#undef STRINGLIT
