#include "caml/mlvalues.h"

extern char caml_bundled_cmis[];
extern char caml_bundled_cmxs[];

CAMLprim value bundled_cmis() {
    return (value) &caml_bundled_cmis;
}

CAMLprim value bundled_cmxs() {
    return (value) &caml_bundled_cmxs;
}
