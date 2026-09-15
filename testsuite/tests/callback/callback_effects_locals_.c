#include <caml/memory.h>
#include <caml/callback.h>

value cb(value loc, value cb) {
  CAMLparam2(loc, cb);
  caml_callback(cb, Val_unit);
  CAMLreturn(Field(loc,0));
}
