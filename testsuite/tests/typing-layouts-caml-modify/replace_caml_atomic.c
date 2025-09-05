#include <caml/mlvalues.h>

#define P(...) __VA_ARGS__
#define TRACK(name, arg_tys, args)             \
  static int called_##name = 0;                \
  CAMLprim value name##_calls()                \
  {                                            \
    return Val_int(called_##name);             \
  }                                            \
  CAMLextern void __real_caml_##name(arg_tys); \
  CAMLprim void __wrap_caml_##name(arg_tys)    \
  {                                            \
    called_##name++;                           \
    __real_caml_##name(args);                  \
  }

TRACK(atomic_load, P(value ref), P(ref))
TRACK(atomic_load_field, P(value obj, value vfield), P(obj, vfield))
TRACK(atomic_exchange, P(value ref, value v), P(ref, v))
TRACK(atomic_exchange_field, P(value obj, value vfield, value v), P(obj, vfield, v))
TRACK(atomic_set, P(value ref, value v), P(ref, v))
TRACK(atomic_set_field, P(value ref, value vfield, value v), P(ref, vfield, v))
TRACK(atomic_cas, P(value ref, value oldv, value newv), P(ref, oldv, newv))
TRACK(atomic_cas_field, P(value ref, value vfield, value oldv, value newv), P(ref, vfield, oldv, newv))
TRACK(atomic_compare_exchange, P(value ref, value oldv, value newv), P(ref, oldv, newv))
TRACK(atomic_compare_exchange_field, P(value ref, value vfield, value oldv, value newv), P(ref, vfield, oldv, newv))
TRACK(atomic_fetch_add, P(value ref, value incr), P(ref, incr))
TRACK(atomic_fetch_add_field, P(value obj, value vfield, value incr), P(obj, vfield, incr))
TRACK(atomic_add, P(value obj, value incr), P(obj, incr))
TRACK(atomic_add_field, P(value obj, value vfield, value incr), P(obj, vfield, incr))
TRACK(atomic_sub, P(value obj, value incr), P(obj, incr))
TRACK(atomic_sub_field, P(value obj, value vfield, value incr), P(obj, vfield, incr))
TRACK(atomic_land, P(value obj, value incr), P(obj, incr))
TRACK(atomic_land_field, P(value obj, value vfield, value incr), P(obj, vfield, incr))
TRACK(atomic_lor, P(value obj, value incr), P(obj, incr))
TRACK(atomic_lor_field, P(value obj, value vfield, value incr), P(obj, vfield, incr))
TRACK(atomic_lxor, P(value obj, value incr), P(obj, incr))
TRACK(atomic_lxor_field, P(value obj, value vfield, value incr), P(obj, vfield, incr))
