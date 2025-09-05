#include <caml/mlvalues.h>
#include <stdbool.h>

// Use this to track how many times caml_modify has been called
static int called_modify = 0;

CAMLprim value replace_caml_modify_called_modify()
{
  return Val_int(called_modify);
}

CAMLprim value replace_caml_modify_reset()
{
  called_modify = 0;
  return Val_unit;
}

// This is a reference to the original caml_modify
CAMLextern void __real_caml_modify(value *fp, value v);

// This is called instead of caml_modify
CAMLprim void __wrap_caml_modify(value *fp, value v)
{
  // Record that caml_modify was called and then call the actual caml_modify
  called_modify += 1;
  __real_caml_modify(fp, v);
}

// Treat caml_modify_local the same was as caml_modify

CAMLextern void __real_caml_modify_local(value obj, intnat i, value val);

CAMLprim void __wrap_caml_modify_local(value obj, intnat i, value val)
{
  __real_caml_modify_local(obj, i, val);
  called_modify = true;
}

// Atomics

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
    __real_caml_##name(args);                  \
    called_##name = true;                      \
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
