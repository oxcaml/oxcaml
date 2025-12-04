#define _GNU_SOURCE
#include <pthread.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

CAMLprim value thread_get_current_thread_name(value unit) {
  CAMLparam1(unit);
  /* NOTE: thread names are restricted to 16 characters including the null
  terminator */
  char buf[16];
  int error = pthread_getname_np(pthread_self(), buf, 16);
  if (error != 0) {
    caml_unix_error(error, "thread_get_current_thread_name", Val_unit);
  }
  CAMLreturn(caml_copy_string(buf));
}
