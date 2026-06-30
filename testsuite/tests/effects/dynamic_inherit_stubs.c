#include <stdlib.h>
#include <pthread.h>

#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

struct c_thread {
  value callback;
  pthread_t thread;
};

static void *thread_func(void *arg)
{
  struct c_thread *th = arg;
  if (!caml_c_thread_register()) abort();
  caml_acquire_runtime_system();
  caml_callback(th->callback, Val_unit);
  caml_remove_generational_global_root(&th->callback);
  caml_release_runtime_system();
  if (!caml_c_thread_unregister()) abort();
  return 0;
}

CAMLprim value dynamic_inherit_run_in_c_thread(value callback)
{
  CAMLparam1(callback);
  struct c_thread *th = malloc(sizeof(struct c_thread));
  if (th == NULL) caml_raise_out_of_memory();

  th->callback = callback;
  caml_register_generational_global_root(&th->callback);

  int err = pthread_create(&th->thread, NULL, thread_func, th);
  if (err != 0) {
    caml_remove_generational_global_root(&th->callback);
    free(th);
    caml_failwith("pthread_create");
  }
  caml_release_runtime_system();
  pthread_join(th->thread, NULL);
  caml_acquire_runtime_system();

  free(th);
  CAMLreturn(Val_unit);
}
