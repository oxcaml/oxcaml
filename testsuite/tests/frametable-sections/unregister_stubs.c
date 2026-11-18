#define CAML_INTERNALS
#define NATIVE_CODE

#include <stdbool.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/domain_state.h>
#include <caml/fiber.h>
#include <caml/frame_descriptors.h>

extern intnat *caml_frametable[];

static intnat empty_table[1] = { 0 };

/* The runtime removes an unregistered table's descriptors only when the next
   table is registered */
static void remove_unregistered(void)
{
  caml_register_frametable(empty_table);
  caml_unregister_frametable(empty_table);
}

static bool found(uintnat pc)
{
  return caml_find_frame_descr(caml_get_frame_descrs(), pc) != NULL;
}

/* Returns 0, or else the number of the first check that failed. Either way,
   every table is registered again on return. This must not allocate on the
   OCaml heap, since the caller's descriptors are missing part of the time. */
value frametable_unregister_test(value unit)
{
  int failed = 0;
#define CHECK(n, cond) do { if (!failed && !(cond)) failed = (n); } while (0)

  /* The tables of the units compiled with -frametable-sections */
  int ntables = 0;
  for (int i = 0; caml_frametable[i] != NULL; i++) ntables++;
  void **tables = malloc(ntables * sizeof(void *));
  if (tables == NULL) return Val_int(1);
  ntables = 0;
  for (int i = 0; caml_frametable[i] != NULL; i++) {
    if (caml_frametable[i][0] == FRAMETABLE_SECTIONS_TAG)
      tables[ntables++] = caml_frametable[i];
  }
  if (ntables < 2) {
    free(tables);
    return Val_int(2);
  }

  /* The caller's return address, whose descriptor is in a piece */
  char *sp;
  uintnat pc;
  caml_get_stack_sp_pc(Caml_state->current_stack, &sp, &pc);
  CHECK(3, found(pc));

  /* Each table leaves in turn. The pieces stay, since other tables still
     refer to them, even when the one leaving is their owner. */
  for (int i = 0; i < ntables; i++) {
    caml_unregister_frametable(tables[i]);
    remove_unregistered();
    CHECK(4, found(pc));
    caml_register_frametable(tables[i]);
    CHECK(5, found(pc));
  }

  /* Once every table has left, so have the pieces */
  caml_unregister_frametables(tables, ntables);
  remove_unregistered();
  CHECK(6, !found(pc));
  caml_register_frametables(tables, ntables);
  CHECK(7, found(pc));

  free(tables);
  return Val_int(failed);
}
