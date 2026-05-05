#define CAML_INTERNALS

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#include "caml/codefrag.h"
#include "caml/fail.h"
#include "caml/frame_descriptors.h"
#include "caml/mlvalues.h"
#include "caml/unloadable.h"

static void record_visit(void *fdata, value v, volatile value *p)
{
  (void)v;
  (void)p;
  intnat *count = (intnat *)fdata;
  (*count)++;
}

CAMLprim value caml_test_unload_code_ptr_slot_gating(value unit)
{
  (void)unit;
  long pagesize = sysconf(_SC_PAGESIZE);
  if (pagesize <= 0) caml_failwith("sysconf(_SC_PAGESIZE) failed");

  size_t len = (size_t)pagesize;
  char *mem =
      mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1,
           0);
  if (mem == MAP_FAILED) caml_failwith("mmap failed");

  int fragnum =
      caml_register_code_fragment(mem, mem + len, DIGEST_IGNORE, NULL);

  value entry = (value)(mem + sizeof(value));
  *((value *)entry - 1) = Val_long(0);
  value cp_slot = entry;

  value regs_dummy[1];
  regs_dummy[0] = Val_unit;

  size_t descr_bytes = sizeof(frame_descr_long) + sizeof(uint32_t);
  frame_descr_long *dl = (frame_descr_long *)malloc(descr_bytes);
  if (dl == NULL) caml_failwith("malloc failed");
  memset(dl, 0, descr_bytes);
  dl->retaddr_rel = 0;
  dl->marker = FRAME_LONG_MARKER;
  dl->frame_data = 16 | FRAME_DESCRIPTOR_HAS_CODE_PTR_SLOTS;
  dl->num_live = 0;

  uint32_t *code_ptr_ofs = (uint32_t *)&dl->live_ofs[0];
  code_ptr_ofs[0] = 1; /* number of code-ptr slots */
  code_ptr_ofs[1] = 0; /* stack offset 0 from sp */

  intnat visits = 0;
  caml_visit_frame_code_ptr_slots(record_visit, &visits, (frame_descr *)dl,
                                  (char *)&cp_slot, regs_dummy);

  struct code_fragment *cf = caml_find_code_fragment_by_num(fragnum);
  if (cf != NULL) caml_remove_code_fragment(cf);

  munmap(mem, len);
  free(dl);

  if (visits != 0) {
    caml_failwith(
        "visited code ptr for non-unloadable registered code fragment");
  }

  return Val_unit;
}
