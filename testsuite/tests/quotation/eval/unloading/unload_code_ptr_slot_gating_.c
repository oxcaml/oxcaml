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

  /* Hand-pack a "long" escaped frame descriptor in the packed byte format
     (see runtime/caml/frame_descriptors.h): escape byte, retaddr_rel,
     FRAME_LONG_MARKER, pad, frame_data, num_live, live_ofs[], then the
     parallel code_ptr_live_ofs array (count + entries, unaligned). */
  size_t descr_bytes = Frame_long_live_ofs + 2 * sizeof(uint32_t);
  unsigned char *dl = (unsigned char *)malloc(descr_bytes);
  if (dl == NULL) caml_failwith("malloc failed");
  memset(dl, 0, descr_bytes);
  dl[0] = FRAME_DELTA_ESCAPE;
  /* retaddr_rel (offset 1) stays 0: unused by this test. */
  uint16_t marker = FRAME_LONG_MARKER;
  memcpy(dl + Frame_data_ofs, &marker, sizeof(marker));
  uint32_t frame_data = 16 | FRAME_DESCRIPTOR_HAS_CODE_PTR_SLOTS;
  memcpy(dl + Frame_long_data_ofs, &frame_data, sizeof(frame_data));
  /* num_live (Frame_long_num_live_ofs) stays 0. */
  uint32_t num_code_ptr = 1; /* number of code-ptr slots */
  uint32_t slot_ofs = 0;     /* stack offset 0 from sp */
  memcpy(dl + Frame_long_live_ofs, &num_code_ptr, sizeof(num_code_ptr));
  memcpy(dl + Frame_long_live_ofs + sizeof(uint32_t), &slot_ofs,
         sizeof(slot_ofs));

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
