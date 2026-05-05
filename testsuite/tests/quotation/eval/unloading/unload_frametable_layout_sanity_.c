#define CAML_INTERNALS

#include <stdint.h>

#include "caml/fail.h"
#include "caml/frame_descriptors.h"
#include "caml/mlvalues.h"

extern intnat *caml_frametable[];

static frame_descr *next_frame_descr_test(frame_descr *d)
{
  unsigned char num_allocs = 0, *p;

  if (!frame_return_to_C(d)) {
    p = frame_end_of_live_ofs(d);

    if (frame_has_allocs(d)) {
      num_allocs = *p;
      p += num_allocs + 1;
    }

    if (frame_has_debug(d)) {
      p = Align_to(p, uint32_t);
      p += sizeof(uint32_t) * (frame_has_allocs(d) ? num_allocs : 1);
    }

    if (frame_has_code_ptr_slots(d)) {
      if (frame_is_long(d)) {
        p = Align_to(p, uint32_t);
        uint32_t n = *(uint32_t *)p;
        p += sizeof(uint32_t) * (1 + n);
      } else {
        p = Align_to(p, uint16_t);
        uint16_t n = *(uint16_t *)p;
        p += sizeof(uint16_t) * (1 + n);
      }
    }

    p = Align_to(p, void *);
    return (frame_descr *)p;
  } else {
    if (d->num_live != 0) caml_failwith("return-to-C frame has live slots");
    p = (unsigned char *)&d->live_ofs[0];
    p = Align_to(p, void *);
    return (frame_descr *)p;
  }
}

static void check_code_ptr_live_ofs(frame_descr *d, frame_descr *next)
{
  if (frame_return_to_C(d)) return;
  if (!frame_has_code_ptr_slots(d)) return;

  unsigned char *p = frame_end_of_live_ofs(d);
  unsigned char *end = (unsigned char *)next;
  unsigned char num_allocs = 0;

  if (frame_has_allocs(d)) {
    if (p >= end) caml_failwith("allocs header out of bounds");
    num_allocs = *p;
    p += num_allocs + 1;
  }

  if (frame_has_debug(d)) {
    p = Align_to(p, uint32_t);
    size_t bytes = sizeof(uint32_t) * (frame_has_allocs(d) ? num_allocs : 1);
    if (p + bytes > end) caml_failwith("debug info out of bounds");
    p += bytes;
  }

  if (frame_is_long(d)) {
    p = Align_to(p, uint32_t);
    if (p + sizeof(uint32_t) > end) caml_failwith("code-ptr header OOB");
    uint32_t n = *(uint32_t *)p;
    p += sizeof(uint32_t);
    if (n > 4096) caml_failwith("code-ptr slot count too large");
    if (p + sizeof(uint32_t) * n > end) caml_failwith("code-ptr slots OOB");
    uint32_t *q = (uint32_t *)p;
    uint32_t frame_sz = frame_size(d);
    for (uint32_t k = 0; k < n; k++) {
      uint32_t ofs = q[k];
      if ((ofs & 1) == 0) {
        if ((ofs % sizeof(value)) != 0) caml_failwith("bad stack offset");
        if (ofs >= frame_sz) caml_failwith("stack offset beyond frame");
      } else {
        if ((ofs >> 1) > 1024) caml_failwith("reg index too large");
      }
    }
  } else {
    p = Align_to(p, uint16_t);
    if (p + sizeof(uint16_t) > end) caml_failwith("code-ptr header OOB");
    uint16_t n = *(uint16_t *)p;
    p += sizeof(uint16_t);
    if (n > 4096) caml_failwith("code-ptr slot count too large");
    if (p + sizeof(uint16_t) * n > end) caml_failwith("code-ptr slots OOB");
    uint16_t *q = (uint16_t *)p;
    uint32_t frame_sz = frame_size(d);
    for (uint16_t k = 0; k < n; k++) {
      uint16_t ofs = q[k];
      if ((ofs & 1) == 0) {
        if ((ofs % sizeof(value)) != 0) caml_failwith("bad stack offset");
        if (ofs >= frame_sz) caml_failwith("stack offset beyond frame");
      } else {
        if ((ofs >> 1) > 1024) caml_failwith("reg index too large");
      }
    }
  }
}

CAMLprim value caml_test_frametable_layout_sanity(value unit)
{
  (void)unit;

  for (int i = 0; caml_frametable[i] != 0; i++) {
    intnat *tbl = caml_frametable[i];
    intnat len = *tbl;
    frame_descr *d = (frame_descr *)(tbl + 1);
    for (intnat j = 0; j < len; j++) {
      frame_descr *next = next_frame_descr_test(d);
      if ((uintptr_t)next <= (uintptr_t)d) caml_failwith("non-increasing");
      check_code_ptr_live_ofs(d, next);
      d = next;
    }
  }

  return Val_unit;
}
