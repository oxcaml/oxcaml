#define CAML_INTERNALS

#include <stdint.h>

#include "caml/fail.h"
#include "caml/frame_descriptors.h"
#include "caml/mlvalues.h"

extern intnat *caml_frametable[];

/* Walk one frametable in the packed descriptor format (see
   runtime/caml/frame_descriptors.h): each descriptor is preceded by a
   ULEB128 return-address delta, except escaped (medium/long) descriptors,
   which begin directly with a zero (escape) byte where the delta would
   be. [*next] points at the next descriptor's delta byte; returns the
   [frame_descr *] for the descriptor body. */
static frame_descr *descr_at(const unsigned char **next)
{
  const unsigned char *p = *next;
  if (*p == FRAME_DELTA_ESCAPE) {
    /* Escaped descriptor: the descriptor pointer is the escape byte. */
    return (frame_descr *)p;
  }
  /* Short descriptor: skip the ULEB128 delta; the descriptor pointer is
     the byte after it (the size+flags byte). */
  while (*p & 0x80) p++;
  p++;
  return (frame_descr *)p;
}

static void check_code_ptr_live_ofs(frame_descr *d,
                                    struct frame_descr_decoded *dec)
{
  if (dec->return_to_C) return;
  if (!frame_has_code_ptr_slots(d)) return;

  const unsigned char *p = dec->code_ptr_slots;
  const unsigned char *end = dec->end;
  uint32_t n = dec->num_code_ptr_slots;
  size_t entry_size = dec->is_long ? sizeof(uint32_t) : sizeof(uint16_t);
  uint32_t frame_sz = dec->frame_size;

  if (p == NULL) caml_failwith("code-ptr flag set but no slots decoded");
  if (n > 4096) caml_failwith("code-ptr slot count too large");
  if (p + entry_size * n != end) caml_failwith("code-ptr slots OOB");

  for (uint32_t k = 0; k < n; k++) {
    uint32_t ofs = dec->is_long ? caml_read_unaligned_uint32(p)
                                : caml_read_unaligned_uint16(p);
    p += entry_size;
    if ((ofs & 1) == 0) {
      if ((ofs % sizeof(value)) != 0) caml_failwith("bad stack offset");
      if (ofs >= frame_sz) caml_failwith("stack offset beyond frame");
    } else {
      if ((ofs >> 1) > 1024) caml_failwith("reg index too large");
    }
  }
}

CAMLprim value caml_test_frametable_layout_sanity(value unit)
{
  (void)unit;

  for (int i = 0; caml_frametable[i] != 0; i++) {
    intnat *tbl = caml_frametable[i];
    intnat len = *tbl;
    const unsigned char *next = (const unsigned char *)(tbl + 1);
    for (intnat j = 0; j < len; j++) {
      frame_descr *d = descr_at(&next);
      struct frame_descr_decoded dec;
      caml_decode_frame_descr(d, &dec);
      if ((uintptr_t)dec.end <= (uintptr_t)next)
        caml_failwith("non-increasing");
      check_code_ptr_live_ofs(d, &dec);
      next = dec.end;
    }
  }

  return Val_unit;
}
