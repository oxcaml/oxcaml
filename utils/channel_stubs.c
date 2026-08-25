/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                        Zesen Qian, Jane Street                         */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* C stubs linked into ocamlcommon itself (rather than the runtime), so
   that they are available when the compiler is built against the boot
   compiler's runtime. */

#define CAML_INTERNALS

#include <string.h>

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/io.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

/* Present in every runtime, but not exported by <caml/io.h>. */
extern intnat caml_input_scan_line(struct channel *channel);

/* Returns all complete lines currently available in the channel buffer,
   in order, taking the channel mutex only once. See misc.mli. */
CAMLprim value caml_oxcaml_input_lines_in_buffer(value vchannel)
{
  CAMLparam1(vchannel);
  CAMLlocal4(head, tail, cell, line);
  struct channel *channel = Channel(vchannel);
  intnat first;
  char *curr, *last_nl, *p;

  caml_channel_lock(channel);
  /* Refills the buffer (compacting as needed) until it contains a
     newline, is full, or the end of file is reached. */
  first = caml_input_scan_line(channel);
  if (first <= 0) {
    intnat remaining = -first;
    if (remaining == 0) {
      /* End of file with no pending bytes. */
      caml_channel_unlock(channel);
      CAMLreturn(Val_emptylist);
    }
    if (channel->max >= channel->end) {
      caml_channel_unlock(channel);
      caml_failwith(
        "input_lines_in_buffer: line longer than the channel buffer");
    }
    /* End of file: the remaining bytes form a final line with no
       terminating newline. */
    line = caml_alloc_initialized_string(remaining, channel->curr);
    channel->curr += remaining;
    cell = caml_alloc(2, Tag_cons);
    Store_field(cell, 0, line);
    Store_field(cell, 1, Val_emptylist);
    caml_channel_unlock(channel);
    CAMLreturn(cell);
  }
  /* At least one newline is in the buffer; find the last one. Pointers
     into the buffer are stable across allocations: the buffer lives in
     malloc'd memory and nothing below touches the channel. */
  curr = channel->curr;
  last_nl = channel->max;
  while (last_nl[-1] != '\n') last_nl--;
  last_nl--;
  head = Val_emptylist;
  tail = Val_emptylist;
  p = curr;
  while (p <= last_nl) {
    char *nl = memchr(p, '\n', last_nl - p + 1);
    line = caml_alloc_initialized_string(nl - p, p);
    cell = caml_alloc(2, Tag_cons);
    Store_field(cell, 0, line);
    Store_field(cell, 1, Val_emptylist);
    if (head == Val_emptylist) head = cell;
    else Store_field(tail, 1, cell);
    tail = cell;
    p = nl + 1;
  }
  channel->curr = p;
  caml_channel_unlock(channel);
  CAMLreturn(head);
}
