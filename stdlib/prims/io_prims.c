/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* C implementations of the Stdlib channel functions (the [caml_ml_*]
   primitives).  The underlying buffered I/O machinery is in
   runtime/io.c. */

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <string.h>
#include <sys/types.h>
#include "caml/config.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#ifdef __CYGWIN__
#include </usr/include/io.h>
#endif
#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/signals.h"
#include "caml/sys.h"

#if defined(_WIN32)
#include <io.h>
#endif

/* Buffer used to mark closed channels; see the channel status comment
   in runtime/io.c. */
static char dummy_buff[1];

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif
#if defined(_WIN32)
#define lseek _lseeki64
#endif

/* Channel positioning and line scanning.  Only the primitives below
   (and external C bindings) use these; the buffered-I/O engine itself
   is in runtime/io.c. */

CAMLexport file_offset caml_channel_size(struct channel *channel)
{
  file_offset here, end;
  int fd;

  caml_channel_check_pending(channel);
  /* We extract data from [channel] before dropping the OCaml lock, in case
     someone else touches the block. */
  fd = channel->fd;
  here = channel->flags & CHANNEL_TEXT_MODE ? -1 : channel->offset;
  caml_enter_blocking_section_no_pending();
  if (here == -1) {
    here = lseek(fd, 0, SEEK_CUR);
    if (here == -1) goto error;
  }
  end = lseek(fd, 0, SEEK_END);
  if (end == -1) goto error;
  if (lseek(fd, here, SEEK_SET) != here) goto error;
  caml_leave_blocking_section();
  return end;
 error:
  caml_leave_blocking_section();
  caml_sys_error(NO_ARG);
}

CAMLexport void caml_seek_out(struct channel *channel, file_offset dest)
{
  file_offset res;
  caml_flush(channel);
  caml_enter_blocking_section_no_pending();
  res = lseek(channel->fd, dest, SEEK_SET);
  if (res < 0 || res != dest) {
    caml_leave_blocking_section();
    caml_sys_error(NO_ARG);
  }
  caml_leave_blocking_section();
  channel->offset = dest;
}

CAMLexport file_offset caml_pos_out(struct channel *channel)
{
  return channel->offset + (file_offset)(channel->curr - channel->buff);
}

CAMLexport void caml_seek_in(struct channel *channel, file_offset dest)
{
  file_offset res;
  if (dest >= channel->offset - (channel->max - channel->buff)
      && dest <= channel->offset
      && (channel->flags & CHANNEL_TEXT_MODE) == 0) {
    channel->curr = channel->max - (channel->offset - dest);
  } else {
    caml_enter_blocking_section_no_pending();
    res = lseek(channel->fd, dest, SEEK_SET);
    if (res < 0 || res != dest) {
      caml_leave_blocking_section();
      caml_sys_error(NO_ARG);
    }
    caml_leave_blocking_section();
    channel->offset = dest;
    channel->curr = channel->max = channel->buff;
  }
}

CAMLexport file_offset caml_pos_in(struct channel *channel)
{
  return channel->offset - (file_offset)(channel->max - channel->curr);
}

intnat caml_input_scan_line(struct channel *channel)
{
  char * p;
  int n;
 again:
  caml_channel_check_pending(channel);
  p = channel->curr;
  do {
    if (p >= channel->max) {
      /* No more characters available in the buffer */
      if (channel->curr > channel->buff) {
        /* Try to make some room in the buffer by shifting the unread
           portion at the beginning */
        memmove(channel->buff, channel->curr, channel->max - channel->curr);
        n = channel->curr - channel->buff;
        channel->curr -= n;
        channel->max -= n;
        p -= n;
      }
      if (channel->max >= channel->end) {
        /* Buffer is full, no room to read more characters from the input.
           Return the number of characters in the buffer, with negative
           sign to indicate that no newline was encountered. */
        return -(channel->max - channel->curr);
      }
      /* Fill the buffer as much as possible */
      n = caml_read_fd(channel->fd, channel->flags,
                       channel->max, channel->end - channel->max);
      if (n == -1) {
        if (errno == EINTR) goto again; else caml_sys_io_error(NO_ARG);
      }
      else if (n == 0) {
        /* End-of-file encountered. Return the number of characters in the
           buffer, with negative sign since we haven't encountered
           a newline. */
        return -(channel->max - channel->curr);
      }
      channel->offset += n;
      channel->max += n;
    }
  } while (*p++ != '\n');
  /* Found a newline. Return the length of the line, newline included. */
  return (p - channel->curr);
}

CAMLprim value caml_ml_open_descriptor_in_with_flags(int fd, int flags)
{
  struct channel * chan = caml_open_descriptor_in(fd);
  caml_channel_register(chan, flags);
  return caml_alloc_channel(chan);
}

CAMLprim value caml_ml_open_descriptor_in(value fd) {
  return caml_ml_open_descriptor_in_with_flags(Int_val(fd), 0);
}

CAMLprim value caml_ml_open_descriptor_out_with_flags(int fd, int flags)
{
  struct channel * chan = caml_open_descriptor_out(fd);
  caml_channel_register(chan, flags);
  return caml_alloc_channel(chan);
}

CAMLprim value caml_ml_open_descriptor_out(value fd) {
  return caml_ml_open_descriptor_out_with_flags(Int_val(fd), 0);
}

CAMLprim value caml_ml_set_channel_name(value vchannel, value vname)
{
  CAMLparam2(vchannel, vname);
  struct channel * channel = Channel(vchannel);
  caml_channel_lock(channel);
  caml_stat_free(channel->name);
  if (caml_string_length(vname) > 0)
    channel->name = caml_stat_strdup(String_val(vname));
  else
    channel->name = NULL;
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

struct channel_list {
  struct channel* channel;
  struct channel_list* next;
};

CAMLprim value caml_ml_out_channels_list (value unit)
{
  CAMLparam0 ();
  CAMLlocal3 (res, tail, chan);
  struct channel_list *channel_list = NULL, *cl_tmp;
  mlsize_t num_channels = 0;

  /* We cannot use [caml_plat_lock_non_blocking] inside
     [caml_finalize_channel], so instead we must be careful here not
     to trigger a STW while holding [caml_all_opened_channels_mutex].
     This is why we allocate a temporary list with malloc. This is
     unsatisfactory because the critical section inside
     caml_ml_out_channels_list is not guaranteed to be short.*/
  caml_plat_lock_blocking(&caml_all_opened_channels_mutex);
  for (struct channel *channel = caml_all_opened_channels;
       channel != NULL;
       channel = channel->next) {
    CAMLassert(channel->flags & CHANNEL_FLAG_MANAGED_BY_GC);
    /* Unclosed output channels are exactly the ones with max == NULL */
    if (channel->max == NULL) {
      /* refcount is incremented here to keep the channel alive */
      channel->refcount ++;
      num_channels++;
      cl_tmp = caml_stat_alloc_noexc (sizeof(struct channel_list));
      if (cl_tmp == NULL)
        caml_fatal_error ("caml_ml_out_channels_list: out of memory");
      cl_tmp->channel = channel;
      cl_tmp->next = channel_list;
      channel_list = cl_tmp;
    }
  }
  caml_plat_unlock (&caml_all_opened_channels_mutex);

  res = Val_emptylist;
  cl_tmp = NULL;
  for (mlsize_t i = 0; i < num_channels; i++) {
    chan = caml_alloc_channel (channel_list->channel);
    tail = res;
    res = caml_alloc_2(Tag_cons, chan, tail);
    cl_tmp = channel_list;
    channel_list = channel_list->next;
    caml_stat_free (cl_tmp);
  }
  CAMLreturn (res);
}

CAMLprim value caml_channel_descriptor(value vchannel)
{
  int fd = Channel(vchannel)->fd;
  if (fd == -1) { errno = EBADF; caml_sys_error(NO_ARG); }
  return Val_int(fd);
}

CAMLprim value caml_ml_close_channel(value vchannel)
{
  CAMLparam1(vchannel);
  int result;
  int fd;

  /* For output channels, must have flushed before */
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  /* If already closed, we are done */
  if (channel->fd != -1) {
    fd = channel->fd;
    channel->fd = -1;
    caml_stat_free(channel->buff);
    channel->buff = dummy_buff;
    channel->end = dummy_buff + 1;
    /* Make sure that every read or write on the channel will cause an
       immediate caml_flush_partial or caml_refill, thus raising a
       Sys_error exception, and that a non-zero seek cannot stay
       within the buffer.
    */
    if (channel->max == NULL){
      /* closed output channel: full buffer with max != NULL */
      channel->curr = channel->max = channel->end;
    }else{
      /* closed input channel: empty buffer */
      channel->curr = channel->max = channel->buff;
    }
    caml_enter_blocking_section_no_pending();
    result = close(fd);
    caml_leave_blocking_section();

    if (result == -1) caml_sys_error (NO_ARG);
  }
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

/* EOVERFLOW is the Unix98 error indicating that a file position or file
   size is not representable.
   ERANGE is the ANSI C error indicating that some argument to some
   function is out of range.  This is less precise than EOVERFLOW,
   but guaranteed to be defined on all ANSI C environments. */
#ifndef EOVERFLOW
#define EOVERFLOW ERANGE
#endif

static file_offset ml_channel_size(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  file_offset size;

  caml_channel_lock(channel);
  size = caml_channel_size(Channel(vchannel));
  caml_channel_unlock(channel);
  CAMLreturnT(file_offset, size);
}

CAMLprim value caml_ml_channel_size(value vchannel)
{
  file_offset size = ml_channel_size(vchannel);
  if (size > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  return Val_long(size);
}

CAMLprim value caml_ml_channel_size_64(value vchannel)
{
  return Val_file_offset(ml_channel_size(vchannel));
}

CAMLprim value caml_ml_set_binary_mode(value vchannel, value mode)
{
  CAMLparam2(vchannel, mode);
#if defined(_WIN32) || defined(__CYGWIN__)
  struct channel * channel = Channel(vchannel);
  caml_channel_lock(channel);
#if defined(_WIN32)
  /* The implementation of [caml_read_fd] and [caml_write_fd] in win32.c
     doesn't support socket I/O with CRLF conversion. */
  if ((channel->flags & CHANNEL_FLAG_FROM_SOCKET) != 0
      && ! Bool_val(mode)) {
    errno = EINVAL;
    caml_sys_error(NO_ARG);
  }
#endif
  if (setmode(channel->fd, Bool_val(mode) ? O_BINARY : O_TEXT) == -1)
    caml_sys_error(NO_ARG);
  if (Bool_val(mode))
    channel->flags &= ~CHANNEL_TEXT_MODE;
  else
    channel->flags |= CHANNEL_TEXT_MODE;
  caml_channel_unlock(channel);
#endif
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_is_binary_mode(value vchannel)
{
  return Val_bool(caml_channel_binary_mode(Channel(vchannel)));
}

/*
   If the channel is closed, DO NOT raise a "bad file descriptor"
   exception, but do nothing.
   This is because some libraries will flush at exit, even on
   channels that may be closed.
*/

CAMLprim value caml_ml_flush(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_flush_noexc(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_set_buffered(value vchannel, value mode)
{
  CAMLparam2(vchannel, mode);
  struct channel * channel = Channel(vchannel);
  caml_channel_lock(channel);
  if (Bool_val(mode)) {
    channel->flags &= ~CHANNEL_FLAG_UNBUFFERED;
  } else {
    channel->flags |= CHANNEL_FLAG_UNBUFFERED;
    if (channel->fd != -1)
      caml_flush(channel);
  }
  caml_channel_unlock(channel);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ml_is_buffered(value vchannel)
{
  const struct channel * channel = Channel(vchannel);
  return Val_bool( ! (channel->flags & CHANNEL_FLAG_UNBUFFERED));
}

CAMLprim value caml_ml_output_char(value vchannel, value ch)
{
  CAMLparam2 (vchannel, ch);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_putch(channel, Long_val(ch));
  caml_flush_if_unbuffered(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_int(value vchannel, value w)
{
  CAMLparam2 (vchannel, w);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_putword(channel, (uint32_t) Long_val(w));
  caml_flush_if_unbuffered(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output_bytes(value vchannel, value buff, value start,
                              value length)
{
  CAMLparam4 (vchannel, buff, start, length);
  struct channel * channel = Channel(vchannel);
  intnat pos = Long_val(start);
  intnat len = Long_val(length);

  caml_channel_lock(channel);
    /* We cannot call caml_really_putblock here because buff may move
       during caml_write_fd */
    while (len > 0) {
      int written = caml_putblock(channel, &Byte(buff, pos), len);
      pos += written;
      len -= written;
    }
    caml_flush_if_unbuffered(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_output(value vchannel, value buff, value start,
                              value length)
{
  return caml_ml_output_bytes (vchannel, buff, start, length);
}

CAMLprim value caml_ml_output_bigarray(value vchannel, value vbuf,
                                       value vpos, value vlen)
{
  CAMLparam4(vchannel, vbuf, vpos, vlen);
  struct channel * channel = Channel(vchannel);
  intnat pos = Long_val(vpos);
  intnat len = Long_val(vlen);

  caml_channel_lock(channel);
  caml_really_putblock(channel, (char *)Caml_ba_data_val(vbuf) + pos, len);
  caml_channel_unlock(channel);

  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_out(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_seek_out(channel, Long_val(pos));
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_out_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_seek_out(channel, File_offset_val(pos));
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_pos_out(value vchannel)
{
  CAMLparam1 (vchannel);
  file_offset pos;
  struct channel *channel = Channel(vchannel);
  caml_channel_lock(channel);
  pos = caml_pos_out(channel);
  caml_channel_unlock(channel);
  if (pos > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  CAMLreturn (Val_long(pos));
}

CAMLprim value caml_ml_pos_out_64(value vchannel)
{
  CAMLparam1 (vchannel);
  file_offset pos;
  struct channel *channel = Channel(vchannel);
  caml_channel_lock(channel);
  pos = caml_pos_out(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_file_offset(pos));
}

CAMLprim value caml_ml_input_char(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  unsigned char c;

  caml_channel_lock(channel);
  c = caml_getch(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_long(c));
}

CAMLprim value caml_ml_input_int(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  intnat i;

  caml_channel_lock(channel);
  i = caml_getword(channel);
  caml_channel_unlock(channel);
#ifdef ARCH_SIXTYFOUR
  i = (i << 32) >> 32;          /* Force sign extension */
#endif
  CAMLreturn (Val_long(i));
}

CAMLprim value caml_ml_input(value vchannel, value buff, value vstart,
                             value vlength)
{
  CAMLparam4 (vchannel, buff, vstart, vlength);
  struct channel * channel = Channel(vchannel);
  intnat start, len;
  int n, avail, nread;

  caml_channel_lock(channel);
 again:
  caml_channel_check_pending(channel);
  /* We cannot call caml_getblock here because buff may move during
     caml_read_fd */
  start = Long_val(vstart);
  len = Long_val(vlength);
  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    memmove(&Byte(buff, start), channel->curr, n);
    channel->curr += n;
  } else if (avail > 0) {
    memmove(&Byte(buff, start), channel->curr, avail);
    channel->curr += avail;
    n = avail;
  } else {
    nread = caml_read_fd(channel->fd, channel->flags, channel->buff,
                         channel->end - channel->buff);
    if (nread == -1) {
      if (errno == EINTR) goto again; else caml_sys_io_error(NO_ARG);
    }
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    memmove(&Byte(buff, start), channel->buff, n);
    channel->curr = channel->buff + n;
  }
  caml_channel_unlock(channel);
  CAMLreturn (Val_long(n));
}

CAMLprim value caml_ml_input_bigarray(value vchannel, value vbuf,
                                      value vpos, value vlen)
{
  CAMLparam4(vchannel, vbuf, vpos, vlen);
  struct channel * channel = Channel(vchannel);
  intnat pos = Long_val(vpos);
  intnat len = Long_val(vlen);
  intnat n;

  caml_channel_lock(channel);
  n = caml_getblock(channel, (char *)Caml_ba_data_val(vbuf) + pos, len);
  caml_channel_unlock(channel);

  CAMLreturn (Val_long(n));
}

CAMLprim value caml_ml_seek_in(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_seek_in(channel, Long_val(pos));
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_seek_in_64(value vchannel, value pos)
{
  CAMLparam2 (vchannel, pos);
  struct channel * channel = Channel(vchannel);

  caml_channel_lock(channel);
  caml_seek_in(channel, File_offset_val(pos));
  caml_channel_unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_ml_pos_in(value vchannel)
{
  CAMLparam1 (vchannel);
  file_offset pos;
  struct channel *channel = Channel(vchannel);
  caml_channel_lock(channel);
  pos = caml_pos_in(channel);
  caml_channel_unlock(channel);
  if (pos > Max_long) { errno = EOVERFLOW; caml_sys_error(NO_ARG); }
  CAMLreturn (Val_long(pos));
}

CAMLprim value caml_ml_pos_in_64(value vchannel)
{
  CAMLparam1 (vchannel);
  file_offset pos;
  struct channel *channel = Channel(vchannel);
  caml_channel_lock(channel);
  pos = caml_pos_in(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_file_offset(pos));
}

CAMLprim value caml_ml_input_scan_line(value vchannel)
{
  CAMLparam1 (vchannel);
  struct channel * channel = Channel(vchannel);
  intnat res;

  caml_channel_lock(channel);
  res = caml_input_scan_line(channel);
  caml_channel_unlock(channel);
  CAMLreturn (Val_long(res));
}

CAMLprim value caml_terminfo_rows(value vchannel)
{
  return Val_int(caml_num_rows_fd(Channel(vchannel)->fd));
}
