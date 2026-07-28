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

/* Buffered input/output. */

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <stdbool.h>
#include "caml/config.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#ifdef __CYGWIN__
#include </usr/include/io.h>
#endif
#include "caml/alloc.h"
#include "caml/camlatomic.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/signals.h"
#include "caml/sys.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#if defined(_WIN32)
#include <io.h>
#define lseek _lseeki64
#endif

/* Representation of channel status and direction:

   Open channels have fd >= 0 && buff != dummy_buff
     Open input channels have max != NULL
     Open output channels have max == NULL

   Closed channels have fd == -1 && buff == dummy_buff && end == dummy_buff+1
     Closed input channels have curr == max == buff (empty buffer)
     Closed output channels have curr == end (full buffer)
       and max == end (not NULL)

*/

/* Locking channels.

   All operations on channels first take the channel lock.

   Exceptions could abort an operation on a channel with its lock
   still held, leaving the channel in an inconsistent state.

   To avoid this, we store the channel currently being locked
   (or NULL) in (thread-local) global state. This can be used to
   define a [caml_channel_cleanup_on_raise] function that unlocks the
   currently locked channel (if any), which is then called by
   [caml_raise].
 */
static CAMLthread_local struct channel* last_channel_locked = NULL;

CAMLexport void caml_channel_lock(struct channel *chan)
{
  caml_plat_lock_non_blocking(&chan->mutex);
  last_channel_locked = chan;
}

CAMLexport void caml_channel_unlock(struct channel *chan)
{
  caml_plat_unlock(&chan->mutex);
  last_channel_locked = NULL;
}

CAMLexport void caml_channel_cleanup_on_raise(void)
{
  struct channel * chan = last_channel_locked;
  if (chan != NULL) caml_channel_unlock(chan);
}

/* List of channels opened from the OCaml side and managed by the GC */
CAMLexport struct channel * caml_all_opened_channels = NULL;

/* The mutex protecting the list above */
CAMLexport caml_plat_mutex
  caml_all_opened_channels_mutex = CAML_PLAT_MUTEX_INITIALIZER;

/* Basic functions over type struct channel *.
   These functions can be called directly from C.
   No locking is performed. */

/* Functions shared between input and output */

CAMLexport void caml_channel_check_pending(struct channel *channel)
{
  if (caml_check_pending_actions()) {
    /* Temporarily unlock the channel, to ensure locks are not held
       while any signal handlers (or finalisers, etc) are running.
       Don't do this for channels allocated and used from C,
       as their locks may or may not be taken depending on the
       usage pattern in the C code. */
    if (channel->flags & CHANNEL_FLAG_MANAGED_BY_GC)
      caml_channel_unlock(channel);
    caml_process_pending_actions();
    if (channel->flags & CHANNEL_FLAG_MANAGED_BY_GC)
      caml_channel_lock(channel);
  }
}

Caml_inline int descriptor_is_in_binary_mode(int fd)
{
#if defined(_WIN32) || defined(__CYGWIN__)
  int oldmode = setmode(fd, O_TEXT);
  if (oldmode != -1 && oldmode != O_TEXT) setmode(fd, oldmode);
  return oldmode == O_BINARY;
#else
  return 1;
#endif
}

static void link_channel (struct channel* channel)
{
  channel->next = caml_all_opened_channels;
  CAMLassert(channel->prev == NULL);
  if (caml_all_opened_channels != NULL)
    caml_all_opened_channels->prev = channel;
  caml_all_opened_channels = channel;
}

static void unlink_channel(struct channel *channel)
{
  if (channel->prev == NULL) {
    CAMLassert (channel == caml_all_opened_channels);
    caml_all_opened_channels = caml_all_opened_channels->next;
    if (caml_all_opened_channels != NULL)
      caml_all_opened_channels->prev = NULL;
  } else {
    channel->prev->next = channel->next;
    if (channel->next != NULL) channel->next->prev = channel->prev;
  }
  channel->next = NULL;
  channel->prev = NULL;
}

/* Mark [chan] as managed by the GC, with [flags] added to its flags,
   and put it on the list of open channels. */
CAMLexport void caml_channel_register(struct channel *chan, int flags)
{
  chan->flags |= flags | CHANNEL_FLAG_MANAGED_BY_GC;
  chan->refcount = 1;
  caml_plat_lock_blocking(&caml_all_opened_channels_mutex);
  link_channel (chan);
  caml_plat_unlock (&caml_all_opened_channels_mutex);
}

CAMLexport struct channel * caml_open_descriptor_in(int fd)
{
  struct channel * channel;

  channel = (struct channel *) caml_stat_alloc(sizeof(struct channel));
  channel->buff = (char *) caml_stat_alloc_noexc(IO_BUFFER_SIZE);
  if (channel->buff == NULL){
    caml_stat_free(channel);
    caml_raise_out_of_memory();
  }
  channel->fd = fd;
  caml_enter_blocking_section_no_pending();
  channel->offset = lseek(fd, 0, SEEK_CUR);
  caml_leave_blocking_section();
  channel->curr = channel->max = channel->buff;
  channel->end = channel->buff + IO_BUFFER_SIZE;
  caml_plat_mutex_init(&channel->mutex);
  channel->refcount = 0;
  channel->prev = NULL;
  channel->next = NULL;
  channel->name = NULL;
  channel->flags = descriptor_is_in_binary_mode(fd) ? 0 : CHANNEL_TEXT_MODE;
  return channel;
}

CAMLexport struct channel * caml_open_descriptor_out(int fd)
{
  struct channel * channel;

  channel = caml_open_descriptor_in(fd);
  channel->max = NULL;
  return channel;
}

CAMLexport void caml_close_channel(struct channel *channel)
{
  CAMLassert((channel->flags & CHANNEL_FLAG_MANAGED_BY_GC) == 0);
  close(channel->fd);
  caml_plat_mutex_free(&channel->mutex);
  caml_stat_free(channel->name);
  caml_stat_free(channel->buff);
  caml_stat_free(channel);
}

CAMLexport int caml_channel_binary_mode(struct channel *channel)
{
  return channel->flags & CHANNEL_TEXT_MODE ? 0 : 1;
}

/* Output */

/* Attempt to flush the buffer. This will make room in the buffer for
   at least one character. Returns true if the buffer is empty at the
   end of the flush, or false if some data remains in the buffer.
 */

static bool flush_partial(struct channel *channel, bool exn_if_closed)
{
  int towrite, written;
 again:
  caml_channel_check_pending(channel);
  if (channel->fd == -1) {
    if (exn_if_closed) {
      errno = EBADF;
      caml_sys_io_error(NO_ARG);
    } else {
      return true;
    }
  }

  towrite = channel->curr - channel->buff;
  CAMLassert (towrite >= 0);
  if (towrite > 0) {
    written = caml_write_fd(channel->fd, channel->flags,
                            channel->buff, towrite);
    if (written == -1) {
      if (errno == EINTR) goto again;
      if (errno == EBADF || errno == EPIPE || errno == ECONNRESET) {
        /* This is a permanent failure: retrying the flush later will
           not make it go away. If the channel is not closed, discard
           the buffered data, so that a subsequent close will succeed,
           or the finalizer can reclaim the channel. */
        if (channel->fd != -1) channel->curr = channel->buff;
      }
      caml_sys_io_error(NO_ARG);
    }
    channel->offset += written;
    if (written < towrite)
      memmove(channel->buff, channel->buff + written, towrite - written);
    channel->curr -= written;
  }
  return (channel->curr == channel->buff);
}

CAMLexport int caml_flush_partial(struct channel *channel)
{
  return flush_partial(channel, true);
}

/* Flush completely the buffer. */

CAMLexport void caml_flush(struct channel *channel)
{
  while (! caml_flush_partial(channel)) /*nothing*/;
}

/* Like [caml_flush], but does nothing rather than raising if the
   channel is closed. */
CAMLexport void caml_flush_noexc(struct channel *channel)
{
  while (! flush_partial(channel, false)) continue;
}

CAMLexport void caml_flush_if_unbuffered(struct channel *channel)
{
  if (channel->flags & CHANNEL_FLAG_UNBUFFERED) caml_flush(channel);
}

/* Output data */

#define Putch(channel, ch) do{                                            \
  if ((channel)->curr >= (channel)->end) caml_flush_partial(channel);     \
  *((channel)->curr)++ = (ch);                                            \
}while(0)

CAMLexport void caml_putch(struct channel *channel, int ch)
{
  Putch(channel, ch);
}

CAMLexport void caml_putword(struct channel *channel, uint32_t w)
{
  if (! caml_channel_binary_mode(channel))
    caml_failwith("output_binary_int: not a binary channel");
  caml_putch(channel, w >> 24);
  caml_putch(channel, w >> 16);
  caml_putch(channel, w >> 8);
  caml_putch(channel, w);
}

CAMLexport int caml_putblock(struct channel *channel, const char *p, intnat len)
{
  int n, free;

  n = len >= INT_MAX ? INT_MAX : (int) len;
  free = channel->end - channel->curr;
  if (n < free) {
    /* Write request small enough to fit in buffer: transfer to buffer. */
    memmove(channel->curr, p, n);
    channel->curr += n;
    return n;
  } else {
    /* Write request overflows buffer (or just fills it up): transfer whatever
       fits to buffer and write the buffer */
    memmove(channel->curr, p, free);
    channel->curr = channel->end;
    caml_flush_partial(channel);
    return free;
  }
}

CAMLexport void caml_really_putblock(struct channel *channel,
                                     const char *p, intnat len)
{
  int written;
  while (len > 0) {
    written = caml_putblock(channel, p, len);
    p += written;
    len -= written;
  }
}

/* Input */

CAMLexport unsigned char caml_refill(struct channel *channel)
{
  int n;
 again:
  caml_channel_check_pending(channel);
  n = caml_read_fd(channel->fd, channel->flags,
                   channel->buff, channel->end - channel->buff);
  if (n == -1) {
    if (errno == EINTR) goto again; else caml_sys_io_error(NO_ARG);
  } else if (n == 0) {
    caml_raise_end_of_file();
  }
  channel->offset += n;
  channel->max = channel->buff + n;
  channel->curr = channel->buff + 1;
  return (unsigned char)(channel->buff[0]);
}

#define Getch(channel)                                                      \
  ((channel)->curr >= (channel)->max                                        \
   ? caml_refill(channel)                                                   \
   : (unsigned char) *((channel)->curr)++)

CAMLexport unsigned char caml_getch(struct channel *channel)
{
  return Getch(channel);
}

CAMLexport uint32_t caml_getword(struct channel *channel)
{
  uint32_t res;

  if (! caml_channel_binary_mode(channel))
    caml_failwith("input_binary_int: not a binary channel");
  res = 0;
  for (int i = 0; i < 4; i++) {
    res = (res << 8) + Getch(channel);
  }
  return res;
}

CAMLexport int caml_getblock(struct channel *channel, char *p, intnat len)
{
  int n, avail, nread;
 again:
  caml_channel_check_pending(channel);
  n = len >= INT_MAX ? INT_MAX : (int) len;
  avail = channel->max - channel->curr;
  if (n <= avail) {
    memmove(p, channel->curr, n);
    channel->curr += n;
    return n;
  } else if (avail > 0) {
    memmove(p, channel->curr, avail);
    channel->curr += avail;
    return avail;
  } else {
    nread = caml_read_fd(channel->fd, channel->flags, channel->buff,
                         channel->end - channel->buff);
    if (nread == -1) {
      if (errno == EINTR) goto again; else caml_sys_io_error(NO_ARG);
    }
    channel->offset += nread;
    channel->max = channel->buff + nread;
    if (n > nread) n = nread;
    memmove(p, channel->buff, n);
    channel->curr = channel->buff + n;
    return n;
  }
}

/* Returns the number of bytes read. */
CAMLexport intnat caml_really_getblock(struct channel *chan, char *p, intnat n)
{
  intnat k = n;
  int r;
  while (k > 0) {
    r = caml_getblock(chan, p, k);
    if (r == 0) break;
    p += r;
    k -= r;
  }
  return n - k;
}

/* OCaml entry points for the I/O functions.  Wrap struct channel *
   objects into a heap-allocated object.  Perform locking
   and unlocking around the I/O operations. */

void caml_finalize_channel(value vchan)
{
  struct channel * chan = Channel(vchan);
  int notflushed = 0;
  if ((chan->flags & CHANNEL_FLAG_MANAGED_BY_GC) == 0) return;
  /* Check for channels that have not been closed explicitly. */
  if (chan->fd != -1 && chan->name && caml_runtime_warnings_active())
    fprintf(stderr,
            "[ocaml] channel opened on file '%s' dies without being closed\n",
            chan->name);
  if (chan->max == NULL && chan->curr != chan->buff) {
    /* This is an unclosed out channel (chan->max == NULL) with a
       non-empty buffer: keep it around so the OCaml [at_exit] function
       gets a chance to flush it.  We would want to simply flush the
       channel now, but (i) flushing can raise exceptions, and (ii) it
       is potentially a blocking operation.  Both are forbidden in a
       finalization function.
       Refs: https://github.com/ocaml/ocaml/issues/6902
             https://github.com/ocaml/ocaml/pull/210
    */
    if (chan->name && caml_runtime_warnings_active())
      fprintf(stderr,
              "[ocaml] (moreover, it has unflushed data)\n");
    notflushed = 1;
  }
  /* Don't run concurrently with caml_ml_out_channels_list that may resurrect
     a dead channel . */
  caml_plat_lock_blocking(&caml_all_opened_channels_mutex);
  chan->refcount --;
  if (chan->refcount > 0 || notflushed) {
    /* We need to keep the channel around, either because it is being
       added to the list returned by caml_ml_out_channels_list,
       or because it contains unflushed data. */
    caml_plat_unlock (&caml_all_opened_channels_mutex);
    return;
  }
  unlink_channel(chan);
  caml_plat_unlock (&caml_all_opened_channels_mutex);
  caml_plat_mutex_free(&chan->mutex);
  caml_stat_free(chan->name);
  if (chan->fd != -1) caml_stat_free(chan->buff);
  caml_stat_free(chan);
}

static int compare_channel(value vchan1, value vchan2)
{
  const struct channel * chan1 = Channel(vchan1);
  const struct channel * chan2 = Channel(vchan2);
  return (chan1 == chan2) ? 0 : (chan1 < chan2) ? -1 : 1;
}

static intnat hash_channel(value vchan)
{
  return (intnat) (Channel(vchan));
}

static struct custom_operations channel_operations = {
  "_chan",
  caml_finalize_channel,
  compare_channel,
  hash_channel,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLexport value caml_alloc_channel(struct channel *chan)
{
  value res;
  res = caml_alloc_custom(&channel_operations, sizeof(struct channel *), 0, 1);
  Channel(res) = chan;
  return res;
}
