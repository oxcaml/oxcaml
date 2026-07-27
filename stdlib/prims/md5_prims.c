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

#include <string.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/md5.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/io.h"
#include "caml/reverse.h"

/* MD5 message digest */

CAMLprim value caml_md5_string(value str, value ofs, value len)
{
  struct MD5Context ctx;
  value res;
  caml_MD5Init(&ctx);
  caml_MD5Update(&ctx, &Byte_u(str, Long_val(ofs)), Long_val(len));
  res = caml_alloc_string(16);
  caml_MD5Final(&Byte_u(res, 0), &ctx);
  return res;
}

CAMLprim value caml_md5_bytes(value b, value ofs, value len)
{
  return caml_md5_string(b, ofs, len);
}

CAMLexport value caml_md5_channel(struct channel *chan, intnat toread)
{
  CAMLparam0();
  struct MD5Context ctx;
  value res;
  intnat read;
  char buffer[4096];

  caml_channel_lock(chan);
  caml_MD5Init(&ctx);
  if (toread < 0){
    while (1){
      read = caml_getblock (chan, buffer, sizeof(buffer));
      if (read == 0) break;
      caml_MD5Update (&ctx, (unsigned char *) buffer, read);
    }
  }else{
    while (toread > 0) {
      read = caml_getblock(chan, buffer,
                           toread > sizeof(buffer) ? sizeof(buffer) : toread);
      if (read == 0) caml_raise_end_of_file();
      caml_MD5Update(&ctx, (unsigned char *) buffer, read);
      toread -= read;
    }
  }
  res = caml_alloc_string(16);
  caml_MD5Final(&Byte_u(res, 0), &ctx);
  caml_channel_unlock(chan);
  CAMLreturn (res);
}

CAMLprim value caml_md5_chan(value vchan, value len)
{
   CAMLparam2 (vchan, len);
   CAMLreturn (caml_md5_channel(Channel(vchan), Long_val(len)));
}
