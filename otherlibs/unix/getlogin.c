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

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "caml/unixsupport.h"
#include <errno.h>

extern char * getlogin(void);

CAMLprim value caml_unix_getlogin(value unit)
{
#ifdef HAS_GETLOGIN_R
  size_t len;
  char *buf;
  int error;
  value result;
  len = 256;
  while (1) {
    buf = malloc(len);
    if (NULL == buf)
      caml_unix_error(ENOMEM, "getlogin", Nothing);
    error = getlogin_r(buf, len);
    if (0 == error) {
      result = caml_copy_string(buf);
      free(buf);
      return result;
    }
    free(buf);
    if (ERANGE != error)
      caml_unix_error(error, "getlogin", Nothing);
    len = len * 2;
  }
#else
  char * name;
  name = getlogin();
  if (name == NULL) caml_unix_error(ENOENT, "getlogin", Nothing);
  return caml_copy_string(name);
#endif
}
