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
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include "caml/unixsupport.h"
#include <errno.h>
#include <stdio.h>
#include <grp.h>

static value alloc_group_entry(struct group *entry)
{
  CAMLparam0();
  CAMLlocal3(name, pass, mem);
  value res;

  name = caml_copy_string(entry->gr_name);
  /* on some platforms, namely Android, gr_passwd can be NULL,
     hence this workaround */
  pass = caml_copy_string(entry->gr_passwd ? entry->gr_passwd : "");
  mem = caml_copy_string_array((const char**)entry->gr_mem);
  res = caml_alloc_small(4, 0);
  Field(res,0) = name;
  Field(res,1) = pass;
  Field(res,2) = Val_int(entry->gr_gid);
  Field(res,3) = mem;
  CAMLreturn(res);
}

#ifdef HAS_GETGRNAM_R
CAMLprim value caml_unix_getgrnam(value name)
{
  struct group * entry, entry_val;
  size_t len;
  char *buf;
  int error;
  value result;
  if (! caml_string_is_c_safe(name)) caml_raise_not_found();
  len = 1024;
  while (1) {
    buf = malloc(len);
    if (NULL == buf)
      caml_unix_error(ENOMEM, "getgrnam", Nothing);
    error = getgrnam_r(String_val(name), &entry_val, buf, len, &entry);
    if (0 == error) {
      if (NULL != entry) {
        result = alloc_group_entry(entry);
        free(buf);
        return result;
      }
      free(buf);
      caml_raise_not_found();
    }
    free(buf);
    if (ERANGE != error)
      caml_unix_error(error, "getgrnam", Nothing);
    len = len * 2;
  }
}
#else
CAMLprim value caml_unix_getgrnam(value name)
{
  struct group * entry;
  if (! caml_string_is_c_safe(name)) caml_raise_not_found();
  errno = 0;
  entry = getgrnam(String_val(name));
  if (entry == NULL) {
    if (errno == EINTR) {
      caml_uerror("getgrnam", Nothing);
    } else {
      caml_raise_not_found();
    }
  }
  return alloc_group_entry(entry);
}
#endif

#ifdef HAS_GETGRGID_R
CAMLprim value caml_unix_getgrgid(value gid)
{
  struct group * entry, entry_val;
  size_t len;
  char *buf;
  int error;
  value result;
  len = 1024;
  while (1) {
    buf = malloc(len);
    if (NULL == buf)
      caml_unix_error(ENOMEM, "getgrgid", Nothing);
    error = getgrgid_r(Int_val(gid), &entry_val, buf, len, &entry);
    if (0 == error) {
      if (NULL != entry) {
        result = alloc_group_entry(entry);
        free(buf);
        return result;
      }
      free(buf);
      caml_raise_not_found();
    }
    free(buf);
    if (ERANGE != error)
      caml_unix_error(error, "getgrgid", Nothing);
    len = len * 2;
  }
}
#else
CAMLprim value caml_unix_getgrgid(value gid)
{
  struct group * entry;
  errno = 0;
  entry = getgrgid(Int_val(gid));
  if (entry == NULL) {
    if (errno == EINTR) {
      caml_uerror("getgrgid", Nothing);
    } else {
      caml_raise_not_found();
    }
  }
  return alloc_group_entry(entry);
}
#endif
