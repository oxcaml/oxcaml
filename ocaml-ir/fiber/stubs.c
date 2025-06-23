/* Extracted from Core_unix */
/* Core_unix support functions written in C. */

#define _GNU_SOURCE

#include <string.h>
#include <pthread.h>
/* Darwin needs this to be included before if.h*/
#if defined(__APPLE__)
#define _POSIX_SOURCE
#include <sys/socket.h>
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#include <sys/socket.h>
#endif
#include <sys/uio.h>
#include <sys/utsname.h>
#include <sys/file.h>
#include <pwd.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <net/if.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <grp.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fnmatch.h>
#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <unistd.h>
#include <sched.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <math.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <ifaddrs.h>

/* makedev */
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
/* The BSDs expose the definition for this macro via <sys/types.h>. */
#else
#include <sys/sysmacros.h>
#endif

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define stat64 stat
#define lstat64 lstat
#define fstat64 fstat
#endif

#include "caml/mlvalues.h"
#include "caml/signals.h"
#include "caml/unixsupport.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
/* #include "unix_utils.h" */

/* Pathname resolution */

/* Seems like a sane approach to getting a reasonable bound for the
   maximum path length */
#ifdef PATH_MAX
#define OCAML_IR_PATH_MAX ((PATH_MAX <= 0 || PATH_MAX > 65536) ? 65536 : PATH_MAX)
#else
#define OCAML_IR_PATH_MAX (65536)
#endif

#ifdef __GLIBC__
CAMLprim value ocaml_ir_realpath(value v_path)
{
  const char *path = String_val(v_path);
  char *res = realpath(path, NULL);
  if (res == NULL) uerror("realpath", v_path);
  else {
    value v_res = caml_copy_string(res);
    free(res);
    return v_res;
  }
}
#else
CAMLprim value ocaml_ir_realpath(value v_path)
{
  char *path = String_val(v_path);
  /* [realpath] is inherently broken without GNU-extension, and this
     seems like a reasonable thing to do if we do not build against
     GLIBC. */
  char resolved_path[OCAML_IR_PATH_MAX];
  if (realpath(path, resolved_path) == NULL) uerror("realpath", v_path);
  return caml_copy_string(resolved_path);
}
#endif


/* Temporary file and directory creation */

static inline void init_mktemp(char *loc, char *buf, value v_path)
{
  int i, len = caml_string_length(v_path);
  if (len > OCAML_IR_PATH_MAX - 12) caml_invalid_argument(loc);
  memcpy(buf, String_val(v_path), len);
  i = len;
  buf[i++] = '.';
  buf[i++] = 't';
  buf[i++] = 'm';
  buf[i++] = 'p';
  buf[i++] = '.';
  while (i < len + 11) buf[i++] = 'X';
  buf[i++] = '\0';
}

CAMLprim value ocaml_ir_mkdtemp(value v_path)
{
  CAMLparam1(v_path);
  char *loc = "mkdtemp";
  char *path;
  char buf[OCAML_IR_PATH_MAX];
  init_mktemp(loc, buf, v_path);
  caml_enter_blocking_section();
    path = mkdtemp(buf);
  caml_leave_blocking_section();
  if (path == NULL) uerror(loc, v_path);
  CAMLreturn(caml_copy_string(buf));
}
