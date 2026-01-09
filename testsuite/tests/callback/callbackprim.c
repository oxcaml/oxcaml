/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1995 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <signal.h>

#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/signals.h"

value mycallback1(value fun, value arg)
{
  value res;
  res = caml_callback(fun, arg);
  return res;
}

value mycallback2(value fun, value arg1, value arg2)
{
  value res;
  res = caml_callback2(fun, arg1, arg2);
  return res;
}

value mycallback3(value fun, value arg1, value arg2, value arg3)
{
  value res;
  res = caml_callback3(fun, arg1, arg2, arg3);
  return res;
}

value mycallback4(value fun, value arg1, value arg2, value arg3, value arg4)
{
  value args[4];
  value res;
  args[0] = arg1;
  args[1] = arg2;
  args[2] = arg3;
  args[3] = arg4;
  res = caml_callbackN(fun, 4, args);
  return res;
}

value mypushroot(value v, value fun, value arg)
{
  CAMLparam1(v);
  caml_callback(fun, arg);
  CAMLreturn(v);
}

value mycamlparam (value v, value fun, value arg)
{
  CAMLparam3 (v, fun, arg);
  CAMLlocal2 (x, y);
  x = v;
  y = caml_callback (fun, arg);
  v = x;
  CAMLreturn (v);
}

value raise_sigusr1(value unused)
{
  raise(SIGUSR1);
  return Val_unit;
}

static volatile sig_atomic_t other_handler_count = 0;

static void myotherhandler(int sig)
{
  ++ other_handler_count;
}

value myotherhandlercount(value unit)
{
  return Val_int(other_handler_count);
}

value mysetotherhandler(value signal)
{
  int res;
  int sig = caml_convert_signal_number(Int_val(signal));
#ifdef POSIX_SIGNALS
  struct sigaction sa;
    sa.sa_handler = myotherhandler;
    sa.sa_flags = SA_ONSTACK;
    sigemptyset(&sa.sa_mask);
    res = sigaction(SIGUSR1, &sa, NULL);
#else
    res = signal(sig, myotherhandler);
#endif
    return Val_int(res);
}
