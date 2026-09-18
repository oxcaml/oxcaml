//******************************************************************************
//*                                  OxCaml                                    *
//* -------------------------------------------------------------------------- *
//*                               MIT License                                  *
//*                                                                            *
//* Copyright (c) 2025 Jane Street Group LLC                                   *
//* opensource-contacts@janestreet.com                                         *
//*                                                                            *
//* Permission is hereby granted, free of charge, to any person obtaining a    *
//* copy of this software and associated documentation files (the "Software"), *
//* to deal in the Software without restriction, including without limitation  *
//* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
//* and/or sell copies of the Software, and to permit persons to whom the      *
//* Software is furnished to do so, subject to the following conditions:       *
//*                                                                            *
//* The above copyright notice and this permission notice shall be included    *
//* in all copies or substantial portions of the Software.                     *
//*                                                                            *
//* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
//* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
//* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
//* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
//* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
//* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
//* DEALINGS IN THE SOFTWARE.                                                  *
//******************************************************************************

// CR metaprogramming jrickard: This file has not been code reviewed

#include "caml/mlvalues.h"

extern char caml_bundled_cmis[] __attribute__((weak));
extern char caml_bundled_cmxs[] __attribute__((weak));
// CR mshinwell: it's a shame that "cmxs" clashes with the extension of
// a shared library file

value caml_bundled_cmis_this_exe()
{
  return (value) &caml_bundled_cmis;
}

value caml_bundled_cmxs_this_exe()
{
  return (value) &caml_bundled_cmxs;
}

value caml_bundle_available(value bundle)
{
  return (bundle == (value) 0) ? Val_false : Val_true;
}

