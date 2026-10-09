/* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/* 32-bit floats as 32-bit integer bit patterns, following
   middle_end/flambda2/numbers/floats/float32_stubs.c in OxCaml. This lets the
   compiler handle float32 values without relying on compiler support for the
   [float32] type, so that this code also builds with upstream OCaml. */

#include <stdint.h>

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

static float float32_of_int32(int32_t i)
{
  union { int32_t i; float f; } u;
  u.i = i;
  return u.f;
}

static int32_t int32_of_float32(float f)
{
  union { int32_t i; float f; } u;
  u.f = f;
  return u.i;
}

int32_t jsoo_float32_of_float(double d)
{
  return int32_of_float32((float) d);
}

double jsoo_float32_to_float(int32_t i)
{
  return (double) float32_of_int32(i);
}

value jsoo_float32_of_float_boxed(value d)
{
  return caml_copy_int32(jsoo_float32_of_float(Double_val(d)));
}

value jsoo_float32_to_float_boxed(value i)
{
  return caml_copy_double(jsoo_float32_to_float(Int32_val(i)));
}

/* The payload of a boxed float32, which is a custom block whose data is a
   single-precision float (see Float32_val in the OxCaml runtime). */
value jsoo_float32_of_boxed(value v)
{
  return caml_copy_int32(*((int32_t *) Data_custom_val(v)));
}
