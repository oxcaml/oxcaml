(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

type t =
  { naming_context : Fdo_function_naming.Context.t;
    instrumentation : Fdo_instrumentation.t
  }

let toplevel compilation_unit module_initializer =
  { naming_context = Fdo_function_naming.Context.toplevel compilation_unit;
    instrumentation =
      Fdo_instrumentation.create
        ~function_:(Fdo_function_naming.toplevel_function compilation_unit)
        ~body_hash:(Fdo_body_hash.of_toplevel module_initializer)
  }

let enter_function t ~binder fn =
  let function_, naming_context =
    Fdo_function_naming.of_function t.naming_context ~binder fn
  in
  { naming_context;
    instrumentation =
      Fdo_instrumentation.create ~function_
        ~body_hash:(Fdo_body_hash.of_function fn)
  }

let entry_annotation t = Fdo_instrumentation.entry_annotation t.instrumentation

let call_counter t loc = Fdo_instrumentation.call t.instrumentation loc

let switch_counters t switch =
  Fdo_instrumentation.switch t.instrumentation switch
