/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                   David Allsopp, Jane Street Europe                    */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#if defined(CAML_INTERNALS) && defined(CAML_NO_INTERNALS) || \
    !defined(CAML_INTERNALS) && defined(CAML_WITH_INTERNALS)
#error "Inconsistent use of CAML_INTERNALS"
#elif defined(CAML_INTERNALS) && !defined(CAML_WITH_INTERNALS)
#define CAML_WITH_INTERNALS
#elif !defined(CAML_INTERNALS) && !defined(CAML_NO_INTERNALS)
#define CAML_NO_INTERNALS
#endif
