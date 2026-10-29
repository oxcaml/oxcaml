/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                       Nick Barnes, Jane Street                         */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Offsets into the C structures which the assembly-language stubs
   (amd64.S, arm64.S) share with the runtime.  This header is included
   both from those stubs and from C, so it may contain only
   preprocessor directives.  The values are hand-maintained here and
   checked against the real structure layouts by static assertions in
   fiber.c; if a structure changes, the build fails there until this
   file is updated to match. */

#ifndef CAML_ASM_OFFSETS_H
#define CAML_ASM_OFFSETS_H

/* Some ABIs reserve space at the bottom of every C stack frame, which
   [struct c_stack_link] (fiber.h) includes as a leading field. */
#if defined(TARGET_amd64) && (defined(_WIN32) || defined(__CYGWIN__))
/* Win64 ABI shadow store for argument registers */
  #define Reserved_space_c_stack_link 4 * 8
#elif defined(TARGET_s390x)
  #define Reserved_space_c_stack_link 160
#elif defined(TARGET_power)
/* ELF ABI: 4 reserved words at bottom of C stack */
  #define Reserved_space_c_stack_link 4 * 8
#else
  #define Reserved_space_c_stack_link 0
#endif

/* struct stack_info */
#define Stack_sp_offset                 0
#define Stack_exception_offset          8
#define Stack_handler_offset            16
#define Stack_id_offset                 48
#define Stack_local_sp_offset           64
#define Stack_local_top_offset          72
#define Stack_local_limit_offset        80
#define Stack_tls_state_offset          88

/* The handler field as addressed from a tagged continuation-fiber
   pointer. */
#define Stack_handler_from_cont_offset  (Stack_handler_offset - 1)

/* struct stack_handler */
#define Handler_value_offset            0
#define Handler_exception_offset        8
#define Handler_effect_offset           16
#define Handler_parent_offset           32

/* dynamic_cache_s (dynamic.h).  If you change Dynamic_cache_entries
   (DYNAMIC_CACHE_BITS in dynamic.h), you must also update
   FLUSH_DYNAMIC_CACHE in the assembly stubs. */
#define Dynamic_cache                   0
#define Dynamic_binding_size            16
#define Dynamic_binding_var             0
#define Dynamic_cache_entries           8

/* struct c_stack_link (its padding field keeps the C stack 16-aligned
   in caml_start_program) */
#define Cstack_stack_offset             (Reserved_space_c_stack_link + 0)
#define Cstack_sp_offset                (Reserved_space_c_stack_link + 8)
#define Cstack_prev_offset              (Reserved_space_c_stack_link + 16)
#define Cstack_async_exn_handler_offset (Reserved_space_c_stack_link + 24)
#define SIZEOF_C_STACK_LINK             (Reserved_space_c_stack_link + 40)

#endif /* CAML_ASM_OFFSETS_H */
