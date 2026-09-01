/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                              Jane Street                               */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/dynamic.h"
#include "caml/fiber.h"
#include "caml/obj.h"

#define Hash_dyn(dyn) Long_val(dyn)

static void dynamic_cache_flush(dynamic_cache_t cache)
{
  for(size_t i = 0; i < DYNAMIC_CACHE_SIZE; ++i) {
    cache->tbl[i].dyn = Val_null;
  }
}

static dynamic_binding_t dynamic_cache_entry(value dyn)
{
  uintnat hash = Hash_dyn(dyn);
  uintnat index = hash & (DYNAMIC_CACHE_SIZE - 1);
  dynamic_cache_t cache = Caml_state->dynamic_bindings;
  CAMLassert(cache);
  return cache->tbl + index;
}

CAMLexport dynamic_cache_t caml_dynamic_cache_new(void)
{
  dynamic_cache_t res = caml_stat_alloc_noexc(sizeof(dynamic_cache_s));
  if(!res) {
    return NULL;
  }
  dynamic_cache_flush(res);
  return res;
}

CAMLexport void caml_dynamic_cache_delete(dynamic_cache_t cache)
{
  caml_stat_free(cache);
}

CAMLexport void caml_dynamic_cache_flush(dynamic_cache_t cache)
{
  dynamic_cache_flush(cache);
}

CAMLexport void caml_dynamic_cache_enter_thread(dynamic_cache_t cache)
{
  Caml_state->dynamic_bindings = cache;
}

CAMLexport void caml_dynamic_cache_scan_roots(dynamic_cache_t cache,
                                              scanning_action f,
                                              scanning_action_flags fflags,
                                              void *fdata)
{
  for(size_t i = 0; i < DYNAMIC_CACHE_SIZE; ++i) {
    if(Is_this(cache->tbl[i].dyn)) {
      f(fdata, cache->tbl[i].dyn, &cache->tbl[i].dyn);
      f(fdata, cache->tbl[i].val, &cache->tbl[i].val);
    }
  }
}

#define Dynamic_node_wosize 3
#define Dynamic_node_dyn(node) Field(node, 0)
#define Dynamic_node_val(node) Field(node, 1)
#define Dynamic_node_next(node) Field(node, 2)

/* Returns the value of the most recent binding of [dyn] visible from [stack],
   walking parent fibers as needed, or Val_null if [dyn] is unbound. */
static value dynamic_lookup(struct stack_info *stack, value dyn)
{
  for(; stack; stack = Stack_parent(stack)) {
    for(value node = stack->dynamic; Is_this(node);
        node = Dynamic_node_next(node)) {
      if(Dynamic_node_dyn(node) == dyn) {
        return Dynamic_node_val(node);
      }
    }
  }
  return Val_null;
}

CAMLprim value caml_dynamic_make(value unit)
{
  CAMLparam1(unit);
  /* TODO: consider other hash functions. This one is ~unique, which is nice */
  value hash = caml_fresh_oo_id(Val_unit);
  CAMLreturn(hash);
}

CAMLprim value caml_dynamic_get(value dyn)
{
  CAMLnoalloc;

  dynamic_binding_t entry = dynamic_cache_entry(dyn);
  if(entry->dyn == dyn) {
    return entry->val;
  }

  /* Not in cache; let's look at the fiber */
  struct stack_info *stack = Caml_state->current_stack;
  CAMLassert(stack);
  value val = dynamic_lookup(stack, dyn);

  entry->dyn = dyn;
  entry->val = val;
  return val;
}

CAMLprim value caml_dynamic_push(value dyn, value val)
{
  CAMLparam2(dyn, val);

  struct stack_info *stack = Caml_state->current_stack;
  CAMLassert(stack);

  value node = caml_alloc_small(Dynamic_node_wosize, 0);
  Dynamic_node_dyn(node) = dyn;
  Dynamic_node_val(node) = val;
  Dynamic_node_next(node) = stack->dynamic;
  stack->dynamic = node;

  dynamic_binding_t entry = dynamic_cache_entry(dyn);
  entry->dyn = dyn;
  entry->val = val;

  CAMLreturn(Val_unit);
}

CAMLprim value caml_dynamic_pop(value dyn)
{
  CAMLnoalloc;

  struct stack_info *stack = Caml_state->current_stack;
  CAMLassert(stack);

  value head = stack->dynamic;
  CAMLassert(Is_this(head));

  if(Is_this(head)) {
    stack->dynamic = Dynamic_node_next(head);

    CAMLassert(Dynamic_node_dyn(head) == dyn);
    dynamic_binding_t entry = dynamic_cache_entry(dyn);
    if(entry->dyn == dyn) {
      entry->dyn = Val_null;
    }
  }

  return Val_unit;
}
