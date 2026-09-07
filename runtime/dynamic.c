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

    // Naively, this would traverse the entire binding chain from [stack] to the
    // root task at every iteration, which is quadratic. Instead, we eagerly
    // advance to our Stack_parent when the lexical chain agrees with it.
    struct stack_info *parent = Stack_parent(stack);
    value shared = parent ? parent->dynamic : Val_null;

    for(value node = stack->dynamic; Is_this(node) && node != shared;
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

  // CR-someday mslater: once the gc supports cross-local-stack pointers, this
  // could be allocated on the current fiber's local stack.
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

CAMLprim value caml_dynamic_freeze_scope(value unit)
{
  CAMLparam0();
  CAMLlocal4(head, last, node, copy);

  struct stack_info *stack = Caml_state->current_stack;
  CAMLassert(stack);

  // Copy bindings from plain fibers on the path to the enclosing task
  head = Val_null;
  last = Val_null;

  while(!stack->is_task && Stack_parent(stack) != NULL) {

    for(node = stack->dynamic; Is_this(node);
        node = Dynamic_node_next(node)) {

      // CR-someday mslater: once the gc supports cross-local-stack pointers,
      // this could be allocated on the current fiber's local stack.
      copy = caml_alloc_small(Dynamic_node_wosize, 0);
      Dynamic_node_dyn(copy) = Dynamic_node_dyn(node);
      Dynamic_node_val(copy) = Dynamic_node_val(node);
      Dynamic_node_next(copy) = Val_null;

      if(Is_null(last)) {
        head = copy;
      } else {
        caml_modify(&Dynamic_node_next(last), copy);
      }
      last = copy;
    }

    stack = Stack_parent(stack);
  }

  // If we reached a task, link it in by reference
  if(Is_this(last)) {
    caml_modify(&Dynamic_node_next(last), stack->dynamic);
  } else {
    head = stack->dynamic;
  }

  CAMLreturn(head);
}

CAMLprim value caml_dynamic_use_scope(value scope)
{
  CAMLnoalloc;

  struct stack_info *stack = Caml_state->current_stack;
  CAMLassert(stack);
  CAMLassert(Is_null(stack->dynamic));

  stack->is_task = true;
  stack->dynamic = scope;

  caml_dynamic_cache_flush(Caml_state->dynamic_bindings);

  return Val_unit;
}
