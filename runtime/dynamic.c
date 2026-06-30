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

#include <string.h>

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/dynamic.h"
#include "caml/fiber.h"
#include "caml/obj.h"

#define Hash_dyn(dyn) Long_val(dyn)

static void dynamic_cache_flush(dynamic_cache_t cache)
{
  for (size_t i = 0; i < DYNAMIC_CACHE_SIZE; ++i) {
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
  if (!res) {
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
  for (size_t i = 0; i < DYNAMIC_CACHE_SIZE; ++i) {
    if (Is_this(cache->tbl[i].dyn)) {
      f(fdata, cache->tbl[i].dyn, &cache->tbl[i].dyn);
      f(fdata, cache->tbl[i].val, &cache->tbl[i].val);
    }
  }
}

typedef struct dynamic_stack_s {
  size_t capacity;
  size_t count;
  value dyn; /* Dynamic id, or Val_null if unbound */
  value* vals;
} dynamic_stack_s, *dynamic_stack_t;

#define DYNAMIC_STACK_INIT_CAPACITY 4

static void dynamic_stack_init(dynamic_stack_t stack)
{
  stack->capacity = 0;
  stack->count = 0;
  stack->dyn = Val_null;
  stack->vals = NULL;
}

static void dynamic_stack_free(dynamic_stack_t stack)
{
  if (stack->vals) {
    caml_stat_free(stack->vals);
  }
  dynamic_stack_init(stack);
}

// Returns false if allocation fails.
static bool dynamic_stack_dup(dynamic_stack_t dst, dynamic_stack_t src)
{
  dst->capacity = src->capacity;
  dst->count = src->count;
  dst->dyn = src->dyn;

  if(src->vals) {
    dst->vals = caml_stat_alloc_noexc(sizeof(value) * src->capacity);
    if(dst->vals == NULL) {
      return false;
    }

    memcpy(dst->vals, src->vals, sizeof(value) * src->count);
  } else {
    dst->vals = NULL;
  }

  return true;
}

// Returns false if allocation fails.
static bool dynamic_stack_grow(dynamic_stack_t stack)
{
  size_t old_capacity = stack->capacity;
  size_t new_capacity = old_capacity ? old_capacity * 2 : DYNAMIC_STACK_INIT_CAPACITY;
  value* new_vals = caml_stat_alloc_noexc(sizeof(value) * new_capacity);
  if (!new_vals) {
    return false;
  }
  if (stack->vals) {
    memcpy(new_vals, stack->vals, sizeof(value) * stack->count);
    caml_stat_free(stack->vals);
  }
  stack->vals = new_vals;
  stack->capacity = new_capacity;
  return true;
}

// Returns false if allocation fails.
static bool dynamic_stack_push(dynamic_stack_t stack, value val)
{
  if (stack->count == stack->capacity) {
    if(!dynamic_stack_grow(stack)) {
      return false;
    }
  }
  stack->vals[stack->count++] = val;
  return true;
}

// Returns true if the stack is now empty.
static bool dynamic_stack_pop(dynamic_stack_t stack)
{
  CAMLassert(stack->count > 0);
  if(--stack->count == 0) {
    dynamic_stack_free(stack);
    return true;
  }
  return false;
}

static void dynamic_stack_scan_roots(dynamic_stack_t stack,
                                     scanning_action f,
                                     scanning_action_flags fflags,
                                     void *fdata)
{
  if(Is_this(stack->dyn)) {
    f(fdata, stack->dyn, &stack->dyn);
    for(size_t j = 0; j < stack->count; ++j) {
      f(fdata, stack->vals[j], &stack->vals[j]);
    }
  }
}

/* Hash tables of binding stacks. Linear probing, growing when half full. */

#define DYNAMIC_TABLE_INIT_CAPACITY 8

static size_t dynamic_table_capacity(dynamic_table_t table) {
  return table->mask + 1;
}

CAMLexport void caml_dynamic_table_init(dynamic_table_t table)
{
  table->mask = (size_t)-1;
  table->count = 0;
  table->bindings = NULL;
}

CAMLexport void caml_dynamic_table_free(dynamic_table_t table)
{
  if (table->bindings) {
    size_t capacity = dynamic_table_capacity(table);
    for (size_t i = 0; i < capacity; ++i) {
      dynamic_stack_free(&table->bindings[i]);
    }
    caml_stat_free(table->bindings);
  }
  caml_dynamic_table_init(table);
}

static void dynamic_table_add(dynamic_table_t table, dynamic_stack_s stack)
{
  size_t mask = table->mask;
  value hash = Hash_dyn(stack.dyn);
  size_t i = hash & mask;
  size_t j = i;
  while(Is_this(table->bindings[j].dyn)) { /* collision */
    j = (j + 1) & mask; /* linear probing */
    CAMLassert(j != i); /* Caller guarantees table has space */
  }
  table->bindings[j] = stack;
}

// Returns false if allocation fails.
static bool dynamic_table_grow(dynamic_table_t table)
{
  size_t old_capacity = dynamic_table_capacity(table);
  size_t new_capacity = old_capacity ? old_capacity * 2 : DYNAMIC_TABLE_INIT_CAPACITY;
  size_t new_mask = new_capacity - 1;
  CAMLassert(Is_power_of_2(new_capacity));

  dynamic_stack_t new_bindings =
    caml_stat_alloc_noexc(sizeof(dynamic_stack_s) * new_capacity);
  if (!new_bindings) {
    return false;
  }
  for (size_t j = 0; j < new_capacity; ++ j) { /* ensure new table is empty */
    dynamic_stack_init(&new_bindings[j]);
  }
  dynamic_stack_t old_bindings = table->bindings;
  table->mask = new_mask;
  table->bindings = new_bindings;

  /* Copy existing bindings */
  for (size_t i = 0; i < old_capacity; ++i) {
    if (Is_this(old_bindings[i].dyn)) {
      dynamic_table_add(table, old_bindings[i]);
    }
  }
  if (old_bindings) {
    caml_stat_free(old_bindings);
  }
  return true;
}

// Returns whether [dyn] is bound in this table. Sets [bindings_out] to the slot
// [dyn] maps to, or NULL if the table is empty.
static bool dynamic_table_find(dynamic_table_t table, value dyn,
                               dynamic_stack_t *bindings_out)
{
  if (table->bindings == NULL) {
    *bindings_out = NULL;
    return false;
  }
  uintnat hash = Hash_dyn(dyn);
  size_t i = hash & table->mask;
  while (true) {
    dynamic_stack_t bindings = table->bindings + i;
    if (bindings->dyn == dyn) { /* Found */
      *bindings_out = bindings;
      return true;
    } else if (!Is_this(bindings->dyn)) { /* Not found */
      *bindings_out = bindings;
      return false;
    }
    /* Linear probe */
    i = (i + 1) & table->mask;
  }
}

// Returns false if allocation fails.
static bool dynamic_table_push(dynamic_table_t table, value dyn, value val)
{
  dynamic_stack_t bindings = NULL;
  bool found = dynamic_table_find(table, dyn, &bindings);
  if (!bindings) { /* Table was empty */
    if (!dynamic_table_grow(table)) {
      return false;
    }
    found = dynamic_table_find(table, dyn, &bindings);
    CAMLassert(!found);
  }
  CAMLassert(bindings);
  if (found) { /* Update binding */
    if(!dynamic_stack_push(bindings, val)) {
      return false;
    }
  } else { /* Not found */
    if (table->count * 2 == dynamic_table_capacity(table)) {
      /* grow when half-full (includes the special case of being empty) */
      if (!dynamic_table_grow(table)) {
        return false;
      }
      return dynamic_table_push(table, dyn, val);
    } else {
      CAMLassert(!Is_this(bindings->dyn));
      if(!dynamic_stack_push(bindings, val)) {
        return false;
      }
      bindings->dyn = dyn;
    }
    ++table->count;
  }
  return true;
}

static void dynamic_table_pop(dynamic_table_t table, value dyn)
{
  dynamic_stack_t bindings = NULL;
  if(dynamic_table_find(table, dyn, &bindings)) {
    if(dynamic_stack_pop(bindings)) {

      --table->count;
      size_t idx = bindings - table->bindings;

      // Rehash chain after removal
      size_t next = (idx + 1) & table->mask;
      while(Is_this(table->bindings[next].dyn)) {
        dynamic_stack_s stack = table->bindings[next];
        dynamic_stack_init(&table->bindings[next]);
        dynamic_table_add(table, stack);
        next = (next + 1) & table->mask;
      }
    }
  }
}

CAMLexport bool caml_dynamic_table_dup(dynamic_table_t dst, dynamic_table_t src)
{
  size_t capacity = dynamic_table_capacity(src);

  dst->mask = src->mask;
  dst->count = src->count;
  dst->bindings = caml_stat_alloc_noexc(sizeof(dynamic_stack_s) * capacity);
  if(dst->bindings == NULL) {
    return false;
  }

  for(size_t i = 0; i < capacity; ++i) {
    if(!dynamic_stack_dup(&dst->bindings[i], &src->bindings[i])) {
      for(size_t j = 0; j < i; ++j) {
        dynamic_stack_free(&dst->bindings[j]);
      }
      caml_stat_free(dst->bindings);
      return false;
    }
  }

  return true;
}

CAMLexport void caml_dynamic_table_scan_roots(dynamic_table_t table,
                                              scanning_action f,
                                              scanning_action_flags fflags,
                                              void *fdata)
{
  if (table->bindings) {
    size_t capacity = dynamic_table_capacity(table);
    for (size_t i = 0; i < capacity; ++i) {
      dynamic_stack_scan_roots(&table->bindings[i], f, fflags, fdata);
    }
  }
}

/* Make a fresh dynamic value, which is an immediate unique ID. */
CAMLprim value caml_dynamic_make(value unit)
{
  CAMLparam1(unit);
  /* TODO: consider other hash functions. This one is ~unique, which is nice */
  value hash = caml_fresh_oo_id(Val_unit);
  CAMLreturn(hash);
}

/* Get the current value of a dynamic variable. Does not allocate. */
CAMLprim value caml_dynamic_get(value dyn)
{
  CAMLnoalloc;

  dynamic_binding_t entry = dynamic_cache_entry(dyn);
  if (entry->dyn == dyn) {
    return entry->val;
  }

  /* Not in cache; let's look at the fiber */
  struct stack_info *stack = Caml_state->current_stack;
  CAMLassert(stack);

  value val = Val_null;
  while (stack) {
    dynamic_stack_t bindings;
    if (dynamic_table_find(&stack->dyn, dyn, &bindings)) {
      if(bindings->count > 0) {
        val = bindings->vals[bindings->count - 1];
        break;
      }
    }
    stack = Stack_parent(stack);
  }

  entry->dyn = dyn;
  entry->val = val;
  return val;
}

/* Push a local binding for a dynamic variable. */
CAMLprim value caml_dynamic_push(value dyn, value val)
{
  CAMLparam2(dyn, val);

  struct stack_info *stack = Caml_state->current_stack;
  CAMLassert(stack);

  if(!dynamic_table_push(&stack->dyn, dyn, val)) {
    caml_raise_out_of_memory();
  }

  dynamic_binding_t entry = dynamic_cache_entry(dyn);
  entry->dyn = dyn;
  entry->val = val;

  CAMLreturn(Val_unit);
}

/* Pop a local binding for a dynamic variable. */
CAMLprim value caml_dynamic_pop(value dyn)
{
  CAMLparam1(dyn);

  struct stack_info *stack = Caml_state->current_stack;
  CAMLassert(stack);

  dynamic_table_pop(&stack->dyn, dyn);

  dynamic_binding_t entry = dynamic_cache_entry(dyn);
  if(entry->dyn == dyn) {
    entry->dyn = Val_null;
  }

  CAMLreturn(Val_unit);
}
