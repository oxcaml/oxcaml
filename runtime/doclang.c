/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   This file is distributed under the terms of the GNU Lesser General   */
/*   Public License version 2.1, with the special exception on linking.    */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <stdio.h>
#include <stdatomic.h>
#include <string.h>

#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/platform.h"
#include "caml/printexc.h"
#include "caml/signals.h"

#define DOCLANG_STACK_LIMIT 4096

static _Atomic uintnat doclang_counter = 0;
static caml_plat_mutex doclang_output_lock = CAML_PLAT_MUTEX_INITIALIZER;
static CAMLthread_local uintnat doclang_stack[DOCLANG_STACK_LIMIT];
static CAMLthread_local uintnat doclang_depth = 0;
static CAMLthread_local uintnat doclang_overflow_depth = 0;

static void doclang_hex_byte(FILE *channel, unsigned char byte)
{
  static const char digits[] = "0123456789abcdef";
  fputc(digits[byte >> 4], channel);
  fputc(digits[byte & 15], channel);
}

static void doclang_hex(FILE *channel, const char *value, mlsize_t length)
{
  mlsize_t index;
  for (index = 0; index < length; index++) {
    doclang_hex_byte(channel, (unsigned char)value[index]);
  }
}

static void doclang_hex_c_string(FILE *channel, const char *value)
{
  doclang_hex(channel, value, strlen(value));
}

static void doclang_hex_separator(FILE *channel)
{
  doclang_hex_byte(channel, 0x1f);
}

static mlsize_t doclang_site_length(value metadata)
{
  const char *source = String_val(metadata);
  mlsize_t length = caml_string_length(metadata);
  mlsize_t index;
  for (index = 0; index < length; index++) {
    if (source[index] == 0x1f) return index;
  }
  return length;
}

static int doclang_type_is(value metadata, const char *expected)
{
  const char *source = String_val(metadata);
  mlsize_t length = caml_string_length(metadata);
  mlsize_t start = length;
  size_t expected_length = strlen(expected);
  while (start > 0 && source[start - 1] != 0x1f) start--;
  return length - start == expected_length
         && memcmp(source + start, expected, expected_length) == 0;
}

static int doclang_type_ends_with(value metadata, const char *suffix)
{
  const char *source = String_val(metadata);
  mlsize_t length = caml_string_length(metadata);
  mlsize_t start = length;
  size_t suffix_length = strlen(suffix);
  while (start > 0 && source[start - 1] != 0x1f) start--;
  return length - start >= suffix_length
         && memcmp(source + length - suffix_length,
                   suffix, suffix_length) == 0;
}

static size_t doclang_append(char *buffer, size_t capacity, size_t position,
                             const char *text)
{
  size_t available;
  size_t length;
  if (position >= capacity) return position;
  available = capacity - position - 1;
  length = strlen(text);
  if (length > available) length = available;
  memcpy(buffer + position, text, length);
  position += length;
  buffer[position] = 0;
  return position;
}

static void doclang_preview_element(char *buffer, size_t capacity,
                                    value observed)
{
  if (Is_long(observed)) {
    snprintf(buffer, capacity, "%ld", Long_val(observed));
  } else {
    switch (Tag_val(observed)) {
      case String_tag:
        snprintf(buffer, capacity, "\"%.60s\"", String_val(observed));
        break;
      case Double_tag:
        snprintf(buffer, capacity, "%.17g", Double_val(observed));
        break;
      case Closure_tag:
      case Infix_tag:
        snprintf(buffer, capacity, "<function>");
        break;
      default:
        snprintf(buffer, capacity, "<value>");
    }
  }
}

static void doclang_preview(char *buffer, size_t capacity, value metadata,
                            value observed, int is_exception)
{
  size_t position = 0;
  if (is_exception) {
    char *formatted = caml_format_exception(observed);
    if (formatted == NULL) {
      snprintf(buffer, capacity, "<exception>");
    } else {
      snprintf(buffer, capacity, "%s", formatted);
      caml_stat_free(formatted);
    }
    return;
  }

  if (Is_long(observed)) {
    intnat immediate = Long_val(observed);
    if (doclang_type_is(metadata, "unit")) {
      snprintf(buffer, capacity, "()");
    } else if (doclang_type_is(metadata, "bool")) {
      snprintf(buffer, capacity, "%s", immediate == 0 ? "false" : "true");
    } else if (doclang_type_is(metadata, "char")) {
      snprintf(buffer, capacity, "'%c'", (char)immediate);
    } else if (immediate == 0 && doclang_type_ends_with(metadata, " option")) {
      snprintf(buffer, capacity, "None");
    } else if (immediate == 0 && doclang_type_ends_with(metadata, " list")) {
      snprintf(buffer, capacity, "[]");
    } else {
      snprintf(buffer, capacity, "%ld", immediate);
    }
    return;
  }

  if (doclang_type_ends_with(metadata, " list")
      && Tag_val(observed) == 0 && Wosize_val(observed) == 2) {
    value current = observed;
    size_t count = 0;
    char element[96];
    position = doclang_append(buffer, capacity, position, "[");
    while (Is_block(current) && Tag_val(current) == 0
           && Wosize_val(current) == 2 && count < 8) {
      if (count > 0) {
        position = doclang_append(buffer, capacity, position, "; ");
      }
      doclang_preview_element(element, sizeof(element), Field(current, 0));
      position = doclang_append(buffer, capacity, position, element);
      current = Field(current, 1);
      count++;
    }
    if (current != Val_long(0)) {
      position = doclang_append(buffer, capacity, position, "; ...");
    }
    doclang_append(buffer, capacity, position, "]");
    return;
  }

  if (doclang_type_ends_with(metadata, " option")
      && Tag_val(observed) == 0 && Wosize_val(observed) == 1) {
    char element[96];
    doclang_preview_element(element, sizeof(element), Field(observed, 0));
    position = doclang_append(buffer, capacity, position, "Some (");
    position = doclang_append(buffer, capacity, position, element);
    doclang_append(buffer, capacity, position, ")");
    return;
  }

  switch (Tag_val(observed)) {
    case String_tag: {
      const char *source = String_val(observed);
      mlsize_t length = caml_string_length(observed);
      mlsize_t shown = length > 80 ? 80 : length;
      mlsize_t index;
      buffer[position++] = '"';
      for (index = 0; index < shown && position + 2 < capacity; index++) {
        unsigned char character = (unsigned char)source[index];
        if (character == '"' || character == '\\') {
          buffer[position++] = '\\';
          buffer[position++] = character;
        } else if (character >= 0x20 && character != 0x7f) {
          buffer[position++] = character;
        } else {
          buffer[position++] = '.';
        }
      }
      if (shown < length && position + 3 < capacity) {
        buffer[position++] = '.';
        buffer[position++] = '.';
        buffer[position++] = '.';
      }
      if (position + 1 < capacity) buffer[position++] = '"';
      buffer[position] = 0;
      return;
    }
    case Double_tag:
      snprintf(buffer, capacity, "%.17g", Double_val(observed));
      return;
    case Closure_tag:
    case Infix_tag:
      snprintf(buffer, capacity, "<function>");
      return;
    default:
      snprintf(buffer, capacity, "<value tag=%u size=%zu>",
               Tag_val(observed), (size_t)Wosize_val(observed));
  }
}

static void doclang_event(const char *phase, uintnat occurrence,
                          uintnat parent, value metadata, value observed,
                          int has_observed)
{
  char occurrence_buffer[32];
  char parent_buffer[32];
  char preview[512] = "";
  char_os *path = caml_secure_getenv(T("DOCLANG_EVENT_PATH"));
  char *metadata_copy;
  mlsize_t metadata_length;
  mlsize_t site_length;
  FILE *channel;
  if (path == NULL) return;

  snprintf(occurrence_buffer, sizeof(occurrence_buffer), "%lu",
           (unsigned long)occurrence);
  if (parent == 0) {
    parent_buffer[0] = 0;
  } else {
    snprintf(parent_buffer, sizeof(parent_buffer), "%lu",
             (unsigned long)parent);
  }

  if (has_observed) {
    doclang_preview(preview, sizeof(preview), metadata, observed,
                    strcmp(phase, "raise") == 0);
  }
  metadata_length = caml_string_length(metadata);
  site_length = doclang_site_length(metadata);
  metadata_copy = caml_stat_alloc_noexc(metadata_length);
  if (metadata_copy == NULL) return;
  memcpy(metadata_copy, String_val(metadata), metadata_length);

  caml_enter_blocking_section();
  caml_plat_lock_blocking(&doclang_output_lock);
  channel = fopen_os(path, T("ab"));
  if (channel != NULL) {
    fputs("observe\t", channel);
    doclang_hex(channel, metadata_copy, site_length);
    fputc('\t', channel);
    doclang_hex_c_string(channel, phase);
    doclang_hex_separator(channel);
    doclang_hex_c_string(channel, occurrence_buffer);
    doclang_hex_separator(channel);
    doclang_hex_c_string(channel, parent_buffer);
    doclang_hex_separator(channel);
    doclang_hex(channel, metadata_copy, metadata_length);
    doclang_hex_separator(channel);
    if (has_observed) doclang_hex_c_string(channel, preview);
    fputc('\n', channel);
    fclose(channel);
  }
  caml_plat_unlock(&doclang_output_lock);
  caml_leave_blocking_section();
  caml_stat_free(metadata_copy);
}

CAMLprim value caml_doclang_observe_enter(value metadata)
{
  uintnat occurrence = atomic_fetch_add(&doclang_counter, 1) + 1;
  uintnat parent = doclang_depth == 0 ? 0 : doclang_stack[doclang_depth - 1];
  if (doclang_depth >= DOCLANG_STACK_LIMIT) {
    doclang_overflow_depth++;
    return Val_long(0);
  }
  doclang_stack[doclang_depth++] = occurrence;
  doclang_event("enter", occurrence, parent, metadata, Val_unit, 0);
  return Val_long(occurrence);
}

static value doclang_leave(const char *phase, value metadata,
                           value occurrence_value, value observed)
{
  uintnat occurrence = Long_val(occurrence_value);
  uintnat parent;
  if (occurrence == 0) {
    if (doclang_overflow_depth > 0) doclang_overflow_depth--;
    return Val_unit;
  }
  if (doclang_depth == 0
      || doclang_stack[doclang_depth - 1] != occurrence) {
    return Val_unit;
  }
  if (doclang_overflow_depth > 0) {
    doclang_overflow_depth--;
    return Val_unit;
  }
  doclang_depth--;
  parent = doclang_depth == 0 ? 0 : doclang_stack[doclang_depth - 1];
  doclang_event(phase, occurrence, parent, metadata, observed, 1);
  return Val_unit;
}

CAMLprim value caml_doclang_observe_parameter(value occurrence_value,
                                              value metadata, value observed)
{
  uintnat occurrence = Long_val(occurrence_value);
  uintnat parent;
  if (occurrence == 0 || doclang_depth == 0
      || doclang_stack[doclang_depth - 1] != occurrence) {
    return Val_unit;
  }
  parent = doclang_depth == 1 ? 0 : doclang_stack[doclang_depth - 2];
  doclang_event("parameter", occurrence, parent, metadata, observed, 1);
  return Val_unit;
}

CAMLprim value caml_doclang_observe_return(value metadata,
                                           value occurrence_value,
                                           value observed)
{
  return doclang_leave("return", metadata, occurrence_value, observed);
}

CAMLprim value caml_doclang_observe_raise(value metadata,
                                          value occurrence_value,
                                          value exception)
{
  return doclang_leave("raise", metadata, occurrence_value, exception);
}
