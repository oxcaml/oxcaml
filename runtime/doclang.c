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
#define DOCLANG_EVENT_BYTE_LIMIT 1900000
#define DOCLANG_FUNCTION_LIMIT 8192

static _Atomic uintnat doclang_counter = 0;
static caml_plat_mutex doclang_output_lock = CAML_PLAT_MUTEX_INITIALIZER;
static FILE *doclang_output_channel = NULL;
static size_t doclang_output_bytes = 0;
static _Atomic int doclang_trace_truncated = 0;
static _Atomic uintnat doclang_function_count = 0;
static _Atomic(code_t) doclang_functions[DOCLANG_FUNCTION_LIMIT];
static _Atomic intnat doclang_function_consumptions[DOCLANG_FUNCTION_LIMIT];
static _Atomic uintnat doclang_partial_function_count = 0;
static _Atomic(code_t) doclang_partial_functions[DOCLANG_FUNCTION_LIMIT];
static _Atomic intnat
  doclang_partial_function_consumptions[DOCLANG_FUNCTION_LIMIT];
static CAMLthread_local uintnat doclang_stack[DOCLANG_STACK_LIMIT];
static CAMLthread_local uintnat doclang_parents[DOCLANG_STACK_LIMIT];
static CAMLthread_local int doclang_tail_capable[DOCLANG_STACK_LIMIT];
static CAMLthread_local uintnat doclang_overapply_parents[DOCLANG_STACK_LIMIT];
static CAMLthread_local uintnat doclang_overapply_remaining[DOCLANG_STACK_LIMIT];
static CAMLthread_local uintnat doclang_depth = 0;
static CAMLthread_local uintnat doclang_overflow_depth = 0;
static CAMLthread_local uintnat doclang_pending_tail_parent = 0;
static CAMLthread_local uintnat doclang_pending_tail_remaining = 0;

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

static mlsize_t doclang_public_metadata_length(value metadata)
{
  const char *source = String_val(metadata);
  mlsize_t length = caml_string_length(metadata);
  mlsize_t index;
  size_t separators = 0;
  for (index = 0; index < length; index++) {
    if (source[index] == 0x1f && ++separators == 9) return index;
  }
  return length;
}

static const char *doclang_metadata_field(value metadata, size_t wanted,
                                          mlsize_t *field_length)
{
  const char *source = String_val(metadata);
  mlsize_t length = caml_string_length(metadata);
  mlsize_t start = 0;
  size_t field = 0;
  mlsize_t index;
  for (index = 0; index <= length; index++) {
    if (index == length || source[index] == 0x1f) {
      if (field == wanted) {
        *field_length = index - start;
        return source + start;
      }
      field++;
      start = index + 1;
    }
  }
  *field_length = 0;
  return NULL;
}

static int doclang_metadata_field_is(value metadata, size_t index,
                                     const char *expected)
{
  mlsize_t length;
  const char *field = doclang_metadata_field(metadata, index, &length);
  size_t expected_length = strlen(expected);
  return field != NULL && length == expected_length
         && memcmp(field, expected, expected_length) == 0;
}

static int doclang_type_is(value metadata, const char *expected)
{
  return doclang_metadata_field_is(metadata, 8, expected);
}

static int doclang_type_ends_with(value metadata, const char *suffix)
{
  mlsize_t length;
  const char *field = doclang_metadata_field(metadata, 8, &length);
  size_t suffix_length = strlen(suffix);
  return field != NULL && length >= suffix_length
         && memcmp(field + length - suffix_length, suffix, suffix_length) == 0;
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

typedef struct {
  char *data;
  size_t capacity;
  size_t position;
} doclang_preview_buffer;

static void doclang_preview_append_n(doclang_preview_buffer *output,
                                     const char *text, size_t length)
{
  size_t available;
  if (output->position >= output->capacity) return;
  available = output->capacity - output->position - 1;
  if (length > available) length = available;
  memcpy(output->data + output->position, text, length);
  output->position += length;
  output->data[output->position] = 0;
}

static void doclang_preview_append(doclang_preview_buffer *output,
                                   const char *text)
{
  doclang_preview_append_n(output, text, strlen(text));
}

static void doclang_preview_append_int(doclang_preview_buffer *output,
                                       intnat number)
{
  char formatted[64];
  snprintf(formatted, sizeof(formatted), "%ld", number);
  doclang_preview_append(output, formatted);
}

static size_t doclang_schema_number(const char **cursor, const char *end,
                                    char separator)
{
  size_t number = 0;
  while (*cursor < end && **cursor >= '0' && **cursor <= '9') {
    number = number * 10 + (size_t)(**cursor - '0');
    (*cursor)++;
  }
  if (*cursor < end && **cursor == separator) (*cursor)++;
  return number;
}

static const char *doclang_schema_name(const char **cursor, const char *end,
                                       size_t *length)
{
  const char *name;
  *length = doclang_schema_number(cursor, end, ':');
  name = *cursor;
  if ((size_t)(end - *cursor) < *length) *length = (size_t)(end - *cursor);
  *cursor += *length;
  return name;
}

static void doclang_skip_schema(const char **cursor, const char *end)
{
  char kind;
  size_t count;
  size_t index;
  if (*cursor >= end) return;
  kind = *(*cursor)++;
  switch (kind) {
    case 'L': case 'O': case 'A': case 'R': case 'F': case 'E':
      doclang_skip_schema(cursor, end);
      break;
    case 'M':
      doclang_skip_schema(cursor, end);
      doclang_skip_schema(cursor, end);
      break;
    case 'T':
      count = doclang_schema_number(cursor, end, ':');
      for (index = 0; index < count; index++) doclang_skip_schema(cursor, end);
      break;
    case 'Q':
      count = doclang_schema_number(cursor, end, ':');
      for (index = 0; index < count; index++) {
        size_t name_length;
        (void)doclang_schema_name(cursor, end, &name_length);
        doclang_skip_schema(cursor, end);
      }
      break;
    case 'V': {
      size_t constants = doclang_schema_number(cursor, end, ':');
      size_t blocks;
      for (index = 0; index < constants; index++) {
        size_t name_length;
        (void)doclang_schema_name(cursor, end, &name_length);
      }
      blocks = doclang_schema_number(cursor, end, ':');
      for (index = 0; index < blocks; index++) {
        size_t arity;
        size_t field;
        size_t name_length;
        (void)doclang_schema_number(cursor, end, ',');
        (void)doclang_schema_name(cursor, end, &name_length);
        arity = doclang_schema_number(cursor, end, ':');
        for (field = 0; field < arity; field++)
          doclang_skip_schema(cursor, end);
      }
      break;
    }
    default:
      break;
  }
}

static void doclang_preview_schema(doclang_preview_buffer *output,
                                   const char **cursor, const char *end,
                                   const char *self, const char *self_end,
                                   value observed, unsigned depth);

static void doclang_preview_string(doclang_preview_buffer *output,
                                   value observed)
{
  const char *source;
  mlsize_t length;
  mlsize_t shown;
  mlsize_t index;
  if (!Is_block(observed) || Tag_val(observed) != String_tag) {
    doclang_preview_append(output, "<opaque>");
    return;
  }
  source = String_val(observed);
  length = caml_string_length(observed);
  shown = length > 240 ? 240 : length;
  doclang_preview_append(output, "\"");
  for (index = 0; index < shown; index++) {
    unsigned char character = (unsigned char)source[index];
    if (character == '"' || character == '\\') {
      char escaped[2] = {'\\', (char)character};
      doclang_preview_append_n(output, escaped, 2);
    } else if (character >= 0x20 && character != 0x7f) {
      doclang_preview_append_n(output, (const char *)&source[index], 1);
    } else {
      doclang_preview_append(output, ".");
    }
  }
  if (shown < length) doclang_preview_append(output, "…");
  doclang_preview_append(output, "\"");
}

static void doclang_preview_dynamic(doclang_preview_buffer *output,
                                    value observed)
{
  if (Is_long(observed)) {
    doclang_preview_append_int(output, Long_val(observed));
  } else {
    switch (Tag_val(observed)) {
      case String_tag:
        doclang_preview_string(output, observed);
        break;
      case Double_tag: {
        char formatted[64];
        snprintf(formatted, sizeof(formatted), "%.17g", Double_val(observed));
        doclang_preview_append(output, formatted);
        break;
      }
      case Closure_tag:
      case Infix_tag:
        doclang_preview_append(output, "<function>");
        break;
      default:
        doclang_preview_append(output, "<opaque>");
        break;
    }
  }
}

static void doclang_preview_child(doclang_preview_buffer *output,
                                  const char *schema, const char *schema_end,
                                  const char *self, const char *self_end,
                                  value observed, unsigned depth)
{
  const char *cursor = schema;
  doclang_preview_schema(output, &cursor, schema_end, self, self_end,
                         observed, depth);
}

static int doclang_preview_map_tree(doclang_preview_buffer *output,
                                    const char *key_schema,
                                    const char *key_end,
                                    const char *value_schema,
                                    const char *value_end,
                                    value tree, size_t *shown,
                                    unsigned depth)
{
  int truncated;
  if (tree == Val_long(0)) return 0;
  if (*shown >= 12) return 1;
  if (!Is_block(tree) || Tag_val(tree) != 0 || Wosize_val(tree) < 5)
    return 1;
  truncated = doclang_preview_map_tree(output, key_schema, key_end,
                                       value_schema, value_end,
                                       Field(tree, 0), shown, depth);
  if (*shown >= 12) return 1;
  if (*shown > 0) doclang_preview_append(output, "; ");
  doclang_preview_child(output, key_schema, key_end, NULL, NULL,
                        Field(tree, 1), depth + 1);
  doclang_preview_append(output, " ↦ ");
  doclang_preview_child(output, value_schema, value_end, NULL, NULL,
                        Field(tree, 2), depth + 1);
  (*shown)++;
  return doclang_preview_map_tree(output, key_schema, key_end,
                                  value_schema, value_end,
                                  Field(tree, 3), shown, depth) || truncated;
}

static int doclang_preview_set_tree(doclang_preview_buffer *output,
                                    const char *element_schema,
                                    const char *element_end,
                                    value tree, size_t *shown,
                                    unsigned depth)
{
  int truncated;
  if (tree == Val_long(0)) return 0;
  if (*shown >= 12) return 1;
  if (!Is_block(tree) || Tag_val(tree) != 0 || Wosize_val(tree) < 4)
    return 1;
  truncated = doclang_preview_set_tree(output, element_schema, element_end,
                                       Field(tree, 0), shown, depth);
  if (*shown >= 12) return 1;
  if (*shown > 0) doclang_preview_append(output, "; ");
  doclang_preview_child(output, element_schema, element_end, NULL, NULL,
                        Field(tree, 1), depth + 1);
  (*shown)++;
  return doclang_preview_set_tree(output, element_schema, element_end,
                                  Field(tree, 2), shown, depth) || truncated;
}

static void doclang_preview_schema(doclang_preview_buffer *output,
                                   const char **cursor, const char *end,
                                   const char *self, const char *self_end,
                                   value observed, unsigned depth)
{
  const char *schema_start = *cursor;
  char kind;
  if (*cursor >= end) {
    doclang_preview_append(output, "<opaque>");
    return;
  }
  if (depth > 7) {
    doclang_skip_schema(cursor, end);
    doclang_preview_append(output, "…");
    return;
  }
  kind = *(*cursor)++;
  switch (kind) {
    case 'I':
      if (Is_long(observed)) doclang_preview_append_int(output, Long_val(observed));
      else doclang_preview_append(output, "<opaque>");
      break;
    case 'B':
      if (Is_long(observed))
        doclang_preview_append(output, Long_val(observed) == 0 ? "false" : "true");
      else doclang_preview_append(output, "<opaque>");
      break;
    case 'U':
      doclang_preview_append(output, "()");
      break;
    case 'C':
      if (Is_long(observed)) {
        char character[4] = {'\'', (char)Long_val(observed), '\'', 0};
        doclang_preview_append(output, character);
      } else doclang_preview_append(output, "<opaque>");
      break;
    case 'D':
      if (Is_block(observed) && Tag_val(observed) == Double_tag) {
        char formatted[64];
        snprintf(formatted, sizeof(formatted), "%.17g", Double_val(observed));
        doclang_preview_append(output, formatted);
      } else doclang_preview_append(output, "<opaque>");
      break;
    case 'S':
      doclang_preview_string(output, observed);
      break;
    case 'F':
      doclang_skip_schema(cursor, end);
      doclang_preview_append(output, "<function>");
      break;
    case 'X':
      if (self == NULL) doclang_preview_append(output, "<opaque>");
      else doclang_preview_child(output, self, self_end, self, self_end,
                                 observed, depth + 1);
      break;
    case 'T': {
      size_t count = doclang_schema_number(cursor, end, ':');
      size_t index;
      doclang_preview_append(output, "(");
      for (index = 0; index < count; index++) {
        const char *field_schema = *cursor;
        const char *field_end = field_schema;
        doclang_skip_schema(&field_end, end);
        if (index > 0) doclang_preview_append(output, ", ");
        if (Is_block(observed) && index < Wosize_val(observed))
          doclang_preview_child(output, field_schema, field_end, self, self_end,
                                Field(observed, index), depth + 1);
        else
          doclang_preview_append(output, "<opaque>");
        *cursor = field_end;
      }
      doclang_preview_append(output, ")");
      break;
    }
    case 'L': {
      const char *element_schema = *cursor;
      const char *element_end = element_schema;
      value current = observed;
      size_t count = 0;
      doclang_skip_schema(&element_end, end);
      *cursor = element_end;
      doclang_preview_append(output, "[");
      while (Is_block(current) && Tag_val(current) == 0
             && Wosize_val(current) == 2 && count < 12) {
        if (count > 0) doclang_preview_append(output, "; ");
        doclang_preview_child(output, element_schema, element_end,
                              self, self_end, Field(current, 0), depth + 1);
        current = Field(current, 1);
        count++;
      }
      if (current != Val_long(0)) doclang_preview_append(output, "; …");
      doclang_preview_append(output, "]");
      break;
    }
    case 'O': {
      const char *element_schema = *cursor;
      const char *element_end = element_schema;
      doclang_skip_schema(&element_end, end);
      *cursor = element_end;
      if (observed == Val_long(0)) {
        doclang_preview_append(output, "None");
      } else if (Is_block(observed) && Tag_val(observed) == 0
                 && Wosize_val(observed) == 1) {
        doclang_preview_append(output, "Some (");
        doclang_preview_child(output, element_schema, element_end,
                              self, self_end, Field(observed, 0), depth + 1);
        doclang_preview_append(output, ")");
      } else {
        doclang_preview_append(output, "<opaque>");
      }
      break;
    }
    case 'A': {
      const char *element_schema = *cursor;
      const char *element_end = element_schema;
      mlsize_t length = 0;
      mlsize_t shown;
      mlsize_t index;
      doclang_skip_schema(&element_end, end);
      *cursor = element_end;
      if (Is_block(observed) && Tag_val(observed) == Double_array_tag
          && element_schema < element_end && *element_schema == 'D') {
        length = Wosize_val(observed) / Double_wosize;
      } else if (Is_block(observed) && Tag_val(observed) == 0) {
        length = Wosize_val(observed);
      }
      shown = length > 12 ? 12 : length;
      doclang_preview_append(output, "[|");
      for (index = 0; index < shown; index++) {
        if (index > 0) doclang_preview_append(output, "; ");
        if (Tag_val(observed) == Double_array_tag) {
          char formatted[64];
          snprintf(formatted, sizeof(formatted), "%.17g",
                   Double_flat_field(observed, index));
          doclang_preview_append(output, formatted);
        } else {
          doclang_preview_child(output, element_schema, element_end,
                                self, self_end, Field(observed, index),
                                depth + 1);
        }
      }
      if (shown < length) doclang_preview_append(output, "; …");
      doclang_preview_append(output, "|]");
      break;
    }
    case 'M': {
      const char *key_schema = *cursor;
      const char *key_end = key_schema;
      const char *value_schema;
      const char *value_end;
      size_t shown = 0;
      int truncated;
      doclang_skip_schema(&key_end, end);
      value_schema = key_end;
      value_end = value_schema;
      doclang_skip_schema(&value_end, end);
      *cursor = value_end;
      doclang_preview_append(output, "{");
      truncated = doclang_preview_map_tree(output, key_schema, key_end,
                                           value_schema, value_end,
                                           observed, &shown, depth);
      if (truncated) {
        if (shown > 0) doclang_preview_append(output, "; ");
        doclang_preview_append(output, "…");
      }
      doclang_preview_append(output, "}");
      break;
    }
    case 'E': {
      const char *element_schema = *cursor;
      const char *element_end = element_schema;
      size_t shown = 0;
      int truncated;
      doclang_skip_schema(&element_end, end);
      *cursor = element_end;
      doclang_preview_append(output, "{");
      truncated = doclang_preview_set_tree(output, element_schema, element_end,
                                           observed, &shown, depth);
      if (truncated) {
        if (shown > 0) doclang_preview_append(output, "; ");
        doclang_preview_append(output, "…");
      }
      doclang_preview_append(output, "}");
      break;
    }
    case 'R': {
      const char *element_schema = *cursor;
      const char *element_end = element_schema;
      doclang_skip_schema(&element_end, end);
      *cursor = element_end;
      if (Is_block(observed) && Tag_val(observed) == 0
          && Wosize_val(observed) == 1) {
        doclang_preview_append(output, "{contents = ");
        doclang_preview_child(output, element_schema, element_end,
                              self, self_end, Field(observed, 0), depth + 1);
        doclang_preview_append(output, "}");
      } else {
        doclang_preview_append(output, "<opaque>");
      }
      break;
    }
    case 'Q': {
      size_t count = doclang_schema_number(cursor, end, ':');
      size_t index;
      doclang_preview_append(output, "{");
      for (index = 0; index < count; index++) {
        size_t name_length;
        const char *name = doclang_schema_name(cursor, end, &name_length);
        const char *field_schema = *cursor;
        const char *field_end = field_schema;
        doclang_skip_schema(&field_end, end);
        if (index > 0) doclang_preview_append(output, "; ");
        doclang_preview_append_n(output, name, name_length);
        doclang_preview_append(output, " = ");
        if (Is_block(observed) && index < Wosize_val(observed))
          doclang_preview_child(output, field_schema, field_end,
                                schema_start, end, Field(observed, index),
                                depth + 1);
        else
          doclang_preview_append(output, "<opaque>");
        *cursor = field_end;
      }
      doclang_preview_append(output, "}");
      break;
    }
    case 'V': {
      size_t constants = doclang_schema_number(cursor, end, ':');
      size_t index;
      int matched = 0;
      for (index = 0; index < constants; index++) {
        size_t name_length;
        const char *name = doclang_schema_name(cursor, end, &name_length);
        if (!matched && Is_long(observed) && (uintnat)Long_val(observed) == index) {
          doclang_preview_append_n(output, name, name_length);
          matched = 1;
        }
      }
      {
        size_t blocks = doclang_schema_number(cursor, end, ':');
        for (index = 0; index < blocks; index++) {
          size_t tag = doclang_schema_number(cursor, end, ',');
          size_t name_length;
          const char *name = doclang_schema_name(cursor, end, &name_length);
          size_t arity = doclang_schema_number(cursor, end, ':');
          size_t field;
          int this_block = !matched && Is_block(observed)
                           && Tag_val(observed) == tag
                           && Wosize_val(observed) >= arity;
          if (this_block) {
            doclang_preview_append_n(output, name, name_length);
            if (arity > 0) doclang_preview_append(output, " (");
          }
          for (field = 0; field < arity; field++) {
            const char *field_schema = *cursor;
            const char *field_end = field_schema;
            doclang_skip_schema(&field_end, end);
            if (this_block) {
              if (field > 0) doclang_preview_append(output, ", ");
              doclang_preview_child(output, field_schema, field_end,
                                    schema_start, end, Field(observed, field),
                                    depth + 1);
            }
            *cursor = field_end;
          }
          if (this_block) {
            if (arity > 0) doclang_preview_append(output, ")");
            matched = 1;
          }
        }
      }
      if (!matched) doclang_preview_append(output, "<opaque>");
      break;
    }
    case '?':
      doclang_preview_dynamic(output, observed);
      break;
    case 'Z':
    default:
      doclang_preview_append(output, "<opaque>");
      break;
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

  {
    mlsize_t schema_length;
    const char *schema = doclang_metadata_field(metadata, 9, &schema_length);
    if (schema != NULL && schema_length > 0) {
      const char *cursor = schema;
      doclang_preview_buffer output = { buffer, capacity, 0 };
      if (capacity > 0) buffer[0] = 0;
      doclang_preview_schema(&output, &cursor, schema + schema_length,
                             NULL, NULL, observed, 0);
      return;
    }
  }

  if (Is_long(observed)) {
    intnat immediate = Long_val(observed);
    if (doclang_type_is(metadata, "unit")
        || doclang_type_ends_with(metadata, "-> unit")) {
      snprintf(buffer, capacity, "()");
    } else if (doclang_type_is(metadata, "bool")
               || doclang_type_ends_with(metadata, "-> bool")) {
      snprintf(buffer, capacity, "%s", immediate == 0 ? "false" : "true");
    } else if (doclang_type_is(metadata, "char")
               || doclang_type_ends_with(metadata, "-> char")) {
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

  if (doclang_type_ends_with(metadata, " ref")
      && Tag_val(observed) == 0 && Wosize_val(observed) == 1) {
    char element[96];
    doclang_preview_element(element, sizeof(element), Field(observed, 0));
    position = doclang_append(buffer, capacity, position, "{contents = ");
    position = doclang_append(buffer, capacity, position, element);
    doclang_append(buffer, capacity, position, "}");
    return;
  }

  if (doclang_type_ends_with(metadata, " array")
      && Tag_val(observed) == 0) {
    mlsize_t length = Wosize_val(observed);
    mlsize_t shown = length > 8 ? 8 : length;
    mlsize_t index;
    char element[96];
    position = doclang_append(buffer, capacity, position, "[|");
    for (index = 0; index < shown; index++) {
      if (index > 0) {
        position = doclang_append(buffer, capacity, position, "; ");
      }
      doclang_preview_element(element, sizeof(element), Field(observed, index));
      position = doclang_append(buffer, capacity, position, element);
    }
    if (shown < length) {
      position = doclang_append(buffer, capacity, position, "; ...");
    }
    doclang_append(buffer, capacity, position, "|]");
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
  char preview[4096] = "";
  char_os *path = caml_secure_getenv(T("DOCLANG_TRACE_PATH"));
  char *metadata_copy;
  mlsize_t metadata_length;
  mlsize_t public_metadata_length;
  mlsize_t site_length;
  FILE *channel;
  /* Keep backwards compatibility for standalone compiler users. Dox gives
     compiler traces their own bounded file so large Doc.* values cannot
     consume the trace budget (or vice versa). */
  if (path == NULL) path = caml_secure_getenv(T("DOCLANG_EVENT_PATH"));
  if (path == NULL) return;
  if (atomic_load(&doclang_trace_truncated)) return;

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
  public_metadata_length = doclang_public_metadata_length(metadata);
  site_length = doclang_site_length(metadata);
  metadata_copy = caml_stat_alloc_noexc(metadata_length);
  if (metadata_copy == NULL) return;
  memcpy(metadata_copy, String_val(metadata), metadata_length);

  caml_enter_blocking_section();
  caml_plat_lock_blocking(&doclang_output_lock);
  if (atomic_load(&doclang_trace_truncated)) {
    caml_plat_unlock(&doclang_output_lock);
    caml_leave_blocking_section();
    caml_stat_free(metadata_copy);
    return;
  }
  if (doclang_output_channel == NULL) {
    doclang_output_channel = fopen_os(path, T("ab"));
    if (doclang_output_channel != NULL)
      setvbuf(doclang_output_channel, NULL, _IOFBF, 65536);
  }
  channel = doclang_output_channel;
  if (channel != NULL) {
    size_t event_bytes =
      10 + (2 * site_length)
      + (2 * (strlen(phase) + 1 + strlen(occurrence_buffer) + 1
              + strlen(parent_buffer) + 1 + public_metadata_length + 1
              + (has_observed ? strlen(preview) : 0)));
    if (doclang_output_bytes + event_bytes > DOCLANG_EVENT_BYTE_LIMIT) {
      fputs("trace-truncated\t\t\n", channel);
      atomic_store(&doclang_trace_truncated, 1);
      caml_plat_unlock(&doclang_output_lock);
      caml_leave_blocking_section();
      caml_stat_free(metadata_copy);
      return;
    }
    fputs("observe\t", channel);
    doclang_hex(channel, metadata_copy, site_length);
    fputc('\t', channel);
    doclang_hex_c_string(channel, phase);
    doclang_hex_separator(channel);
    doclang_hex_c_string(channel, occurrence_buffer);
    doclang_hex_separator(channel);
    doclang_hex_c_string(channel, parent_buffer);
    doclang_hex_separator(channel);
    doclang_hex(channel, metadata_copy, public_metadata_length);
    doclang_hex_separator(channel);
    if (has_observed) doclang_hex_c_string(channel, preview);
    fputc('\n', channel);
    doclang_output_bytes += event_bytes;
  }
  caml_plat_unlock(&doclang_output_lock);
  caml_leave_blocking_section();
  caml_stat_free(metadata_copy);
}

static void doclang_release_frame(uintnat index)
{
  doclang_tail_capable[index] = 0;
  doclang_overapply_parents[index] = 0;
  doclang_overapply_remaining[index] = 0;
}

static value doclang_enter(value metadata, int tail_capable)
{
  uintnat occurrence = atomic_fetch_add(&doclang_counter, 1) + 1;
  uintnat parent;
  if (doclang_depth >= DOCLANG_STACK_LIMIT) {
    doclang_overflow_depth++;
    return Val_long(0);
  }
  if (doclang_pending_tail_parent != 0) {
    parent = doclang_pending_tail_parent;
    doclang_overapply_parents[doclang_depth] = parent;
    doclang_overapply_remaining[doclang_depth] =
      doclang_pending_tail_remaining;
    doclang_pending_tail_parent = 0;
    doclang_pending_tail_remaining = 0;
  } else {
    parent = doclang_depth == 0 ? 0 : doclang_stack[doclang_depth - 1];
    doclang_overapply_parents[doclang_depth] = 0;
    doclang_overapply_remaining[doclang_depth] = 0;
  }
  doclang_stack[doclang_depth] = occurrence;
  doclang_parents[doclang_depth] = parent;
  doclang_tail_capable[doclang_depth] = tail_capable;
  doclang_depth++;
  doclang_event("enter", occurrence, parent, metadata, Val_unit, 0);
  return Val_long(occurrence);
}

CAMLprim value caml_doclang_observe_enter(value metadata)
{
  return doclang_enter(metadata, 0);
}

CAMLprim value caml_doclang_observe_enter_tail(value metadata)
{
  return doclang_enter(metadata, 1);
}

static code_t doclang_function_code(value function)
{
  if (!Is_block(function)
      || (Tag_val(function) != Closure_tag && Tag_val(function) != Infix_tag)) {
    return NULL;
  }
  return Code_val(function);
}

static intnat doclang_registered_code_consumption(value function)
{
  code_t code = doclang_function_code(function);
  uintnat count;
  uintnat index;
  if (code == NULL) return 0;
  count = atomic_load_explicit(&doclang_function_count, memory_order_acquire);
  if (count > DOCLANG_FUNCTION_LIMIT) count = DOCLANG_FUNCTION_LIMIT;
  for (index = 0; index < count; index++) {
    if (atomic_load_explicit(&doclang_functions[index], memory_order_acquire)
      == code) {
      return atomic_load_explicit(&doclang_function_consumptions[index],
                                  memory_order_acquire);
    }
  }
  return 0;
}

static intnat doclang_partial_code_consumption(value function)
{
  code_t code = doclang_function_code(function);
  uintnat count;
  uintnat index;
  if (code == NULL) return 0;
  count = atomic_load_explicit(&doclang_partial_function_count,
                               memory_order_acquire);
  if (count > DOCLANG_FUNCTION_LIMIT) count = DOCLANG_FUNCTION_LIMIT;
  for (index = 0; index < count; index++) {
    if (atomic_load_explicit(&doclang_partial_functions[index],
                             memory_order_acquire) == code) {
      return atomic_load_explicit(
        &doclang_partial_function_consumptions[index], memory_order_acquire);
    }
  }
  return 0;
}

static intnat doclang_registered_function_consumption(
  value function, intnat supplied_arguments)
{
  intnat consumption;
  intnat total_consumption;
  intnat captured_arguments;
  if (doclang_function_code(function) == NULL) return 0;
  consumption = doclang_registered_code_consumption(function);
  if (consumption == 0) {
    total_consumption = doclang_partial_code_consumption(function);
    if (total_consumption > 0 && Wosize_val(function) >= 3) {
      captured_arguments = (intnat)Wosize_val(function) - 3;
      if (captured_arguments < total_consumption) {
        consumption = total_consumption - captured_arguments;
      }
    }
  }
  if (consumption > 0 && supplied_arguments >= consumption) {
    return consumption;
  }
  return 0;
}

CAMLprim value caml_doclang_observe_register_function(value function,
                                                       value consumption_value)
{
  code_t code = doclang_function_code(function);
  intnat consumption = Long_val(consumption_value);
  uintnat index;
  if (code == NULL || consumption <= 0
      || doclang_registered_code_consumption(function) != 0) {
    return Val_unit;
  }
  index = atomic_fetch_add_explicit(&doclang_function_count, 1,
                                    memory_order_acq_rel);
  if (index < DOCLANG_FUNCTION_LIMIT) {
    atomic_store_explicit(&doclang_function_consumptions[index], consumption,
                          memory_order_relaxed);
    atomic_store_explicit(&doclang_functions[index], code,
                          memory_order_release);
  }
  return Val_unit;
}

CAMLexport void caml_doclang_observe_register_partial(value original,
                                                       value partial)
{
  intnat consumption = doclang_registered_code_consumption(original);
  code_t code = doclang_function_code(partial);
  uintnat index;
  if (consumption <= 0 || code == NULL
      || doclang_partial_code_consumption(partial) != 0) {
    return;
  }
  index = atomic_fetch_add_explicit(&doclang_partial_function_count, 1,
                                    memory_order_acq_rel);
  if (index < DOCLANG_FUNCTION_LIMIT) {
    atomic_store_explicit(&doclang_partial_function_consumptions[index],
                          consumption, memory_order_relaxed);
    atomic_store_explicit(&doclang_partial_functions[index], code,
                          memory_order_release);
  }
}

CAMLprim value caml_doclang_observe_is_registered_function(
  value function, value supplied_arguments)
{
  return Val_bool(doclang_registered_function_consumption(
                    function, Long_val(supplied_arguments)) > 0);
}

CAMLprim value caml_doclang_observe_tail_handoff(value metadata,
                                                 value occurrence_value,
                                                 value function,
                                                 value supplied_arguments)
{
  uintnat occurrence = Long_val(occurrence_value);
  uintnat index;
  intnat supplied = Long_val(supplied_arguments);
  intnat consumed =
    doclang_registered_function_consumption(function, supplied);
  if (consumed == 0 || occurrence == 0
      || doclang_depth == 0
      || doclang_stack[doclang_depth - 1] != occurrence) {
    return Val_unit;
  }
  index = doclang_depth - 1;
  doclang_event("tail", occurrence, doclang_parents[index], metadata,
                Val_unit, 0);
  doclang_release_frame(index);
  doclang_depth--;
  if (doclang_depth > 0 && doclang_tail_capable[doclang_depth - 1]) {
    index = doclang_depth - 1;
    doclang_event("tail", doclang_stack[index], doclang_parents[index],
                  metadata, Val_unit, 0);
    doclang_release_frame(index);
    doclang_depth--;
  }
  doclang_pending_tail_parent = occurrence;
  doclang_pending_tail_remaining = (uintnat)(supplied - consumed);
  return Val_unit;
}

static value doclang_leave(const char *phase, value metadata,
                           value occurrence_value, value observed)
{
  uintnat occurrence = Long_val(occurrence_value);
  uintnat parent;
  uintnat overapply_parent;
  uintnat overapply_remaining;
  doclang_pending_tail_parent = 0;
  doclang_pending_tail_remaining = 0;
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
  parent = doclang_parents[doclang_depth - 1];
  overapply_parent = doclang_overapply_parents[doclang_depth - 1];
  overapply_remaining = doclang_overapply_remaining[doclang_depth - 1];
  doclang_release_frame(doclang_depth - 1);
  doclang_depth--;
  doclang_event(phase, occurrence, parent, metadata, observed, 1);
  if (overapply_parent != 0 && overapply_remaining > 0
      && strcmp(phase, "return") == 0) {
    intnat consumed;
    uintnat fallback =
      doclang_depth == 0 ? 0 : doclang_stack[doclang_depth - 1];
    doclang_event("tail", overapply_parent, fallback, metadata, Val_unit, 0);
    consumed = doclang_registered_function_consumption(
      observed, (intnat)overapply_remaining);
    if (consumed > 0) {
      doclang_pending_tail_parent = overapply_parent;
      doclang_pending_tail_remaining = overapply_remaining - consumed;
    }
  }
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
  parent = doclang_parents[doclang_depth - 1];
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
