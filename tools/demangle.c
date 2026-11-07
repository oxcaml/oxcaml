/******************************************************************************
 *                                  OxCaml                                    *
 *                          Kevin Svetlitski, Jane Street                     *
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
 ******************************************************************************/

/* A demangler for the linker symbols of compiled OCaml code.

   This file is a self-contained C99 reimplementation of the default behavior
   of ocamlfilt (tools/ocamlfilt.ml in the OxCaml repository), so that it can
   be dropped as is into tools such as perf, llvm-symbolizer or addr2line. It
   depends only on the C standard library and recognizes the two schemes that
   OxCaml uses to mangle OCaml names into symbols. Everything else (runtime
   functions such as caml_alloc, C and C++ symbols) is reported as "not an
   OCaml symbol".

   1. The flat scheme (OCaml's default) is used for all data symbols and, in
      units compiled without -name-mangling-scheme structured, for functions
      too. A flat symbol is "caml" followed by an uppercase letter (the first
      letter of the compilation unit's name); macOS adds a leading underscore,
      so "_caml" followed by an uppercase letter is accepted as well. The
      prefix is dropped and the rest is decoded in one of two styles, chosen
      by scanning the symbol first:
      - Linux-like: "__" and "." both separate path components and become
        "."; "$xx" (two hexadecimal digits of either case) is an escaped byte.
        OCaml 5.2 and earlier only ever produced this style (with "__"), so it
        subsumes the demangling of those older symbols.
      - macOS-like: a bare "$" separates components and becomes "."; "$$xx" is
        an escaped byte.
      The style is decided by the first decisive sequence found scanning
      from left to right: "." or "__" means Linux-like; "$$", or a "$" not
      followed by two hexadecimal digits, means macOS-like. Without one, a
      symbol that contains "$" followed by two hexadecimal digits is
      macOS-like, and any other symbol is Linux-like. Everything else is
      copied through, so stamps such as "_123" or "_12_34_code" are kept.

   2. The structured scheme is used for the functions of units compiled with
      -name-mangling-scheme structured. A structured symbol is "_Caml"
      ("__Caml" on macOS) followed by a non-empty sequence of items, each a
      one-letter tag and a payload:
        U<ident>   compilation unit        I          inline marker
        M<ident>   module                  O<ident>   class
        F<ident>   function or value       L<n>_      anonymous function
        S<n>_      anonymous module        Z<n>_      lazy expression
        D<n>_      compiler stamp          P<k>_ item_1 ... item_k
                                                      partial application
      Numbers are decimal and terminated by "_"; the k items after P<k>_ are
      the path of the function that was partially applied (k = 0 when it is
      unknown). An identifier is a decimal byte count followed by that many
      bytes, which are the identifier itself when it only contains
      [0-9A-Za-z_] and does not start with a digit. Otherwise it is written
      "u<count><escaped>_<raw>", where <raw> holds the identifier's bytes that
      are in [0-9A-Za-z_] and <escaped> says where the other bytes go: for
      each run of such bytes, the number of raw bytes since the previous run
      in base 26 written with A-Z (A = 0), then each byte of the run as two
      lowercase hexadecimal digits. No byte may remain after the last item.

      A path is rendered as its items joined with ".": a unit as its name with
      a run of exactly two interior underscores turned into "." (the unit of a
      wrapped library, Stdlib__List, is a module path, Stdlib.List); M, O and
      F as the decoded identifier; L, S and Z as fn_{n}, mod_{n} and
      lazy_{n}; P as partial{<callee path>}, or partial{?} when k = 0; I as
      <specialization_of>. Stamps (D) are omitted so that the demangled name
      only depends on the source and is stable from one build to the next.
      When the path contains an I marker, the code was copied by cross-module
      inlining from the unit named after the marker into the unit named
      before it; only the items after the first marker are rendered, so that
      every copy of a function has the name of its definition site.

   The complete specification of the structured scheme, with test vectors, is
   doc/STRUCTURED_NAME_MANGLING.md in the OxCaml repository. The authoritative
   encoder and parser are in utils/structured_mangling.ml.

   Limits: decimal numbers above 2^62 - 1 (the largest OCaml integer, which
   is what the reference implementation parses them into) are rejected, as
   are partial applications nested more than MAX_PARTIAL_DEPTH deep. A
   demangled name that contains a NUL byte is truncated at that byte by the
   C string interface. */

#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "demangle.h"

/* The largest number accepted in a payload, 2^62 - 1: see above. */
#define MAX_DECIMAL 4611686018427387903ULL

/* Nesting bound for the callee paths of partial applications. In practice a
   callee path never contains a partial application at all. */
#define MAX_PARTIAL_DEPTH 64

/* Character classes, restricted to ASCII so that they do not depend on the
   locale. */

static int is_digit(char c) { return c >= '0' && c <= '9'; }

static int is_upper(char c) { return c >= 'A' && c <= 'Z'; }

static int is_lower_hex_digit(char c)
{
  return is_digit(c) || (c >= 'a' && c <= 'f');
}

static int is_hex_digit(char c)
{
  return is_lower_hex_digit(c) || (c >= 'A' && c <= 'F');
}

static int hex_digit_value(char c)
{
  if (is_digit(c)) return c - '0';
  if (c >= 'a' && c <= 'f') return c - 'a' + 10;
  return c - 'A' + 10;
}

/* A growable output buffer. An allocation failure is sticky: the buffer
   accepts and discards all further output, and the failure is checked once
   at the end. */

struct buffer {
  char *data;
  size_t length;
  size_t capacity;
  int failed;
};

static void buffer_init(struct buffer *b)
{
  b->data = NULL;
  b->length = 0;
  b->capacity = 0;
  b->failed = 0;
}

/* Ensures room for [extra] more bytes plus the terminating NUL. */
static int buffer_reserve(struct buffer *b, size_t extra)
{
  size_t needed, capacity;
  char *data;
  if (b->failed) return 0;
  if (extra >= SIZE_MAX - b->length) {
    b->failed = 1;
    return 0;
  }
  needed = b->length + extra + 1;
  if (needed <= b->capacity) return 1;
  capacity = b->capacity == 0 ? 64 : b->capacity;
  while (capacity < needed) {
    if (capacity > SIZE_MAX / 2) {
      capacity = needed;
    } else {
      capacity *= 2;
    }
  }
  data = realloc(b->data, capacity);
  if (data == NULL) {
    b->failed = 1;
    return 0;
  }
  b->data = data;
  b->capacity = capacity;
  return 1;
}

static void buffer_append(struct buffer *b, const char *bytes, size_t n)
{
  if (!buffer_reserve(b, n)) return;
  memcpy(b->data + b->length, bytes, n);
  b->length += n;
}

static void buffer_append_char(struct buffer *b, char c)
{
  buffer_append(b, &c, 1);
}

static void buffer_append_string(struct buffer *b, const char *s)
{
  buffer_append(b, s, strlen(s));
}

static void buffer_append_number(struct buffer *b, unsigned long long n)
{
  char digits[32];
  int length = snprintf(digits, sizeof digits, "%llu", n);
  if (length < 0 || (size_t)length >= sizeof digits) {
    b->failed = 1;
    return;
  }
  buffer_append(b, digits, (size_t)length);
}

/* Returns the contents as a NUL-terminated string that the caller must free,
   or NULL if an allocation failed. */
static char *buffer_finish(struct buffer *b)
{
  if (!buffer_reserve(b, 0)) {
    free(b->data);
    return NULL;
  }
  b->data[b->length] = '\0';
  return b->data;
}

/* Reads a decimal number at [*cursor], which must have at least one digit,
   and advances the cursor past it. Fails on overflow of MAX_DECIMAL. */
static int parse_decimal(const char **cursor, const char *end,
                         unsigned long long *value)
{
  const char *p = *cursor;
  unsigned long long n = 0;
  if (p >= end || !is_digit(*p)) return 0;
  for (; p < end && is_digit(*p); p++) {
    unsigned long long digit = (unsigned long long)(*p - '0');
    if (n > (MAX_DECIMAL - digit) / 10) return 0;
    n = n * 10 + digit;
  }
  *cursor = p;
  *value = n;
  return 1;
}

/* A numeric payload: a decimal number terminated by "_". */
static int parse_number(const char **cursor, const char *end,
                        unsigned long long *value)
{
  const char *p = *cursor;
  if (!parse_decimal(&p, end, value)) return 0;
  if (p >= end || *p != '_') return 0;
  *cursor = p + 1;
  return 1;
}

/* Decodes the payload of an escaped identifier, "<escaped>_<raw>", into
   [out]: see the header comment and Section 2 of the specification. */
static int decode_escaped_identifier(const char *payload, size_t n,
                                     struct buffer *out)
{
  const char *end = payload + n;
  const char *separator = memchr(payload, '_', n);
  const char *escaped = payload;
  const char *raw;
  if (separator == NULL) return 0;
  raw = separator + 1;
  for (;;) {
    unsigned long long position = 0;
    if (*escaped == '_') {
      buffer_append(out, raw, (size_t)(end - raw));
      return 1;
    }
    if (!is_upper(*escaped)) return 0;
    /* [escaped] stays before [separator]: the loop stops at a non-letter and
       the separator is one. */
    while (is_upper(*escaped)) {
      if (position > (ULLONG_MAX - 25) / 26) return 0;
      position = position * 26 + (unsigned long long)(*escaped - 'A');
      escaped++;
    }
    if (position > (unsigned long long)(end - raw)) return 0;
    buffer_append(out, raw, (size_t)position);
    raw += position;
    /* The escaped bytes of this run, each as a pair of lowercase hexadecimal
       digits; a digit without its partner, or a partner that is not a
       lowercase hexadecimal digit, is malformed. */
    while (is_lower_hex_digit(*escaped)) {
      if (escaped + 1 >= separator || !is_lower_hex_digit(escaped[1]))
        return 0;
      buffer_append_char(out, (char)((hex_digit_value(escaped[0]) << 4)
                                     | hex_digit_value(escaped[1])));
      escaped += 2;
    }
  }
}

/* Decodes the identifier at [*cursor], appends it to [out] and advances the
   cursor past it. */
static int decode_identifier(const char **cursor, const char *end,
                             struct buffer *out)
{
  const char *p = *cursor;
  unsigned long long n;
  int escaped = 0;
  if (p < end && *p == 'u') {
    escaped = 1;
    p++;
  }
  if (!parse_decimal(&p, end, &n)) return 0;
  if (n == 0 || n > (unsigned long long)(end - p)) return 0;
  if (escaped) {
    if (!decode_escaped_identifier(p, (size_t)n, out)) return 0;
  } else {
    buffer_append(out, p, (size_t)n);
  }
  *cursor = p + n;
  return 1;
}

/* Rewrites the compilation unit name at [out->data + start] in place: a run
   of exactly two underscores strictly inside the name becomes ".", every
   other run is kept. The rewritten name is never longer than the original,
   and the write position never overtakes the read position, so this is safe
   to do in place. */
static void render_unit_name(struct buffer *out, size_t start)
{
  size_t read, write = start, pending = 0;
  if (out->failed) return;
  for (read = start; read < out->length; read++) {
    char c = out->data[read];
    if (c == '_') {
      pending++;
    } else {
      if (pending == 2 && write > start) {
        out->data[write++] = '.';
      } else {
        memset(out->data + write, '_', pending);
        write += pending;
      }
      pending = 0;
      out->data[write++] = c;
    }
  }
  memset(out->data + write, '_', pending);
  out->length = write + pending;
}

/* Rendering of a sequence of items: [rendered] counts the items of the
   sequence that produced output (all but stamps), so that they can be joined
   with ".". */
static void begin_item(struct buffer *out, size_t *rendered)
{
  if (*rendered > 0) buffer_append_char(out, '.');
  (*rendered)++;
}

/* Parses the item at [*cursor], renders it into [out] and advances the cursor
   past it. [depth] is the number of enclosing partial-application items. */
static int parse_item(const char **cursor, const char *end, int depth,
                      struct buffer *out, size_t *rendered)
{
  const char *p = *cursor;
  unsigned long long n;
  size_t start;
  char tag;
  if (p >= end) return 0;
  tag = *p++;
  switch (tag) {
  case 'U':
    begin_item(out, rendered);
    start = out->length;
    if (!decode_identifier(&p, end, out)) return 0;
    render_unit_name(out, start);
    break;
  case 'M':
  case 'O':
  case 'F':
    begin_item(out, rendered);
    if (!decode_identifier(&p, end, out)) return 0;
    break;
  case 'L':
  case 'S':
  case 'Z':
    if (!parse_number(&p, end, &n)) return 0;
    begin_item(out, rendered);
    buffer_append_string(out, tag == 'L' ? "fn_{"
                              : tag == 'S' ? "mod_{"
                                           : "lazy_{");
    buffer_append_number(out, n);
    buffer_append_char(out, '}');
    break;
  case 'D':
    if (!parse_number(&p, end, &n)) return 0;
    break;
  case 'I':
    begin_item(out, rendered);
    buffer_append_string(out, "<specialization_of>");
    break;
  case 'P': {
    size_t callee_rendered = 0;
    unsigned long long i;
    if (depth >= MAX_PARTIAL_DEPTH) return 0;
    if (!parse_number(&p, end, &n)) return 0;
    begin_item(out, rendered);
    buffer_append_string(out, "partial{");
    if (n == 0) buffer_append_char(out, '?');
    for (i = 0; i < n; i++) {
      if (!parse_item(&p, end, depth + 1, out, &callee_rendered)) return 0;
    }
    buffer_append_char(out, '}');
    break;
  }
  default:
    return 0;
  }
  *cursor = p;
  return 1;
}

/* Demangles the items of a structured symbol, [p] pointing just after the
   prefix. */
static int demangle_structured(const char *p, const char *end,
                               struct buffer *out)
{
  size_t rendered = 0;
  /* Where the rendering of the items after the first inline marker starts,
     if there is such a marker and at least one item follows it. */
  int seen_marker = 0, items_after_marker = 0;
  size_t cut = 0;
  if (p >= end) return 0;
  while (p < end) {
    int is_marker = !seen_marker && *p == 'I';
    if (!parse_item(&p, end, 0, out, &rendered)) return 0;
    if (is_marker) {
      seen_marker = 1;
      items_after_marker = p < end;
      cut = out->length;
    }
  }
  if (items_after_marker && !out->failed) {
    /* Keep only the definition site. Whatever follows the marker in the
       output starts with the "." that joined it to the marker. */
    if (out->length > cut) {
      memmove(out->data, out->data + cut + 1, out->length - cut - 1);
      out->length -= cut + 1;
    } else {
      out->length = 0;
    }
  }
  return 1;
}

/* Returns the length of the flat prefix of [symbol], or 0 if it has none. */
static size_t flat_prefix_length(const char *symbol, size_t length)
{
  if (length > 4 && memcmp(symbol, "caml", 4) == 0 && is_upper(symbol[4]))
    return 4;
  if (length > 5 && memcmp(symbol, "_caml", 5) == 0 && is_upper(symbol[5]))
    return 5;
  return 0;
}

enum flat_style { FLAT_LINUX_LIKE, FLAT_MACOS_LIKE };

static enum flat_style detect_flat_style(const char *p, const char *end)
{
  int saw_dollar_hex_pair = 0;
  while (p < end) {
    switch (*p) {
    case '.':
      return FLAT_LINUX_LIKE;
    case '_':
      if (p + 1 < end && p[1] == '_') return FLAT_LINUX_LIKE;
      p++;
      break;
    case '$':
      if (p + 1 < end && p[1] == '$') return FLAT_MACOS_LIKE;
      if (p + 2 < end && is_hex_digit(p[1]) && is_hex_digit(p[2])) {
        saw_dollar_hex_pair = 1;
        p += 3;
        break;
      }
      return FLAT_MACOS_LIKE;
    default:
      p++;
      break;
    }
  }
  return saw_dollar_hex_pair ? FLAT_MACOS_LIKE : FLAT_LINUX_LIKE;
}

/* Demangles a flat symbol, [p] pointing just after the prefix. */
static void demangle_flat(const char *p, const char *end, struct buffer *out)
{
  enum flat_style style = detect_flat_style(p, end);
  while (p < end) {
    if (style == FLAT_MACOS_LIKE && *p == '$') {
      if (p + 3 < end && p[1] == '$' && is_hex_digit(p[2])
          && is_hex_digit(p[3])) {
        buffer_append_char(out, (char)((hex_digit_value(p[2]) << 4)
                                       | hex_digit_value(p[3])));
        p += 4;
      } else {
        buffer_append_char(out, '.');
        p++;
      }
    } else if (style == FLAT_LINUX_LIKE && *p == '$' && p + 2 < end
               && is_hex_digit(p[1]) && is_hex_digit(p[2])) {
      buffer_append_char(out, (char)((hex_digit_value(p[1]) << 4)
                                     | hex_digit_value(p[2])));
      p += 3;
    } else if (style == FLAT_LINUX_LIKE && *p == '_' && p + 1 < end
               && p[1] == '_') {
      buffer_append_char(out, '.');
      p += 2;
    } else {
      buffer_append_char(out, *p);
      p++;
    }
  }
}

static int has_prefix(const char *symbol, size_t length, const char *prefix)
{
  size_t n = strlen(prefix);
  return length >= n && memcmp(symbol, prefix, n) == 0;
}

const char *demangle_ocaml_symbol(size_t length, const char *symbol)
{
  struct buffer out;
  const char *end;
  size_t prefix_length;
  if (symbol == NULL) return NULL;
  end = symbol + length;
  buffer_init(&out);
  /* The structured prefix is tried first; a symbol that has it but is
     malformed is not an OCaml symbol, rather than a flat one. */
  if (has_prefix(symbol, length, "_Caml")) {
    if (!demangle_structured(symbol + 5, end, &out)) goto malformed;
  } else if (has_prefix(symbol, length, "__Caml")) {
    if (!demangle_structured(symbol + 6, end, &out)) goto malformed;
  } else if ((prefix_length = flat_prefix_length(symbol, length)) > 0) {
    demangle_flat(symbol + prefix_length, end, &out);
  } else {
    return NULL;
  }
  return buffer_finish(&out);

malformed:
  free(out.data);
  return NULL;
}
