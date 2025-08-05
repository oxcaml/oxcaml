# Static_cast Operator Documentation

The `Static_cast` operator in `scalar.mli` allows conversions between different kinds of numerical types in OCaml. This document provides a comprehensive matrix of all possible conversions.

## Conversion Matrix

The conversion tables are split by destination type for better readability. Each cell describes the operation performed.

### Table 1: Conversions to Tagged/Untagged Integers

| Source ↓ / Destination → | Tagged int | Untagged int | Untagged int8 | Untagged int16 |
|--------------------------|------------|--------------|----------------|-----------------|
| **Tagged int**           | no-op      | untag        | untag + trunc  | untag + trunc   |
| **Untagged int**         | tag        | no-op        | trunc          | trunc           |
| **Untagged int8**        | sign-ext + tag | sign-ext | no-op          | sign-ext        |
| **Untagged int16**       | sign-ext + tag | sign-ext | trunc          | no-op           |
| **Boxed int32**          | unbox + trunc + tag (32-bit: unbox + tag) | unbox + sign-ext (32-bit: unbox) | unbox + trunc | unbox + trunc |
| **Unboxed int32**        | trunc + tag (32-bit: tag) | sign-ext (32-bit: no-op) | trunc          | trunc           |
| **Boxed int64**          | unbox + trunc + tag | unbox + trunc | unbox + trunc | unbox + trunc |
| **Unboxed int64**        | trunc + tag | trunc        | trunc          | trunc           |
| **Boxed nativeint**      | unbox + trunc + tag (32-bit: unbox + tag) | unbox + trunc (32-bit: unbox) | unbox + trunc | unbox + trunc |
| **Unboxed nativeint**    | trunc + tag (32-bit: tag) | trunc (32-bit: no-op) | trunc          | trunc           |
| **Boxed float**          | unbox + float→int + tag | unbox + float→int | unbox + float→int + trunc | unbox + float→int + trunc |
| **Unboxed float**        | float→int + tag | float→int    | float→int + trunc      | float→int + trunc       |
| **Boxed float32**        | unbox + float→int + tag | unbox + float→int | unbox + float→int + trunc | unbox + float→int + trunc |
| **Unboxed float32**      | float→int + tag | float→int    | float→int + trunc      | float→int + trunc       |

### Table 2: Conversions to Boxed/Unboxed int32/int64/nativeint

| Source ↓ / Destination → | Unboxed int32 | Unboxed int64 | Unboxed nativeint | Boxed int32 | Boxed int64 | Boxed nativeint |
|--------------------------|----------------|----------------|-------------------|-------------|-------------|------------------|
| **Tagged int**           | untag + trunc (32-bit: untag) | untag + sign-ext | untag + sign-ext (32-bit: untag) | untag + trunc + box (32-bit: untag + box) | untag + sign-ext + box | untag + sign-ext + box (32-bit: untag + box) |
| **Untagged int**         | trunc (32-bit: no-op) | sign-ext (32-bit: no-op) | sign-ext (32-bit: no-op) | trunc + box (32-bit: box) | sign-ext + box (32-bit: box) | sign-ext + box (32-bit: box) |
| **Untagged int8**        | sign-ext      | sign-ext      | sign-ext          | sign-ext + box | sign-ext + box | sign-ext + box   |
| **Untagged int16**       | sign-ext      | sign-ext      | sign-ext          | sign-ext + box | sign-ext + box | sign-ext + box   |
| **Boxed int32**          | unbox          | unbox + sign-ext | unbox + sign-ext (32-bit: unbox) | no-op       | unbox + sign-ext + box | unbox + sign-ext + box (32-bit: no-op) |
| **Unboxed int32**        | no-op          | sign-ext      | sign-ext (32-bit: no-op) | box         | sign-ext + box | sign-ext + box (32-bit: box) |
| **Boxed int64**          | unbox + trunc | unbox          | unbox (32-bit: unbox + trunc) | unbox + trunc + box | no-op       | no-op (32-bit: unbox + trunc + box) |
| **Unboxed int64**        | trunc         | no-op          | no-op (32-bit: trunc) | trunc + box | box         | trunc + box      |
| **Boxed nativeint**      | unbox + trunc (32-bit: unbox) | unbox (32-bit: unbox + sign-ext) | unbox             | unbox + trunc + box (32-bit: no-op) | unbox + sign-ext + box (32-bit: unbox + sign-ext + box) | no-op            |
| **Unboxed nativeint**    | trunc (32-bit: no-op) | no-op (32-bit: sign-ext) | no-op             | trunc + box (32-bit: box) | box (32-bit: sign-ext + box) | box              |
| **Boxed float**          | unbox + float→int + trunc | unbox + float→int (32-bit: unbox + float→int + trunc) | unbox + float→int | unbox + float→int + trunc + box | unbox + float→int + box (32-bit: unbox + float→int + trunc + box) | unbox + float→int + box |
| **Unboxed float**        | float→int + trunc     | float→int (32-bit: float→int + trunc)     | float→int         | float→int + trunc + box | float→int + box (32-bit: float→int + trunc + box) | float→int + box  |
| **Boxed float32**        | unbox + float→int + trunc | unbox + float→int (32-bit: unbox + float→int + trunc) | unbox + float→int | unbox + float→int + trunc + box | unbox + float→int + box (32-bit: unbox + float→int + trunc + box) | unbox + float→int + box |
| **Unboxed float32**      | float→int + trunc     | float→int (32-bit: float→int + trunc)     | float→int         | float→int + trunc + box | float→int + box (32-bit: float→int + trunc + box) | float→int + box  |

### Table 3: Conversions to Floating-Point Types

| Source ↓ / Destination → | Boxed float | Unboxed float | Boxed float32 | Unboxed float32 |
|--------------------------|-------------|----------------|---------------|------------------|
| **Tagged int**           | untag + int→float + box | untag + int→float | untag + int→float + box | untag + int→float |
| **Untagged int**         | int→float + box | int→float     | int→float + box | int→float        |
| **Untagged int8**        | sign-ext + int→float + box | sign-ext + int→float | sign-ext + int→float + box | sign-ext + int→float |
| **Untagged int16**       | sign-ext + int→float + box | sign-ext + int→float | sign-ext + int→float + box | sign-ext + int→float |
| **Boxed int32**          | unbox + int→float + box | unbox + int→float | unbox + int→float + box | unbox + int→float |
| **Unboxed int32**        | int→float + box | int→float     | int→float + box | int→float        |
| **Boxed int64**          | unbox + int→float + box | unbox + int→float | unbox + int→float + box | unbox + int→float |
| **Unboxed int64**        | int→float + box | int→float     | int→float + box | int→float        |
| **Boxed nativeint**      | unbox + int→float + box | unbox + int→float | unbox + int→float + box | unbox + int→float |
| **Unboxed nativeint**    | int→float + box | int→float     | int→float + box | int→float        |
| **Boxed float**          | no-op       | unbox          | unbox + f64→f32 + box | unbox + f64→f32  |
| **Unboxed float**        | box         | no-op          | f64→f32 + box | f64→f32          |
| **Boxed float32**        | unbox + f32→f64 + box | unbox + f32→f64 | no-op         | unbox            |
| **Unboxed float32**      | f32→f64 + box | f32→f64       | box           | no-op            |

## Operation Descriptions

- **no-op**: No conversion (same type)
- **tag**: Shift left by 1 and add 1 (tagging operation: (x << 1) | 1)
- **untag**: Arithmetic shift right by 1 (untagging operation, preserves sign)
- **box**: Allocate a boxed representation
- **unbox**: Extract value from boxed representation
- **sign-ext**: Sign-extend from narrower to wider integer type
- **trunc**: Truncate from wider to narrower integer type
- **int→float**: Convert integer to floating-point
- **float→int**: Convert floating-point to nativeint-sized integer (truncates towards zero)
- **f32→f64**: Convert float32 to float64 (lossless)
- **f64→f32**: Convert float64 to float32 (may lose precision)
- **+**: Sequential operations (e.g., "untag + sign-ext" means untag first, then sign-extend)

## Notes

1. **Platform Differences**: The table shows operations for 64-bit systems by default. When operations differ on 32-bit systems, they are shown in parentheses with "(32-bit: ...)". Key differences:
   - **64-bit systems**: tagged int = 63 bits, untagged int = 64 bits, nativeint = 64 bits
   - **32-bit systems**: tagged int = 31 bits, untagged int = 32 bits, nativeint = 32 bits
   - This means some conversions that require truncation/sign-extension on 64-bit systems are no-ops on 32-bit systems

2. **Tagging**: OCaml uses tagged integers where the value is shifted left by 1 bit and the bottom bit is set to 1 to distinguish integers from pointers.

3. **Boxing**: Creates a heap-allocated wrapper around primitive values. The allocation mode (heap vs local) depends on the context.

4. **Integer Width Conversions**: 
   - **Sign-extension**: Used when converting from narrower to wider integer types (e.g., int8 → int32), preserving the sign bit
   - **Truncation**: Used when converting from wider to narrower integer types (e.g., int64 → int32), keeping only the lower bits
   
5. **Integer/Float Conversions**:
   - **int→float**: Converts integers to floating-point representation
   - **float→int**: Converts floating-point to integer by truncating towards zero

6. **Two-step Operations**: Many conversions require two steps, such as unboxing a value and then converting it to a different numeric type, or converting a value and then boxing the result.

7. **Int8/Int16 Special Handling**: These smaller integer types require special handling when tagging/untagging as they need to be converted to/from the native immediate representation.

## Implementation Reference

The semantics of these operations are implemented in `lambda_to_flambda_primitives.ml` in the `static_cast` and `static_cast0` functions.