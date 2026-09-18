# Scalar Types

This document specifies the semantics of scalar types and their conversions in the OxCaml compiler. It covers both the high-level OCaml value representation (front-end) and the lower-level representation used during compilation and at runtime (back-end).

## Front-End Semantics

All OCaml values are interpreted as signed, and all operations are on signed integers, unless otherwise specified.

Integral OCaml values (i.e., non-naked types) with 31 data bits or fewer are represented as tagged integers, stored in a machine-width container. To conform to the uniform value representation, they are always stored sign-extended inside an immediate.
Larger integral types and all floating-point types have explicit boxed representations.

Casting between scalar types is described in the next section.

## Back-End Semantics

### Core Concepts

#### 1. Scalar Type Categories

Scalar types fall into two main categories, describing their bit representation and their numeric properties:

- **Integral types**: Integer values that can be:
    - **Tagged**: Integers with a reserved tag bit (lowest bit set to 1) - used for compatability with the OCaml value representation
    - **Untagged**: Integers without tagging

- **Float types**: Floating-point values:
    - **Float32**: Single-precision IEEE 754
    - **Float64**: Double-precision IEEE 754

#### 2. Integral Type Properties

Every integral type is defined by three properties:

- **Bit width**: The number of bits needed to store the value
- **Signedness**: Whether inhabitants of the type are interpreted as signed or unsigned
- **Tagging**: Whether the type reserves a bit for runtime tagging

The tag bit serves to distinguish integer values from pointers during garbage collection. In the OCaml runtime, all GC-visible integers are either tagged (for small integer values) or pointers to heap-allocated objects.

**Naming convention**: Tagged integer types are named by their data bit capacity, not their total width. For example, a "tagged `int63`" is 64 bits wide total, storing 63 data bits (the actual numeric value) plus 1 tag bit (always set to 1). The tag bit always occupies the least significant bit position.

To illustrate: the OCaml integer 5 is represented as a tagged integer with:
- Numeric value: 5
- Data bits: `101` (stored in bits `Sys.int_size`..1)
- Tag bit: `1` (stored in bit 0)
- Full representation: `(5 << 1) | 1 = 11 = 0x0B`

#### 3. Value Representation

In reality, integer bit representations don't exist in a vacuum, they exist inside of their "containers". A "container" refers to the physical storage location (register or memory word) that holds a value.

Currently, all containers are assumed to be `Sys.word_size` bits, and integral values must fill their entire container in a manner determined by their signedness:
- **Signed**: Sign-extended (sign bit replicated to fill high bits)
- **Unsigned**: Zero-extended (high bits filled with zeros)

**Future Direction**: We plan to move away from requiring untagged values to fill their containers (see Future Work section).
## Conversion Semantics

### Information Preservation

A conversion preserves information when the destination type can represent every possible value of the source type.

#### Rules for Information-Preserving Conversions:

1. **Same signedness** (signed→signed or unsigned→unsigned):
    - Preserves information if: `src_bit_width ≤ dst_bit_width`

2. **Unsigned to signed**:
    - Preserves information if: `src_bit_width < dst_bit_width`
    - The extra bit is needed to ensure non-negative representation

3. **Signed to unsigned**:
    - Never preserves information (negative values cannot be represented)

4. **Float conversions**:
    - Float32→Float64: Always preserves information
    - Float64→Float32: May lose precision

### Extension Strategies

When converting to a wider type, the signedness of the *source* is used to fill the additional bits:

- **Sign extension**: For signed source types, the sign bit (most significant bit) is replicated to fill the additional bits. This preserves the numeric value for both positive and negative numbers.
- **Zero extension**: For unsigned source types, the additional bits are filled with zeros. This preserves the numeric value for non-negative numbers.

**Note**: In the current model, these extensions fill the entire container. In the future model, extension will only be needed up to the destination type's bit width, with container bits beyond that being undefined.

### Truncation to Narrower Types

When converting an integer to a type with fewer bits, the value is truncated by keeping only the lower bits that fit in the destination type. The semantics depend on the destination signedness:

1. **Truncation Process**:
    - Keep only the least significant data bits that fit in the destination
    - Discard all higher-order data bits
    - For tagged destinations, the destination width includes space for both data bits and tag bit
    - For tagged sources, first extract the data bits (remove tag), then truncate

**Note**: In the current model, truncation must be followed by sign- or zero-extension to fill the container. In the future C-like model, truncation that preserves tagging (i.e., both source and destination are tagged or both are untagged) will be a no-op, as the value bits remain in the same positions and high bits are ignored.

2. **Result Interpretation**:
    - **To unsigned**: The truncated bits are interpreted as an unsigned value
    - **To signed**: The truncated bits are interpreted as a two's complement signed value

3. **Value Changes**:
    - **Unsigned to smaller unsigned**: Values exceeding the destination range wrap around modulo `2^dst_bit_width`
    - **Signed to smaller signed**: Values outside [`-2^(dst_bit_width-1)`, `2^(dst_bit_width-1)-1`] wrap around
    - **Cross-signedness truncation**: Can change both magnitude and sign


Example effects (see full examples section for detailed walkthroughs):
- 300 (`int16`) → `int8` unsigned: keeps lower 8 bits, wraps to 44
- -100 (`int16`) → `int8` unsigned: keeps lower 8 bits, becomes 156
- 200 (`int16`) → `int8` signed: keeps lower 8 bits, wraps to -56

## Cast Operations

Static cast operations convert values between types with these semantics:

### Integral to Integral

1. **Information-preserving casts**:
    - Current model: No operation needed (value already fills container)
    - Future model: May require extension/truncation to match destination width

2. **Non-preserving casts**: Apply appropriate extension or truncation:
    - To signed: Use sign extension (currently to fill container, future only to destination width)
    - To unsigned: Use zero extension (currently to fill container, future only to destination width)

3. **Tagged/Untagged conversions**:

    **Untagged to Tagged**:
    - First apply any necessary conversion to the untagged value (extension/truncation)
    - Shift the resulting data bits left by 1 position
    - Set the least significant bit to 1 (the tag bit)
    - Formula: `tagged_value = (untagged_value << 1) | 1`

    **Tagged to Untagged**:
    - Remove the tag bit by arithmetic shift right by 1 (preserves sign for signed values)
    - This extracts the data bits from positions 1..n to positions 0..n-1
    - Then apply any necessary conversion (extension/truncation) to reach destination width
    - Formula: `untagged_value = tagged_value >> 1` (arithmetic shift)

    **Tagged to Tagged**:
    - Extract data bits from source (remove tag)
    - Convert data bits as if they were untagged integers
    - Re-tag the result for the destination type

### Float to Float

- **Float64 to Float32**: Standard IEEE 754 conversion with potential rounding
    - May round to nearest representable value
    - Infinities are preserved, NaN values remain NaN (but payloads are undefined)
    - May overflow to infinity for large values
    - May underflow to zero for very small values

- **Float32 to Float64**: Exact conversion (no information loss)
    - All `float32` values are exactly representable in `float64`
    - NaN values remain NaN (but payloads are undefined)

- **Same width**: No operation needed

### Cross-Category Conversions

1. **Integer to Float**:
    - Source must fit in a machine-width signed integer
    - Convert to machine-width signed first if needed
    - Apply integer-to-float conversion

    **Important**: Unsigned integers that exceed the range of signed machine-width integers cannot be converted to floats. For example, on a 64-bit machine, unsigned values greater than `2^63-1` produce undefined results when cast to float.

2. **Float to Integer**:
    - Target must be signed (unsigned targets are not currently supported, but they may be in the future)
    - Apply float-to-integer conversion with truncation
    - The result is undefined if rounded value exceeds target range
    - Further truncate to target width if needed

## Examples

### Example 1: Tagged to Untagged Conversion

Converting a tagged signed `int63` to an untagged signed `int32`.

Starting with tagged value `0x00000000_00000003` (represents the OCaml integer 1):
- The tag bit (LSB) is 1
- The data bits represent 1 (revealed by `0x00000000_00000003 >> 1 = 1`)

The conversion process:
1. Remove the tag bit using arithmetic shift right by 1: `0x00000000_00000003 >> 1 = 0x00000000_00000001`
2. The data bits now directly represent the value 1
3. Since the value already fits in 32 bits, no truncation is needed

Result: `0x00000000_00000001`

### Example 2: Untagged to Tagged Conversion

Converting an untagged signed `int32` to a tagged signed `int63`.

Starting with untagged value -5 (numeric value), represented as `0xFFFFFFFB` in 32-bit two's complement.

The conversion process:
1. In the current model, the value is already sign-extended to fill the container: `0xFFFFFFFFFFFFFFFB`
2. Shift all bits left by 1 to make room for the tag: `0xFFFFFFFFFFFFFFF6`
3. Set the tag bit by OR-ing with 1: `0xFFFFFFFFFFFFFFF7`

Result: The tagged representation of -5.

### Example 3: Unsigned to Signed Conversion

Converting an untagged unsigned `int8` to an untagged signed `int16`.

Starting with value `0xFF`, which represents 255 as an unsigned 8-bit value (though it would represent -1 if interpreted as signed).

The conversion process:
1. Start with the 8-bit value: `0xFF`
2. Since the source is unsigned, zero-extend to 16 bits: `0x00FF`

Result: `0x00FF`, which represents 255 when interpreted as a signed `int16`. Note that the unsigned interpretation is preserved because we zero-extended rather than sign-extended.

### Example 4: Tagged to Tagged with Truncation

Converting a tagged signed `int63` to a tagged signed `int31`.

Starting with tagged source value `0x00000001_00000003` (represents a large OCaml integer):
- After removing the tag bit, the data bits are `0x80000001` (numeric value 2,147,483,649)

The conversion process:
1. Remove the source tag bit: `0x00000001_00000003 >> 1 = 0x00000000_80000001`
2. Truncate data bits to fit the destination's 30 data bits (31 total bits minus 1 tag bit) - keep only the lower 30 bits: `0x00000001`
3. Re-tag the result: `(0x00000001 << 1) | 1 = 0x00000003`

Result: A tagged 30-bit value. The high bit was lost during truncation, significantly changing the value.

### Example 5: Information Loss

Converting an untagged signed `int16` to an untagged unsigned `int8`.

Starting with value -100, represented as `0xFF9C` in 16-bit two's complement.

The conversion process:
1. Truncate to 8 bits by keeping only the lower byte: `0x9C`
2. Interpret these bits as an unsigned value: 156

Result: Information is lost - the negative value -100 became the positive value 156. This demonstrates how cross-signedness truncation can dramatically change both magnitude and sign.

### Example 6: Float Conversion

**Float32 to Float64 conversion:**

- The single-precision value is representable in double-precision format without any loss.
- `NaN` values remain `NaN` but their payload bits are undefined.


**Float64 to Float32 conversion:**

This conversion may lose information in several ways:
- Values are rounded to the nearest representable `float32` value
- Precision beyond what `float32` can represent is lost
- Very large values may overflow to `inf` or `-inf`
- Very small values may underflow to zero
- `NaN` values remain `NaN` but their payload bits are undefined.

## Future Work

This section outlines planned extensions to the scalar type system:

### Relaxed Container Requirements

We plan to move away from requiring untagged values to fill their containers. This change will:
- Allow untagged integers to occupy only their specified bit width in the lower bits of containers
- Leave high bits beyond the type's bit width undefined/ignored
- Make signedness affect only interpretation of bits within the type's width, not container filling
- Align with the C standard, where only bits within a type's width are significant
- Enable more efficient use of 32-bit operations on 64-bit machines
- Maintain easy interoperability with tagged OCaml values (which remain unchanged)

### Enhanced Type Support

- **Unsigned integer ↔ float conversions**: Currently limited to signed integers
- **Front-end unsigned integers**: Extend OCaml's type system to include unsigned integer types
- **Extended width integers**: Support for `int128`/`uint128`

# Byte Ordering
Byte ordering is that of the target machine, and matches C stubs compiled for that machine.

## Byte Swapping

The `bswap` intrisics work as expected on integers that have a bit-width that is a multiple of 8 - it reverses the order of the bytes in the integer.

For compatibility with OCaml, the `%bswap16` primitive operates on tagged immediate values. It swaps the low two bytes, zeroing all bits above that. It does *not* sign-extend the result. It operates this way on both 32- and 64-bit targets.

## Future Work
At the moment, "standard" byte-swaps and the `%bswap16` primitive share the same operation in flambda, and are only distinguished by the `Naked_number_kind`, but we should change that.
