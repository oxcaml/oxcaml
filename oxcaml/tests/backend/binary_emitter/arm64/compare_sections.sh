#!/bin/bash
# Compare binary emitter output against assembler output
# Usage: compare_sections.sh <test_name> [object_file]
# Expects: <test_name>.binary-sections/ to exist
# If object_file not specified, uses <test_name>.o
# Supports both macOS (Mach-O) and Linux (ELF) object files

set -e

TEST_NAME="$1"
OBJ_FILE="$2"

if [ -z "$TEST_NAME" ]; then
    echo "Usage: $0 <test_name> [object_file]"
    exit 1
fi

# If no object file specified, default to <test_name>.o
if [ -z "$OBJ_FILE" ]; then
    OBJ_FILE="${TEST_NAME}.o"
fi

BINARY_SECTIONS_DIR="${TEST_NAME}.binary-sections"

if [ ! -f "$OBJ_FILE" ]; then
    echo "ERROR: Object file $OBJ_FILE not found"
    exit 1
fi

if [ ! -d "$BINARY_SECTIONS_DIR" ]; then
    echo "ERROR: Binary sections directory $BINARY_SECTIONS_DIR not found"
    exit 1
fi

# Detect platform
if [ "$(uname)" = "Darwin" ]; then
    IS_MACOS=1
else
    IS_MACOS=0
fi

# Create temp directory for extracted sections
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

# Helper function to extract a section from object file
extract_section() {
    local section_name="$1"
    local output_file="$2"

    if [ $IS_MACOS -eq 1 ]; then
        # macOS: use segedit with Mach-O segment/section names
        case "$section_name" in
            text)
                segedit "$OBJ_FILE" -extract __TEXT __text "$output_file" 2>/dev/null
                ;;
            data)
                segedit "$OBJ_FILE" -extract __DATA __data "$output_file" 2>/dev/null || \
                segedit "$OBJ_FILE" -extract __DATA __const "$output_file" 2>/dev/null
                ;;
        esac
    else
        # Linux: use objcopy with ELF section names
        case "$section_name" in
            text)
                objcopy --dump-section .text="$output_file" "$OBJ_FILE" 2>/dev/null
                ;;
            data)
                objcopy --dump-section .data="$output_file" "$OBJ_FILE" 2>/dev/null || \
                objcopy --dump-section .rodata="$output_file" "$OBJ_FILE" 2>/dev/null
                ;;
        esac
    fi
}

TEXT_FAILED=0
DATA_FAILED=0

# Extract and compare text section
if [ -f "$BINARY_SECTIONS_DIR/section_text.bin" ]; then
    extract_section text "$TMPDIR/asm_text.bin" || {
        echo "WARNING: Could not extract text section from $OBJ_FILE"
    }

    if [ -f "$TMPDIR/asm_text.bin" ]; then
        if ! cmp -s "$TMPDIR/asm_text.bin" "$BINARY_SECTIONS_DIR/section_text.bin"; then
            echo "MISMATCH: text section differs"
            echo "  Assembler size: $(wc -c < "$TMPDIR/asm_text.bin")"
            echo "  Binary emitter size: $(wc -c < "$BINARY_SECTIONS_DIR/section_text.bin")"

            # Find first difference
            DIFF_OFFSET=$(cmp -l "$TMPDIR/asm_text.bin" "$BINARY_SECTIONS_DIR/section_text.bin" 2>/dev/null | head -1 | awk '{print $1}')
            if [ -n "$DIFF_OFFSET" ]; then
                # Convert to instruction offset (ARM64 instructions are 4 bytes)
                INSTR_OFFSET=$(( (DIFF_OFFSET - 1) / 4 * 4 ))
                echo "  First difference at byte offset: $((DIFF_OFFSET - 1)) (instruction at offset $INSTR_OFFSET)"
                echo ""
                echo "  Assembler bytes at offset $INSTR_OFFSET:"
                xxd -s $INSTR_OFFSET -l 16 "$TMPDIR/asm_text.bin" | sed 's/^/    /'
                echo "  Binary emitter bytes at offset $INSTR_OFFSET:"
                xxd -s $INSTR_OFFSET -l 16 "$BINARY_SECTIONS_DIR/section_text.bin" | sed 's/^/    /'
            fi
            TEXT_FAILED=1
        else
            echo "OK: text section matches ($(wc -c < "$TMPDIR/asm_text.bin") bytes)"
        fi
    fi
fi

# Extract and compare data section
if [ -f "$BINARY_SECTIONS_DIR/section_data.bin" ]; then
    extract_section data "$TMPDIR/asm_data.bin" || {
        echo "WARNING: Could not extract data section from $OBJ_FILE"
    }

    if [ -f "$TMPDIR/asm_data.bin" ]; then
        if ! cmp -s "$TMPDIR/asm_data.bin" "$BINARY_SECTIONS_DIR/section_data.bin"; then
            echo "MISMATCH: data section differs"
            echo "  Assembler size: $(wc -c < "$TMPDIR/asm_data.bin")"
            echo "  Binary emitter size: $(wc -c < "$BINARY_SECTIONS_DIR/section_data.bin")"

            # Find first difference
            DIFF_OFFSET=$(cmp -l "$TMPDIR/asm_data.bin" "$BINARY_SECTIONS_DIR/section_data.bin" 2>/dev/null | head -1 | awk '{print $1}')
            if [ -n "$DIFF_OFFSET" ]; then
                OFFSET=$((DIFF_OFFSET - 1))
                echo "  First difference at byte offset: $OFFSET"
                echo ""
                echo "  Assembler bytes at offset $OFFSET:"
                xxd -s $OFFSET -l 16 "$TMPDIR/asm_data.bin" | sed 's/^/    /'
                echo "  Binary emitter bytes at offset $OFFSET:"
                xxd -s $OFFSET -l 16 "$BINARY_SECTIONS_DIR/section_data.bin" | sed 's/^/    /'
            fi
            DATA_FAILED=1
        else
            echo "OK: data section matches ($(wc -c < "$TMPDIR/asm_data.bin") bytes)"
        fi
    fi
fi

# Fail on either text or data section mismatch
if [ $TEXT_FAILED -eq 1 ] || [ $DATA_FAILED -eq 1 ]; then
    exit 1
fi

echo "All sections match!"
exit 0
