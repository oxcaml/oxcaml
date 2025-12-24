#!/bin/bash
# Generate expected output for owee archive test using system tools
# This produces the same output format as test_archive.ml
#
# Note: This script parses the archive member headers directly to match
# what Owee reads. The 'ar' command has different output formats.

ARCHIVE="$1"
TMPDIR="${TMPDIR:-/tmp}"

echo "Archive: $ARCHIVE"

# Parse archive directly to get member count and info
# Archive format: !<arch>\n followed by member headers
# Each member header is 60 bytes:
#   - name: 16 bytes (space-padded, may end with /)
#   - date: 12 bytes
#   - uid: 6 bytes
#   - gid: 6 bytes
#   - mode: 8 bytes
#   - size: 10 bytes (decimal, space-padded)
#   - magic: 2 bytes (0x60 0x0a)

# Count members by parsing headers
OFFSET=8
FILESIZE=$(stat -c%s "$ARCHIVE")
MEMBER_COUNT=0
declare -a MEMBER_NAMES
declare -a MEMBER_SIZES

while [ $OFFSET -lt $FILESIZE ]; do
  # Read 60-byte header
  HEADER=$(dd if="$ARCHIVE" bs=1 skip=$OFFSET count=60 2>/dev/null)

  # Extract name (first 16 bytes, trim trailing spaces and /)
  NAME=$(echo "$HEADER" | cut -c1-16 | sed 's/[/ ]*$//')

  # Extract size (bytes 49-58, decimal)
  SIZE=$(echo "$HEADER" | cut -c49-58 | tr -d ' ')

  MEMBER_NAMES[$MEMBER_COUNT]="$NAME"
  MEMBER_SIZES[$MEMBER_COUNT]="$SIZE"
  MEMBER_COUNT=$((MEMBER_COUNT + 1))

  # Move to next member: header (60) + size (rounded up to even)
  NEXT_SIZE=$((SIZE + (SIZE % 2)))
  OFFSET=$((OFFSET + 60 + NEXT_SIZE))
done

echo "Number of members: $MEMBER_COUNT"
echo ""

# Process each member
for ((i=0; i<MEMBER_COUNT; i++)); do
  NAME="${MEMBER_NAMES[$i]}"
  SIZE="${MEMBER_SIZES[$i]}"

  echo "Member: $NAME (size=$SIZE)"

  # Calculate offset to member data
  OFFSET=8
  for ((j=0; j<i; j++)); do
    PREV_SIZE="${MEMBER_SIZES[$j]}"
    NEXT_SIZE=$((PREV_SIZE + (PREV_SIZE % 2)))
    OFFSET=$((OFFSET + 60 + NEXT_SIZE))
  done
  DATA_OFFSET=$((OFFSET + 60))

  # Extract member data and try to parse as ELF
  TMPFILE="$TMPDIR/owee_test_member_$$.o"
  dd if="$ARCHIVE" bs=1 skip=$DATA_OFFSET count=$SIZE of="$TMPFILE" 2>/dev/null

  SECTIONS_FILE="$TMPDIR/owee_sections_$$.txt"
  # Use -SW for wide output (full section names, one line per section)
  if readelf -SW "$TMPFILE" > "$SECTIONS_FILE" 2>/dev/null; then
    # Parse readelf -SW output for section names and sizes
    # Format: [ N] name TYPE addr off size es flg lk inf al
    awk '
      /^\s+\[[ 0-9]+\]/ {
        # Skip header line [Nr]
        if ($0 ~ /\[Nr\]/) next

        # Find the name - it comes after the ] bracket
        line = $0
        gsub(/^\s+\[[ 0-9]+\]\s*/, "", line)
        split(line, parts, /\s+/)
        name = parts[1]
        # Size is field 5 (after name, type, addr, off)
        size_hex = parts[5]

        # Truncate long section names for consistency across systems
        # E.g., ".note.gnu.property" -> ".note.gnu.prope"
        if (length(name) > 15) {
          name = substr(name, 1, 15)
        }

        size_dec = strtonum("0x" size_hex)

        if (size_dec > 0 && substr(name, 1, 1) == ".") {
          printf "  %s: size=%d\n", name, size_dec
        }
      }
    ' "$SECTIONS_FILE"
    echo ""
  else
    # Read first 4 bytes to match Owee's error message format
    MAGIC_BYTES=$(od -An -tx1 -N4 "$TMPFILE" | tr -d ' \n')
    BYTE0=$(echo "$MAGIC_BYTES" | cut -c1-2)
    BYTE1=$(echo "$MAGIC_BYTES" | cut -c3-4)
    BYTE2=$(echo "$MAGIC_BYTES" | cut -c5-6)
    BYTE3=$(echo "$MAGIC_BYTES" | cut -c7-8)
    echo "  (not ELF: No ELF magic number (found $BYTE0 $BYTE1 $BYTE2 $BYTE3))"
    echo ""
  fi
  rm -f "$TMPFILE" "$SECTIONS_FILE"
done
