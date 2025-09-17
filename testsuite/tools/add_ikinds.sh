#!/bin/bash

# Script to add or remove -ikinds flag from typing-* tests
# Usage: ./add_ikinds.sh [add|remove]

MODE="${1:-add}"

if [[ "$MODE" != "add" && "$MODE" != "remove" ]]; then
    echo "Usage: $0 [add|remove]"
    echo "  add    - Add -ikinds flag to all typing-* tests (default)"
    echo "  remove - Remove -ikinds flag from all typing-* tests"
    exit 1
fi

# Find all .ml files in typing-* directories with TEST blocks
for file in $(find testsuite/tests -type f -name "*.ml" -path "*/typing-*/*" | xargs grep -l "^(\\* TEST" 2>/dev/null); do
    if [[ "$MODE" == "add" ]]; then
        # Check if file already has -ikinds flag
        if grep -q "flags.*-ikinds" "$file"; then
            echo "Skipping $file (already has -ikinds)"
        else
            # Add -ikinds to existing flags or create new flags line
            if grep -q "flags = " "$file"; then
                # Update existing flags line to append -ikinds
                sed -i 's/\(flags = "[^"]*\)"/\1 -ikinds"/' "$file"
                echo "Updated $file (appended -ikinds to existing flags)"
            else
                # Add flags line after TEST
                sed -i '/^(\* TEST$/a\    flags = "-ikinds";' "$file"
                echo "Updated $file (added new flags line with -ikinds)"
            fi
        fi
    elif [[ "$MODE" == "remove" ]]; then
        # Check if file has -ikinds flag
        if grep -q "flags.*-ikinds" "$file"; then
            # Check if -ikinds is the only flag
            if grep -q 'flags = "-ikinds";' "$file"; then
                # Remove the entire flags line if -ikinds is the only flag
                sed -i '/flags = "-ikinds";/d' "$file"
                echo "Removed flags line from $file (-ikinds was the only flag)"
            else
                # Remove -ikinds from the flags line
                # Handle cases where -ikinds might be at beginning, middle, or end
                sed -i 's/\(flags = "[^"]*\) -ikinds"/\1"/' "$file"
                sed -i 's/\(flags = "\)-ikinds \([^"]*"\)/\1\2/' "$file"
                sed -i 's/\(flags = "[^"]*\)-ikinds \([^"]*"\)/\1\2/' "$file"
                echo "Removed -ikinds from $file (kept other flags)"
            fi
        else
            echo "Skipping $file (no -ikinds flag found)"
        fi
    fi
done

echo "Done! Mode: $MODE"