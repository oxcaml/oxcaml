#!/bin/bash
set -euo pipefail

# Script to collect file size metrics from _install directory
# Usage: collect-metrics.sh <install_directory> <output_csv_file> <commit_hash> <pr_number>

if [ $# -lt 3 ] || [ $# -gt 4 ]; then
    echo "Usage: $0 <install_directory> <output_csv_file> <commit_hash> [pr_number]" >&2
    exit 1
fi

INSTALL_DIR="$1"
CSV_FILE="$2"
COMMIT_HASH="$3"
PR_NUMBER="${4:-N/A}"

# Validate input directory exists
if [ ! -d "$INSTALL_DIR" ]; then
    echo "Error: Install directory '$INSTALL_DIR' does not exist" >&2
    exit 1
fi

# Generate timestamp
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Extensions to track
EXTENSIONS="exe opt a cmxa cma cmi cmx cmo cms cmsi cmt cmti o"

# Write CSV header
echo "timestamp,commit_hash,pr_number,extension,total_size_bytes" > "$CSV_FILE"

# Collect metrics for each extension
for ext in $EXTENSIONS; do
    total_size=0
    temp_file=$(mktemp)
    if find "$INSTALL_DIR" -name "*.${ext}" -type f > "$temp_file" 2>/dev/null; then
        # Calculate total size of all files with this extension using du
        if [ -s "$temp_file" ]; then
            # Use du to get size in bytes, summing all files
            total_size=$(du -bc $(cat "$temp_file") 2>/dev/null | tail -n1 | cut -f1 || echo 0)
        fi
    fi
    # Clean up temporary file

    if [ -n "$temp_file" ]; then
        rm -f "$temp_file"
    fi
    # Write to CSV
    echo "${TIMESTAMP},${COMMIT_HASH},${PR_NUMBER},${ext},${total_size}" >> "$CSV_FILE"
done

echo "Generated metrics file: $CSV_FILE"
echo "Metrics collected for commit: $COMMIT_HASH"