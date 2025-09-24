#!/bin/bash
set -euo pipefail

# Script to collect file size metrics from _install directory
# Usage: collect-metrics.sh <install_directory> <output_csv_file> <commit_hash>

if [ $# -ne 3 ]; then
    echo "Usage: $0 <install_directory> <output_csv_file> <commit_hash>" >&2
    exit 1
fi

INSTALL_DIR="$1"
CSV_FILE="$2"
COMMIT_HASH="$3"

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
echo "timestamp,commit_hash,extension,total_size_bytes" > "$CSV_FILE"

# Collect metrics for each extension
for ext in $EXTENSIONS; do
    total_size=0
    if find "$INSTALL_DIR" -name "*.${ext}" -type f > "/tmp/files_${ext}" 2>/dev/null; then
        # Calculate total size of all files with this extension
        while IFS= read -r file; do
            if [ -f "$file" ]; then
                size=$(stat -c%s "$file" 2>/dev/null || echo 0)
                total_size=$((total_size + size))
            fi
        done < "/tmp/files_${ext}"
        # Clean up temporary file
        rm -f "/tmp/files_${ext}"
    fi
    # Write to CSV
    echo "${TIMESTAMP},${COMMIT_HASH},${ext},${total_size}" >> "$CSV_FILE"
done

echo "Generated metrics file: $CSV_FILE"
echo "Metrics collected for commit: $COMMIT_HASH"