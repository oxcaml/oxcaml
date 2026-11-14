#!/bin/sh
# Print the call-site attributes of each DW_TAG_inlined_subroutine in
# unknown_line.o, as "<callee>: call_line=<n|absent> call_column=<n|absent>".

set -eu -o pipefail

if ! command -v llvm-dwarfdump >/dev/null 2>&1; then
  echo "llvm-dwarfdump not available" > "${ocamltest_response}"
  exit 125
fi

llvm-dwarfdump --debug-info unknown_line.o | awk '
  function flush() {
    if (inblk && callee != "")
      print callee ": call_line=" line " call_column=" col
    inblk = 0
  }
  /DW_TAG_/ { flush() }
  /DW_TAG_inlined_subroutine/ {
    inblk = 1; callee = ""; line = "absent"; col = "absent"
    next
  }
  inblk && /DW_AT_abstract_origin/ {
    if ($0 ~ /f_unknown/) callee = "f_unknown"
    else if ($0 ~ /f_known/) callee = "f_known"
  }
  inblk && /DW_AT_call_line/ { gsub(/[()]/, "", $2); line = $2 }
  inblk && /DW_AT_call_column/ { gsub(/[()]/, "", $2); col = $2 }
  END { flush() }
' | sort > "${output}"
