#!/bin/bash
# Print "<callee>: call_line=<n|absent> call_column=<n|absent>" for the
# inlined calls to f_unknown and f_known in unknown_line.o. $1 is the ocamlopt
# under test; the DWARF is dumped with its toolchain: Xcode's dwarfdump on
# macOS, otherwise the llvm-dwarfdump or GNU objdump next to its objcopy.

set -eu -o pipefail

ocamlopt=$1

# llvm-dwarfdump and Xcode's dwarfdump print the abstract origin's name.
llvm_call_sites () {
  awk '
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
  '
}

# objdump prints the abstract origin only as a .debug_info offset, resolved
# at the end against the DIE headers ("<depth><offset>:").
gnu_call_sites () {
  awk '
    function key(s) {
      s = tolower(s); sub(/^0x/, "", s); sub(/^0+/, "", s); return s
    }
    function flush() {
      if (inblk) { n++; org[n] = o; ln[n] = line; cl[n] = col }
      inblk = 0
    }
    /^ *<[0-9]+><[0-9a-f]+>:/ {
      flush()
      split($1, p, /[<>]/); die = key(p[4])
      if ($0 ~ /DW_TAG_inlined_subroutine/) {
        inblk = 1; o = ""; line = "absent"; col = "absent"
      }
      next
    }
    /DW_AT_name/ { v = $0; sub(/.*: /, "", v); name[die] = v }
    inblk && /DW_AT_abstract_origin/ {
      v = $NF; gsub(/[<>]/, "", v); o = key(v)
    }
    inblk && /DW_AT_call_line/ { line = $NF }
    inblk && /DW_AT_call_column/ { col = $NF }
    END {
      flush()
      for (i = 1; i <= n; i++) {
        callee = name[org[i]]
        if (callee ~ /f_unknown/) callee = "f_unknown"
        else if (callee ~ /f_known/) callee = "f_known"
        else continue
        print callee ": call_line=" ln[i] " call_column=" cl[i]
      }
    }
  '
}

case "$("$ocamlopt" -config-var system)" in
  macosx) xcrun dwarfdump --debug-info unknown_line.o | llvm_call_sites ;;
  *)
    objcopy=$(command -v "$("$ocamlopt" -config-var objcopy)")
    dir=$(dirname "$objcopy")
    name=$(basename "$objcopy")
    case "$name" in
      *llvm-objcopy*)
        "$dir/${name/llvm-objcopy/llvm-dwarfdump}" --debug-info unknown_line.o \
          | llvm_call_sites ;;
      *)
        "$dir/${name/objcopy/objdump}" --dwarf=info unknown_line.o \
          | gnu_call_sites ;;
    esac ;;
esac | sort > "${output}"
