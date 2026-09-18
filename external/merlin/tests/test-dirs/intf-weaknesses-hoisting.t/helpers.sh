# Helpers for the intf-weaknesses cram suites, sourced by run.t.
#
# [strengthen foo.mli] runs the query on foo.ml and prints, in order: the
# implementation, foo.mli with every suggested edit applied, and — for a
# unit interface — the result of re-checking the *unmodified* foo.ml against
# the strengthened interface with the batch compiler. A "CONFORMANCE ERROR"
# line means the query suggested something that does not typecheck, which is
# always a bug.
#
# Every value whose analysis was abandoned by a failed moregen re-run is
# printed before the strengthened interface, so no block can hide an
# analysis gap by silently suggesting nothing.

# Apply the separated edits targeting file $1 to its contents.
apply_edits() {
  local target="$1"
  local edits="$2"
  awk -F'\t' -v edits="$edits" -v target="$(basename "$target")" '
    BEGIN {
      n = 0
      while ((getline l < edits) > 0) {
        split(l, a, "\t")
        if (a[1] == target) {
          n++
          line[n] = a[2] + 0; col[n] = a[3] + 0
          eline[n] = a[4] + 0; ecol[n] = a[5] + 0
          # multi-line edit texts travel newline-escaped (one edit per row)
          gsub(/\\n/, "\n", a[7])
          txt[n] = a[7]
        }
      }
    }
    {
      s = $0
      # Edits are pre-sorted by descending (line, col, array index), so
      # applying in order never invalidates later offsets.
      for (i = 1; i <= n; i++)
        if (line[i] == FNR) {
          end = (eline[i] == line[i]) ? ecol[i] : col[i]
          s = substr(s, 1, col[i]) txt[i] substr(s, end + 1)
        }
      print s
    }' "$target"
}

# Compile the *strengthened interface* against the unit's unmodified
# implementation with the batch compiler. A strengthened interface the
# implementation no longer satisfies is a bug in the analysis, not a test
# expectation to update.
conformance_check() {
  local base="$1" # unit basename, e.g. values
  local mli="$2"  # file containing the strengthened .mli text
  local here
  here="$(pwd)"
  local w
  w="$(mktemp -d)"
  cp "$mli" "$w/$base.mli"
  cp "./$base.ml" "$w/$base.ml"
  if ! (cd "$w" \
    && $OCAMLC -c -w -a \
      -I "$here" "$base.mli" "$base.ml") 2> "$w/err"; then
    echo "CONFORMANCE ERROR:"
    head -8 "$w/err"
  fi
}

# Run the intf-weaknesses query on $1's implementation sibling and print the
# implementation, then each affected file with the suggested edits applied,
# followed by the conformance check.
strengthen() {
  local file="./$1"
  local impl="$file"
  case "$impl" in
    *.mli) impl="${impl%.mli}.ml" ;;
  esac
  echo "=== $(basename "$impl") (implementation) ==="
  cat "$impl"
  local json
  json="$(mktemp)"
  # [revert-newlines] undoes merlin-wrapper's unescaping of \n inside string
  # literals: a hoisted clause's edit text spans several lines, and jq needs
  # valid json.
  $MERLIN single intf-weaknesses -filename "$impl" < "$impl" 2> /dev/null \
    | revert-newlines > "$json"
  jq -r '.notifications[]?
         | select(contains("moregen re-run failed"))
         | split(";")[0]' "$json"
  echo "=== $(basename "$file") (strengthened) ==="
  local edits
  edits="$(mktemp)"
  jq -r '[.value[].edits[]] | to_entries[]
             | [(.value.file | split("/") | last),
                (.value.start.line | tostring), (.value.start.col | tostring),
                (.value.end.line | tostring), (.value.end.col | tostring),
                (.key | tostring), (.value.new_text | gsub("\n"; "\\n"))]
             | join("\t")' "$json" \
    | sort -t$'\t' -k1,1 -k2,2nr -k3,3nr -k6,6nr > "$edits"
  local out
  out="$(mktemp)"
  apply_edits "$file" "$edits" > "$out"
  cat "$out"
  case "$file" in
    *.mli) conformance_check "$(basename "${file%.mli}")" "$out" ;;
  esac
  cut -f1 "$edits" | sort -u | while read -r other; do
    if [ -n "$other" ] && [ "$other" != "$(basename "$file")" ]; then
      echo "=== $other (edits apply here) ==="
      apply_edits "./$other" "$edits"
    fi
  done
}
