#!/usr/bin/env bash

# Boilerplate:
shopt -s nullglob
set -euo pipefail

# Note important directories:
root="${PWD}"
echo 'Building the compiler at `'"${root}"'`...'

# Configure if necessary:
if ! rg -qx 'WITH_FRAME_POINTERS[[:space:]]*=[[:space:]]*true' Makefile.config
then
  autoconf
  ./configure --prefix="$PWD/_install" --enable-frame-pointers --disable-dev
fi

# Build and install the compiler:
make -s install

# Install all binaries we'll need, so we can surgically overwrite the compiler:
output="$(mktemp -d)"
mkdir -p "${output}/bin"
for tool in "$root/_install/bin/"*; do
  name="$(basename -- "$tool")"
  if [[ ! -e "$output/bin/$name" ]]; then
    ln -fs "$tool" "$output/bin/$name"
  fi
done

# Make a fake `ocamlopt` that collects profiling data:
rm -- "$output/bin/ocamlopt" "$output/bin/ocamlopt.opt"
cat > "${output}/bin/ocamlopt" <<EOF
#!/usr/bin/env bash

# Boilerplate:
shopt -s nullglob
set -euo pipefail

# Configure memtrace:
export MEMTRACE="$output/alloc.\$\$.ctf" # Note that $$ means "this shell's PID"
export MEMTRACE_RATE='1e-4'
printf '%q ' "\$@" > "$output/command.\$\$.txt" # Record the compiler's arguments
exec "$root/_install/bin/ocamlopt.opt" \
  -dprofile \
  -dgranularity func \
  -dtimings-precision 6 \
  -dgc-timings \
  -inlining-report \
  -dump-into-file \
  -dump-dir "${output}" \
  -dprofile-output "gc.\$\$.dump" \
  "\$@" # Invoke the compiler
EOF
chmod +x "$output/bin/ocamlopt"
cp "$output/bin/ocamlopt" "$output/bin/ocamlopt.opt"

# Ask the build to use our compiler instead of the default:
sed \
  -e "s|$root/_build/_bootinstall/bin|$output/bin\" \"$root/_install/bin|g" \
  -e "s|$root/_build/_bootinstall/lib/ocaml|$root/_install/lib/ocaml|g" \
  duneconf/runtime_stdlib.ws > "$output/stdlib.ws"

# Build and collect profiling data:
echo 'Using that compiler to build stdlib, placing artifacts in `'"${output}"'`...'
dune build \
  --root "$root" \
  --workspace "$output/stdlib.ws" \
  --build-dir "$output/build" \
  --cache disabled \
  -j 1 \
  stdlib/stdlib.cmxa \
  stdlib/.stdlib.objs/native/std_exit.cmx

# Make memtrace data (machine-)readable:
for trace in "${output}"/alloc.*.ctf
do
  memtrace-dump "$trace" > "${trace%.ctf}.txt"
done

# Print a summary:
echo
echo 'Built successfully.'

reports=("$output"/gc.*.dump)
if [[ ${#reports[@]} -eq 0 ]]; then
  printf 'No GC reports found\n' >&2
  exit 1
fi

echo
awk '
  $2 == "alloc" {allocated_bytes += $1}
  /^[0-9]/ && $1 ~ /s$/ && $2 != "gc" {cpu_seconds += $1}
  $1 ~ /^[0-9]+$/ && $2 == "minor" {minor += $1}
  $1 ~ /^[0-9]+$/ && $2 == "major" {major += $1}
  END {
    printf "Allocated %.2f GiB total.\n",
      allocated_bytes / (1024 * 1024 * 1024)
    printf "The compiler spent %.3f seconds of CPU time.\n", cpu_seconds
    printf "There were %d heap collections (%d minor & %d major).\n",
      minor + major, minor, major
  }
' "${reports[@]}"

echo
top_n='10'
echo "Top ${top_n} source compilations by allocation:"
(
  cd "$output"
  awk '
    $2 == "alloc" {bytes[FILENAME] = $1 + 0}
    $2 ~ /^file=/ {source[FILENAME] = substr($2, 6)}
    END {
      for (file in bytes)
        printf "%.0f  %-40s  %s\n",
          bytes[file], (file in source ? source[file] : "n/a"), file
    }
  ' gc.*.dump |
    sort -nr |
    sed -n "1,${top_n}p" |
    numfmt --field=1 --to-unit=Gi --round=nearest --format='%8.3fG'
)

echo
echo "Top ${top_n} source compilations by CPU time:"
(
  cd "$output"
  awk '$2 ~ /^file=/ {
    printf "%8.3fs  %-40s  %s\n",
      $1 + 0, substr($2, 6), FILENAME
  }' gc.*.dump |
    sort -nr |
    sed -n "1,${top_n}p"
)

echo
echo 'All profiling files can be found in `'"${output}"'`.'
