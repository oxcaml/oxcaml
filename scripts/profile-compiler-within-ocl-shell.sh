#!/usr/bin/env bash

# Boilerplate:
shopt -s nullglob
set -euo pipefail

# Configure if necessary:
if ! rg -qx 'WITH_FRAME_POINTERS[[:space:]]*=[[:space:]]*true' Makefile.config
then
  autoconf
  ./configure --prefix="$PWD/_install" --enable-frame-pointers --disable-dev
fi

# Build and install the compiler:
make -s install 2>/dev/null

# Note important directories:
root="${PWD}"
output="$(mktemp -d)"
mkdir -p "${output}/bin"

# Install all binaries we'll need, so we can surgically overwrite the compiler:
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
printf '%q ' "\$@" > "$output/command.\$\$.txt" # Record the compiler's arguments
exec "$root/_install/bin/ocamlopt.opt" \
  -dgc-timings \
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
echo

reports=("$output"/gc.*.dump)
if [[ ${#reports[@]} -eq 0 ]]; then
  printf 'No GC reports found\n' >&2
  exit 1
fi

total_allocated=0
for report in "${reports[@]}"; do
  while read -r amount label; do
    if [[ "$label" == alloc ]]; then
      total_allocated=$((total_allocated + ${amount%b}))
    fi
  done < "$report"
done

echo "Allocated ${total_allocated} bytes total"

echo 'All profiling files (e.g. `memtrace-dump` `.txt` files) can be found in `'"${output}"'`.'
