#!/bin/sh
set -eu

cd "${test_build_directory}"
sanitizer="${test_source_directory}/sanitize.awk"

cat > expected <<'EOF'
* thread #1, stop reason = breakpoint 1.1
    frame #0: meander`first
    frame #1: meander`second
EOF

normalize () {
    awk -f "$sanitizer" input > actual
}

must_differ () {
    normalize
    if cmp -s expected actual; then
        echo "Sanitizer hid a meaningful change: $1" >&2
        exit 1
    fi
}

for width in 0 1 4 7; do
    head -n 1 expected > input
    printf '%*sframe #0: meander`first\n' "$width" '' >> input
    printf '%*sframe #1: meander`second\n' "$width" '' >> input
    normalize
    diff -u expected actual
done

head -n 1 expected > input
printf '\tframe #0: meander`first\n\tframe #1: meander`second\n' >> input
normalize
diff -u expected actual

sed 's/`first/`wrong_function/' expected > input
must_differ 'function name'
sed 's/frame #1:/frame #2:/' expected > input
must_differ 'frame number'
awk 'NR == 2 { first = $0; next } NR == 3 { print; print first; next } { print }' expected > input
must_differ 'frame order'
sed '2d' expected > input
must_differ 'missing frame'
cp expected input
printf '    frame #2: meander`unexpected\n' >> input
must_differ 'extra frame'
sed 's/breakpoint 1.1/signal SIGSEGV/' expected > input
must_differ 'stop reason'
cp expected input
printf 'error: debugger failed\n' >> input
must_differ 'debugger error'

cat > untouched <<'EOF'
       unrelated indentation
frame 0: meander`first
  not a frame #0: summary
EOF
cp untouched input
normalize
diff -u untouched actual
