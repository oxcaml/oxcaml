#!/bin/sh
# Exercise `dev-watcher.py build`'s recovery from a wedged watcher.
#
# The failure this guards against is an rpc client that waits forever against a
# watcher that is alive and answers pings but never starts the build. That is not
# reproducible on demand, so it is simulated: the tests run against a stub
# `dune` whose `rpc build` hangs, dies, fails or succeeds on command, in a
# throwaway root so no real watcher is touched.
#
# Run from the repo root: sh tools/dev-watcher-test.sh

set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
scratch=$root/_build/dev/watcher-test
export TMPDIR=$scratch

failures=0

setup() {
  rm -rf "$scratch"
  mkdir -p "$scratch/tools" "$scratch/bin"
  cp "$root/tools/dev-watcher.py" "$scratch/tools/dev-watcher.py"
  cat > "$scratch/bin/dune" <<'STUB'
#!/bin/sh
# Stub dune. Successive `rpc build` calls behave as BUILD_1, BUILD_2, ...
# falling back to BUILD_DEFAULT.
case "$1 ${2-}" in
  "rpc ping") exit 0 ;;
  "rpc build")
    count=$(cat "$COUNTER" 2>/dev/null || echo 0)
    count=$((count + 1))
    echo "$count" > "$COUNTER"
    eval "behaviour=\${BUILD_$count-}"
    [ -n "$behaviour" ] || behaviour=$BUILD_DEFAULT
    case "$behaviour" in
      success)
        # Reproduce dune's three-line rpc forwarding notice verbatim.
        printf 'Warning:\n'
        printf 'Your build request is being forwarded to a running Dune '
        printf 'instance. Note that\n'
        printf 'certain command line arguments may be ignored.\n'
        printf 'Success\n'
        exit 0 ;;
      hang) exec sleep 600 ;;
      dead)
        echo 'Error: Server returned error: Connection terminated (error' \
             'kind: Connection_dead)'
        exit 1 ;;
      failure) printf 'Failure\n'; exit 0 ;;
    esac ;;
esac
case "$1" in
  build) echo "FALLBACK BUILD RAN"; exit 0 ;;
  diagnostics) echo "DIAGNOSTICS RAN"; exit 0 ;;
esac
echo "stub dune: unexpected arguments: $*" >&2
exit 99
STUB
  chmod +x "$scratch/bin/dune"
  export COUNTER=$scratch/build-count
  export PATH=$scratch/bin:$PATH
}

watcher() {
  python3 "$scratch/tools/dev-watcher.py" "$@"
}

# A `VAR=value function` prefix persists in the calling shell under POSIX sh, so
# each case must clear the previous one's behaviours explicitly rather than rely
# on the prefix being scoped.
start_stub_watcher() {
  : > "$COUNTER"
  unset BUILD_1 BUILD_2 BUILD_3 BUILD_4 BUILD_5 || true
  BUILD_DEFAULT=${1-success}
  export BUILD_DEFAULT
  watcher start --idle-timeout 60 -- sleep 600 >/dev/null
}

behaviour() {
  eval "export BUILD_$1=\$2"
}

# run_build <expected status> <name>; remaining args are `build` arguments.
run_build() {
  expected=$1; name=$2; shift 2
  status=0
  watcher build \
    --ping "dune rpc ping" \
    --fallback "dune build" \
    --diagnostics "dune diagnostics" \
    "$@" -- dune rpc build > "$scratch/out" 2>&1 || status=$?
  if [ "$status" != "$expected" ]; then
    echo "FAIL: $name: expected exit $expected, got $status"
    sed 's/^/    /' "$scratch/out"
    failures=$((failures + 1))
    return 0
  fi
  echo "ok: $name (exit $status)"
}

expect_output() {
  if grep -qF "$1" "$scratch/out"; then
    echo "  ok: output contains '$1'"
  else
    echo "  FAIL: output does not contain '$1'"
    sed 's/^/    /' "$scratch/out"
    failures=$((failures + 1))
  fi
}

expect_no_output() {
  if grep -qF "$1" "$scratch/out"; then
    echo "  FAIL: output should not contain '$1'"
    sed 's/^/    /' "$scratch/out"
    failures=$((failures + 1))
  else
    echo "  ok: output omits '$1'"
  fi
}

expect_build_count() {
  actual=$(cat "$COUNTER")
  if [ "$actual" = "$1" ]; then
    echo "  ok: made $actual rpc build attempt(s)"
  else
    echo "  FAIL: expected $1 rpc build attempt(s), made $actual"
    failures=$((failures + 1))
  fi
}

setup
trap 'watcher stop >/dev/null 2>&1 || true' EXIT

echo "== a successful build stays quiet and does not retry"
start_stub_watcher success
run_build 0 "success"
expect_output "Success"
expect_build_count 1
# The forwarding notice and the bare "Warning:" that introduces it are dropped.
expect_no_output "being forwarded"
expect_no_output "Warning:"

echo "== a wedged build times out, bounces the watcher, and retries once"
start_stub_watcher success
behaviour 1 hang
run_build 0 "timeout then retry" --timeout 2 --heartbeat 1
expect_output "exceeded 2s"
expect_output "restarting the watcher and retrying"
expect_output "Success"
expect_build_count 2
expect_no_output "FALLBACK BUILD RAN"

echo "== a heartbeat is printed while a build is running"
start_stub_watcher success
behaviour 1 hang
run_build 0 "heartbeat" --timeout 3 --heartbeat 1
expect_output "still building"
expect_output "progress: make dev-log"

echo "== a persistently wedged watcher falls back to a direct build"
start_stub_watcher hang
run_build 0 "fallback" --timeout 2 --heartbeat 1
expect_output "building directly"
expect_output "FALLBACK BUILD RAN"
# Exactly two rpc attempts, then the fallback: retrying without a bound would
# be a new silent hang.
expect_build_count 2

echo "== a dead connection is recovered without waiting for the timeout"
start_stub_watcher success
behaviour 1 dead
run_build 0 "connection died" --timeout 600 --heartbeat 300
expect_output "lost its connection to the watcher"
expect_output "Success"
expect_build_count 2

echo "== a genuine build failure reports diagnostics and does not retry"
start_stub_watcher failure
run_build 1 "build failure" --timeout 60
expect_output "DIAGNOSTICS RAN"
expect_build_count 1
expect_no_output "FALLBACK BUILD RAN"

watcher stop >/dev/null 2>&1 || true
rm -rf "$scratch"

if [ "$failures" = 0 ]; then
  echo "all dev-watcher build tests passed"
else
  echo "$failures dev-watcher build check(s) failed"
  exit 1
fi
