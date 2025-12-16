#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF' >&2
Usage: typing/ikinds/bench_make.sh <description...>

Runs `make clean && make` three times and appends wall/user/sys timings to:
  typing/ikinds/benchresults.txt
EOF
}

if [[ $# -lt 1 ]]; then
  usage
  exit 2
fi

desc="$*"

script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
repo_root="$(git -C "$script_dir" rev-parse --show-toplevel)"
results="$script_dir/benchresults.txt"

timestamp="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
git_head="$(git -C "$repo_root" rev-parse HEAD)"
git_status="$(git -C "$repo_root" status --porcelain=v1 || true)"

dirty="clean"
if [[ -n "$git_status" ]]; then
  dirty="dirty"
fi

logdir="$(mktemp -d /tmp/oxcaml-ikinds-bench-XXXXXXXX)"

{
  echo
  echo "=== $timestamp ==="
  echo "desc: $desc"
  echo "git: $git_head ($dirty)"
  echo "host: $(uname -srm)"
  echo "logdir: $logdir"
  echo "cmd: make clean && /usr/bin/time -p make"
  echo "runs: 3"
  if [[ -n "$git_status" ]]; then
    echo "dirty files:"
    echo "$git_status" | sed 's/^/  /'
  else
    echo "dirty files: (none)"
  fi
  echo
} >>"$results"

for i in 1 2 3; do
  clean_log="$logdir/run$i.makeclean.log"
  make_log="$logdir/run$i.make.log"
  time_log="$logdir/run$i.time"

  (cd "$repo_root" && make clean) >"$clean_log" 2>&1

  if ! (cd "$repo_root" && { /usr/bin/time -p make >"$make_log"; } \
    2>"$time_log"); then
    {
      echo "run $i: FAIL"
      echo "  make log: $make_log"
      echo "  time log: $time_log"
      sed 's/^/  /' "$time_log"
      echo
    } >>"$results"
    exit 1
  fi

  real="$(awk '/^real /{print $2}' "$time_log")"
  user="$(awk '/^user /{print $2}' "$time_log")"
  sys="$(awk '/^sys /{print $2}' "$time_log")"

  {
    echo "run $i:"
    echo "  real $real"
    echo "  user $user"
    echo "  sys  $sys"
    echo
  } >>"$results"
done
