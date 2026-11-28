#!/usr/bin/env bash

#**************************************************************************#
#*                                                                        *#
#*                                 OCaml                                  *#
#*                                                                        *#
#*                  Jacob Van Buren, Jane Street, New York                *#
#*                                                                        *#
#*   Copyright 2026 Jane Street Group LLC                                 *#
#*                                                                        *#
#*   All rights reserved.  This file is distributed under the terms of    *#
#*   the GNU Lesser General Public License version 2.1, with the          *#
#*   special exception on linking described in the file LICENSE.          *#
#*                                                                        *#
#**************************************************************************#

# Ship a build to the build.yml jobs that test it on other machines.
#
#   test-build.sh pack ARCHIVE
#     Pack what `make test-no-rebuild` reads: the install tree, the test
#     tree, and the two configure outputs that the top-level Makefile
#     includes. Run it where `make install_for_test` has succeeded.
#
#   test-build.sh digest NAME JOB
#     Print `result=` and the result that the job named JOB, the one that
#     uploads this workflow run's artifact NAME, has in this attempt of the
#     run, then `digest=` and the SHA-256 that the artifact was uploaded
#     with. Nothing follows `digest=` unless the result is `success`, or
#     when the run has no such artifact made since that job started. In a
#     workflow, the two lines are also outputs of the step. Needs `gh`, and
#     GH_TOKEN with the `actions: read` permission.
#
#   test-build.sh unpack ARCHIVE [SHA256]
#     Unpack in a fresh checkout of the same commit, after checking that
#     ARCHIVE has the digest SHA256, if given. ARCHIVE may also be a
#     directory that holds the archive and nothing else.
#
# The trees contain absolute paths (shebangs, ld.conf, the compiler's
# default library directory) and code compiled against the builder's C
# library. They only work on a machine like the builder, in a checkout at
# the same path. `pack` describes the builder in _runtest/BUILD_INFO, and
# `unpack` reads that first: when this machine differs it fails and unpacks
# nothing. An `unpack` that fails later leaves nothing either.

set -euo pipefail
usage() {
  echo "Usage: $0 pack ARCHIVE | digest NAME JOB |" \
    "unpack ARCHIVE [SHA256]" >&2
  exit 2
}
case ${1:-}/$# in
  pack/2 | digest/3 | unpack/2 | unpack/3) ;;
  *) usage ;;
esac
action=$1
# For `digest`, the name of the artifact.
archive=$2

# In the order of the archive: the makefiles before the trees, so that
# `unpack` reads Makefile.config without them.
packed=(Makefile.config Makefile.build_config _install _runtest)
info=_runtest/BUILD_INFO
# What an `unpack` that does not finish leaves nothing of: the directory in
# which it reads the description of the build, and what it has unpacked.
staged=''
unpacking=false
cleanup() {
  [ -z "$staged" ] || rm -rf -- "$staged"
  if $unpacking; then
    # The entries of a directory that the archive made without a
    # permission of its owner would stay: each directory gets them before
    # find looks inside it. find does not follow links.
    find "${packed[@]}" -type d ! -perm -700 -exec chmod u+rwx {} \; \
      2>/dev/null || :
    rm -rf -- "${packed[@]}"
  fi
}
trap cleanup EXIT
# The trees repeat a lot of code far apart: each codegen variant's native
# libraries and test programs are near copies of those of base, and many
# executables link the same libraries. Only a window this long lets zstd
# find the repeats: without it the archive is nearly twice the size.
# Decompression must allow the same window.
zstd_window=--long=30

# On standard error: some callers have their output in a file.
die() {
  if [ "${GITHUB_ACTIONS:-}" = true ]; then
    echo "::error::$*" >&2
  else
    echo "$0: error: $*" >&2
  fi
  exit 1
}

# What must agree between the builder and a machine that runs its tests,
# as FIELD=VALUE lines. The C compiler and the C library are described by
# their own versions, not by the revisions of the distribution's packages:
# the jobs of a run install and upgrade packages at different times, and a
# package revision that changes in between does not stop the tests from
# running.
# $1 is the Makefile.config of the build.
describe_machine() {
  local commit machine cc command version
  # Each on a line of its own: a command that fails inside the argument of
  # another one would leave its field empty, here as on the other machine.
  commit=$(git rev-parse --verify 'HEAD^{commit}') ||
    die "cannot tell which commit this checkout is at"
  machine=$(uname -m)
  echo "commit=$commit"
  echo "workspace=$PWD"
  echo "machine=$machine"
  # The whole command, as the build runs it: its first word can be a
  # launcher, which says nothing of the compiler behind it.
  cc=$(sed -n 's/^CC=//p' "$1")
  read -ra command <<< "$cc"
  [ ${#command[@]} -gt 0 ] ||
    die "Makefile.config does not name the C compiler"
  # gcc prints its full version for the first option and clang for the
  # second; each ignores the other.
  version=$("${command[@]}" -dumpfullversion -dumpversion) &&
    [ -n "$version" ] ||
    die "cannot tell the version of the C compiler, $cc"
  echo "cc=$cc $version"
  version=$(getconf GNU_LIBC_VERSION 2>/dev/null) || version=unknown
  echo "libc=$version"
}

# Succeeds if every symbolic link of the packed trees resolves to a file
# inside them. The trees hold tens of thousands of links, so this resolves
# them all with a few processes.
links_stay_inside() {
  local root=$1
  find _install _runtest -type l -print0 |
    xargs -0 -r realpath -e -- 2>&1 |
    awk -v install="$root/_install" -v runtest="$root/_runtest" '
      $0 != install && index($0, install "/") != 1 &&
      $0 != runtest && index($0, runtest "/") != 1 { outside = 1; exit }
      END { exit outside }'
}

# `unpack` stops reading the archive after its first members, with an
# option that only GNU tar has: `pack` asks for the same tar, so that what
# it packs can be unpacked where it was packed.
need_gnu_tar() {
  case $(tar --version 2>/dev/null) in
    *'GNU tar'*) ;;
    *) die "tar is not GNU tar, which \`unpack\` needs" ;;
  esac
}

pack() {
  local path prefix link target root
  need_gnu_tar
  root=$(pwd -P)
  for path in "${packed[@]}"; do
    [ -e "$path" ] ||
      die "$path is missing: run \`make install_for_test\` first"
  done
  # `unpack` reads Makefile.config before the trees: as a link to a file
  # of theirs it would point nowhere then. configure writes a file.
  [ ! -L Makefile.config ] ||
    die "Makefile.config is a symbolic link, which \`unpack\` cannot read"
  prefix=$(sed -n 's/^prefix=//p' Makefile.config)
  [ "$prefix" = "$PWD/_install" ] ||
    die "the configured prefix is $prefix, not $PWD/_install"
  # A link that leaves the two trees would dangle on the other machine.
  if ! links_stay_inside "$root"; then
    # Name the first such link, one link at a time.
    while IFS= read -r -d '' link; do
      target=$(realpath -e -- "$link" 2>/dev/null) ||
        die "$link is a dangling link"
      case $target in
        "$root"/_install | "$root"/_install/* | \
        "$root"/_runtest | "$root"/_runtest/*) ;;
        *) die "$link points outside the packed trees, to $target" ;;
      esac
    done < <(find _install _runtest -type l -print0)
    die "a link of the packed trees could not be resolved"
  fi
  describe_machine Makefile.config > "$info"
  # The description comes first, so that `unpack` reads it without the
  # trees; it is then packed again where it is, in _runtest.
  tar -cf - "$info" "${packed[@]}" |
    zstd -q -T0 -3 "$zstd_window" -f -o "$archive"
  cat "$info"
  echo "Packed ${packed[*]} into $archive:" \
    "$(du -h "$archive" | cut -f1) in $SECONDS s"
}

# query PATH -f PARAMETER --jq FILTER
#   Print what `gh api -X GET` prints for these arguments. The API can be
#   late for an upload that has just ended, so this tries again for a while
#   when the command fails or prints nothing. It fails unless its last try
#   succeeded: what the API does not say is never taken for an answer.
query() {
  local try answer answered
  for try in 1 2 3 4 5; do
    if answer=$(gh api -X GET "$@"); then
      answered=true
      [ -z "$answer" ] || break
      echo "The GitHub API has no answer yet (try $try of 5)." >&2
    else
      answered=false
      # $1 and $3: the path and the one parameter of the request.
      echo "This request failed (try $try of 5):" \
        "gh api -X GET $1 -f $3" >&2
    fi
    [ "$try" -eq 5 ] || sleep 5
  done
  $answered && printf '%s\n' "$answer"
}

# jobs_named JOB
#   Print a line for each job named JOB in this attempt of the run,
#   finished or not: `unfinished`, or its result and the time it started.
#   Fail with status 2 if the list was not whole, which here means: the
#   pages all give the same number of jobs for the attempt, none is short
#   while jobs remain, and they hold that many together. Which jobs they
#   hold is not checked: a job given twice for another one is not noticed.
#
# The API takes a time that grows with the number of jobs on the page that
# it is asked for, about 0.2 s a job in an attempt whose jobs are mostly
# those of an earlier one, and fails after 10 s: it has no answer at all
# for 100 jobs of such an attempt. So the pages are small after the first
# attempt, where they take 4 s; a page that fails is asked for again after
# a pause, and then the whole list in pages of half the size.
jobs_named() {
  local job=$1 url size=100 page seen total try answer count lines
  url=repos/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID
  url+=/attempts/$GITHUB_RUN_ATTEMPT/jobs
  [ "$GITHUB_RUN_ATTEMPT" = 1 ] || size=20
  while :; do
    page=1 seen=0 total='' lines=''
    while :; do
      for try in 1 2; do
        # The numbers of jobs in the attempt and on the page, then the
        # lines to print.
        if answer=$(gh api -X GET "$url" -f per_page="$size" \
            -f page="$page" --jq "\"\(.total_count) \(.jobs | length)\",
              (.jobs[] | select(.name == \"$job\") |
               if .conclusion == null then \"unfinished\"
               else \"\(.conclusion) \(.started_at)\" end)"); then
          break
        fi
        echo "This request failed (try $try of 2):" \
          "gh api -X GET $url -f per_page=$size -f page=$page" >&2
        answer=''
        [ "$try" -eq 2 ] || sleep 5
      done
      [ -n "$answer" ] || break
      count=${answer#* }
      count=${count%%$'\n'*}
      [[ ${answer%% *} =~ ^[0-9]+$ && $count =~ ^[0-9]+$ ]] ||
        die "the list of jobs has no counts: $answer"
      if [ "${answer%% *}" != "${total:-${answer%% *}}" ]; then
        echo "The number of jobs changed from $total to ${answer%% *}" \
          "while the list was read." >&2
        return 2
      fi
      total=${answer%% *}
      seen=$((seen + count))
      case $answer in
        *$'\n'*) lines+=${answer#*$'\n'}$'\n' ;;
      esac
      if [ "$seen" -eq "$total" ]; then
        printf '%s' "$lines"
        return
      fi
      # Only the last page may hold fewer jobs than it was asked for.
      if [ "$count" -lt "$size" ] || [ "$seen" -gt "$total" ]; then
        echo "The list of jobs is not whole: $seen jobs on $page pages" \
          "of $size, for $total in the attempt." >&2
        return 2
      fi
      page=$((page + 1))
    done
    [ "$size" -ge 10 ] || return 1
    size=$((size / 2))
    echo "Asking for the jobs again, $size on a page." >&2
    sleep 5
  done
}

# A build is the one to use only if the job that uploads it succeeded in
# this attempt of the run. The jobs of an attempt include those that were
# not run again in it, with the result and the times of their last run: a
# job that is re-run on its own finds the build of an earlier attempt, and
# a job re-run with a build job that failed this time finds none, although
# the earlier artifact is still there.
#
# The artifacts of a run are those of all its attempts. When the job that
# uploads NAME runs again, its upload replaces the earlier one. Only an
# artifact made since the job started is its own: the list of artifacts
# can be late, and show the earlier one for a while.
digest() {
  local job=$1 run try status jobs result started listing digest=''
  run=repos/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID
  # The name is written into a jq string.
  [[ $job != *[\"\\]* ]] || die "the job name $job cannot be looked up"
  # A job that is not finished, or not the only one of its name, says
  # nothing about the artifact, and neither does a list that was not
  # whole. The API can be late for a job that has just ended, and show it
  # unfinished or not at all: the list is read again for a while.
  for try in 1 2 3; do
    status=0
    jobs=$(jobs_named "$job") || status=$?
    case $status/$jobs in
      0/) echo "The list has no job named $job." >&2 ;;
      0/unfinished) echo "The list has the job $job as not finished." >&2 ;;
      # jobs_named has said what is wrong with the list.
      2/*) ;;
      0/*) break ;;
      *) die "could not list the jobs of this attempt of the workflow" \
        "run: see the requests that failed in the log of this step" ;;
    esac
    [ "$try" -eq 3 ] || {
      echo "Reading the list of jobs again (try $((try + 1)) of 3)." >&2
      sleep 5
    }
  done
  [ "$status" -eq 0 ] ||
    die "3 readings of the list of the jobs of this attempt of the" \
      "workflow run did not say which build to use, and the last was not" \
      "whole: see the log of this step"
  case $jobs in
    '') die "this attempt of the workflow run lists no job named $job," \
      "so there is no build to use: re-run all the jobs of the run, not" \
      "only this one" ;;
    *$'\n'*) die "this attempt of the workflow run has several jobs" \
      "named $job" ;;
    unfinished) die "the job $job has not finished in this attempt of" \
      "the workflow run" ;;
  esac
  result=${jobs%% *}
  started=${jobs#* }
  if [ "$result" = success ]; then
    # The time is written into a jq string below.
    [[ $started =~ ^[0-9]{4}(-[0-9]{2}){2}T[0-9]{2}(:[0-9]{2}){2}Z$ ]] ||
      die "the job $job has no start time: $started"
    listing=$(query "$run/artifacts" -f name="$archive" \
      --jq "[.artifacts[] | select((.expired | not)
          and .created_at >= \"$started\")]
        | max_by(.id) | .digest // empty") ||
      die "could not list the artifacts of this workflow run: see the" \
        "requests that failed in the log of this step"
    digest=${listing#sha256:}
    [ -z "$digest" ] || [[ $digest =~ ^[0-9a-f]{64}$ ]] ||
      die "the digest of the artifact $archive is not a SHA-256: $digest"
    [ -n "$digest" ] ||
      echo "The run lists no artifact $archive made since $started," \
        "when the job $job started." >&2
  fi
  {
    echo "result=$result"
    echo "digest=$digest"
  } | tee -a "${GITHUB_OUTPUT:-/dev/null}"
}

unpack() {
  local files path field actual expected differing=''
  need_gnu_tar
  if [ -d "$archive" ]; then
    # actions/download-artifact names the file after a header of the
    # server's response: take whatever single file it left.
    files=("$archive"/*)
    [ -e "${files[0]}" ] || die "$archive is empty"
    [ ${#files[@]} -eq 1 ] ||
      die "expected exactly one file in $archive, found ${#files[@]}"
    archive=${files[0]}
  fi
  [ -f "$archive" ] || die "$archive does not exist"
  if [ $# -eq 1 ]; then
    [[ $1 =~ ^[0-9a-f]{64}$ ]] || die "'$1' is not a SHA-256"
    actual=$(sha256sum < "$archive")
    actual=${actual%% *}
    [ "$actual" = "$1" ] ||
      die "the SHA-256 of $archive is $actual, not $1: it is not the build" \
        "to use"
  fi
  for path in "${packed[@]}"; do
    # -e is false for a link that points nowhere.
    [ ! -e "$path" ] && [ ! -L "$path" ] ||
      die "$path already exists: unpack in a fresh checkout"
  done
  # The description first, with the makefile that this machine describes
  # itself with, into a directory of its own: nothing is unpacked here for
  # a machine that differs. tar stops reading once it has the two files,
  # which closes the pipe on zstd: zstd is not in a pipeline with it, whose
  # status that would make a failure.
  staged=$(mktemp -d)
  tar -xf - -C "$staged" --occurrence=1 "$info" Makefile.config \
    < <(zstd -q -d "$zstd_window" -c "$archive" 2>/dev/null) ||
    die "cannot read $info and Makefile.config from $archive"
  echo "$archive was built on:"
  cat "$staged/$info"
  # Written to a file by a command of its own, so that a failure to
  # describe this machine stops the script.
  describe_machine "$staged/Makefile.config" > "$staged/here"
  while IFS='=' read -r field actual; do
    expected=$(sed -n "s/^$field=//p" "$staged/$info")
    if [ "$expected" != "$actual" ]; then
      echo "$field differs: this machine has $actual" >&2
      differing+=" $field"
    fi
  done < "$staged/here"
  [ -z "$differing" ] ||
    die "this machine differs from the one that built the tests" \
      "(${differing# }: see the log of this step), so they cannot run" \
      "here and nothing was unpacked. Re-run all the jobs of the run, not" \
      "only the failed ones, so that the build is made again."
  unpacking=true
  zstd -q -d "$zstd_window" -c "$archive" | tar -xf -
  for path in "${packed[@]}" "$info"; do
    [ -e "$path" ] || die "$archive has no $path"
  done
  unpacking=false
  echo "Unpacked $archive in $SECONDS s."
}

case $action in
  pack) pack ;;
  digest) digest "$3" ;;
  unpack) unpack "${@:3}" ;;
esac
