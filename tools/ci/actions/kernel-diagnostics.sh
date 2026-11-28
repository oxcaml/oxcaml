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

# After a failed build.yml job on Linux, print what distinguishes real
# crashes from runner-level problems: the kernel's segfault records include
# the fault address and instruction pointer, and MCE/EDAC records indicate
# hardware memory errors.

set -euo pipefail

echo '::group::rlimits'
ulimit -a
echo '::endgroup::'
echo '::group::Kernel segfault / trap records'
sudo dmesg -T | grep -iE 'segfault|traps:|general protection' \
  | tail -n 100 || echo 'none found'
echo '::endgroup::'
echo '::group::Hardware error records (MCE / EDAC)'
sudo dmesg -T | grep -iE 'mce:|edac|hardware error|machine check' \
  || echo 'none found'
echo '::endgroup::'
echo '::group::Last 100 kernel log lines'
sudo dmesg -T | tail -n 100
echo '::endgroup::'
