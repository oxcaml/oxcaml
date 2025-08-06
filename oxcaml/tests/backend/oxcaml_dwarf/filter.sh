#!/bin/bash

# Filter script for DWARF test output to remove sensitive/unstable information

sed \
  -e 's|/[^[:space:]]*/\([^/]*\.exe\)|<PATH>/\1|g' \
  -e 's|Process [0-9]*|Process <PID>|g' \
  -e 's|address = 0x[0-9a-f]*|address = <ADDRESS>|g' \
  -e 's|frame #[0-9]*: 0x[0-9a-f]*|frame #N: <ADDRESS>|g' \
  -e 's|argv=0x[0-9a-f]*|argv=<ADDRESS>|g' \
  -e "s|'$PWD[^']*'|'<BUILD_DIR>'|g" \
  -e 's|\r$||g' | tr -d '\r'
