#!/bin/sh

# gdb is very spammy, even in 'quiet' mode, and --batch kills all output
# so instead, the script wraps intended output in LOG_BEGIN/LOG_END
gdb --nh --nx -q -ex 'source gdb_stepper.py' "$1" < /dev/null 2>&1 | \
    sed -n '/^LOG_BEGIN/,/^LOG_END/p' | sed '/^LOG_/d'
