#!/bin/sh

# hide flaky parts of error messages
grep "Inconsistent use of ~enabled_at_init" | sed 's/Error producing assembly code for .*: /Error producing assembly code for HIDE_NAME: / '
