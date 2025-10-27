#!/bin/sh

# Extract variable names from config.mli
# Exclude special variables that are not configuration values

grep '^val ' "$1" | \
  grep -v 'interface_suffix\|print_config\|config_var\|merlin\|flexdll_dirs' | \
  sed 's/^val \([^ :]*\).*/\1/' | \
  sort
