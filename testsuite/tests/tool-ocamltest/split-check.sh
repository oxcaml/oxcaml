#!/bin/sh

# Helper for split.ml: check that every split alternative ran, in order.

printf 'a\nb\nc\n' | cmp -s - arms.txt
