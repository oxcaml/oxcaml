#!/bin/sh

# Helper for split.ml: check that every split alternative ran, in order.

printf 'a-x-1-ax1\na-x-2-ax2\nb-y-0-by0\nc-z-0-cz0\n' | cmp -s - arms.txt
