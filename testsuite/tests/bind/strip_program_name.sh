#!/bin/sh

# Keep the error message in "$1", without the program name it starts with.
sed -n '1s/^[^:]*: //p' "$1"
