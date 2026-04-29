#!/bin/bash

F=$1
[ -z "$F" ] && echo "Usage: $(basename $0) <Fahrenheit>" && exit 1
C=$(awk "BEGIN {printf \"%.4f\", ($F - 32) * 5/9}")
echo "$F °F = $C °C"

