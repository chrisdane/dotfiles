#!/bin/bash

K=$1
[ -z "$K" ] && echo "Usage: $(basename $0) <Kelvin>" && exit 1
C=$(awk "BEGIN {printf \"%.4f\", $K - 273.15}")
echo "$K K = $C °C"

