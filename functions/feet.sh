#!/bin/bash

feet=$1
[ -z "$feet" ] && echo "Usage: $0 <feet>" && exit 1
#meter=$(echo "scale=6; $feet/3.28084" | bc -l) # with bc
meter=$(awk "BEGIN {print $feet/3.28084}") # wout bc
echo "$feet feet = $meter meter"

