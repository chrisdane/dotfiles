#!/bin/bash

# tree `-J` needs tree version 1.7.0
tree_version=$(tree --version | awk -F ' ' '{print $2}')
tree_version=${tree_version:1}
function version { echo "$@" | awk -F. '{ printf("%d%03d%03d%03d\n", $1,$2,$3,$4); }'; }
if [ $(version $tree_version) -lt $(version "1.7.0") ]; then
    echo "tree version $tree_version < 1.7.0 --> -J argument not available"
    exit 1
fi

# e.g. /mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/NCAR/CESM2/piControl/r1i1p1f1/Omon/fgco2/gr/v20190320
# --> path $1 = /mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP --> -L 6

path=$(readlink -f $1)
echo "run 'tree -d -L 6 -J $path > $2' ..."
tree -d -L 6 -J $path > $2


