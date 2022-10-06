#!/bin/bash

# usage:
# ./esgf_json_tree.sh esgfpath remove_ndir logfile

# tree `-J` needs tree version 1.7.0
tree_version=$(tree --version | awk -F ' ' '{print $2}')
tree_version=${tree_version:1}
function version { echo "$@" | awk -F. '{ printf("%d%03d%03d%03d\n", $1,$2,$3,$4); }'; }
if [ $(version $tree_version) -lt $(version "1.7.0") ]; then
    echo "tree version $tree_version < 1.7.0 --> -J argument not available"
    exit 1
fi

# tree -L: Max display depth of the directory tree.
# e.g. path = /mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP --> -L 6

echo "run 'tree -d -L $2 -J $1 > $3' ..."
tree -d -L $2 -J $1 > $3

