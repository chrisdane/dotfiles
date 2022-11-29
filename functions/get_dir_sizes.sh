#!/bin/bash

# return sizes of non-empty directories

path=$(readlink -f $1) # normalize path
find $path -type d ! -empty -exec du -sh '{}' \; | sort -h

