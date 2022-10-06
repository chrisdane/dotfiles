# r

rm(list=ls()); graphics.off()

library(jsonlite)

f <- "esgf_tree.json" # produced with esgf_json_tree.sh

json <- jsonlite::read_json(f)
json <- json[[1]]

# main dir
path <- json$name # /mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP

# 1st subdirs
json <- json$contents
models <- vector("list", l=length(json))
names(models) <- sapply(json, "[[", "name") # "AS-RCEC" "AWI" ...

# 2nd subdirs
for (mi in seq_along(models)) {
    models[[mi]] <- json[[mi]]$contents
}

