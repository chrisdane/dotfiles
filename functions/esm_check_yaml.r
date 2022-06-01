#!/usr/bin/env Rscript

if (interactive()) {
    #file <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585/config/ssp585_finished_config.yaml_20150101-20151231"
    file <- "output_finished_config.yaml"
} else {
    args <- commandArgs(trailingOnly=T) # user args only
    if (length(args) != 1) {
        # try() suppress `ignoring SIGPIPE signal` when run with | head/more etc
        try(cat("Usage: esm_check_yaml.r /path/to/yaml/file\n"), silent=T)
        quit()
    } else {
        file <- args[1]
    }
}

library(yaml)
if (interactive()) message("read", file, "\n")
yaml <- yaml::read_yaml(file)
# e.g.
#List of 10
# $ computer  :List of 56
# $ debug_info:List of 1
# $ defaults  :List of 3
# $ echam     :List of 128
# $ fesom     :List of 113
# $ general   :List of 110
# $ hdmodel   :List of 61
# $ jsbach    :List of 92
# $ oasis3mct :List of 86
# $ recom     :List of 85

include <- c("additional_files", "adj_input_dir", "bin_sources", "config_sources", "dataset", 
             "forcing_dir", "forcing_sources", "greenhouse_dir", "input_dir", 
             "input_sources", "pool_dir", "restart_in_sources",
             "runtime_environment_changes")

# read yaml
for (i in seq_along(yaml)) {
    keys <- names(yaml[[i]])
    keys <- keys[match(include, keys)] # continue only with wanted keys
    if (any(is.na(keys))) keys <- keys[-which(is.na(keys))]
    if (length(keys) > 0) {
        try(cat(names(yaml)[i], "\n"), silent=T)
        for (j in seq_along(keys)) {
            try(cat(paste0("    ", keys[j]), "\n"), silent=T)
            nams <- names(yaml[[i]][[keys[j]]])
            vals <- sapply(yaml[[i]][[keys[j]]], "[")
            if (class(vals) == "list") {
                try(cat(capture.output(str(vals, vec.len=1000)), sep="\n"), silent=T)
            } else if (class(vals) == "character") { # default, e.g. a path
                try(cat(paste(paste0("        ", nams, ": ", vals), collapse="\n"), "\n"), silent=T) 
            } else {
                stop("class ", paste(class(vals), collapse=", "), " of vals not known")
            }
        } # for j
    } # if any of wanted keys exist
} # for i

