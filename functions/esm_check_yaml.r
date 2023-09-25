#!/usr/bin/env Rscript

if (interactive()) {
    #args <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585/config/ssp585_finished_config.yaml_20150101-20151231"
    #args <- "/work/ab1095/a270073/out/awicm-1.0-recom/sofia/fwf_01/run_18700101-18701231/config/fwf_01_finished_config.yaml"
    #args <- "/scratch/de2d/out/awicm3-v3.1/test/run_20000101-20000103/config/test_finished_config.yaml"
    args <- c("general", "/scratch/de2d/out/awicm3-v3.1/test/run_20000101-20000103/config/test_finished_config.yaml")
    #args <- "output_finished_config.yaml"
} else {
    args <- commandArgs(trailingOnly=T) # user args only
    if (length(args) == 0) {
        # try() suppresses `ignoring SIGPIPE signal` when run with | head/more etc
        try(cat("Usage: esm_check_yaml.r /path/to/yaml/file\n"), silent=T)
        quit()
    }
}

file <- args[length(args)]
if (!file.exists(file)) stop("file ", file, " does not exist")
if (file.access(file, mode=4) == -1) stop("file ", file, " not readable")

user_sections <- NULL
if (length(args) > 1) { # user provided section
    user_sections <- args[1:(length(args)-1)]
}

library(yaml)
try(cat("read ", file, "\n", sep=""), silent=T)
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

if (!is.null(user_sections)) {
    inds <- match(user_sections, names(yaml))
    if (anyNA(inds)) {
        nainds <- which(is.na(inds))
        stop("the ", length(nainds), "/", length(user_section), " provided user sections ", 
             paste(user_sections[nainds], collapse=", "), " are not in this yaml file (",
             paste(names(yaml), collapse=", "), ")")
    }
    yaml <- yaml[inds]
} # if !is.null(user_sections)

# order of appearance
include <- c("account",
             "compute_time",
             "esm_function_dir",
             "esm_namelist_dir",
             "esm_runscript_dir",
             "use_venv",
             "install_esm_tools_branch",
             "couplings",
             "model_dir",
             "bin_sources", 
             "branch",
             "version",
             "execution_command",
             "comp_command",
             "include_models", 
             "pool_dir", 
             "mesh_dir",
             "resolution",
             "res_level",
             "res_number",
             "truncation",
             "nproc",
             "nproca",
             "nprocb",
             "omp_num_threads",
             "threads_per_core",
             "nlon",
             "nlat",
             "nx",
             "ny",
             "nlev",
             "levels",
             "levels_number",
             "grids",
             "time_step",
             "scenario",
             "scenario_type",
             "config_sources", 
             "namelist_dir", 
             "namelists",
             "namelist_changes", 
             "lresume",
             "restart_in_sources",
             "preprocess",
             "post_processing",
             "postprocess",
             "dataset", 
             "forcing_dir", "forcing_sources", 
             "input_dir", 
             "input_sources", 
             "adj_input_dir", 
             "additional_files", 
             "greenhouse_dir", 
             "ifsdata_dir",
             "further_reading"
             #, "runtime_environment_changes"
             )

# read yaml
for (i in seq_along(yaml)) {
    keys <- names(yaml[[i]])
    keys <- keys[match(include, keys)] # continue only with wanted keys
    if (any(is.na(keys))) keys <- keys[-which(is.na(keys))]
    if (length(keys) > 0) {
        try(cat("\n******************************************* section ", i, "/", length(yaml), 
                " *******************************************\n",
                names(yaml)[i], "\n", sep=""), silent=T)
        for (j in seq_along(keys)) {
            try(cat(paste0("    ", keys[j]), "\n"), silent=T)
            nams <- names(yaml[[i]][[keys[j]]])
            vals <- sapply(yaml[[i]][[keys[j]]], "[")
            if (typeof(vals) == "list") {
                if (length(vals) > 0) { # exclude empty `named list()`s 
                    if (length(vals) == 1) { # special case
                        if (F) {
                            str(sapply(yaml$fesom$namelist_changes, "[")) # list of length > 1: names ok
                            str(sapply(yaml$rnfmap$namelist_changes, "[")) # list of length = 1: names broken
                        }
                        vals <- sapply(yaml[[i]][[keys[j]]], "[", simplify=F)
                    } # if list of length 1
                    for (li in seq_along(vals)) {
                        try(cat(capture.output(str(vals[li], nchar.max=1e5, vec.len=1000)), sep="\n"), silent=T)
                    }
                    try(cat("\n"), silent=T)
                    #if (i == 8 && keys[j] == "namelist_changes") stop("asd")
                }
            } else {
                try(cat(paste(paste0("        ", nams, ": ", vals), collapse="\n"), "\n"), silent=T) 
            }
        } # for j
    } # if any of wanted keys exist
} # for i

