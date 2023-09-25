#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=T) 
if (length(args) != 3) {
    cat("Usage: esm_check_paths.r computer.pool_directories.pool dataset resolution\n",
        "  e.g. esm_check_paths.r /pool/data r0008 T63\n", sep="")
    quit()
}

computer.pool_directories.pool <- args[1]
dataset <- args[2]
resolution <- args[3]

pool_dir <- paste0(computer.pool_directories.pool, "/ECHAM6")
input_dir <- paste0(pool_dir, "/input/", dataset)
adj_input_dir <- paste0(input_dir, "/", resolution)
forcing_dir <- paste0(input_dir, "/", resolution)
greenhouse_dir <- pool_dir

cat("computer.pool_directories.pool = ", computer.pool_directories.pool, "\n",
    "dataset                        = ", dataset, "\n",
    "resolution                     = ", resolution, "\n",
    "pool_dir                       = computer.pool_directories.pool/ECHAM6 = ", pool_dir, "\n",
    "input_dir                      = pool_dir/input/dataset                = ", input_dir, "\n",
    "adj_input_dir                  = input_dir/resolution                  = ", adj_input_dir, "\n",
    "forcing_dir                    = input_dir/resolution                  = ", forcing_dir, "\n",
    "greenhouse_dir                 = pool_dir                              = ", greenhouse_dir, "\n", sep="")

