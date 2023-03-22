#!/usr/bin/env Rscript

# get MPI npes depending on `srun --multi-prog=<config file>` argument; e.g. `configfile`
       
# from man srun:
# --multi-prog
#  Run a job with different programs and different arguments for each task. In this case, the executable 
#  program specified is actually a configuration file specifying the  executable  and  arguments for each
#  task. See MULTIPLE PROGRAM CONFIGURATION below for details on the configuration file contents. This 
#  option applies to step allocations.

# in the srun config file each line has 2 (optional 3) fields separated by space:
# Task rank (=processors)       Executable       [Arguments]

# in fesom1, the used meshpath depends on number of processors (MPI variable `npes`):
#fesom_env.F90:  function fesom_env_mesh_partition_path(meshdir, partition_count) result(resultdir)
#oce_read_mesh.F90:  dist_mesh_dir = fesom_env_mesh_partition_path(trim(meshpath), npes)
#fesom_env.F90:    resultdir = trim(meshdir)//'/dist/'//partition_count_str//'p'

if (interactive()) {
    # from `man srun`
    configfile <- c("       ###################################################################", 
                  "       # srun multiple program configuration file", "       #", 
                  "       # srun -n8 -l --multi-prog silly.conf", "       ###################################################################", 
                  "       4-6       hostname", "       1,7       echo  task:%t", 
                  "       0,2-3     echo  offset:%o", "")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    usage <- paste0("\nUsage:\n $ ", me, " /path/to/srun_--multi-prog=config_file\n")

    # check
    if (length(args) != 1) { # configfile
        message(usage)
        quit()
    }
    configfile <- args[1]
    if (!file.exists(configfile)) stop("configfile ", configfile, " does not exist")
    configfile <- normalizePath(configfile)
    message("read srun configfile ", configfile, " ...")
    configfile <- readLines(configfile)
} # if interactive or not

# remove any empty and comment lines
configfile <- trimws(configfile)
inds <- which(configfile == "")
if (length(inds) > 0) configfile <- configfile[-inds]
inds <- which(grepl("^#", configfile))
if (length(inds) > 0) configfile <- configfile[-inds]
cat(configfile, sep="\n")

warn <- options()$warn
options(warn=2) # stop on warning
npes_all <- 0
for (bini in seq_along(configfile)) { # for all binaries
    tmp <- configfile[bini]
    tmp <- strsplit(tmp, "\\s+")[[1]]
    bin <- tmp[2]
    pes <- tmp[1]
    pes <- strsplit(pes, ",")[[1]]
    pes_all <- vector("list", l=length(pes))
    for (pei in seq_along(pes_all)) {
        tmp <- strsplit(pes[[pei]], "-")[[1]]
        tmp <- as.integer(tmp)
        pes_all[[pei]]$pes_from <- tmp[1]
        if (length(tmp) == 1) {
            pes_all[[pei]]$pes_to <- tmp[1]
        } else if (length(tmp) == 2) {
            pes_all[[pei]]$pes_to <- tmp[2]
        } else {
            stop("config file entry \"", configfile[bini], "\" has non-valid PE entry \"", pes[[pei]])
        }
        pes_all[[pei]]$pes <- seq(pes_all[[pei]]$pes_from, pes_all[[pei]]$pes_to, b=1L)
        pes_all[[pei]]$npes <- length(pes_all[[pei]]$pes)
    } # for pei
    cat("--> bin ", bini, "/", length(configfile), ": ", bin, ", PEs: ", 
        paste(paste0(sapply(pes_all, "[[", "pes_from"), "-", sapply(pes_all, "[[", "pes_to"), collapse=",")), 
        ", nPEs = ", sum(sapply(pes_all, "[[", "npes")), "\n", sep="")
    npes_all <- npes_all + sum(sapply(pes_all, "[[", "npes"))
} # for bini
cat("--> total nPEs = ", npes_all, "\n", sep="")

options(warn=warn) # restore default

