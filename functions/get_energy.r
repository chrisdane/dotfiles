#!/usr/bin/env Rscript

# get energy of slurm job in J (= kg m2 s-2):
#   energy = energy_transfer_node [W] * n_node [#] * runtime [s]
# with W = kg m2 s-3 based on lines
#   * StepID | JobName      NodeHours    MaxRSS [Byte] (@task)   ReqCPUFreq [Hz]
#   * batch  | batch             46.0
# from slurm logfiles
# --> assumption: average energy transfer (or power) of node = 350 W
energy_transfer_node <- 350 # W

# usage
args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
usage <- paste0("\nUsage:\n $ ", me, " logfile1 [logfile2 ... logileN]\n")

# check logfile(s)
logs <- commandArgs(trailingOnly=T)
if (length(logs) == 0) {
    message(usage)
    quit()
}
if (any(!file.exists(logs))) {
    inds <- which(!file.exists(logs))
    stop(length(inds), " provided logfile", ifelse(length(inds) > 1, "s", ""), " ",
         ifelse(length(inds) > 1, "do", "does"), " not exist:\n",
         paste(logs[inds], collapse="\n"))
}

