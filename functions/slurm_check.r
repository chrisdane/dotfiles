#!/usr/bin/env Rscript

# get args
args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
help <- paste0("\nUsage:\n $ ", me, " [user=`whoami`]\n")

if (any(grepl("--help", args))) {
    message(help)
    quit()
}

# get user
args <- commandArgs(trailingOnly=T)
if (length(args) == 0) { # default: own user
    user <- Sys.info()["user"]
} else if (length(args) == 1) { # user was provided
    # check if provided user exists
    user <- args[1]
    cmd <- paste0("id -u ", user)
    message("run `", cmd, "` ...")
    check <- system(cmd)  # script stops here if provided user is invalid
    if (check == 1) quit()
} else {
    message(help)
    quit()
}

options(width=3000) # increase length per print line from default 80

# get jobs of user
cmd <- paste0("squeue -u ", user)
message("run `", cmd, "` ...")
squeue <- base::pipe(cmd)
squeue <- read.table(squeue, header=T, stringsAsFactors=F)
print(squeue)

# check if user has running jobs
status <- squeue$ST
if (!any(status == "R")) {
    message("no running jobs. quit slurm_check.r")
    #quit()

} else {
    inds <- which(status == "R")
    squeue <- squeue[inds,]
    message("\nthere are ", length(inds), " running jobs:")
    print(squeue)

    # get logfiles of running jobs
    jobids <- squeue$JOBID
    if (T) jobids <- rev(jobids) # show newest job last
    for (jobi in seq_along(jobids)) {

        message("\n******************************************")
        cmd <- paste0("scontrol show jobid -dd ", jobids[jobi])
        message("run `", cmd, "` ...")
        scontrol <- system(cmd, intern=T)
        if (!any(grepl("StdOut=", scontrol))) {
            message("--> did not find a \"StdOut=\" line. skip") 
        } else {
            logfile <- scontrol[which(grepl("StdOut=", scontrol))]
            logfile <- substr(logfile, 
                              start=regexpr("StdOut=", logfile) + 7,
                              stop=nchar(logfile))
            if (!file.exists(logfile)) {
                message("logfile \"", logfile, "\" does not exist. skip")
            } else {
                cmd <- paste0("tail -20 ", logfile)
                message("run `", cmd, "` ...")
                system(cmd)
            }
        }
    } # for jobi

} # if any running job or not



