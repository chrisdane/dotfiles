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

# step 1/2: check if there are jobs in the queue
cmd <- paste0("squeue -u ", user, " --start")
message("run `", cmd, "` ...")
squeue <- base::pipe(cmd)
squeue <- read.table(squeue, header=T, stringsAsFactors=F) # does not work for the 3rd line in
#             JOBID    PARTITION     NAME     USER ST          START_TIME  NODES SCHEDNODES           NODELIST(REASON)
#          31927891    shared co2_flx_  a270073 PD                 N/A      1 (null)               (None)                               
#          31927015   compute esm-piCo  a270073 PD 2021-08-30T21:00:00     72 m[10322-10323,10504- (ReqNodeNotAvail, Reserved for maintenance)
#Error in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,  : 
#  line 2 did not have 9 elements
#squeue <- read.table(squeue, header=F, stringsAsFactors=F, sep="\t")
#squeue <- readLines(squeue)
#print(squeue)
if (any(squeue$ST != "R")) { # not running
    inds <- which(squeue$ST != "R")
    squeue <- squeue[inds,]
    message("--> there are ", length(inds), " non-running jobs (in alphabetical order of workdirs via `scontrol show jobid -dd <jobid>`):\n")
    #print(squeue)
    jobids <- squeue$JOBID
    workdirs <- rep(NA, t=length(jobids))
    for (jobi in seq_along(jobids)) { # get workpath of job
        cmd <- paste0("scontrol show jobid -dd ", jobids[jobi])
        scontrol <- system(cmd, intern=T)
        workdir <- scontrol[which(grepl("WorkDir=", scontrol))]
        workdir <- substr(workdir, 
                          start=regexpr("WorkDir=", workdir) + 8,
                          stop=nchar(workdir))
        workdirs[jobi] <- workdir
    }
    inds <- sort(workdirs, index.return=T)$ix
    squeue <- squeue[inds,]
    workdirs <- workdirs[inds]
    for (jobi in seq_along(jobids)) { # show job and workpath of job
        print(squeue[jobi,], row.names=jobi)
        message("workdir = ", workdirs[jobi], "\n")
    } # for jobi
} else {
    message("--> there are no non-running jobs")
}

# step 2/2: check if user has running jobs
cmd <- paste0("squeue -u ", user)
message("\nrun `", cmd, "` ...")
squeue <- base::pipe(cmd)
squeue <- read.table(squeue, header=T, stringsAsFactors=F)
#print(squeue)
if (any(squeue$ST == "R")) { # running
    inds <- which(squeue$ST == "R")
    squeue <- squeue[inds,]
    message("--> there are ", length(inds), " running jobs:")
    print(squeue)

    # get logfiles of running jobs
    jobids <- squeue$JOBID
    if (T) jobids <- rev(jobids) # show newest job last
    for (jobi in seq_along(jobids)) {
        message("\n****************** job ", jobi, "/", length(jobids), " ***********************")
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
} else { # if any running job or not
    message("--> there are no running jobs")
} 



