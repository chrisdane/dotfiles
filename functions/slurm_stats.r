#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()

# get energy of slurm job in J (= kg m2 s-2):
#   energy = energy_transfer_node [W] * n_node [#] * runtime [s]
# with W = kg m2 s-3 based on lines
#   * StepID | JobName      NodeHours    MaxRSS [Byte] (@task)   ReqCPUFreq [Hz]
#   * batch  | batch             46.0
# from slurm logfiles (string "* StepID | JobName      NodeHours" in logfile is used for node hours detection")

# assumption: average energy transfer (or power) of node = 350 W
energy_transfer_node <- 350 # W; from H. B. per mail

# for comparison
usage <- list()
usage[[length(usage)+1]] <- list(what="1 person household per year", units=list(kWh_year=2300))
usage[[length(usage)+1]] <- list(what="2 person household per year", units=list(kWh_year=3500))

#######################################################
    
exclude <- logs <- NULL # default
lhelp <- F

if (interactive()) {
    me <- "slurm_stats.r"
    #path <- getwd()
    #path <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical/scripts"
    #path <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/scripts"
    #path <- "/work/ab1095/a270094/AWIESM/SR_output/scripts" # chunk 1
    #path <- "/work/ba1103/a270094/AWIESM/test/scripts" # chunk 2
    #path <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/scripts"
    path <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl/scripts"
    #logs <- c("piControl_compute_29970101-29971231_33239962.log", "piControl_compute_33239962.log")
    #path <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/scripts"

} else { # if not interactive

    args <- commandArgs(trailingOnly=F) # internal and user args
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T) # user args only
    if (any(grepl("--help", args))) lhelp <- T
    path <- getwd()
    #print(args)

} # if interactive or not 
    
help <- paste0("\nUsage:\n $ ", me, " [--help] [--exclude=1,2,3] [logfile1 [logfile2 ... logileN]]\n",
               "   e.g. ", me, " --help\n", 
               "        ", me, " *compute_*.log # default\n",
               "        ", me, " *compute_????????.log\n")

# stop if help
if (lhelp) {
    message(help)
    if (interactive()) {
        stop("stop here")
    } else {
        quit()
    }
}

# continue if not help
if (!interactive()) {
    if (any(grepl("--exclude", args))) {
        exclude <- sub("--exclude=", "", args[grep("--exclude=", args)])
        exclude <- strsplit(exclude, ",")[[1]]
        oo <- options()$warn; options(warn=2) # error on warning
        exclude <- as.integer(exclude)
        options(warn=oo) # restore value from before
        message("provided `exclude` of length ", length(exclude), " = ", 
                paste(exclude, collapse=", "))
        #print(args)   
        if (length(args) > 1) { # exclude and logs were provided
            args <- args[(grep("--exclude=", args)+1):length(args)] # everything after --exclude
            logs <- args # remaining: either log files or grep pattern
        } else {
            # `logs` stays NULL
        }
        #print(args)   
    }
    #print(args)   
} # if not interactive 
#print(args)   

# check log files or grep pattern
if (is.null(logs)) { # no log files or grep pattern provided
    grep_pattern <- "*compute*.log" # default
    message("\nno logfiles provided --> use default grep pattern \"", grep_pattern, "\" ...")
} else { # log files or grep pattern provided
    # check if log files or grep pattern provided
    message(str(logs))
    if (file.exists(logs[1])) { # provided arg is a file
        message("\nprovided ", length(logs), " logfile", ifelse(length(logs) > 1, "s", ""))
        grep_pattern <- NULL
    } else { # provided arg is grep pattern
        grep_pattern <- logs
    } 
}

# get logs from grep pattern
if (interactive()) {
    opath <- getwd()
    setwd(path)
}
if (!is.null(grep_pattern)) {
    cmd <- paste0("find ", path, " -maxdepth 1 -type f -name \"", grep_pattern, "\" -printf \"%f\\n\" | sort")
    message("--> run `", cmd, "` ...")
    logs <- system(cmd, intern=T)
    message("--> found ", length(logs), " logfile", ifelse(length(logs) > 1, "s", ""))
    if (length(logs) == 0) {
        grep_pattern <- "*.log"
        message("--> use 2nd default grep pattern \"", grep_pattern, "\" ...")
        cmd <- paste0("find ", path, " -maxdepth 1 -type f -name \"", grep_pattern, "\" -printf \"%f\\n\" | sort")
        message("--> run `", cmd, "` ...")
        logs <- system(cmd, intern=T)
        message("--> found ", length(logs), " logfiles")
    }
}

if (length(logs) == 0) { # no log files found
    message(help)
    if (interactive()) {
        stop("stop here")
    } else {
        quit()
    }
}

# check existance of all logfiles
logs <- paste0(path, "/", logs)
if (any(!file.exists(logs))) {
    inds <- which(!file.exists(logs))
    stop(length(inds), " provided logfile", ifelse(length(inds) > 1, "s", ""), " ",
         ifelse(length(inds) > 1, "do", "does"), " not exist:\n",
         paste(logs[inds], collapse="\n"), "\n",
         help)
}

# apply exclude if provided
if (is.null(exclude)) { # default: keep all logs
    message("\nargument `--exclude=1,2,3` not provided. use it to drop certain logs")
} else {
    if (any(is.na(match(exclude, seq_along(logs))))) {
        inds <- which(is.na(match(exclude, seq_along(logs))))
        stop("`exclude` = ", paste(exclude[inds], collapse=", "), 
             " ", ifelse(length(inds) > 1, "are", "is"), 
             " not within 1:", length(logs))
    }
    message("\nargument `--exclude` was provided --> drop ", 
            length(exclude), " log", ifelse(length(exclude) > 1, "s", ""), ":\n",
            paste(paste0("   ", logs[exclude]), collapse="\n"))
    logs <- logs[-exclude]
}

if (length(logs) == 0) { # exclude removed all log files
    message("--> exclude removed all logfiles")
    message(help)
    if (interactive()) {
        stop("stop here")
    } else {
        quit()
    }
}

# for all logfiles
jobids <- rep(NA, t=length(logs))
grep_pattern <- "* JobID            : "
message("\ncheck ", length(logs), " log", ifelse(length(logs)> 1, "s", ""), 
        " for pattern \"", grep_pattern, "\" ...")
for (logi in seq_along(logs)) {

    # get jobid
    cmd <- paste0("grep \"", grep_pattern, "\" ", logs[logi])
    jobid <- suppressWarnings(system(cmd, intern=T)) # e.g. "* JobID            : 32651697"
    if (length(jobid) == 0) {
        if (T) message("logfile ", logi, "/", length(logs), " ", logs[logi], 
                       " does not contain this pattern. skip")
    } else {
        jobid <- strsplit(jobid, " ")[[1]]
        jobid <- jobid[length(jobid)]
        jobid <- as.integer(jobid)
        jobids[logi] <- jobid
    }

} # for logi logs
if (any(is.na(jobids))) {
    logs <- logs[which(!is.na(jobids))]
    jobids <- jobids[which(!is.na(jobids))]
}
if (length(jobids) == 0) {
    stop("zero log files contain the pattern \"", grep_pattern, "\"")
}
names(logs) <- jobids
jobids <- unique(jobids) # remove potential duplicates e.g. by symlink logfiles

# get stats of found jobids via `sacct`
# https://slurm.schedmd.com/sacct.html
message("\nget infos of ", length(jobids), " unique jobid", 
        ifelse(length(jobids) > 1, "s", ""), " via `sacct`:") 
colnames <- c("jobid", "elapsed", "nnodes", "submit", "start") 
cmd <- paste0("sacct ",
              "--noheader ",
              "--jobs=<jobids> ",
              #"--partition=compute,compute2 ",
              "--state=completed ",
              "--format=", paste(colnames, collapse=","))
#message("run `", cmd, "` ...")
cmd <- sub("<jobids>", paste(jobids, collapse=","), cmd)
message("run `", cmd, "` ...")
sacct <- base::pipe(cmd)
df <- read.table(sacct, header=F, col.names=colnames, stringsAsFactors=F)
# for one jobid e.g.:
# 32259196        compute       36   03:13:38 2021-09-10T09:02:20 2021-09-10T09:06:20
# 32259196.ba+                   1   03:13:38 2021-09-10T09:06:20 2021-09-10T09:06:20
# 32259196.0                    36   03:09:48 2021-09-10T09:06:23 2021-09-10T09:06:23

# old jobs are not in sql datsabase anymore --> `sacct` results nothing
if (nrow(df) == 0) {
    stop("those jobs are not in the sacct sql database anymore (6 months lifetime)")
    # todo: old 

} else { # sacct successfull

    # remove "<jobid>.ba+" and "<jobid>.0" entries
    inds <- match(df$jobid, jobids)
    df <- df[which(!is.na(inds)),]

} # if sacct was successfull or not

# update successfull jobids and logs
jobids <- df$jobid
inds <- match(jobids, names(logs))
logs <- logs[inds]

# calc nodes hours = nnodes * elapsed_hours
# elapsed in HH:MM:SS
elapsed_hour <- as.integer(substr(df$elapsed, 1, 2))
elapsed_hour <- elapsed_hour + as.integer(substr(df$elapsed, 4, 5))/60
elapsed_hour <- elapsed_hour + as.integer(substr(df$elapsed, 7, 8))/60/60
df$node_hours <- df$nnodes * elapsed_hour

# calc queue time = start - submit
# start and sbmit in YYYY-MM-DDTHH:MM:SS 
start <- as.POSIXct(paste0(substr(df$start, 1, 10), " ", substr(df$start, 12, 19)), tz="UTC")
submit <- as.POSIXct(paste0(substr(df$submit, 1, 10), " ", substr(df$submit, 12, 19)), tz="UTC")
queue_sec <- as.numeric(difftime(start, submit, units="sec"))
queue_min <- as.numeric(difftime(start, submit, units="min"))
queue_hour <- as.numeric(difftime(start, submit, units="hour"))
# get queue time with auto format for every run
queue <- rep(NA, t=dim(df)[1])
for (logi in seq_along(queue)) {
    tmp <- difftime(start[logi], submit[logi], units="auto")
    queue[logi] <- paste0(round(tmp, 2), " ", attributes(tmp)$units)
}
df$queue <- queue
# start and submit not needed anymore --> drop from data frame
df$start <- df$submit <- NULL

# calc energy assuming a constant energy transfer per node
df$energy_kWh <- energy_transfer_node * df$node_hours / 1e3 # W * h / 1e3 = kWh

# print result
print(df, row.names=seq_len(dim(df)[1]))

# print elapsed stats without queue time
message("\nelapsed stats without queue time:")
print(summary(elapsed_hour))
elapsed_per_run_hour <- mean(elapsed_hour, na.rm=T)
message("\nmean elapsed per run without queue time: ", round(elapsed_per_run_hour, 2), " hours")
message("--> throughput per day without queue time = 24 hours/day / ", round(elapsed_per_run_hour, 2), " hours/run = ", 
        round(24/elapsed_per_run_hour, 2), " ~ ", 
        floor(24/elapsed_per_run_hour), " runs/day")
facs <- c(10, 100, 150, 165, seq(200, 1000, b=100))
message(paste(paste0("--> ", facs, " runs need ", 
                     round(facs*elapsed_per_run_hour, 2), " hours = ",
                     round(facs*elapsed_per_run_hour/24, 2), " days = ",
                     round(facs*elapsed_per_run_hour/24/7, 2), " weeks = ",
                     round(facs*elapsed_per_run_hour/24/30.5, 2), " months"),
              collapse="\n"))

# print elapsed stats with queue time
elapsed_per_run_hour_with_queue <- elapsed_per_run_hour + mean(queue_hour, na.rm=T)
message("\nmean elapsed per run with queue time: ", round(elapsed_per_run_hour_with_queue, 2), " hours")
message("--> throughput per day without queue time = 24 hours/day / ", round(elapsed_per_run_hour_with_queue, 2), " hours/run = ", 
        round(24/elapsed_per_run_hour_with_queue, 2), " ~ ", 
        floor(24/elapsed_per_run_hour_with_queue), " runs/day")
facs <- c(10, 100, 150, 165, seq(200, 1000, b=100))
message(paste(paste0("--> ", facs, " runs need ", 
                     round(facs*elapsed_per_run_hour_with_queue, 2), " hours = ",
                     round(facs*elapsed_per_run_hour_with_queue/24, 2), " days = ",
                     round(facs*elapsed_per_run_hour_with_queue/24/7, 2), " weeks = ",
                     round(facs*elapsed_per_run_hour_with_queue/24/30.5, 2), " months"),
              collapse="\n"))

# print nnodes stats
message("\nmean nnodes per run: ", mean(df$nnodes, na.rm=T))

# print node hour stats
message("\ntotal node hours of ", dim(df)[1], " runs: ", 
        round(sum(df$node_hours, na.rm=T)), " node hours")
node_hours_per_run <- mean(df$node_hours, na.rm=T)
message("mean node hours per run: ", round(node_hours_per_run), " node hours")
facs <- c(10, 100, 150, 165, seq(200, 1000, b=100))
message(paste(paste0("--> ", facs, " runs need ", 
                     round(facs*node_hours_per_run), " node hours"), 
              collapse="\n"))

# print queue stats
message("\nqueue stats in secs:")
print(summary(queue_sec))
message("\nqueue stats in mins:")
print(summary(queue_min))
message("\nqueue stats in hours:")
print(summary(queue_hour))

# print energy stats
energy_kWh_per_run <- mean(df$energy_kWh, na.rm=T)
message("\nenery assumption: power of 1 node = ", energy_transfer_node, " W (kg m2 s-3)")
message("--> energy consumption per run = ", 
        round(node_hours_per_run), " node hours per run * ", energy_transfer_node, " W per node = ", 
        energy_kWh_per_run, " kWh")
energy_kWh_total <- sum(df$energy_kWh, na.rm=T)
message("--> total energy consumption = ", energy_kWh_total, " kWh")
message("for comparison:")
for (i in seq_along(usage)) {
    message(usage[[i]]$what, ": ", appendLF=F)
    for (j in seq_along(usage[[i]]$units)) {
        if (j > 1) message(rep(" ", t=nchar(usage[[i]]$what)), appendLF=F)
        message(usage[[i]]$units[[j]], " ", names(usage[[i]]$units)[j])
    }
}

# plot queue time
grep_pattern <- "* JobName          : "
cmd <- paste0("grep \"", grep_pattern, "\" ", logs[1])
jobname <- suppressWarnings(system(cmd, intern=T)) # e.g. "* JobName          : piControl"
if (length(jobname) == 0) {
    jobname <- basename(dirname(path))
} else {
    jobname <- trimws(jobname)
    jobname  <- strsplit(jobname, " ")[[1]]
    jobname <- jobname[length(jobname)]
}
y <- queue_hour # default
unit <- "hours"
if (all(queue_hour < 1)) {
    y <- queue_min
    unit <- "mins"
}
if (all(queue_min < 1)) {
    y <- queue_sec
    unit <- "secs"
}
plotname <- paste0(normalizePath("~"), "/queue_time_", jobname, ".png")
message("\nplot ", plotname, " ...")
png(plotname, width=2000, height=2000/(4/3), res=200, family="Nimbus Sans L")
plot(start, y, t="n", xaxt="n", yaxt="n",
     xlab="date", ylab=paste0("queue time [", unit, "]"),
     main=jobname)
axis.POSIXct(1, at=pretty(start, n=10), format="%b %d")
axis(2, at=pretty(y, n=10), las=2)
points(start, y)
lines(start, y)
invisible(dev.off())

if (interactive()) setwd(opath)

message()
