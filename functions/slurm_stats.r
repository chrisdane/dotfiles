#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()

# get energy of slurm job in J (= kg m2 s-2):
#   energy = energy_transfer_node [W] * n_node [#] * runtime [s]
# with W = kg m2 s-3 based on lines
#   * StepID | JobName      NodeHours    MaxRSS [Byte] (@task)   ReqCPUFreq [Hz]
#   * batch  | batch             46.0
# from slurm logfiles (string "* StepID | JobName      NodeHours" in logfile is used for node hours detection")

# assumption: mean energy transfer (or power) of node = 350 W
energy_transfer_node <- 350 # W; from hendryk per mail

# for comparison
usage <- list()
usage[[length(usage)+1]] <- list(what="1 person household per year", units=list(kWh_year=2300))
usage[[length(usage)+1]] <- list(what="2 person household per year", units=list(kWh_year=3500))

#######################################################
    
if (interactive()) {
    me <- "slurm_stats.r"
    #args <- ""
    #args <- c("a", "b")
    #args <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl/scripts/*_compute_*-*_*.log"
    #args <- c("--exclude=3", "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl/scripts/*_compute_*-*_*.log")
    #args <- c("--exclude=1,2,3,18,44,45", "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest/log/*_compute_*-*_*.log")
    #args <- c("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl/scripts/esm-piControl_compute_31480101-31481231_34357868.log", "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl/scripts/esm-piControl_compute_31490101-31491231_34359503.log")
    #args <- "*_awicm_compute_*"
    #args <- "*_compute_*-*_*.log"
    #args <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/log/*_compute_*-*_*.log"
    #args <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/log/*_compute_*-*_(2100732|2100996).log"
    args <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical3/log/*_compute_*-*_*.log"

} else { # if not interactive
    args <- commandArgs(trailingOnly=F) # internal and user args
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T) # user args only
    #print(args)

} # if interactive or not 
    
help <- paste0("\nUsage:\n $ ", me, " [--exclude=1,2,3] logfile1 [logfile2 ... logileN>]\n",
               "   e.g. ", me, " # shows this help\n", 
               "        ", me, " *_awicm_compute_*\n",
               "        ", me, " *_compute_*-*_*.log\n")
oo <- options() # save old/default options
options(warn=2) # error on warning

# stop if help
if (length(args) == 0) {
    message(help)
    if (interactive()) {
        stop("stop interactive call")
    } else {
        quit()
    }
}

# continue if not help
exclude <- NULL # default
if (!any(grepl("--exclude", args))) {
    logs <- args # "logfile1" or "logfile1 logfile2" or "logfile*"
} else { # --exclude was provided
    exclude <- sub("--exclude=", "", args[grep("--exclude=", args)])
    exclude <- strsplit(exclude, " ")[[1]]
    exclude <- exclude[1] # "1,2"
    exclude <- strsplit(exclude, ",")[[1]] # "1" "2"
    exclude <- as.integer(exclude) # 1 2
    message("provided `--exclude` of length ", length(exclude), ": ", paste(exclude, collapse=", "))
    logs <- args
    logs <- logs[-grep("--exclude", args)]
}
#print(logs)   
#stop("asd")

if (length(logs) == 0) { # no log files found
    message(help)
    if (interactive()) {
        stop("stop interactive call")
    } else {
        quit()
    }
} else {
    message("check provided ", length(logs), " logs/patterns ...")
    head(logs, n=10)
    message("...")
    tail(logs, n=10)
}

# apply potential wildcards
if (interactive()) {
    tmp <- list.files(dirname(logs), pattern=basename(logs), full.names=T)
    if (length(tmp) == 0) {
        tmp <- list.files(dirname(logs), pattern=glob2rx(basename(logs)), full.names=T)
        if (length(tmp) == 0) {
            stop("interactive found zero files")
        }
    }
    logs <- tmp
}

if (any(!file.exists(logs))) {
    inds <- which(!file.exists(logs))
    print(logs[inds])
    stop("these ", length(inds), "/", length(logs), " provided logs do not exist")
}
logs <- normalizePath(logs) # clean path and dereference potential links
logs <- unique(logs)
dirs <- dirname(logs)
logs <- basename(logs)

# finished loading logfiles
message("\nwork on ", length(logs), " logs ...")
head(logs, n=10)
message("...")
tail(logs, n=10)

# ask user which substring in logfile is jobid
nchars <- nchar(logs)
nchars_unique <- unique(nchars)
message("\nprovided ", length(logs), " logfile filename patterns have ", length(nchars_unique), " different numbers of characters. where is the jobid?")
jobids <- vector("list", l=length(nchars_unique))
for (nci in seq_along(nchars_unique)) {
    message("logfile pattern ", nci, "/", length(nchars_unique), ": ", nchars_unique[nci], " characters")
    inds <- which(nchars == nchars_unique[nci])
    if (nchars_unique[nci] < 1000) collapse <- "   "
    if (nchars_unique[nci] < 100) collapse <- "  "
    if (nchars_unique[nci] < 10) collapse <- " "
    message(paste(strsplit(logs[inds[1]], "")[[1]], collapse=collapse), "\n",
            paste(sprintf(paste0("%0", nchar(collapse), "i"), seq_len(nchars_unique[nci])), collapse=rep(" ", t=nchar(collapse)-1)), "\n",
            "Enter start and end positions between 1 and ", nchars_unique[nci], 
            " separated by space to get jobid from filename:")
    fromto <- base::scan("stdin", character(), n=2)
    fromto <- as.integer(fromto) # 1 2 
    if (any(is.na(match(fromto, seq_len(nchars_unique[nci]))))) stop("start and end positions must be between 1 and ", nchars_unique[nci])
    jobid <- substr(logs[inds[1]], fromto[1], fromto[2])
    message("--> run `as.numeric(\"", jobid, "\")` ... ", appendLF=F)
    jobid <- as.numeric(jobid) # error if no success (e.g. some letters included)
    message(jobid, " ok")
    jobids[[nci]] <- data.frame(log=logs[inds], jobid=substr(logs[inds], fromto[1], fromto[2]))
} # for nci
logs <- sapply(jobids, "[[", "log")
jobids <- as.vector(sapply(jobids, "[[", "jobid"))

# apply exclude if provided
if (is.null(exclude)) { # default: keep all logs
    message("\nargument `--exclude=1,2,3` not provided. use it if you want drop certain logs")
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
    dirs <- dirs[-exclude]
    jobids <- jobids[-exclude]
}

if (length(logs) == 0) { # exclude removed all log files
    message("--> exclude removed all logfiles")
    message(help)
    if (interactive()) {
        stop("stop interactive call")
    } else {
        quit()
    }
}

# get job infos 
message("\nget job infos of ", length(logs), " jobs ...")

if (F) { # debug
    jobids <- as.character(c(1392153, 1392499, 12329873492873423)) # successful, non-successful, non-existing
}

# try 1/2: `sacct`
sacct_colnames <- c("jobid", "cluster", "partition", "account", "submit", "start", "end", "elapsed", "nnodes") 
sacct_nchars <-   c(     20,        20,          20,        20,       20,      20,    20,        20,       10)
if (T) {
    sacct_colnames <- c(sacct_colnames, "NodeList")
    sacct_nchars <-   c(sacct_nchars,           500)
}
cmd <- paste0("sacct ",
              "--noheader ",
              "--jobs=", paste(jobids, collapse=","), " ",
              #"--jobs=", jobids[jobi], " ",
              #"--partition=compute,compute2 ",
              "--state=completed ",
              "--format=", paste(paste0(sacct_colnames, "%", sacct_nchars), collapse=","))
message("\ntry 1/2: run `", cmd, "` ...")
sacct <- base::pipe(cmd)
#df <- read.table(sacct, header=F, col.names=sacct_colnames, stringsAsFactors=F)
df <- read.fwf(sacct, widths=sacct_nchars+1, header=F, col.names=sacct_colnames, stringsAsFactors=F)
# e.g.
#sacct --noheader --jobs=1392153,1392499,12329873492873424 --state=completed --format=jobid%20,cluster%20,partition%20,account%20,submit%20,start%20,end%20,elapsed%20,nnodes%10
#             1392153              levante              compute               ba1103  2022-08-08T02:34:44  2022-08-08T02:35:12  2022-08-08T04:54:54             02:19:42         10 
#       1392153.batch              levante                                    ba1103  2022-08-08T02:35:12  2022-08-08T02:35:12  2022-08-08T04:54:54             02:19:42          1 
#      1392153.extern              levante                                    ba1103  2022-08-08T02:35:12  2022-08-08T02:35:12  2022-08-08T04:54:54             02:19:42         10 
#           1392153.0              levante                                    ba1103  2022-08-08T02:35:15  2022-08-08T02:35:15  2022-08-08T04:53:11             02:17:56         10
if (nrow(df) > 0) { # slurm job info found from at least one job via sacct
    df <- data.frame(apply(df, 2, function(x) gsub("\\s+", "", x)))
    #     jobid            cluster   partition account  submit                start                 end                   elapsed    nnodes
    #[1,] "1392153"        "levante" "compute" "ba1103" "2022-08-08T02:34:44" "2022-08-08T02:35:12" "2022-08-08T04:54:54" "02:19:42" "10"  
    #[2,] "1392153.batch"  "levante" ""        "ba1103" "2022-08-08T02:35:12" "2022-08-08T02:35:12" "2022-08-08T04:54:54" "02:19:42" "1"   
    #[3,] "1392153.extern" "levante" ""        "ba1103" "2022-08-08T02:35:12" "2022-08-08T02:35:12" "2022-08-08T04:54:54" "02:19:42" "10"  
    #[4,] "1392153.0"      "levante" ""        "ba1103" "2022-08-08T02:35:15" "2022-08-08T02:35:15" "2022-08-08T04:53:11" "02:17:56" "10"    
    
    # remove "jobid.batch", "jobid.extern" and "jobid.0" entries without a partition
    inds <- which(df$partition == "")
    if (length(inds) > 0) df <- df[-inds,]
}

# get job infos try 2/2: dkrz job summary
dkrz_grep_pattern <- "This is the automated job summary provided by DKRZ."
if (nrow(df) != length(jobids)) { # not all slurm job infos were found via sacct; 2 reasons:
    # 1: job was not finished --> sacct returns something option `--state=completed` is not provided
    # 2: job is too old and not in sacct database anymore --> sacct returns nothing
    
    missing_jobid_inds <- which(is.na(match(jobids, df$jobid)))
    message("\nsome jobids were not found in sacct database or not completed yet --> try 2/2: slurm jobinfos of ", length(missing_jobid_inds), 
            " jobs not found in sacct database --> check if there is a dkrz job summary for these jobs via pattern:\n   \"",
            dkrz_grep_pattern, "\" ...")
    for (jobi in seq_along(missing_jobid_inds)) {
        logfile <- paste0(dirs[missing_jobid_inds[jobi]], "/", logs[missing_jobid_inds[jobi]])
        cmd <- paste0("grep \"", dkrz_grep_pattern, "\" ", logfile)
        tmp <- suppressWarnings(system(cmd, intern=T)) # e.g. "* JobID            : 32651697"
        row <- df[1,] # placeholder
        row[] <- NA
        row$jobid <- jobids[missing_jobid_inds[jobi]]
        if (length(tmp) == 0) { # no success
            #message("--> this logfile does not have a job summary")
        } else { # success
            cmd <- paste0("grep \"* Submit date      : \" ", logfile)
            tmp <- suppressWarnings(system(cmd, intern=T))
            tmp <- strsplit(tmp, " ")[[1]]
            tmp <- tmp[length(tmp)]
            row$submit <- tmp # e.g. "2022-01-31T13:22:51"
            cmd <- paste0("grep \"* Start time       : \" ", logfile)
            tmp <- suppressWarnings(system(cmd, intern=T))
            tmp <- strsplit(tmp, " ")[[1]]
            tmp <- tmp[length(tmp)]
            row$start <- tmp # e.g. "2022-01-31T13:22:51"
            cmd <- paste0("grep \"* End time         : \" ", logfile)
            tmp <- suppressWarnings(system(cmd, intern=T))
            tmp <- strsplit(tmp, " ")[[1]]
            tmp <- tmp[length(tmp)]
            row$end <- tmp # e.g. "2022-01-31T13:22:51"
            cmd <- paste0("grep \"* Elapsed time     : \" ", logfile)
            tmp <- suppressWarnings(system(cmd, intern=T))
            tmp <- strsplit(tmp, " ")[[1]]
            tmp <- tmp[9]
            row$elapsed <- tmp # e.g. "03:40:44"
            cmd <- paste0("grep \"* Nodelist         : \" ", logfile)
            tmp <- suppressWarnings(system(cmd, intern=T)) 
            tmp <- strsplit(tmp, " ")[[1]]
            tmp <- gsub("[[:punct:]]", "", tmp[13]) # e.g. "(24)" --> "24"
            row$nnodes <- as.integer(tmp)
        } # if dkrz job summary available or not
        df <- rbind(df, row)
    } # for jobi

} # if (nrow(df) != length(jobids))

# exclude non-successful jobs
nainds <- which(is.na(df$submit))
if (length(nainds) > 0) {
    if (length(nainds) == length(logs)) {
        if (interactive()) {
            stop("could no infer any job stats")
        } else {
            message("could no infer any job stats")
            quit()
        }
    }
    message("\nremove ", length(nainds), " jobs from which no slurm jobinfos could be derived:")
    options(width=1000)
    print(data.frame(log=paste0(dirs[nainds], "/", logs[nainds]), jobid=jobids[nainds]))
    options(width=oo$width)
    df <- df[-nainds,]
    logs <- logs[-nainds]
    dirs <- dirs[-nainds]
    jobids <- jobids[-nainds]
}

## finish

# sort along jobid
inds <- sort(df$jobid, index.return=T)$ix
df <- df[inds,]

start <- df$start # "2022-08-08T02:35:12"
submit <- df$submit # "2022-08-08T02:34:44"
end <- df$end # 2022-08-08T04:54:54"
elapsed <- df$elapsed # "02:19:42"
nnodes <- as.integer(df$nnodes) # 10

# calc nodes hours = nnodes * elapsed_hours
# elapsed in HH:MM:SS
elapsed_hour <- as.integer(substr(elapsed, 1, 2))
elapsed_hour <- elapsed_hour + as.integer(substr(elapsed, 4, 5))/60
elapsed_hour <- elapsed_hour + as.integer(substr(elapsed, 7, 8))/60/60
node_hours <- nnodes * elapsed_hour

# calc queue time = start - submit
# start and sbmit in YYYY-MM-DDTHH:MM:SS 
start <- as.POSIXct(paste0(substr(start, 1, 10), " ", substr(start, 12, 19)), tz="UTC")
submit <- as.POSIXct(paste0(substr(submit, 1, 10), " ", substr(submit, 12, 19)), tz="UTC")
queue_sec <- as.numeric(difftime(start, submit, units="sec"))
queue_min <- as.numeric(difftime(start, submit, units="min"))
queue_hour <- as.numeric(difftime(start, submit, units="hour"))
# get queue time with auto format for every run
queue <- rep(NA, t=length(logs))
for (logi in seq_along(queue)) {
    tmp <- difftime(start[logi], submit[logi], units="auto")
    queue[logi] <- paste0(round(tmp, 2), " ", attributes(tmp)$units)
}

# calc energy assuming a constant energy transfer per node
energy_kWh <- energy_transfer_node * node_hours / 1e3 # W * h / 1e3 = kWh

# clean node lists
nodes <- df$NodeList
nodes <- gsub("l\\[", "", nodes)
nodes <- gsub("\\]", "", nodes)
nodes <- strsplit(nodes, ",")
for (ni in seq_along(nodes)) {
    tmp <- vector("list", l=length(nodes[[ni]]))
    for (nj in seq_along(tmp)) {
        tmp2 <- as.integer(strsplit(nodes[[ni]][[nj]], "-")[[1]]) # e.g. 30156-30157
        if (length(tmp2) == 1) {
            tmp[[nj]] <- tmp2
        } else if (length(tmp2) == 2) {
            tmp[[nj]] <- seq(tmp2[1], tmp2[2], b=1L)
        } else {
            stop("should not happen")
        }
    } # for nj
    nodes[[ni]] <- unlist(tmp) # sort not necessary; sacct returnes nodelist already sorted
} # for ni
nodes_unique <- unique(unlist(nodes)) 
nodes_cumulative_hode_hours <- nodes_cumulative_njobs <- nodes_cumulative_jobids <- rep(0, t=length(nodes_unique))
for (ni in seq_along(nodes_unique)) {
    inds <- which(sapply(nodes, function(x) any(x == nodes_unique[ni])))
    if (length(inds) > 0) {
        nodes_cumulative_hode_hours[ni] <- nodes_cumulative_hode_hours[ni] + sum(node_hours[inds])
        nodes_cumulative_njobs[ni] <- nodes_cumulative_njobs[ni] + length(inds)
        nodes_cumulative_jobids[ni] <- nodes_cumulative_jobids[ni] + sum(as.integer(jobids[inds])) # proxy for point of experiment
    }
} # for ni
nodes_cumulative_hode_hours_mean <- nodes_cumulative_hode_hours/nodes_cumulative_njobs
nodes_cumulative_jobids_mean <- nodes_cumulative_jobids/nodes_cumulative_njobs

# print result
message("\n****************************************************************\n",
        "results of ", length(jobids), " jobids (check if you want to rerun with `--exclude=1,2,...`):\n")
options(width=1000)
df <- data.frame(log=logs, jobid=jobids, elapsed_h=elapsed_hour, node_hours=node_hours, nnodes=nnodes, queue=queue, energy_kWh=energy_kWh)
print(df, row.names=seq_len(nrow(df)))
options(width=oo$width)

# print elapsed stats without queue time
message("\nelapsed stats (in hours) without queue time:")
print(summary(elapsed_hour))
elapsed_per_run_hour_mean <- mean(elapsed_hour, na.rm=T)
elapsed_per_run_hour_median <- median(elapsed_hour, na.rm=T)
message("\nmean (median) elapsed per run without queue time: ", round(elapsed_per_run_hour_mean, 2), 
        " (", round(elapsed_per_run_hour_median, 2), ") hours")
message("--> mean (median) throughput per day without queue time = 24 hours/day / ", round(elapsed_per_run_hour_mean, 2), 
        " (", round(elapsed_per_run_hour_median, 2), ") hours/run = ", 
        round(24/elapsed_per_run_hour_mean, 2), " (", round(24/elapsed_per_run_hour_median, 2), ") ~ ", 
        floor(24/elapsed_per_run_hour_mean), " (", floor(24/elapsed_per_run_hour_median), ") runs/day")
facs <- c(10, 30, 50, 100, 150, 165, 200, 250, seq(300, 1000, b=100))
message(paste(paste0("--> ", facs, " runs need ", 
                     round(facs*elapsed_per_run_hour_mean, 2), " (", round(facs*elapsed_per_run_hour_median, 2), ") hours = ",
                     round(facs*elapsed_per_run_hour_mean/24, 2), " (", round(facs*elapsed_per_run_hour_median/24, 2), ") days = ",
                     round(facs*elapsed_per_run_hour_mean/24/7, 2), " (", round(facs*elapsed_per_run_hour_median/24/7, 2), ") weeks = ",
                     round(facs*elapsed_per_run_hour_mean/24/30.5, 2), " (", round(facs*elapsed_per_run_hour_median/24/30.5, 2), ") months"),
              collapse="\n"))

# print queue stats
message("\nqueue stats in secs:")
print(summary(queue_sec))
message("\nqueue stats in mins:")
print(summary(queue_min))
message("\nqueue stats in hours:")
print(summary(queue_hour))

# print elapsed stats with queue time
elapsed_per_run_hour_mean_with_queue <- elapsed_per_run_hour_mean + mean(queue_hour, na.rm=T)
elapsed_per_run_hour_median_with_queue <- elapsed_per_run_hour_median + median(queue_hour, na.rm=T)
message("\nmean (median) elapsed per run including mean (median) queue time: ", round(elapsed_per_run_hour_mean_with_queue, 2), 
        " (", round(elapsed_per_run_hour_median_with_queue, 2), ") hours")
message("--> throughput per day including mean (median) queue time = 24 hours/day / ", round(elapsed_per_run_hour_mean_with_queue, 2), 
        " (", round(elapsed_per_run_hour_median_with_queue, 2), ") hours/run = ", 
        round(24/elapsed_per_run_hour_mean_with_queue, 2), " (", round(24/elapsed_per_run_hour_median_with_queue, 2), ") ~ ", 
        floor(24/elapsed_per_run_hour_mean_with_queue), " (", floor(24/elapsed_per_run_hour_median_with_queue), ") runs/day")
message(paste(paste0("--> ", facs, " runs need ", 
                     round(facs*elapsed_per_run_hour_mean_with_queue, 2), " (", round(facs*elapsed_per_run_hour_median_with_queue, 2), ") hours = ",
                     round(facs*elapsed_per_run_hour_mean_with_queue/24, 2), " (", round(facs*elapsed_per_run_hour_median_with_queue/24, 2), ") days = ",
                     round(facs*elapsed_per_run_hour_mean_with_queue/24/7, 2), " (", round(facs*elapsed_per_run_hour_median_with_queue/24/7, 2), ") weeks = ",
                     round(facs*elapsed_per_run_hour_mean_with_queue/24/30.5, 2), " (", round(facs*elapsed_per_run_hour_median_with_queue/24/30.5, 2), ") months"),
              collapse="\n"))

# print nnodes stats
message("\nmean nnodes per run: ", mean(nnodes, na.rm=T))

# print node hour stats
message("\ntotal node hours of all ", length(logs), " input runs: ", 
        round(sum(node_hours, na.rm=T)), " node hours")
node_hours_per_run <- mean(node_hours, na.rm=T)
message("mean node hours per run: ", round(node_hours_per_run), " node hours")
message(paste(paste0("--> ", facs, " runs need ", 
                     round(facs*node_hours_per_run), " node hours"), 
              collapse="\n"))

# print energy stats
energy_kWh_per_run <- mean(energy_kWh, na.rm=T)
message("\nenery assumption: power of 1 node = ", energy_transfer_node, " W (kg m2 s-3)")
message("--> energy consumption per run = ", 
        round(node_hours_per_run), " node hours per run * ", energy_transfer_node, " W per node = ", 
        energy_kWh_per_run, " kWh")
energy_kWh_total <- sum(energy_kWh, na.rm=T)
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
jobname <- logs[1]
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

if (F) { # scatter
    plot(nodes_unique, nodes_cumulative_hode_hours_mean, t="n",
         xlab="node", ylab="mean node hours", xaxt="n", yaxt="n")
    axis(1, pretty(nodes_unique, n=10))
    axis(2, pretty(nodes_cumulative_hode_hours_mean, n=20), las=2)
    mtext("blue/red ~ start/end of experiment", 3)
    cols <- rev(RColorBrewer::brewer.pal(10, "Spectral"))
    colvec <- seq(min(nodes_cumulative_jobids_mean), max(nodes_cumulative_jobids_mean), l=10)
    colvec <- findInterval(nodes_cumulative_jobids_mean, colvec, all.inside=T)
    points(nodes_unique, nodes_cumulative_hode_hours_mean, pch=16, col=cols[colvec])
}

options(oo) # restore value from before
message()

