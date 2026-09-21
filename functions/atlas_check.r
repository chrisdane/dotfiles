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
    user <- args[1]
} else {
    message(help)
    quit()
}
# note: unlike slurm's `squeue -u`, atlas's `-u`/`--user` filter on `jobs list`
# is admin-only (see `atlas jobs list --help`); for a regular user it is
# silently ignored and the command always shows that user's own jobs.

options(width=3000) # increase length per print line from default 80

# `atlas` lives in the `tape` module, not on PATH by default, and `module
# load` only affects the subshell of a single `system()` call (it does not
# persist across separate calls within this R session), so it is prepended
# to every command below rather than run once up front.
module_load_tape <- "module load tape >/dev/null 2>&1 && "

# `atlas jobs list`'s CREATED column contains an embedded space
# ("2026-08-13 08:52"), which would make read.table() (as used for squeue in
# slurm_check.r) misparse it into two extra columns. parse the fixed-width
# table by hand instead: the first ncol-1 header names map 1:1 onto the first
# ncol-1 tokens of each row, and everything left over is folded back into the
# (possibly multi-token) last column.
parse_atlas_jobs <- function(lines) {
    if (length(lines) == 0 || grepl("^No jobs found", lines[1])) return(NULL)
    header <- strsplit(trimws(lines[1]), "\\s+")[[1]] # ID TYPE STATE USER NODE RESOURCE PROGRESS CREATED
    ncol_header <- length(header)
    rows <- lines[-1]
    if (length(rows) == 0) return(NULL)
    tab <- as.data.frame(matrix(NA, nrow=length(rows), ncol=ncol_header, dimnames=list(NULL, header)), stringsAsFactors=F)
    for (i in seq_along(rows)) {
        toks <- strsplit(trimws(rows[i]), "\\s+")[[1]]
        tab[i, seq_len(ncol_header - 1)] <- toks[seq_len(ncol_header - 1)]
        tab[i, ncol_header] <- paste(toks[ncol_header:length(toks)], collapse=" ")
    }
    return(tab)
}

# `atlas jobs list` has no elapsed-runtime column (unlike squeue's `%M`); for
# a completed job, get it via `atlas jobs get <id>`'s Started/Completed lines.
get_atlas_job_runtime <- function(jobid) {
    cmd <- paste0(module_load_tape, "atlas jobs get ", jobid)
    detail <- system(cmd, intern=T)
    started_line <- detail[grep("^Started:", detail)]
    completed_line <- detail[grep("^Completed:", detail)]
    if (length(started_line) != 1 || length(completed_line) != 1) return(NA)
    started <- as.POSIXct(trimws(sub("^Started:", "", started_line)))
    completed <- as.POSIXct(trimws(sub("^Completed:", "", completed_line)))
    return(format(completed - started))
}

# same idea but for a still-running job: no Completed line yet, so elapsed is
# measured against the current time instead.
get_atlas_job_elapsed <- function(jobid) {
    cmd <- paste0(module_load_tape, "atlas jobs get ", jobid)
    detail <- system(cmd, intern=T)
    started_line <- detail[grep("^Started:", detail)]
    if (length(started_line) != 1) return(NA)
    started <- as.POSIXct(trimws(sub("^Started:", "", started_line)))
    return(format(Sys.time() - started))
}

# step 1/2: check if there are non-running (pending/completed/failed/cancelled) jobs
cmd <- paste0(module_load_tape, "atlas jobs list -u ", user)
message("run `", cmd, "` ...")
lines <- system(cmd, intern=T)
jobs <- parse_atlas_jobs(lines)

if (is.null(jobs)) {
    message("--> there are no jobs at all")
} else {
    if (any(jobs$STATE != "running")) {
        inds <- which(jobs$STATE != "running")
        message("--> there are ", length(inds), " non-running jobs:\n")
        tab <- jobs[inds,]
        # for completed jobs, replace CREATED with RUNTIME = Completed - Started
        tab$RUNTIME <- NA
        for (ci in which(tab$STATE == "completed")) {
            tab$RUNTIME[ci] <- get_atlas_job_runtime(tab$ID[ci])
        }
        tab$CREATED[tab$STATE == "completed"] <- tab$RUNTIME[tab$STATE == "completed"]
        names(tab)[names(tab) == "CREATED"] <- "CREATED/RUNTIME"
        tab$RUNTIME <- NULL
        print(tab, row.names=F)
    } else {
        message("--> there are no non-running jobs")
    }

    # step 2/2: check if user has running jobs
    if (any(jobs$STATE == "running")) {
        inds <- which(jobs$STATE == "running")
        message("\n--> there are ", length(inds), " running jobs:")
        tab <- jobs[inds,]
        tab$ELAPSED <- sapply(tab$ID, get_atlas_job_elapsed)
        print(tab, row.names=F)

        # get details of running jobs
        jobids <- jobs$ID[inds]
        if (T) jobids <- rev(jobids) # show newest job last
        for (jobi in seq_along(jobids)) {
            message("\n****************** job ", jobi, "/", length(jobids), " ***********************")
            cmd <- paste0(module_load_tape, "atlas jobs get ", jobids[jobi])
            message("run `", cmd, "` ...")
            system(cmd)
        } # for jobi
    } else {
        message("--> there are no running jobs")
    }
} # if is.null(jobs)
