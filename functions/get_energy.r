#!/usr/bin/env Rscript

# get energy of slurm job in J (= kg m2 s-2):
#   energy = energy_transfer_node [W] * n_node [#] * runtime [s]
# with W = kg m2 s-3 based on lines
#   * StepID | JobName      NodeHours    MaxRSS [Byte] (@task)   ReqCPUFreq [Hz]
#   * batch  | batch             46.0
# from slurm logfiles (string "* StepID | JobName      NodeHours" in logfile is used for node hours detection")

# assumption: average energy transfer (or power) of node = 350 W
energy_transfer_node <- 350 # W

# for comparison
usage <- list()
usage[[length(usage)+1]] <- list(what="1 person household per year", units=list(kWh_year=2300))
usage[[length(usage)+1]] <- list(what="2 person household per year", units=list(kWh_year=3500))

#######################################################

# get args
args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
help <- paste0("\nUsage:\n $ ", me, " logfile1 [logfile2 ... logileN]\n",
                "   e.g. ", me, " *compute_*.log\n")

# check logfile(s)
logs <- commandArgs(trailingOnly=T)
if (length(logs) == 0) {
    message(help)
    quit()
}
if (any(!file.exists(logs))) {
    inds <- which(!file.exists(logs))
    stop(length(inds), " provided logfile", ifelse(length(inds) > 1, "s", ""), " ",
         ifelse(length(inds) > 1, "do", "does"), " not exist:\n",
         paste(logs[inds], collapse="\n"))
}

# for all logfiles
df <- data.frame()
message("check ", length(logs), " logs for pattern \"* StepID | JobName      NodeHours\" ...")
for (logi in seq_along(logs)) {

    cmd <- paste0("grep \"* StepID | JobName      NodeHours\" ", logs[logi])
    check <- suppressWarnings(system(cmd, intern=T))
    if (length(check) == 0) { # pattern not in logfile
        if (T) message("logfile ", logi, "/", length(logs), " ", logs[logi], 
                       " does not contain this pattern. skip")
    } else { # pattern in logfile
        # get node hours
        cmd <- paste0("grep \"* batch  | batch\" ", logs[logi])
        node_hours <- suppressWarnings(system(cmd, intern=T)) # "* batch  | batch             41.0"
        if (length(check) == 0) stop("this never happened")
        node_hours <- strsplit(node_hours, "\\s+")[[1]] # "*"     "batch" "|"     "batch" "41.0
        node_hours <- suppressWarnings(as.numeric(node_hours)) # NA NA NA NA 41 
        if (length(which(!is.na(node_hours))) != 1) stop("this never happened")
        node_hours <- node_hours[which(!is.na(node_hours))]
        energy_kWh <- energy_transfer_node * node_hours / 1e3 # W * h / 1e3 = kWh
        row <- data.frame(logfile=logs[logi], 
                          node_hours=node_hours,
                          energy_kWh=energy_kWh, 
                          energy_mWh=energy_kWh/1e3,
                          stringsAsFactors=F)
        df <- rbind(df, row)
    }

} # for logi

if (length(df) > 0) {
    row <- data.frame(logfile="sum", 
                      node_hours=sum(df$node_hours), 
                      energy_kWh=sum(df$energy_kWh), 
                      energy_mWh=sum(df$energy_kWh)/1e3, 
                      stringsAsFactors=F)
    df <- rbind(df, row)
    print(df)
    message("assumption: power of 1 node = ", energy_transfer_node, " W")
    message("for comparison:")
    for (i in seq_along(usage)) {
        message(usage[[i]]$what, ": ", appendLF=F)
        for (j in seq_along(usage[[i]]$units)) {
            if (j > 1) message(rep(" ", t=nchar(usage[[i]]$what)), appendLF=F)
            message(usage[[i]]$units[[j]], " ", names(usage[[i]]$units)[j])
        }
    }
} else {
    if (length(logs) > 1) {
        message("pattern was found in none of the provided logfiles")
    }
}

