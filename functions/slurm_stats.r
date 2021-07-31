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

# internal and user args
args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
help <- paste0("\nUsage:\n $ ", me, " [--exclude=1,2,3] [logfile1 [logfile2 ... logileN]]\n",
               "   e.g. ", me, "\n", 
               "        ", me, " *compute_*.log\n",
               "        ", me, " *compute_????????.log\n")

# user args only
args <- commandArgs(trailingOnly=T)

# check for exclude
exclude <- NULL
if (any(grepl("--exclude", args))) {
    exclude <- sub("--exclude=", "", args[grep("--exclude=", args)])
    exclude <- strsplit(exclude, ",")[[1]]
    exclude <- as.integer(exclude) # error if not successful
    message("provided `exclude` of length ", length(exclude), " = ", 
            paste(exclude, collapse=", "))
    if (length(args) > 1) { # exclude and logs were provided
        logs <- args[(grep("--exclude=", args)+1):length(args)] # everything after --exclude
    } else { # only `exclude` was provided
        logs <- c() # length 0 
    }
} else {
    logs <- args
}

#message(str(logs))

# check logfile(s)
if (length(logs) == 0) { # no log files provided
    message("\nno logfiles provided")
    #logs <- list.files(pattern=glob2rx("*.log")) # use find to exclude links
    cmd <- paste0("find . -maxdepth 1 -type f -name \"*compute*.log\" -printf \"%f\\n\" | sort")
    message("--> run `", cmd, "` ...")
    logs <- system(cmd, intern=T)
    message("--> found ", length(logs), " logfiles")
} else {
    message("provided ", length(logs), " logfiles")
}
if (length(logs) == 0) { # no log files found
    message(help)
    quit()
}

if (any(!file.exists(logs))) {
    inds <- which(!file.exists(logs))
    stop(length(inds), " provided logfile", ifelse(length(inds) > 1, "s", ""), " ",
         ifelse(length(inds) > 1, "do", "does"), " not exist:\n",
         paste(logs[inds], collapse="\n"))
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

# for all logfiles
df <- data.frame()
message("\ncheck ", length(logs), " log", ifelse(length(logs)> 1, "s", ""), 
        " for pattern \"* StepID | JobName      NodeHours\" ...\n")
elapsed_difftime <- rep(NA, t=length(logs))
for (logi in seq_along(logs)) {

    # defaults  
    elapsed <- nnodes <- node_hours <- energy_kWh <- NA

    # check current log
    grep_pattern <- "* Elapsed time     : "
    cmd <- paste0("grep \"* StepID | JobName      NodeHours\" ", logs[logi])
    check <- suppressWarnings(system(cmd, intern=T))
    if (length(check) == 0) { # pattern not in logfile
        if (T) message("logfile ", logi, "/", length(logs), " ", logs[logi], 
                       " does not contain this pattern. skip")

    } else { # pattern in logfile
        
        # get elasped time
        cmd <- paste0("grep \"", grep_pattern, "\" ", logs[logi])
        elapsed <- suppressWarnings(system(cmd, intern=T))
        if (length(elapsed) == 0) {
            warning("`", cmd, "` results nothing")
        } else {
            elapsed <- strsplit(elapsed, " ")[[1]][9] # e.g. "03:12:15"
            elapsed_difftime[logi] <- as.difftime(elapsed, units="hours")
        }

        # get number of nodes
        grep_pattern <- "* Nodelist         : "
        cmd <- paste0("grep \"", grep_pattern, "\" ", logs[logi])
        nnodes <- suppressWarnings(system(cmd, intern=T)) # "* Nodelist         : m[10144-10161,10234-10251] (36)                           "
        if (length(nnodes) == 0) {
            warning("`", cmd, "` results nothing")
        } else {
            nnodes <- strsplit(nnodes, " ")[[1]]
            open_ind <- which(grepl("\\(", nnodes))
            close_ind <- which(grepl("\\)", nnodes))
            if (length(open_ind) == 1 && length(close_ind) == 1) {
                if (open_ind == close_ind) {
                    nnodes <- nnodes[open_ind]
                    nnodes <- gsub("[[:punct:]]", "", nnodes)
                    nnodes <- as.integer(nnodes)
                } else {
                    warning("found \"(\" and \")\" patterns in \"", paste(nnodes, collapse=" "), 
                            "\" but they do not belong together") 
                }
            } else {
                warning("did not find \"(\" and \")\" patterns in \"", 
                        paste(nnodes, collapse=" "), "\"") 
            }
        }

        # get node hours and energy
        grep_pattern <- "* batch  | batch"
        cmd <- paste0("grep \"", grep_pattern, "\" ", logs[logi])
        node_hours <- suppressWarnings(system(cmd, intern=T)) # "* batch  | batch             41.0"
        if (length(node_hours) == 0) {
            warning("`", cmd, "` results nothing")
        } else {
            node_hours <- strsplit(node_hours, "\\s+")[[1]] # "*"     "batch" "|"     "batch" "41.0
            node_hours <- suppressWarnings(as.numeric(node_hours)) # NA NA NA NA 41 
            if (length(which(!is.na(node_hours))) != 1) stop("this never happened")
            node_hours <- node_hours[which(!is.na(node_hours))]
            energy_kWh <- energy_transfer_node * node_hours / 1e3 # W * h / 1e3 = kWh
        }
    } # current log is ok or not
    
    # save as df
    row <- data.frame(logfile=logs[logi],
                      elapsed=elapsed,
                      nnodes=nnodes,
                      node_hours=node_hours,
                      energy_kWh=energy_kWh, 
                      stringsAsFactors=F)
    df <- rbind(df, row)

} # for logi

# add stats
if (length(df) > 0) {
    # add mean
    elapsed_per_run <- mean(elapsed_difftime, na.rm=T)
    nnodes_per_run <- table(df$nnodes)
    node_hours_per_run <- mean(df$node_hours, na.rm=T)
    energy_kWh_per_run <- mean(df$energy_kWh, na.rm=T)
    row <- data.frame(logfile="mean", 
                      elapsed=round(elapsed_per_run, 2),
                      nnodes=NA,
                      node_hours=round(node_hours_per_run), 
                      energy_kWh=energy_kWh_per_run,
                      stringsAsFactors=F)
    df <- rbind(df, row)
    # add sum
    elapsed_total <- sum(elapsed_difftime, na.rm=T)
    node_hours_total <- sum(df$node_hours, na.rm=T)
    energy_kWh_total <- sum(df$energy_kWh, na.rm=T)
    row <- data.frame(logfile="sum", 
                      elapsed=round(elapsed_total, 2),
                      nnodes=NA,
                      node_hours=node_hours_total, 
                      energy_kWh=energy_kWh_total, 
                      stringsAsFactors=F)
    df <- rbind(df, row)
    
    # print result
    options(width=3000)
    message()
    print(df)

    # print elapsed stats
    if (!is.na(elapsed_per_run)) {
        message("\nelapsed per run: ", round(elapsed_per_run, 2), " hours")
        message("--> throughput per day = 24 hours/day / ", round(elapsed_per_run, 2), " hours/run = ", 
                round(24/elapsed_per_run, 2), " ~ ", 
                floor(24/elapsed_per_run), " runs/day")
        facs <- c(10, 100, 150, 165, seq(200, 1000, b=100))
        message(paste(paste0("--> ", facs, " runs need ", 
                             round(facs*elapsed_per_run, 2), " hours = ",
                             round(facs*elapsed_per_run/24, 2), " days = ",
                             round(facs*elapsed_per_run/24/7, 2), " weeks = ",
                             round(facs*elapsed_per_run/24/30.5, 2), " months"),
                      collapse="\n"))
    }
    
    # print nnodes stats
    message("\nnnodes per run:")
    print(nnodes_per_run)

    # print node hour stats
    if (!is.na(node_hours_per_run)) {
        message("\nnode hours per run: ", round(node_hours_per_run), " node hours")
        facs <- c(10, 100, 150, 165, seq(200, 1000, b=100))
        message(paste(paste0("--> ", facs, " runs need ", 
                             round(facs*node_hours_per_run), " node hours"), 
                      collapse="\n"))
    }

    # print energy stats
    if (!is.na(energy_kWh_per_run)) { 
        message("\nassumption: power of 1 node = ", energy_transfer_node, " W (kg m2 s-3)")
        message("--> energy consumption per run = ", round(node_hours_per_run), " node hours * ", 
                energy_transfer_node, " W = ", 
                round(node_hours_per_run*energy_transfer_node), " Wh = ",
                energy_kWh_per_run, " kWh")
        message("--> total energy consumption = ", energy_kWh_total, " kWh")
        message("for comparison:")
        for (i in seq_along(usage)) {
            message(usage[[i]]$what, ": ", appendLF=F)
            for (j in seq_along(usage[[i]]$units)) {
                if (j > 1) message(rep(" ", t=nchar(usage[[i]]$what)), appendLF=F)
                message(usage[[i]]$units[[j]], " ", names(usage[[i]]$units)[j])
            }
        }
    }

} else {
    if (length(logs) > 1) {
        message("pattern was found in none of the provided logfiles")
    }
}

