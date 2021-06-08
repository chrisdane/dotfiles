#!/usr/bin/env Rscript

# get args
args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
default_logs <- c("atmout", "echam.stderr", "echam.stdout", 
                  "debug.root.01", "debug.root.02",
                  "debug_notroot.01", "debug_notroot.02",
                  "debug.01.000000", "debug.02.000000",
                  "nout.000000")
help <- paste0("\nUsage:\n $ ", me, " [logfile1 ... logileN]\n",
               "  e.g. ", me, "\n",
               "   or  ", me, " ", paste(default_logs, collapse=" "), "\n")

if (any(grepl("--help", args))) {
    message(help)
    quit()
}

# run `grep -Ein "warn|err|\!"` on every log
logs <- commandArgs(trailingOnly=T)
if (length(logs) == 0) {
    message("no log files given. use default") 
    logs <- default_logs
}
for (logi in seq_along(logs)) {
    log <- logs[logi]
    message("***********************************************\n",
            "log ", logi, "/", length(logs), ": ", log, appendLF=F)
    if (file.exists(log)) {
        cmd <- paste0("grep -Ein \"warn|err|\\!\" ", logs[logi])
        message(" --> run `", cmd, "` ...")
        system(cmd)
    } else {
        message(" --> does not exist")
    }
} # for logi

