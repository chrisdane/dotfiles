#!/usr/bin/env Rscript

# get args
args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
default_logs <- c(# echam6
                  "echam6.log", "atmout", "echam.stderr", "echam.stdout", 
                  # oasis
                  "debug.", "debug.root.", "debug.notroot.", "debug_notroot.", "lucia.", "nout.0",
                  # oifs
                  "NODE.",
                  # xios
                  "xios_client_", "xios_server_")

help <- paste0("\nUsage:\n $ ", me, " [logfile1 ... logileN]\n",
               "  e.g. ", me, "\n",
               "   or  ", me, " ", paste(default_logs, collapse=" "), "\n")

if (any(grepl("--help", args))) {
    message(help)
    quit()
}

# run `grep -Ein "warn|err|severe|\!"` on every log
logs <- commandArgs(trailingOnly=T)
if (length(logs) == 0) {
    message("no log files given. use default") 
    logs <- default_logs
}
for (logi in seq_along(logs)) {
    log <- logs[logi]
    message("***********************************************\n",
            "log ", logi, "/", length(logs), ": \"", log, "\"", appendLF=F)
    fs <- list.files(pattern=log)
    if (length(fs) > 0) {
        message()
        for (fi in seq_along(fs)) {
            if (file.info(fs[fi])["size"] > 0) {
                cmd <- paste0("grep -Ein \"warn|err|fail|severe|abort|invalid|stop|abort|\\!|No such file or directory\" ", fs[fi])
                message("--> run `", cmd, "` ...")
                system(cmd)
            } # if file is larger than 0 bytes
        } # for fi
    } else {
        message(" --> does not exist")
    }
} # for logi

