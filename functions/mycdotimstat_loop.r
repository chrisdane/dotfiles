#!/usr/bin/env Rscript

if (interactive()) {
    me <- "mycdotimstat_loop.r"
    args <- c("yearmean", "Jan-Dec", "annual", "/work/ab1095/a270073/post/echam6/fldmean/temp2/*Jan-Dec*")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage:\n $ ", me, " timstat_e.g.yearmean pattern_in pattern_out fin [fin2 ...]\n")

# check
if (length(args) < 4) {
    message(usage)
    quit()
}

cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")

# check
timstat <- args[1]
pattern_in <- args[2]
pattern_out <- args[3]
fins <- args[4:length(args)]

for (fi in seq_along(fins)) {
    fin <- fins[fi]
    fout <- basename(fin)
    fout <- gsub(pattern_in, pattern_out, fout)
    fout <- paste0(dirname(fin), "/", fout)
    cmd <- paste0(cdo, " ", timstat, " ", fin, " ", fout)
    message("file ", fi, "/", length(fins), ": run `", cmd, "` ...")
    if (file.exists(fout)) {
        message("fout already exists. skip")
    } else {
        if (!file.exists(fin)) {
            message("fin does not exist. skip")
        } else {
            check <- system(cmd)
            if (check != 0) message("error: some error")
        }
    }
} # for fi

