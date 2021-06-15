#!/usr/bin/env Rscript

if (interactive()) {
    atmout <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/hist3/work/atmout"
    streamsout <- paste0(getwd(), "/streamsout")

} else {
    args <- commandArgs(trailingOnly=T)
    if (length(args) != 2) stop("Must provide 2 args: atmout and streamsout")
    atmout <- args[1]
    streamsout <- args[2]
    if (file.access(streamsout, mode=2) == -1) {
        stop("Your given streamsout=\"", streamsout, "\" is not writable")
    }
}

from <- " Initializing mvstreams"
to <- " Finished initialization in control"
exp <- " Experiment: "

fromind <- paste0("grep -n \"", from, "\" ", atmout)
fromind <- system(fromind, intern=T)
fromind <- as.integer(substr(fromind, 1, regexpr(":", fromind) - 1))
if (!is.finite(fromind)) {
    stop("fromind=", fromind)
} else {
    message("Found from line \"", from, "\"")
}

toind <- paste0("grep -n \"", to, "\" ", atmout)
toind <- system(toind, intern=T)
toind <- as.integer(substr(toind, 1, regexpr(":", toind) - 1))
if (!is.finite(toind)) {
    stop("toind=", toind)
} else {
    message("Found to line \"", to, "\"")
}

mvstreams <- paste0("sed -n \"", fromind, ",", toind, "p\" ", atmout)
mvstreams <- system(mvstreams, intern=T)

experiment <- mvstreams[grep(exp, mvstreams)]
if (length(experiment) != 0) {
    # e.g. " Experiment: hist3 -  data path: /work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/hist3/work//"
    message("Found experiment line \"", experiment, "\"")
} else {
    message("Did not find experiment line \"", experiment, "\"")
}

message("Save ", length(mvstreams), " lines to ", streamsout, " ...")
system(paste0("echo \"",  atmout, "\" > ", streamsout))
system(paste0("echo \"experiment: ", experiment, "\" >> ", streamsout))
system(paste0("echo >> ", streamsout))
write(mvstreams, streamsout, append=T)

message("finish")

