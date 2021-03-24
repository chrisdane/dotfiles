#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
args <- commandArgs(trailingOnly=T)

usage <- paste0("\nUsage:\n $ ", me, " file_list1.txt file_list2.txt\n")

# check
if (length(args) != 2) {
    cat(usage)
    quit()
}

f1 <- readLines(args[1])
f2 <- readLines(args[2])

f1_and_f2 <- intersect(f1, f2)
f1_not_f2 <- f1[which(is.na(match(f1, f2)))]
f2_not_f1 <- f2[which(is.na(match(f2, f1)))]
cat("\n", length(f1_and_f2), " files are in f1 (", 
    args[1], ") and in f2 (", args[2], ")\n\n", sep="")
if (length(f1_and_f2) > 0) cat(paste(f1_and_f2, collapse="\n"), "\n", sep="")
cat("\n", length(f1_not_f2), " files are in f1 (", 
    args[1], ") but not in f2 (", args[2], ")\n\n", sep="")
if (length(f1_not_f2) > 0) cat(paste(f1_not_f2, collapse="\n"), "\n", sep="")
cat("\n", length(f2_not_f1), " files are in f2 (", args[2], 
    ") but not in f1 (", args[1], ")\n\n", sep="")
if (length(f2_not_f1) > 0) cat(paste(f2_not_f1, collapse="\n"), "\n", sep="")

