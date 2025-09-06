#!/usr/bin/env Rscript

if (interactive()) { # test
    rm(list=ls())
    args <- "~/machines/dkrz/proj/ba1103/slk_arch_ba1103_2024-10-04.txt" # `slk list -R`
} else {
    args <- commandArgs(trailingOnly=T)
}

if (length(args) != 1) {
    stop("\nUsage: slk2list.r <result of `slk list -R /path> result.txt`>\n")
}
slk_list_result <- args[1]

# read
message("read `slk list` result ", slk_list_result, " ...")
slk <- readLines(slk_list_result)

# e.g:
# /arch/ba1103:
# -rw-r------ stronglink  ba1103          1.2M   14 Oct 2021 11:02 _PROJECT.ba1103.file-list.GIGA
# drwxr-x---- a270094     ba1103             0   27 Aug 2021 12:06 a270094
# drwxrwxr-x- a270109     ab0995             0   03 Jan 2022 13:25 a270109
# Files: 3
#
# /arch/ba1103/a270094:
# drwxr-x---- a270094     ba1103             0   11 Jan 2022 13:15 SR_output
# drwxr-x---- a270094     ba1103             0   21 Oct 2022 14:09 awicm-1.0-recom
# drwxr-x---- a270094     ba1035             0   27 Oct 2023 09:56 awicm-1.0-recom-coccos
# drwxr-x---- a270094     ba1103             0   30 Aug 2021 05:47 hist
# drwxr-x---- a270094     ba1103             0   27 Aug 2021 12:01 test
# Files: 5
#
# ...

# check if `slk list ...` ended with an error
inds <- grep("error|warn", slk, ignore.case=T)
if (length(inds) > 0) {
    stop("\nthere were errors:\n*****************************************\n", 
         slk[inds], "\n*****************************************\n",
         "check ~/.slk/slk-cli.log")
}

inds <- which(slk == "")
message("work on ", length(inds), " `slk list` blocks ...")
li <- vector("list", l=length(inds))
for (i in seq_along(li)) {
    if (i == 1) {
        li[[i]] <- slk[1:(inds[i]-2)]
    } else {
        li[[i]] <- slk[(inds[i-1]+1):(inds[i]-2)]
    }
    path <- sub(":", "/", li[[i]][1])
    df <- li[[i]]
    df <- df[2:length(df)]
    if (length(df) == 1 && df == "") {
        li[[i]] <- NA
    } else {
        df <- unname(sapply(df, strsplit, "\\s+"))
        sizes <- sapply(df, "[[", 4)
        inds_size <- which(sizes != "0")
        if (length(inds_size) > 0) {
            fs <- sapply(df, "[[", 9)[inds_size]
            df <- data.frame(size=sizes[inds_size], file=paste0(path, fs))
            li[[i]] <- df
        } else {
            li[[i]] <- NA
        }
    }
} # for i

inds <- which(is.na(li))
if (length(inds) == length(li)) {
    message("found not a single 0-byte file in this `slk list` result")
} else {
    li <- li[-inds]
    df <- do.call("rbind", li)
    non_empty_paths <- unique(dirname(df$file))
    if (length(non_empty_paths) != length(li)) stop("this should not happen?")
    width <- max(nchar(df$size)) + max(nchar(df$file))
    for (i in seq_along(li)) {
        try(cat("\nnon-empty dir ", i, "/", length(li), ": ", non_empty_paths[i], "\n", sep=""), silent=T)
        try(print(li[[i]], row.names=T, width=width, right=F), silent=T)
    } # for i
}

