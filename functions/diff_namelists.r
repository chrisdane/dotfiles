#!/usr/bin/env Rscript

if (!interactive()) {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    usage <- paste0("\nUsage:\n $ ", me, " namelist1 namelist2\n\n")

    # check
    if (length(args) != 2) {
        cat(usage)
        quit()
    }
} else {
    args <- c("/work/ba1103/a270094/AWIESM/test/work/namelist.recom", # old
              "/work/ba1103/a270073/out/awicm-1.0-recom/test/run_19500101-19500131/work/namelist.recom") # new
}

# check
if (!file.exists(args[1])) stop("file ", args[1], " does not exist")
if (!file.exists(args[2])) stop("file ", args[2], " does not exist")

# read input namelists line by line; ignore comments
f1 <- scan(args[1], what="char", sep="\n", quiet=T, comment.char="!") # allowEscapes=F or T makes no difference
f2 <- scan(args[2], what="char", sep="\n", quiet=T, comment.char="!")

# remove leading and trailing white spaces and regular expressions 
f1 <- trimws(f1)
f2 <- trimws(f2)

# remove lines which now equal ""
inds <- which(f1 == "")
if (length(inds) > 0) f1 <- f1[-inds]
inds <- which(f2 == "")
if (length(inds) > 0) f2 <- f2[-inds]

# remove lines which equal "/"
inds <- which(f1 == "/")
if (length(inds) > 0) f1 <- f1[-inds]
inds <- which(f2 == "/")
if (length(inds) > 0) f2 <- f2[-inds]

# remove lines which start with "&"
inds <- which(grepl("^&", f1))
if (length(inds) > 0) f1 <- f1[-inds]
inds <- which(grepl("^&", f2))
if (length(inds) > 0) f2 <- f2[-inds]

# remove lines which dont have a "=" in it
inds <- which(!grepl("=", f1))
if (length(inds) > 0) f1 <- f1[-inds]
inds <- which(!grepl("=", f2))
if (length(inds) > 0) f2 <- f2[-inds]

# lower all capitals
f1 <- tolower(f1)
f2 <- tolower(f2)

# split every line by "="
f1 <- strsplit(f1, "=") # must result in two entries per line
inds <- which(sapply(f1, length) != 2)
if (length(inds) > 0) f1 <- f1[-inds]
f2 <- strsplit(f2, "=") # must result in two entries per line
inds <- which(sapply(f2, length) != 2)
if (length(inds) > 0) f2 <- f2[-inds]

# remove leading and trailing white spaces and regular expressions in both entries per line
f1 <- lapply(f1, trimws)
f2 <- lapply(f2, trimws)

# list as data.frame
f1 <- data.frame(var=sapply(f1, "[", 1), val=sapply(f1, "[", 2), stringsAsFactors=F)
f2 <- data.frame(var=sapply(f2, "[", 1), val=sapply(f2, "[", 2), stringsAsFactors=F)

# check if there are any duplicates
if (any(duplicated(f1$var))) {
    inds <- which(duplicated(f1$var))
    stop("found duplicated entries in namelist 1 ", args[1], ":\n",
         paste(f1$var[inds], collapse=", "))
}
if (any(duplicated(f2$var))) {
    inds <- which(duplicated(f2$var))
    stop("found duplicated entries in namelist 2 ", args[2], ":\n",
         paste(f2$var[inds], collapse=", "))
}

# check all f1 variables if they also occur in f2 (case 1/3) or if they are unique to f1 (case 2/3)
f1_and_f2 <- f1_but_not_f2 <- f2_but_not_f1 <- data.frame()
for (i in seq_along(f1$var)) {

    var1 <- f1$var[i]
    if (any(f2$var == var1)) { # case 1: current variable is in both namelists
        val1 <- f1$val[i]
        val2 <- f2$val[which(f2$var == var1)]
        row <- data.frame(var=var1, val1=val1, val2=val2, identical=val1==val2, stringsAsFactors=F)
        f1_and_f2 <- rbind(f1_and_f2, row)
    
    } else { # current variable is in f1 but not in f2 (case 2/3)
        row <- data.frame(var1=var1, val1=val1, stringsAsFactors=F)
        f1_but_not_f2 <- rbind(f1_but_not_f2, row)

    } # if current variable is in both namelists or not

} # for i

# check all f2 variables if they they are unique to f2 (case 3/3)
f2_but_not_f1 <- data.frame()
for (i in seq_along(f2$var)) {

    var2 <- f2$var[i]
    if (!any(f1$var == var2)) { # current variable is in f2 but not in f1 (case 3/3)
        row <- data.frame(var2=var2, val2=f2$val[i], stringsAsFactors=F)
        f2_but_not_f1 <- rbind(f2_but_not_f1, row)
    } # if current variable is in f2 but not in f1

} # for i 

# print result
options(width=3000) # increase length per print line from default 80
if (length(f1_and_f2) > 0) {
    if (any(f1_and_f2$identical)) {
        inds <- which(f1_and_f2$identical)
        cat("\n\n", length(inds), " identical entries in f1 (", args[1], ") and in f2 (", args[2], "):\n\n", sep="")
        print(f1_and_f2[inds,], quote=F)
    }
    if (!all(f1_and_f2$identical)) {
        inds <- which(!f1_and_f2$identical)
        cat("\n\n", length(inds), " differing entries in f1 (", args[1], ") and in f2 (", args[2], "):\n\n", sep="")
        print(f1_and_f2[inds,], quote=F)
    }
} else {
    cat("\nno common entries in f1 (", args[1], ") and f2 (", args[2], ")\n", sep="")
}
if (length(f1_but_not_f2) > 0) {
    cat("\n\n", dim(f1_but_not_f2)[1], " entries in f1 (", args[1], ") but not in f2 (", args[2], "):\n\n", sep="")
    print(f1_but_not_f2, quote=F)
} else {
    cat("\nno entries unique to f1 (", args[1], ")\n\n", sep="")
}
if (length(f2_but_not_f1) > 0) {
    cat("\n\n", dim(f2_but_not_f1)[1], " entries in f2 (", args[2], ") but not in f1 (", args[1], "):\n\n", sep="")
    print(f2_but_not_f1, quote=F)
} else {
    cat("\nno entries unique to f2 (", args[2], ")\n\n", sep="")
}


