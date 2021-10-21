#!/usr/bin/env Rscript

# run `finger`, if available, for a list of users given by `from` (and `to`) 

# get args
if (interactive()) {
    me <- "myfinger.r"
    #args <- c("a270073", "a270100")
    args <- c("a270000", "a279999")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    help <- paste0("\nUsage:\n $ ", me, " from [to]\n",
                    "   e.g. ", me, " a270000 # the usual `finger` command\n",
                    "        ", me, " a270000 a270099 # finger users from a270000 to a270099\n")
    args <- commandArgs(trailingOnly=T)
}

# check args
if (length(args) == 0 || length(args) > 2) {
    message(help)
    quit()
}
# check `finger` availability
finger <- Sys.which("finger")
if (finger == "") stop("program `finger` not found")

if (length(args) == 2) {
    if (args[1] == args[2]) args <- args[1]
}

## start

# if 1 arg
if (length(args) == 1) {
    fromto <- args[1]

# if 2 args: build user list from `from` to `to`
} else if (length(args) == 2) {
    from_char <- args[1]
    to_char <- args[2]
    if (nchar(from_char) != nchar(to_char)) {
        stop("number of characters of from = ", from_char, " (", nchar(from_char), 
             ") and to = ", to_char, " (", nchar(to_char), ") must be the same.")
    }
    nchar_fromto <- nchar(from_char)
    fromto <- array("", c(1, nchar_fromto)) # default: no numbers given --> only 1 entry
    from_char_vec <- strsplit(from_char, "")[[1]] # "a" "2" "7" "0" "0" "7" "3"
    to_char_vec <- strsplit(to_char, "")[[1]]

    # get numeric values from provided from/to
    from_int_vec <- suppressWarnings(as.integer(from_char_vec)) # NA  2  7  0  0  7  3
    to_int_vec <- suppressWarnings(as.integer(to_char_vec))
    if (any(!is.na(from_int_vec)) && any(!is.na(to_int_vec))) { # numbers in wanted pattern
        from_int_inds <- which(!is.na(from_int_vec))
        to_int_inds <- which(!is.na(to_int_vec))
        if (length(from_int_inds) != length(to_int_inds)) {
            stop(length(from_int_inds), " integer", ifelse(length(from_int_inds) > 1, "s", ""), 
                 " in from = ", from_char, " but ",
                 length(to_int_inds), " integer", ifelse(length(to_int_inds) > 1, "s", ""), 
                 " in to = ", to_char, ". need to equal")
        }
        nchar_int <- length(from_int_inds)
        if (!(all(from_int_inds == to_int_inds))) {
            stop(length(from_int_inds), " integer", ifelse(length(from_int_inds) > 1, "s", ""), 
                 " in from = ", from_char, " at position", ifelse(length(from_int_inds) > 1, "s", ""), " ",
                 paste(from_int_inds, collapse=","), " but ", length(to_int_inds), " integer",
                 ifelse(length(to_int_inds) > 1, "s", ""), " in to = ",
                 to_char, " at position", ifelse(length(to_int_inds) > 1, "s", ""), " ", 
                 paste(to_int_inds, collapse=","), ". need to be the same positions")
        }
        int_inds <- from_int_inds
        from_int <- as.integer(paste(from_int_vec[int_inds], collapse=""))
        to_int <- as.integer(paste(to_int_vec[int_inds], collapse=""))
        if (from_int >= to_int) {
            stop("from = ", from_int, " >= to = ", to_int, ". must be <.")
        }
        fromto_int <- from_int:to_int
        fromto_int_char <- sprintf(paste0("%0", nchar_int, "i"), fromto_int)
        fromto <- array("", c(length(fromto_int), nchar_fromto))
        fromto[,int_inds] <- t(sapply(strsplit(fromto_int_char, ""), "["))
    }

    # get character values from provided from/to
    if (any(is.na(from_int_vec)) && any(is.na(to_int_vec))) { # characters in wanted pattern
        from_char_inds <- which(is.na(from_int_vec))
        to_char_inds <- which(is.na(to_int_vec))
        if (length(from_char_inds) != length(to_char_inds)) {
            stop(length(from_char_inds), " character", ifelse(length(from_char_inds) > 1, "s", ""),
                 " in from = ", from_char, " but ",
                 length(to_char_inds), " character", ifelse(length(to_char_inds) > 1, "s", ""), 
                 " in to = ", to_char, ". must be equal")
        }
        if (!(all(from_char_inds == to_char_inds))) {
            stop("detected ", length(from_char_inds), " character", ifelse(length(from_char_inds) > 1, "s", ""), 
                 " in from = ", from_char, " at position", ifelse(length(from_char_inds) > 1, "s", ""), " ",
                 paste(from_char_inds, collapse=","), " but ", length(to_char_inds), " character",
                 ifelse(length(to_char_inds) > 1, "s", ""), " in to = ",
                 to_char, " at position", ifelse(length(to_char_inds) > 1, "s", ""), " ", 
                 paste(to_char_inds, collapse=","), ". need to be the same positions")
        }
        char_inds <- from_char_inds
        if (!(all(from_char_vec[char_inds] == to_char_vec[char_inds]))) {
            stop("character", ifelse(length(char_inds) > 1, "s", ""), 
                 " in from = ", from_char, ": ", paste(from_char_vec[char_inds], collapse=","), 
                 " but character", ifelse(length(char_inds) > 1, "s", ""), " in to = ",
                 to_char, ": ", paste(to_char_vec[char_inds], collapse=","), ". need to be the same")
        }
        fromto_char_vec <- from_char_vec[char_inds]
        fromto[,char_inds] <- fromto_char_vec
    }
    fromto <- apply(fromto, 1, paste, collapse="")
} # if 2 args

# run finger for all ids
# --> run finger with thousands of ids in once call is too slow --> use loop
for (i in seq_along(fromto)) {
    cmd <- paste0("finger ", fromto[i])
    #message(cmd)
    val <- system(cmd, intern=T, ignore.stderr=T)
    if (length(val) != 0) { # user known
        cat(val[1], "\n")
        # [1] "Login: a270073        \t\t\tName: Christopher Danek"       
        # [2] "Directory: /pf/a/a270073            \tShell: /bin/bash"    
        # [3] "On since Wed Jun  2 16:04 (CEST) on pts/98 from 134.1.1.80"
        # [4] "No mail."                                                  
        # [5] "No Plan."     
    } else {
        #message("finger: ", fromto[i], ": no such user.")
    }
} # for

