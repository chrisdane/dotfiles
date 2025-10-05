#!/usr/bin/env Rscript

# filter /var/log/pacman.log for date and packages

if (interactive()) {
    me <- "mydowngrade.r"
    #args <- NULL
    args <- c("patterns=2025-10-05T15:, upgraded ")
} else { # if not interactive
    args <- commandArgs(trailingOnly=F) # internal and user args
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T) # user args only
    #print(args)

} # if interactive or not 

flog <- "/var/log/pacman.log"
if (!file.exists(flog)) stop("pacman log ", flog, " does not exist")
flog_tail <- system(paste0("\\grep \" upgraded \" ", flog, " | tail"), intern=T)

oo <- options() # save old/default options
help <- paste0("\nUsage:\n $ ", me, " [patterns=\"2025-10-05T15:2, upgraded ,pkgname\"] [fout=/path/to/cmd/file]\n",
               "\n",
               "`grep \" upgraded \" ", flog, " | tail`:\n",
               paste(flog_tail, collapse="\n"))

# stop if help
if (length(args) == 0) {
    message(help)
    if (interactive()) {
        stop("stop interactive call")
    } else {
        quit()
    }
}

# check
if (any(grepl("patterns", args))) {
    patterns <- sub("patterns=", "", args[grep("patterns=", args)]) # "2025-10-05T15:" "\" upgraded \""
    patterns <- strsplit(patterns, ",")[[1]] # "2025-10-05T15:" "\" upgraded \"" 
    message("--> provided `patterns` (n=", length(patterns), "):")
} else {
    patterns <- " upgraded " # default
    message("--> default `patterns`:")
}
print(patterns)

if (any(grepl("fout", args))) {
    fout <- sub("fout=", "", args[grep("fout=", args)])
    if (file.exists(fout)) {
        stop("provided `fout` = ", fout, " already exists")
    }
    dir.create(dirname(fout), recursive=T, showWarnings=F)
    if (!dir.exists(dirname(fout))) {
        stop("could not create dir ", dirname(fout))
    }
    message("\n--> provided `fout` = ", fout)
} else {
    fout <- NULL
}

# filter log
# e.g. `grep -P '(?=.*2025-10-05T15:)(?=.* upgraded )' /var/log/pacman.log`
cmd <- paste0("\\grep -P '", paste(paste0("(?=.*", patterns, ")"), collapse=""), "' ", flog)
message("run `", cmd, "` ...")
res <- system(cmd, intern=T)
message("--> found ", length(res), " lines with these patterns")
if (length(res) == 0) {
    if (interactive()) {
        stop("stop interactive call")
    } else {
        quit()
    }
} else { # found some lines
    print(res)

    # get previous versions of filter
    if (any(patterns == " upgraded ")) {
        message("\n`patterns` contains \" upgraded \" --> get previous versions ...")

        # e.g. "[2025-10-05T15:26:03+0200] [ALPM] upgraded openssl (3.5.3-1 -> 3.5.4-1)"
        inds1 <- regexpr(" upgraded ", res)
        if (length(inds1) == 0) stop("this should not happen")
        inds2 <- regexpr(" \\(", res)
        if (length(inds2) == 0) stop("this should not happen")
        inds3 <- regexpr(" -> ", res)
        if (length(inds3) == 0) stop("this should not happen")
        if (length(inds1) != length(inds2)) stop("this should not happen")
        if (length(inds1) != length(inds3)) stop("this should not happen")
        pkgs <- substr(res, inds1 + 10, inds2 - 1)
        versions_prev <- substr(res, inds2 + 2, inds3 - 1) # e.g. "3.5.3-1"

        # prepare downgrade cmd
        # e.g. `downgrade 'foo=1.0.0-1' 'bar>=1.2.1-1' 'baz=~^1.2'`
        cmd <- paste0("sudo downgrade ", paste(paste0("'", pkgs, "=", versions_prev, "'"), collapse=" "))
        message("\n-->\n", cmd)
        if (is.null(fout)) {
            message("\n`fout` not provided --> will not save cmd to file")
        } else {
            message("\n`fout` = ", fout, " --> save cmd to that file ...")
            write(cmd, file=fout) 
        }

    } # if (any(patterns == " upgraded ")) {

} # if found some lines based on patterns

message("\nfinished\n")

