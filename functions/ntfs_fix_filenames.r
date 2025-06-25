#!/usr/bin/env Rscript

# ntfs partitions do not support these characters in file names: <>:"/\|?*
# --> find all files containing these characters and replace them with "" (nothing)
# --> work on basenames since "/" is directory separator
# --> add additional characters ’—ñ
# --> add additional weird slash: ⁄
# --> add additional glob characters []{}
patterns <- list(list(patt="[<>\"/\\|?*’]", # [] are needed for regex
                      repl=""),
                 list(patt="\\[|\\{",
                      repl="("),
                 list(patt="\\]|\\}",
                      repl=")"),
                 list(patt=":",
                      repl="-"),
                 list(patt="—",
                      repl="-"),
                 list(patt="ñ",
                      repl="n"),
                 list(patt="⁄",
                      repl=" "))
fs <- base::list.files(".", recursive=T, full.names=T)
fsb <- basename(fs)
for (pati in seq_along(patterns)) {
    pattern <- patterns[[pati]]$patt
    replace <- patterns[[pati]]$repl
    message("**********************************************************\n",
            "pattern ", pati, ": find all files containing pattern ", pattern)
    inds <- which(regexpr(pattern, fsb) != -1)
    if (length(inds) == 0) {
        message("--> found zero files with this pattern")
    } else {
        message("--> found ", length(inds), " files with this pattern:")
        print(fs[inds])
        fs_new <- paste0(dirname(fs[inds]), "/", gsub(pattern, replace, fsb[inds]))
        message("--> replace this pattern with \"", replace, "\":")
        print(fs_new)
        message("--> rename files ...")
        invisible(file.rename(fs[inds], fs_new)) # will not work when no write permission
        
        # update all files based on already made replacements:
        fs <- base::list.files(".", recursive=T, full.names=T)
        fsb <- basename(fs)
    } # if any files were found
} # for pati

