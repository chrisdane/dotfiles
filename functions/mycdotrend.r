#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
args <- commandArgs(trailingOnly=T)

#print(args)

# from R > 3.2
trimws <- function (x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
{
    which <- match.arg(which)
    mysub <- function(re, x) sub(re, "", x, perl = TRUE)
    switch(which, left = mysub(paste0("^", whitespace, "+"),
        x), right = mysub(paste0(whitespace, "+$"), x), both = mysub(paste0(whitespace,
        "+$"), mysub(paste0("^", whitespace, "+"), x)))
}

# check
usage <- paste0("\nUsage:\n $ ", me, " --fin=<fin> [--dry] [--outdir=<outdir>] [--varname=<varname>]\n")
if (length(args) == 0) {
    message(usage)
    quit()
}

# check fin
if (!any(grepl("--fin", args))) {
    stop("must provide --fin=<fin> argument", usage)
} else {
    fin <- sub("--fin=", "", args[grep("--fin=", args)])
    message("fin = ", fin)
    if (!file.exists(fin)) stop("fin = \"", fin, "\" does not exist")
}

# check varname
if (!any(grepl("--varname", args))) {
    cmd <- paste0("cdo -s showname ", fin)
    message("varname not provided. run `", cmd, "` ... ", appendLF=F)
    varname <- trimws(system(cmd, intern=T))
    message("\"", varname, "\"")
} else {
    varname <- sub("--varname=", "", args[grep("--varname=", args)])
}
varnameout <- paste0("lm_", varname, "_as_time_slope")
message("--> varnameout = \"", varnameout, "\"")

# check outpath
if (!any(grepl("--outdir", args))) { # outdir not provided
    outdir <- dirname(fin)
    message("outdir not provided. use `dirname(fin)` = \"", outdir, "\"")
} else { # outdir provided
    outdir <- sub("--outdir=", "", args[grep("--outdir=", args)])
}
if (file.access(outdir, mode=0) == -1) { # not existing
    message("outdir = \"", outdir, "\" does not exist. try to create ... ", appendLF=F)
    dir.create(outdir, recursive=T)
    if (!file.exists(outdir)) {
        stop("not successful. error msg:")
    } else {
        message("success")
    }
} else { # outdir exists
    if (file.access(outdir, mode=2) == -1) { # not writable
        stop("provided outdir = \"", outdir, "\" not writeable.")
    }
}
outdir <- normalizePath(outdir)
message("--> outdir = \"", outdir, "\"")

# check dry
dry <- F
if (any(args == "--dry")) {
    message("argument `--dry` provided --> dry run")
    dry <- T
}

# check if input is monthly or not
monthly <- F
if (grepl("Jan-Dec", fin)) monthly <- T

# run annual and seasonal means if monthly before trend
fin_all <- c()
if (monthly) { # if input is monthly
    message("string \"Jan-Dec\" detected in fin --> calc annual and seasonal means ...")

    # calc annual mean
    fout <- sub("Jan-Dec", "annual", fin)
    if (file.exists(fout)) {
        message("file \"", fout, "\" already exists. skip cdo yearmean")
    } else {
        cmd <- paste0("cdo yearmean ", fin, " ", fout)
        if (file.access(dirname(fin), mode=2) == -1) {
            stop("input is monthly but cannot run\n`", cmd, "` since output dir is not writable.")
        }
        message("run `", cmd, "` ...")
        if (!dry) system(cmd)
    }
    fin_all <- c(fin_all, fout)

    # calc seasonal means and select seasons
    fout <- sub("Jan-Dec", "seasmean", fin)
    if (file.exists(fout)) {
        message("file \"", fout, "\" already exists. skip cdo seasmean")
    } else {
        cmd <- paste0("cdo seasmean ", fin, " ", fout)
        message("run `", cmd, "` ...")
        if (!dry) system(cmd)
    }
    seasons <- c("DJF", "MAM", "JJA", "SON")
    for (si in seq_along(seasons)) {
        fout_si <- sub("seasmean", paste0(seasons[si], "mean"), fout)
        if (file.exists(fout_si)) {
            message("file \"", fout_si, "\" already exists. skip cdo selseas,", seasons[si])
        } else {
            cmd <- paste0("cdo -selseas,", seasons[si], " ", fout, " ", fout_si)
            message("run `", cmd, "` ...")
            if (!dry) system(cmd)
        }
        fin_all <- c(fin_all, fout_si)
    } # for si

} else { # if input is not monthly
    fin_all <- c(fin_all, fin)

} # if monthly
    
# calc trend for all files
fout_all <- rep(NA, t=length(fin_all))
for (fi in seq_along(fin_all)) {

    message("***************** file ", fi, "/", length(fin_all), " ********************")

    # cdo trend
    fout_intercept <- sub(varname, paste0("lm_", varname, "_as_time_intercept"), fin_all[fi])
    fout_slope <- sub(varname, varnameout, fin_all[fi])
    fout_all[fi] <- fout_slope
    cmd <- paste0("cdo trend ", fin_all[fi], " ", 
                  outdir, "/", fout_intercept, " ", 
                  outdir, "/", fout_slope) 
    message("run `", cmd, "` ...")
    if (!dry) system(cmd)

    # change name and multiply
    cmd <- paste0("cdo nyear ", fin_all[fi])
    message("run `", cmd, "` ...")
    if (!dry) {
        nyears <- trimws(system(cmd, intern=T))
    } else {
        nyears <- "<cdo nyear>"
    }
    cmd <- paste0("cdo -setname,", varnameout, " -mulc,", nyears, " ",
                  outdir, "/", fout_slope, " ", outdir, "/tmp && mv ", outdir, "/tmp ",
                  outdir, "/", fout_slope)
    message("run `", cmd, "` ...")
    if (!dry) system(cmd)
    
    # change units 
    cmd <- paste0("cdo showunit ", fin)
    unit <- trimws(system(cmd, intern=T))
    cmd <- paste0("ncatted -O -a units,", varnameout, ",o,c,\"", unit, "/", nyears, " years\" ", outdir, "/", fout_slope)
    message("run `", cmd, "` ...")
    if (!dry) system(cmd)

    # change long name
    if (F) { # command for getting longname?
        cmd <- ""
        longname <- system(cmd, intern=T)
        cmd <- paste0("ncatted -O -a long_name,", varnameout, ",o,c,\"", longname, " trend\" ", outdir, "/", fout_slope) 
        message("run `", cmd, "` ...")
        if (!dry) system(cmd)
    }

} # for fi in fin_all

# transfer to other server if wanted
if (grepl("stan", Sys.info()["nodename"])) {
    message("##########################")
    outdir_paleosrv <- paste0("/isibhv/projects/paleo_work/cdanek/post", 
                              substr(outdir, regexpr("post/", outdir)+4, nchar(outdir)))
    cmd <- paste0("scp ", paste(paste0(outdir, "/", fout_all), collapse=" "), 
                  " cdanek@paleosrv1.awi.de:", outdir_paleosrv, "/.")
    message("run `", cmd, "` ...")
    if (!dry) system(cmd)
} # if on stan

