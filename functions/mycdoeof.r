#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
args <- commandArgs(trailingOnly=T)

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
usage <- paste0("\nUsage:\n $ ", me, " --anom_file=<anom_file> --outdir=`dirname(anom_file)` --dry --neof=3 --method=eof --cdo_weight_mode=off --max_jacobi_iter=100 --P=`min($nproc, 4)`\n")
if (length(args) == 0) {
    message(usage)
    quit()
}

# check anom_file
if (!any(grepl("--anom_file", args))) {
    stop("must provide --anom_file=<anom_file> argument", usage)
} else {
    anom_file <- sub("--anom_file=", "", args[grep("--anom_file=", args)])
    message("anom_file = ", anom_file)
    if (!file.exists(anom_file)) stop("anom_file = \"", anom_file, "\" does not exist")
    indir <- normalizePath(dirname(anom_file))
}
# check if input is monthly or not
monthly <- F
if (grepl("Jan-Dec", anom_file)) monthly <- T

# check method
if (any(grepl("--method", args))) {
    method <- sub("--method=", "", args[grep("--method=", args)])
    method <- as.character(method)
    if (!any(method == c("eof", "eoftime", "eofspatial"))) {
        stop("provided method = ", method, 
             " must be either \"eof\" (default), \"eoftime\" or \"eofspatial\"")
    } else {
        message("provided method = ", method)
    }
} else {
    method <- "eof"
    message("--method not provided. use default ", method) 
}

# check cdo_weight_mode
if (any(grepl("--cdo_weight_mode", args))) {
    cdo_weight_mode <- sub("--cdo_weight_mode=", "", args[grep("--cdo_weight_mode=", args)])
    cdo_weight_mode <- as.character(cdo_weight_mode)
    if (cdo_weight_mode != "on" && cdo_weight_mode != "off") {
        stop("provided cdo_weight_mode = ", cdo_weight_mode, 
             " must be either \"off\" (recommended) or \"on\"")
    } else {
        message("provided cdo_weight_mode = ", cdo_weight_mode)
    }
} else {
    cdo_weight_mode <- "off"
    message("--cdo_weight_mode not provided. use default ", cdo_weight_mode) 
}

# check max_jacobi_iter
if (any(grepl("--max_jacobi_iter", args))) {
    max_jacobi_iter <- sub("--max_jacobi_iter=", "", args[grep("--max_jacobi_iter=", args)])
    max_jacobi_iter <- as.numeric(max_jacobi_iter)
    message("provided max_jacobi_iter = ", max_jacobi_iter)
} else {
    max_jacobi_iter <- 100
    message("--max_jacobi_iter not provided. use default ", max_jacobi_iter) 
}

# check P
if (any(grepl("--P", args))) {
    nparallel <- sub("--P=", "", args[grep("--P=", args)])
    nparallel<- as.numeric(nparallel)
    message("provided P = ", nparallel)
} else {
    nparallel <- 4
    message("--P not provided. use default ", nparallel) 
}

# check neof
if (any(grepl("--neof", args))) {
    neof <- sub("--neof=", "", args[grep("--neof=", args)])
    if (!is.numeric(neof)) stop("provided neof=", neof, " is not numeric")
} else {
    neof <- 3
    message("--neof not provided. use default ", neof) 
}

# check dry
dry <- F
if (any(args == "--dry")) {
    message("--dry provided --> dry run")
    dry <- T
}

# check outdir
if (!any(grepl("--outdir", args))) { # outdir not provided
    outdir <- normalizePath(dirname(anom_file))
    message("outdir not provided. use default `dirname(anom_file)`")
} else { # outdir provided
    outdir <- sub("--outdir=", "", args[grep("--outdir=", args)])
}
if (file.access(outdir, mode=0) == -1) { # not existing
    message("outdir = \"", outdir, "\" does not exist. try to create ... ", appendLF=F)
    dir.create(outdir, recursive=T)
    if (!file.exists(outdir)) {
        stop("not successful. error msg:")
    } else {
        message("ok")
    }
} else { # outdir exists
    if (file.access(outdir, mode=2) == -1) { # not writable
        stop("provided outdir = \"", outdir, "\" not writeable.")
    }
}
outdir <- normalizePath(outdir)
message("--> outdir = \"", outdir, "\"")


# run annual and seasonal means if monthly before trend
fin_all <- c()
if (monthly) { # if input is monthly
    message("string \"Jan-Dec\" detected in anom_file --> calc annual and seasonal means ...")

    # calc annual mean
    fout <- sub("Jan-Dec", "annual", anom_file)
    if (file.exists(fout)) {
        message("file \"", fout, "\" already exists. skip cdo yearmean")
    } else {
        cmd <- paste0("cdo yearmean ", anom_file, " ", fout)
        if (file.access(dirname(anom_file), mode=2) == -1) {
            stop("input is monthly but cannot run\n`", cmd, "` since output dir is not writable.")
        }
        message("run `", cmd, "` ...")
        if (!dry) system(cmd)
    }
    fin_all <- c(fin_all, fout)

    # calc seasonal means and select seasons
    fout <- sub("Jan-Dec", "seasmean", anom_file)
    if (file.exists(fout)) {
        message("file \"", fout, "\" already exists. skip cdo seasmean")
    } else {
        cmd <- paste0("cdo seasmean ", anom_file, " ", fout)
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
    fin_all <- c(fin_all, anom_file)

} # if monthly
    
# calc eof for all files
for (fi in seq_along(fin_all)) {

    message("***************** file ", fi, "/", length(fin_all), " ********************")

    # cdo eof
    fout_eigval <- paste0(fin_all[fi], "_", neof, "_", method, "_eigval.nc")
    fout_eigvec <- paste0(fin_all[fi], "_", neof, "_", method, "_eigvec.nc")
    cmd <- paste0("export CDO_WEIGHT_MODE=", cdo_weight_mode, "; ", 
                  "export MAX_JACOBI_ITER=", max_jacobi_iter, "; ", 
                  "cdo -v -P ", nparallel, " ", method, ",", neof, " ", indir, "/", fin_all[fi], " ", 
                  outdir, "/", fout_eigval, " ", outdir, "/", fout_eigvec)
    message("run `", cmd, "` ...")
    if (!dry) {
        tic <- Sys.time()
        system(cmd)
        toc <- Sys.time()
        elapsed <- toc - tic
        elapsed <- paste0("cdo ", method, " call took ", round(elapsed), " ", attr(elapsed, "units"))
        message(elapsed)
        # set elapsed time as global nc attribute
        cmd <- paste0("ncatted -O -a cdo_", method, "_elapsed,global,c,c,\"", elapsed, "\" ",
                      outdir, "/", fout_eigval, " ", outdir, "/", fout_eigval)
        message("run `", cmd, "` ...")
        system(cmd)
        cmd <- paste0("ncatted -O -h -a cdo_", method, "_elapsed,global,c,c,\"", elapsed, "\" ",
                      outdir, "/", fout_eigvec, " ", outdir, "/", fout_eigvec)
        message("run `", cmd, "` ...")
        system(cmd)
    } # if not dry

    # cdo eofcoeff
    fout_pc <- paste0(fin_all[fi], "_", neof, "_", method, "_pc.nc")
    cmd <- paste0("export CDO_FILE_SUFFIX=NULL; ",
                  "cdo -v eofcoeff ", outdir, "/", fout_eigvec, " ", indir, "/", fin_all[fi], " ",
                  outdir, "/", fout_pc)
    message("run `", cmd, "` ...")
    if (!dry) system(cmd)

    # rename the *nc00000, *.nc000001, ... files
    for (i in seq_len(neof)-1) { # 0, 1, 2, ...
        fout_pc_i <- paste0(fin_all[fi], "_", neof, "_", method, "_pc", i+1, ".nc")
        cmd <- paste0("mv ", outdir, "/", fout_pc, sprintf("%05i", i), " ", 
                      outdir, "/", fout_pc_i)
        message("run `", cmd, "` ...")
        if (!dry) system(cmd)
    }

    # derive and cat described variance
    fout_explvar <- paste0(fin_all[fi], "_", neof, "_", method, "_descrvar.nc")
    cmd <- paste0("cdo -seltimestep,1/", neof, " -div ", fout_eigval, " -timsum ", 
                  fout_eigval, " ", outdir, "/", fout_explvar)
    message("run `", cmd, "` ...")
    if (!dry) {
        system(cmd)
        cmd <- paste0("ncdump ", fout_explvar)
        dump <- system(cmd, intern=T)
        dump <- dump[(length(dump)-neof):(length(dump)-1)]
        dump <- gsub(",", "", dump)
        dump <- gsub(";", "", dump)
        dump <- trimws(dump)
        dump <- as.numeric(dump)
        message("described variance:\n",
                paste(paste0("   ", method, seq_len(neof), ": ", 100*dump, "%"), collapse="\n"))
    }

} # for fi in fin_all

# transfer to other server if wanted
if (F && grepl("stan", Sys.info()["nodename"])) {
    message("##########################")
    outdir_paleosrv <- paste0("/isibhv/projects/paleo_work/cdanek/post", 
                              substr(outdir, regexpr("post/", outdir)+4, nchar(outdir)))
    cmd <- paste0("scp ", paste(paste0(outdir, "/", fout_all), collapse=" "), 
                  " cdanek@paleosrv1.awi.de:", outdir_paleosrv, "/.")
    message("run `", cmd, "` ...")
    if (!dry) system(cmd)
} # if on stan

