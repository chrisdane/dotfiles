#!/usr/bin/env Rscript

if (interactive()) {
    me <- "mycdosplitlevel.r"
    args <- c("/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/awi-esm-1-1-lr_kh800_historical2_jsbach_fldsum_pft_box_global_Jan-Dec_1850-2014.nc",
              "/work/ba1103/a270073/post/jsbach/fldsum/pft_fract_box/test<level>.nc")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

# check which mode (seasmean, seassum) is executed
if (grepl("level", me)) {
    mode <- "level"
} else {
    stop("script ", me, " not implemented")
}

if (mode == "level") {
    usage <- paste0("\nUsage:\n $ ", me, " in.nc \"out<", mode, ">name.nc\" (must provide quotes)\n")
}

# check
if (length(args) != 2) {
    message(usage)
    quit()
}

# check input file
fin <- args[1]
if (!file.exists(fin)) stop("fin = \"", fin, "\" does not exist")
indir <- normalizePath(dirname(fin))

# check output file
fout <- args[2]
if (file.exists(fout)) stop("fout = \"", fout, "\" already exists")
if (!grepl(paste0("<", mode, ">"), fout)) {
    stop("fout = \"", fout, "\" must contain the string \"<", mode, ">\"")
}
outdir <- dirname(fout)
fout <- basename(fout)
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
#message("--> outdir = \"", outdir, "\"")

# check if cdo exists
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")

# calc splitlevel, year, ...
cmd <- paste0(cdo, " split", mode, " ", fin, " ", outdir, "/tmp_", Sys.getpid(), "_split", mode)

# run split command 
message("\nrun `", cmd, "` ...")
system(cmd)

# rename results as wanted by user
files <- list.files(outdir, pattern=paste0("tmp_", Sys.getpid(), "_split", mode)) # e.g. "tmp_41477_splitlevel000021.nc"
message("\nrename ", length(files), " files ...")
levels <- tools::file_path_sans_ext(files)
levels <- substr(levels, 
                 start=regexpr(paste0("_split", mode), levels) + nchar(paste0("_split", mode)),
                 stop=nchar(levels)) # e.g. "000021"
levels <- as.integer(levels) # e.g. 21
fout <- rep(fout, t=length(levels))
for (fi in seq_along(fout)) {
    fout[fi] <- sub(paste0("<", mode, ">"), levels[fi], fout[fi])
}
invisible(file.rename(paste0(outdir, "/", files), paste0(outdir, "/", fout)))

