#!/usr/bin/env Rscript

if (interactive()) {
    me <- "rechunk.r"
    args <- c("time/ntime,nodes_2d/126859",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/rechunk_ntime_126859",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/tos_fesom_29900101.nc",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/tos_fesom_29910101.nc")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    #./rechunk.r "time/ntime,nodes_2d/126859" /work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/rechunk_ntime_126859 /work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom/shifttime/tos_fesom_* > rechunk.log 2>&1 &
}

usage <- paste0("\nUsage:\n", 
                " $ ", me, " \"time/ntime,nodes_2d/126859\" <outpath> <files>\n",
                " or\n",
                " $ ", me, " \"timr/ntime,lat/1,lon/1440\" <outpath> <files>\n")

# check
if (length(args) < 3) {
    message(usage)
    quit()
}
chunkarg <- args[1]
if (!grepl("/ntime", chunkarg)) stop("chunkarg \"", chunkarg, "\" must contain \"/ntime\"")
pathout <- args[2]
if (file.access(pathout, mode=0) == -1) { # not existing
    message("pathout = \"", pathout, "\" does not exist. try to create ... ", appendLF=F)
    dir.create(pathout, recursive=T)
    if (!file.exists(pathout)) {
        stop("not successful. error msg:")
    } else {
        message("success")
    }
} else { # pathout exists
    if (file.access(pathout, mode=2) == -1) { # not writable
        stop("provided pathout = \"", pathout, "\" not writeable.")
    }
}
files <- args[3:length(args)]

# rechunk
message("\nrechunk ", length(files), " files ...")
for (fi in seq_along(files)) {
    if (!file.exists(files[fi])) {
        message("fin ", files[fi], " does not exist. skip")
    } else {
        cmd <- paste0("cdo -s ntime ", files[fi])
        message("file ", fi, "/", length(files), ": run `", cmd, "` ... ", appendLF=F)
        ntime <- as.integer(system(cmd, intern=T))
        if (is.na(ntime)) stop("no success")
        message(ntime)
        tmp <- sub("ntime", ntime, chunkarg)
        #fout <- paste0(pathout, "/", basename(files[fi]), "_", gsub("[[:punct:]]", "_", tmp))
        fout <- paste0(pathout, "/", basename(files[fi]))
        if (file.exists(fout)) {
            message("fout ", fout, " already exists. skip")
        } else {
            cmd <- paste0("nccopy -u -w -c ", tmp, " ", files[fi], " ", fout)
            message("run `", cmd, "` ...")
            system(cmd)
        }
    }
} # for fi

message("\nfinished")

