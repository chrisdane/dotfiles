#!/usr/bin/env Rscript

# - convert fesom1 non-levelwise 3D data to levelwise using `ncks -d nod3d,from-1,to-1` (`-1` because ncks counts from zero)
# - spheRlab::sl.grid.FESOM3Ddata1Dto2D() (https://github.com/FESOM/spheRlab.git) does the same thing but much slower

rm(list=ls()); graphics.off()
warn <- options()$warn

if (interactive()) {
    me <- "fesom1_nod3d_levelwise_fast.r"
    if (F) {
        args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
                  "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/fesom/levelwise",
                  #"shifttime=-1s",
                  #"settbounds=day",
                  #"/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/thetao_fesom_20130101.nc")
                  "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/uo_fesom_20130101.nc")
    } else if (F) {
        args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
                  "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/post/fesom/levelwise",
                  "shifttime=-1s",
                  "settbounds=day",
                  "timstat=monmean",
                  #"/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/fesom/so_fesom_20200101.nc")
                  "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/fesom/so_fesom_*")
    } else if (F) {
        args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
                  "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/post/recom",
                  "shifttime=-1s",
                  "settbounds=mon",
                  "sellevel=100,1337.8,1338",
                  "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/fesom/thetao_fesom_39440101.nc")
                  #"/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/recom/bgc22_fesom_39440101.nc")
    } else if (F) {
        args <- c("meshdir=/work/ollie/cdanek/mesh/fesom/CbSCL",
                  "outdir=~/test",
                  "~/test/temp.nc")
    } else if (F) {
        args <- c("meshdir=/work/ab0246/a270073/mesh/fesom/CbSCL",
                  "outdir=/work/ba1103/a270073/out/fesom-1.4_old/Low01/s52/levelwise",
                  "/work/ba1103/a270073/out/fesom-1.4_old/Low01/s52/temp.fesom.2009.nc",
                  "/work/ba1103/a270073/out/fesom-1.4_old/Low01/s52/salt.fesom.2009.nc")
    } else if (T) {
        args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
                  "outdir=/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical3/outdata/post/fesom/levelwise",
                  #"sellevel=100,1337.8,1338,1339",
                  "shifttime=-1s",
                  "settbounds=day",
                  "timstat=monmean",
                  #"/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical3/outdata/fesom/thetao_fesom*.nc")
                  "/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical3/outdata/fesom/so_fesom_{1970..2014}*.nc")
    }
} else { # if not interactive
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)

    #./fesom1_nod3d_levelwise_fast.r meshdir=/pool/data/AWICM/FESOM1/MESHES/core outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/fesom/levelwise shifttime=-1s /work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/thetao_fesom_20140101.nc > levelwise.log 2>&1 &
}

usage <- paste0("\n",
                "This function does the same as `spheRlab::sl.grid.FESOM3Ddata1Dto2D()`\n",
                "from https://github.com/FESOM/spheRlab.git but faster.",
                "\n",
                "\nUsage:\n",
                "[sbatch -p shared -A <account> -t 08:00:00 -o lvl.log -e lvl.log --wrap=\"]",
                me,
                " meshdir=/path/to/mesh outdir=/path/to/save/result [griddes=/path/to/griddes.nc] [sellevel=100 or sellevel=100,1337.8,1338 or sellevel=1000/1338] [timstat=monmean] [settbounds=day] [shifttime=-1s] [reduce_dim=false] file1 [file2 fileN; e.g.: {thetao,so}_fesom_{1970..1972}*.nc] [\" or > lvl.log 2>&1 &]\n",
                "\n",
                " with e.g. (albedo) meshdir=/albedo/pool/FESOM/meshes_default/core\n",
                "                    meshdir=/albedo/work/projects/p_pool_recom/meshes/fesom2/core2\n",
                "           (ecmwf) meshdir=/scratch/deu5912/pool/FESOM2/awicm3/core2\n",
                "           (levante) meshdir=/pool/data/AWICM/FESOM1/MESHES/core\n",
                "                     meshdir=/pool/data/AWICM/FESOM2/MESHES_FESOM2.1/core2\n",
                "                     meshdir=/work/ab0246/a270073/mesh/fesom/LSea2\n",
                #"           (ollie) meshdir=/work/ollie/pool/FESOM/meshes_default/core\n",
                #"                   meshdir=/work/ollie/projects/clidyn/FESOM2/meshes/core2\n",
                "\n",
                " with e.g. (albedo) griddes=/albedo/pool/FESOM/meshes_default/core/griddes.nc\n",
                "                    griddes=/albedo/work/projects/p_pool_recom/meshes/fesom2/core2/core2_griddes_nodes.nc\n",
                "                    griddes=/albedo/work/projects/p_pool_recom/meshes/fesom2/core2/core2_griddes_elements.nc\n",
                "           (ecmwf) griddes=/scratch/deu5912/pool/FESOM2/awicm3/core2/core2_griddes_nodes.nc\n",
                "           (levante) griddes=/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc\n",
                "                     griddes=/pool/data/AWICM/FESOM2/MESHES_FESOM2.1/core2/core2_griddes_nodes.nc\n",
                "                     griddes=/pool/data/AWICM/FESOM2/MESHES_FESOM2.1/core2/core2_griddes_elements.nc\n",
                "                     griddes=/work/ab0246/a270073/mesh/fesom/LSea2/griddes_LSea2.nc\n",
                #"           (ollie) griddes=/work/ollie/pool/FESOM/meshes_default/core/griddes.nc\n",
                #"                   griddes=/work/ollie/projects/clidyn/FESOM2/meshes/core2/core2_griddes_nodes.nc\n",
                #"                   griddes=/work/ollie/projects/clidyn/FESOM2/meshes/core2/core2_griddes_elements.nc\n",
                "\n",
                "If `griddes` is given (not by default), this griddes will be set to the levelwise result file.\n",
                "If `reduce_dim=false` (default), dimensions of length 1 of levelwise result file will not be dropped.\n",
                "Get your slurm accounts and fairshre with `sshare -U -u $(whoami) --format=\"Account%-30,User%15,NormShares,RawUsage,EffectvUsage,FairShare\"`\n")

# check
if (length(args) < 3) { # meshdir, outdir, file1
    if (interactive()) {
        stop(usage)
    } else {
        message(usage)
        quit()
    }
}

message("\nstart ", me, " ...")
if (any(grepl("^meshdir=", args))) {
    ind <- which(grepl("^meshdir=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"meshdir=\". must be 1")
    meshdir <- sub("meshdir=", "", args[ind])
    args <- args[-ind]
} else {
    stop("provide `meshdir=/path/to/mesh")
}
if (file.access(meshdir, mode=0) == -1) { # not existing
    stop("provided meshdir = \"", meshdir, "\" does not exist")
}
if (file.access(meshdir, mode=4) == -1) { # not readable
    stop("provided meshdir = \"", meshdir, "\" not readable.")
}
meshdir <- normalizePath(meshdir)
message("meshdir = ", meshdir)
if (!file.exists(paste0(meshdir, "/nod2d.out"))) stop("did not find nod2d.out in this dir")
if (!file.exists(paste0(meshdir, "/nod3d.out"))) stop("did not find nod3d.out in this dir")
if (!file.exists(paste0(meshdir, "/aux3d.out"))) stop("did not find aux3d.out in this dir")

if (any(grepl("^outdir=", args))) {
    ind <- which(grepl("^outdir=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"outdir=\". must be 1")
    outdir <- sub("outdir=", "", args[ind])
    args <- args[-ind]
} else {
    stop("provide `outdir=/path/to/save/result")
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
message("outdir = ", outdir)

sellevel <- NULL # default
if (any(grepl("^sellevel=", args))) {
    ind <- which(grepl("^sellevel=", args))
    sellevel <- sub("sellevel=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(sellevel)) {
    message("check `sellevel` = \"", sellevel, "\" ...")
    sellevel_method <- "select" # default
    if (grepl(",", sellevel)) { # e.g. "100,1337.8,1338"
        sellevel <- strsplit(sellevel, ",")
    } else if (grepl("/", sellevel)) { # e.g. "1000/1338"
        sellevel <- strsplit(sellevel, "/")
        sellevel_method <- "range"
    }
    if (length(sellevel) != 1) stop("sellevel argument must be of form `sellevel=100`, `sellevel=100,1337.8,1338` or `sellevel=1000/1338` in meter (positive depth)")
    sellevel <- sellevel[[1]] # e.g. "100" "1337.8" "1338" or "1000" "1338"
    sellevel <- unique(sellevel)
    if (sellevel_method == "range" && length(sellevel) == 1) {
        message("depth range is wanted but both are levels are the same. continue with ", sellevel[1], " m ...")
        sellevel_method <- "select"
    }
    if (sellevel_method == "range") {
        if (length(sellevel) != 2) stop("if depth range is wanted, `sellevel` must contain two numbers separated by one \"/\"")
    }
    options(warn=2); sellevel <- as.numeric(sellevel); options(warn=warn) # stop on warning
    sellevel <- abs(sellevel) # in case "-100" was provided
    message("--> sellevel = ", paste(sellevel, collapse=", ")) # e.g. 100, 1337.8, 1338 or 1000, 1338
} # sellevel argument provided or not

shifttime <- NULL # default
if (any(grepl("^shifttime=", args))) {
    ind <- which(grepl("^shifttime=", args))
    shifttime <- sub("shifttime=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(shifttime)) message("shifttime = ", shifttime)

settbounds <- NULL # default
if (any(grepl("^settbounds=", args))) {
    ind <- which(grepl("^settbounds=", args))
    settbounds <- sub("settbounds=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(settbounds)) message("settbounds = ", settbounds)

timstat <- NULL # default
if (any(grepl("^timstat=", args))) {
    ind <- which(grepl("^timstat=", args))
    timstat <- sub("timstat=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(timstat)) message("timstat = ", timstat)

reduce_dim <- "false" # default
if (any(grepl("^reduce_dim=", args))) {
    ind <- which(grepl("^reduce_dim=", args))
    reduce_dim <- sub("reduce_dim=", "", args[ind])
    args <- args[-ind]
}
message("reduce_dim = ", reduce_dim)
if (reduce_dim == "true") {
    message("--> reduce dimensions of length 1")
    reduce_dim <- "--reduce_dim"
} else if (reduce_dim == "false") {
    message("--> do not reduce dimensions of length 1")
    reduce_dim <- NULL
} else {
    stop("`reduce_dim` must be \"true\" or \"false\"")
}

griddes <- NULL # default
if (any(grepl("^griddes=", args))) {
    ind <- which(grepl("^griddes=", args))
    griddes <- sub("griddes=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(griddes)) {
    message("griddes = ", griddes)
    if (!file.exists(griddes)) {
        stop("provided `griddes` = ", griddes, " does not exist")
    }
    if (file.access(outdir, mode=4) == -1) { # not readable
        stop("provided `griddes` = ", griddes, " not readable")
    }
}

if (length(args) == 0) { # zero input files
    if (interactive()) {
        stop(usage)
    } else {
        message(usage)
        quit()
    }
}

# apply potential regex
files <- args
message("\n", length(files), " input files:")
options(width=1000); print(data.frame(file=files)); options(width=80)
files2 <- vector("list", l=length(files))
for (fi in seq_along(files)) {
    if (T) { # use ls
             # todo: might raise error `bash: /bin/ls: Argument list too long`
        pattern <- basename(files[fi])
        cmd <- paste0("\\ls ", files[fi])
        message("run `", cmd, "` ...")
        fnames <- system(cmd, intern=T)
    } else if (F) { # use find
                    # todo: cannot directly apply brace expansion
                    # --> workaround that works:
                    # ```
                    # expr=$(printf -- '-name "so_fesom_%s*" -o ' {1970..1971} | sed 's/ -o $//')
                    # eval "find $path -type f \( $expr \)"
                    # ```
    } else if (F) { # use list.files
                    # todo: brace expansion "so_fesom_{1970..2014}*.nc" cannot be translated to r in a non-painful way? 
        pattern <- glob2rx(basename(files[fi]))
        message("run `list.files(\"", dirname(files[fi]), "\", pattern=\"", pattern, "\", full.names=T)` ...")
        fnames <- list.files(dirname(files[fi]), pattern=pattern, full.names=T) # pattern is regular expression
    }
    if (length(fnames) == 0) {
        stop("found zero files with pattern \"", pattern, "\" as given by file ", fi, ":\n", files[fi])
    }
    files2[[fi]] <- fnames
}
files2 <- unlist(files2)
files <- files2
message("--> work on ", length(files), " files:")
#stop("asd")
options(width=1000); print(data.frame(file=files)); options(width=80)
message()

################

# check
message("\ncheck ncks ... ", appendLF=F)
ncks <- Sys.which("ncks")
if (ncks == "") stop("could not find ncks")
message("ok")
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")

# load ncdf4 package
check <- T
message("check r package ncdf4 ... ", appendLF=F)
if (!any(search() == paste0("package:ncdf4"))) {
    check <- suppressWarnings(library(ncdf4, logical.return=T))
}
if (!check) { # no success
    stop("could not load ncdf4 package from\n",
         paste(paste0("   ", .libPaths()), collapse="\n"), "\n",
         "install with\n   `install.packages(\"ncdf4\")`\n",
         "or\n   `install.packages(\"ncdf4\", lib=\"/path/where/the/package/should/get/installed\")`\n",
         "in an R session")
}
message("ok")

# check if data.table package is available
message("check r package data.table ... ", appendLF=F)
data.table_check <- suppressWarnings(library(data.table, logical.return=T))
if (!data.table_check) {
    message("\ncould not load data.table package from\n",
         paste(paste0("   ", .libPaths()), collapse="\n"), "\n",
         "install with\n   `install.packages(\"data.table\")`\n",
         "or\n   `install.packages(\"data.table\", lib=\"/path/where/the/package/should/get/installed\")`\n",
         "in an R session\n",
         "--> will continue using base::scan() which is much slower")
} else {
    message("ok")
}

# read mesh once
message("\nget number of 2d nodes, 3d nodes and levels, depth levels, and aux3d (assume that all input files use the same mesh) ...")
n2 <- base::scan(paste0(meshdir, "/nod2d.out"), what=integer(), n=1, quiet=T)
n3 <- base::scan(paste0(meshdir, "/nod3d.out"), what=integer(), n=1, quiet=T)
nlev_aux3d <- base::scan(paste0(meshdir, "/aux3d.out"), what=integer(), n=1, quiet=T)
message("--> n2 = ", n2, "\n",
        "--> n3 = ", n3, "\n",
        "--> nlev_aux3d = ", nlev_aux3d)
if (data.table_check) {
    message("run `data.table::fread(nod3d.out)` ...")
    nod3d <- data.table::fread(paste0(meshdir, "/nod3d.out"), skip=1, showProgress=F)
    nod3d <- as.matrix(nod3d)
} else { # use base::scan
    message("run `base::scan(nod3d.out)` ...")
    nod3d <- base::scan(paste0(meshdir, "/nod3d.out"), skip=1, quiet=T)
    nod3d <- matrix(nod3d, nrow=n3, byrow=T)
}
depth <- drop(nod3d[,4]) # model depths in m at every 3D node; negative downwards
depth <- abs(unique(depth))
message("--> depths (n = ", length(depth), ") = ", paste(depth, collapse=", "))
if (is.null(sellevel)) { # no sellevel provided
    sellevel_method <- "range" # all levels
    sellevel <- range(depth) # e.g. 0 5900
}
if (sellevel_method == "range") { # correct given range for actual mesh depths
    inds <- c(which.min(abs(depth - sellevel[1])),
              which.min(abs(depth - sellevel[2])))
    inds <- seq(inds[1], inds[2], b=1L)
    tmp <- depth[inds]
    if (sellevel[1] < min(tmp)) tmp <- c(sellevel[1], tmp)
    if (sellevel[2] > max(tmp)) tmp <- c(tmp, sellevel[2])
    sellevel <- tmp
}
sellevel <- sort(sellevel)
message("--> sellevel (n = ", length(sellevel), ") = ", paste(sellevel, collapse=", "))

message("\nconvert ", length(files), " files from nod3d to levelwise ...\n")
elapsed <- c()
cnt <- 0
get_level_info <- T # check mesh on 1st file
if (!is.null(timstat)) tmp_files_timstat <- rep(NA, t=length(files))
for (fi in seq_along(files)) {
    message("******************************************************\n",
            "file ", fi, "/", length(files), ": ", files[fi])
    if (!file.exists(files[fi])) {
        message("file does not exist. skip")
    } else {

        suffix <- tools::file_path_sans_ext(basename(files[fi]))
        file_ext <- tools::file_ext(files[fi])
        ofile <- paste0(outdir, "/", suffix, "_levelwise_")
        if (sellevel_method == "select") {
            ofile <- paste0(ofile, paste(sellevel, collapse="_")) # e.g. "100_1337.8_1338"
        } else if (sellevel_method == "range") {
            ofile <- paste0(ofile, sellevel[1], "-", sellevel[length(sellevel)]) # "1000-1338"
        }
        ofile <- paste0(ofile, "m.", file_ext)
        if (file.exists(ofile)) { # levelwise does already exist
            message("ofile ", ofile, " already exists. skip")

        } else { # levelwise does not exist

            if (get_level_info) {
                #sellevel_li <- as.list(sellevel)
                inds_from <- inds_to <- interps <- rep(NA, t=length(sellevel))
                for (li in seq_along(sellevel)) {
                    if (sellevel[li] < min(depth) || sellevel[li] > max(depth)) {
                        stop("wanted level ", li, "/", length(sellevel), ": ", sellevel[li], "m is not within model depths")
                    }
                    ind <- which.min(abs(depth - sellevel[li]))[1]
                    if (depth[ind] != sellevel[li]) { # wanted depth is not one of model depths
                        if (depth[ind] < sellevel[li]) {
                            inds <- c(ind, ind+1)
                        } else if (depth[ind] > sellevel[li]) {
                            inds <- c(ind-1, ind)
                        }
                        interp <- T
                    } else { # wanted depth is one of model depths
                        inds <- c(ind, ind) # redundant; just repeat
                        interp <- F
                    }
                    #sellevel_li[[li]] <- list(depth=sellevel[li], inds=ind, depths=depth[ind], interp=interp)
                    inds_from[li] <- inds[1]
                    inds_to[li] <- inds[2]
                    interps[li] <- interp
                } # for li
                sellevel_df <- data.frame(sellevel=sellevel,
                                          ind_from=inds_from, ind_to=inds_to,
                                          depth_from=depth[inds_from], depth_to=depth[inds_to],
                                          interp=interps)
                #cat(capture.output(str(sellevel_li, digits=10)), sep="\n") # todo: print as df
                print(sellevel_df, width=200)
                #sellevel_needed <- unique(as.vector(unlist(lapply(sellevel_li, "[[", "depths"))))
                sellevel_needed <- unique(c(sellevel_df$depth_from, sellevel_df$depth_to))
                #sellevel_needed_inds <- unique(as.vector(unlist(lapply(sellevel_li, "[[", "inds"))))
                sellevel_needed_inds <- match(sellevel_needed, depth)
                if (anyNA(sellevel_needed_inds)) stop("this should not happen")
                sellevel_needed_df <- data.frame(depth=sellevel_needed, ind=sellevel_needed_inds)
                nlev_needed <- nrow(sellevel_needed_df)
                message("--> nlev_needed = ", nlev_needed, ":")
                print(sellevel_needed_df)
                sellevel_interp <- F
                if (any(sellevel_df$interp)) sellevel_interp <- T
                message("--> any vertical interp needed = ", sellevel_interp)

                # prepare output
                n2_dim <- ncdf4::ncdim_def(name="ncells", units="", vals=seq_len(n2), create_dimvar=F) # from spheRlab::sl.grid.FESOM3Ddata1Dto2D.R
                depth_dim <- ncdf4::ncdim_def(name="depth", units="m", vals=sellevel)

                # get nod3d dimension name
                message("\nget nod3d dimname ... ", appendLF=F)
                n3dimname <- paste0(ncks, " -m ", files[fi]) # like ncdump -h
                n3dimname <- system(n3dimname, intern=T) # e.g. "    nodes_3d = 3668773 ;"
                ind <- grep(paste0(" = ", n3, " ;"), n3dimname)
                if (length(ind) != 1) {
                    stop("--> did not find pattern \" = ", n3, " ;\" in return of `ncks -m` of this file, i.e. no dimension with number of 3d nodes")
                }
                n3dimname <- n3dimname[ind] # "    nodes_3d = 3668773 ;"
                n3dimname <- strsplit(n3dimname, " = ")[[1]][1] # "    nodes_3d"
                n3dimname <- trimws(n3dimname) # "nodes_3d"
                message(n3dimname)

                # get aux3d
                if (data.table_check) {
                    message("run `data.table::fread(aux3d.out)` ...")
                    aux3d <- data.table::fread(paste0(meshdir, "/aux3d.out"), skip=1, nrows=n2*nlev_aux3d,
                                               na.strings="-999", showProgress=F)
                    aux3d <- matrix(aux3d$V1, nrow=nlev_aux3d, ncol=n2)
                } else { # use base::scan
                    message("run `base::scan(aux3d.out)` (install `data.table` package for faster reading) ...")
                    aux3d <- base::scan(paste0(meshdir, "/aux3d.out"), skip=1, nlines=n2*nlev_aux3d,
                                        na.strings="-999", quiet=T)
                    aux3d <- matrix(aux3d, nrow=nlev_aux3d, ncol=n2)
                }

                get_level_info <- F # do not check again (assuming all input files have same mesh)

            } # if get_level_info

            # apply timstat before
            if (!is.null(timstat)) {
                message("\n`timstat` = \"", timstat, "\"")
                cmd <- paste0(cdo, " -", timstat)
                if (!is.null(settbounds)) cmd <- paste0(cmd, " -settbounds,", settbounds) # not needed since `timstat` sets tbounds
                if (!is.null(shifttime)) cmd <- paste0(cmd, " -shifttime,", shifttime)
                tmp_files_timstat[fi] <- paste0(dirname(ofile), "/tmp_", basename(ofile), "_", timstat)
                cmd <- paste0(cmd, " ", files[fi], " ", tmp_files_timstat[fi])
                message("run timstat `", cmd, "` ...") # e.g. "cdo -monmean (-settbounds) -shifttime fin"
                check <- system(cmd)
                if (check != 0) stop("cmd failed")
                files[fi] <- tmp_files_timstat[fi] # continue with timstat file
            } # if timstat

            # step 1: select 3D nodes; they are all non-NA and of different length per level
            message("\nstep 1: split nod3d data to `nlev_needed` = ", nlev_needed, " files ...")
            ofiles_lev_woutNA <- rep(NA, t=length(sellevel_needed))
            for (li in seq_len(nlev_needed)) {
                inds <- aux3d[sellevel_needed_df$ind[li],]
                message("needed level ", li, "/", nlev_needed, ": ind = ", sellevel_needed_df$ind[li], ", depth = ", sellevel_needed_df$depth[li], "m ...")
                if (all(is.na(inds))) { # bottom layer in aux3d; fesom1 bug?
                    message("--> all NA. skip")
                } else { # some non-NA values
                    ofiles_lev_woutNA[li] <- paste0(dirname(ofile), "/tmp_", basename(ofile),
                                                    "_lev", sellevel_needed_df$ind[li], "_",
                                                    sellevel_needed_df$depth[li], "m_woutNA_pid",
                                                    Sys.getpid(), "_",format(Sys.time(), "%Y-%m-%d_%H_%M_%S"))
                    if (file.exists(ofiles_lev_woutNA[li])) stop("this should not happen")
                    cmd <- paste0(ncks, " -O", # -O: overwrite
                                  " -d ", n3dimname, ",", min(inds, na.rm=T)-1, ",", max(inds, na.rm=T)-1, # -1 since ncks starts counting from zero
                                  # todo: -F, --ftn, fortran      Fortran indexing conventions (1-based) for I/O
                                  " ", files[fi], " ", ofiles_lev_woutNA[li])
                    message("--> run `", cmd, "` ...") # ncks -d nod3d,from-1,to-1
                    check <- system(cmd)
                    if (check != 0) stop("cmd failed")
                } # if current level has non-NA values
            } # for li sellevel_needed

            # check if data from all wanted levels is NA (usually the last level)
            nonNAinds <- which(!is.na(ofiles_lev_woutNA))
            if (length(nonNAinds) == 0) {
                message("\n--> all data from wanted levels is NA. skip to next input file")

            } else { # some non-NA data

                # step 2: load all non-NA nodes of current level and save as array of dim (n2,ndepth,all other dims),
                # filling additional values in n2-dim with NA
                message("\nstep 2: load 3D data of ", length(nonNAinds),
                        " non-NA levels and save levelwise (n2=", n2,
                        ") taking NA positions into account ...")
                has3D <- NULL
                for (li in seq_along(nonNAinds)) {
                    if (is.null(has3D) ||             # 1st level
                        (!is.null(has3D) && has3D)) { # all other levels
                        nonNAind <- nonNAinds[li]
                        message("needed level ", li, "/", length(nonNAinds), ": ind = ", sellevel_needed_df$ind[nonNAind],
                                ", depth = ", sellevel_needed_df$depth[nonNAind], "m\n",
                                "--> open ", ofiles_lev_woutNA[nonNAind], " ...")
                        nc <- ncdf4::nc_open(ofiles_lev_woutNA[nonNAind])
                        if (li == 1) { # create once new file that saves levelwise data on all depths of all nod3d-variables
                            ncvars3D <- vector("list", l=nc$nvars)
                            for (vi in seq_len(nc$nvars)) {
                                nod_ind <- which(sapply(nc$var[[vi]]$dim, "[[", "name") == n3dimname)
                                if (length(nod_ind) == 1) { # current variable has nod3d dimi
                                                            # --> todo: this excludes potential variable time_bnds tbounds
                                    ncdims <- c(list(n2_dim), list(depth_dim), nc$var[[vi]]$dim[-nod_ind]) # n2, depth, all other dims
                                    ncvars3D[[vi]] <- ncdf4::ncvar_def(name=names(nc$var)[vi], units=nc$var[[vi]]$units, dim=ncdims, missval=NA)
                                }
                            } # for vi
                            inds <- which(sapply(ncvars3D, is.null))
                            if (length(inds) > 0) ncvars3D <- ncvars3D[-inds] # remove all non-nod3d vars
                            if (length(ncvars3D) == 0) {
                                message("not a single variable has nod3d dimname = \"", n3dimname, "\". skip to next input file")
                                has3D <- F
                            } else {
                                has3D <- T
                                message("current level is first level --> create ofile = ", ofile, " ...")
                                ncout <- ncdf4::nc_create(ofile, ncvars3D, force_v4=T)
                                ncdf4::ncatt_put(ncout, "depth", "positive", "down")
                                ncdf4::ncatt_put(ncout, "depth", "axis", "Z")
                            }
                        } # if li == 1
                        if (has3D) { # necessary for first level
                            okinds <- which(!is.na(aux3d[sellevel_needed_df$ind[nonNAind],])) # e.g. 1:119130
                            for (vi in seq_along(ncvars3D)) {
                                message("var ", vi, "/", length(ncvars3D), ": ", ncvars3D[[vi]]$name, " ", appendLF=F)
                                arr_woutNA <- ncdf4::ncvar_get(nc, ncvars3D[[vi]]$name, collapse_degen=F) # load data of current level wout NA; keep dims of length 1
                                var_dims <- dim(arr_woutNA) # e.g. (119130,12); n2=126859
                                nod_ind <- which(var_dims == length(okinds)) # in this loop all variables have nod3d dim
                                if (length(nod_ind) == 0) stop("this should not happen")
                                new_dims <- c(nod2=n2, depth=1, var_dims[-nod_ind]) # n2, 1 placeholder for depth, all other dims
                                arr_wNA <- array(NA, dim=new_dims)
                                lhsinds <- rep(",", t=length(new_dims))
                                lhsinds[nod_ind] <- "okinds"
                                lhsinds <- paste(lhsinds, collapse="")
                                cmd <- paste0("arr_wNA[", lhsinds, "] <- arr_woutNA")
                                base::eval(parse(text=cmd)) # e.g. "arr_wNA[okinds,] <- arr_woutNA" or "arr_wNA[okinds,,,] <- arr_woutNA" depending on input dims
                                start <- c(nod2=1,  depth=li, rep(1, t=length(var_dims)-1)) # nod2, depth, all_other_dims_without_input_nod3_dim
                                count <- c(nod2=n2, depth=1,  rep(-1, t=length(var_dims)-1)) # -1 for complete dim
                                #stop("asd")
                                ncdf4::ncvar_put(ncout, ncvars3D[[vi]], vals=arr_wNA, start=start, count=count)
                                if (li == 1) {
                                    varatts <- ncdf4::ncatt_get(nc, ncvars3D[[vi]]$name) # original atts
                                    for (ai in seq_along(varatts)) {
                                        if (names(varatts)[ai] == "_FillValue" && varatts[[ai]] >= 1e30) {
                                            # todo:
                                            # "Error in ncatt_put, while writing attribute _FillValue with value 1.00000001504747e+30"
                                            # Error in ncatt_put_inner(idobj$group_id, idobj$id, attname, attval, prec = prec,  :
                                            #  Error return from C call R_nc4_put_att_double for attribute _FillValue
                                        } else {
                                            ncdf4::ncatt_put(ncout, ncvars3D[[vi]], names(varatts)[ai], varatts[[ai]])
                                        }
                                    } # for ai
                                    if (!any(names(varatts) == "grid_type")) {
                                        ncdf4::ncatt_put(ncout, ncvars3D[[vi]], "grid_type", "unstructured")
                                    }
                                } # if li == 1
                                if (vi == length(ncvars3D)) message() # last
                            } # for vi
                            ncdf4::nc_close(nc)
                        } # if has3D fist level
                    } # if has3D all other levels
                } # for li depth
                message("\nsave ", ofile, " ...")
                ncdf4::nc_close(ncout)

                # rm tmp files
                if (T) {
                    message("\nrm ", length(ofiles_lev_woutNA[nonNAinds]), " tmp files ...")
                    invisible(file.remove(ofiles_lev_woutNA[nonNAinds]))
                }
                if (T && !is.null(timstat)) {
                    message("\nrm tmp timstat file ", tmp_files_timstat[fi])
                    invisible(file.remove(tmp_files_timstat[fi]))
                }

                # apply level bounds and, if needed, vertical interpolation and others
                cmd <- cdo
                if (!is.null(reduce_dim)) cmd <- paste0(cmd, " ", reduce_dim) # --reduce_dim
                if (!is.null(griddes)) cmd <- paste0(cmd, " -setgrid,", griddes) # apply griddes
                if (!is.null(settbounds) && is.null(timstat)) cmd <- paste0(cmd, " -settbounds,", settbounds) # apply settbounds if not already done before
                if (!is.null(shifttime) && is.null(timstat)) cmd <- paste0(cmd, " -shifttime,", shifttime) # apply shifttime if not already done before
                if (sellevel_interp) cmd <- paste0(cmd, " -intlevel,", paste(sellevel, collapse=",")) # apply vertical interpolation
                cmd <- paste0(cmd, " -genlevelbounds ", ofile, " ", ofile, "_tmp && mv ", ofile, "_tmp ", ofile)
                message("\nrun `", cmd, "` ...")
                check <- system(cmd)
                if (check != 0) stop("cmd failed")

            } # if some non-NA data
        } # if ofile already exists or not
    } # if fin exists or not
} # for fi

message("\nfinished ", me, "\n")

