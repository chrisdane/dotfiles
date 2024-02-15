#!/usr/bin/env Rscript

# convert fesom1 non-levelwise 3D variables to levelwise using `ncks -d nod3d,from-1,to-1` (-1 because ncks counts from zero)

# spheRlab::sl.grid.FESOM3Ddata1Dto2D() (https://github.com/FESOM/spheRlab.git) does the same thing but much slower

rm(list=ls()); graphics.off()

if (interactive()) {
    me <- "fesom1_nod3d_levelwise_fast.r"
    if (F) {
        args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
                  "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/fesom/levelwise",
                  #"shifttime=-1day",
                  #"/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/thetao_fesom_20130101.nc")
                  "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/uo_fesom_20130101.nc")
    } else if (T) {
        args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
                  "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/post/fesom/levelwise",
                  "shifttime=-1day",
                  "timstat=monmean",
                  #"/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/fesom/so_fesom_20200101.nc")
                  "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/fesom/so_fesom_*")
    } else if (F) {
        args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
                  "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/post/recom",
                  "shifttime=-1mon",
                  "sellevel=100,1337.8,1338",
                  "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/fesom/thetao_fesom_39440101.nc")
                  #"/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/recom/bgc22_fesom_39440101.nc")
    } else if (F) {
        args <- c("meshdir=/work/ollie/cdanek/mesh/fesom/CbSCL",
                  "outdir=~/test",
                  "~/test/temp.nc")
    }
} else { # if not interactive
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    #./fesom1_nod3d_levelwise_fast.r meshdir=/pool/data/AWICM/FESOM1/MESHES/core outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/post/fesom/levelwise shifttime=-1day /work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/thetao_fesom_20140101.nc > levelwise.log 2>&1 &
}

usage <- paste0("\n",
                "This function does the same as `spheRlab::sl.grid.FESOM3Ddata1Dto2D()`\n",
                "from https://github.com/FESOM/spheRlab.git but faster.",
                "\n",
                "\nUsage:\n", me,
                " meshdir=/path/to/mesh outdir=/path/to/save/result [sellevel=100 or sellevel=100,1337.8,1338 or sellevel=1000/1338] [timstat=monmean] [shifttime=-1d] file1 [file2 filen]\n",
                "\n",
                " with e.g. (albedo) meshdir=/albedo/pool/FESOM/meshes_default/core\n",
                "                    meshdir=/albedo/work/projects/p_pool_recom/meshes/fesom2/core2\n",
                "           (ecmwf) meshdir=/scratch/deu5912/pool/FESOM2/awicm3/core2\n",
                "           (levante) meshdir=/pool/data/AWICM/FESOM1/MESHES/core\n",
                "                     meshdir=/pool/data/AWICM/FESOM2/MESHES_FESOM2.1/core2\n",
                "                     meshdir=/work/ab0246/a270073/mesh/fesom/LSea2\n",
                "           (ollie) meshdir=/work/ollie/pool/FESOM/meshes_default/core\n",
                "                   meshdir=/work/ollie/projects/clidyn/FESOM2/meshes/core2",
                "\n")

# check
if (length(args) < 3) { # meshdir, outdir, file1
    if (interactive()) {
        stop(usage)
    } else {
        message(usage)
        quit()
    }
}

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

shifttime <- NULL # default
if (any(grepl("^shifttime=", args))) {
    ind <- which(grepl("^shifttime=", args))
    shifttime <- sub("shifttime=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(shifttime)) message("shifttime = ", shifttime)

timstat <- NULL # default
if (any(grepl("^timstat=", args))) {
    ind <- which(grepl("^timstat=", args))
    timstat <- sub("timstat=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(timstat)) message("timstat = ", timstat)

sellevel <- NULL # default
if (any(grepl("^sellevel=", args))) {
    ind <- which(grepl("^sellevel=", args))
    sellevel <- sub("sellevel=", "", args[ind])
    args <- args[-ind]
}
if (!is.null(sellevel)) message("sellevel = ", sellevel)

if (length(args) == 0) {
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
options(width=1000)
print(data.frame(file=files))
options(width=80)
message()
files2 <- vector("list", l=length(files))
for (fi in seq_along(files)) {
    files2[[fi]] <- list.files(dirname(files[fi]), glob2rx(basename(files[fi])), full.names=T)
}
files2 <- unlist(files2)
files <- files2
message("\n--> will work on ", length(files), " files:")
options(width=1000)
print(data.frame(file=files))
options(width=80)
message()

################

# check
message("\ncheck ncks ... ", appendLF=F)
ncks <- Sys.which("ncks")
if (ncks == "") stop("could not find ncks")
message("ok")
cdo <- NULL
if (!is.null(shifttime) || !is.null(timstat)) {
    message("`shifttime` or `timstat` is wanted --> check cdo ... ", appendLF=F)
    cdo <- Sys.which("cdo")
    if (cdo == "") stop("could not find cdo")
    message("ok")
}

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
    message("available")
}
           
message("\nget number of 2d nodes, 3d nodes and levels, depth levels, and aux3d (assume that all input files use the same mesh) ...")
n2 <- base::scan(paste0(meshdir, "/nod2d.out"), what=integer(), n=1, quiet=T)
n3 <- base::scan(paste0(meshdir, "/nod3d.out"), what=integer(), n=1, quiet=T)
nlev_total <- base::scan(paste0(meshdir, "/aux3d.out"), what=integer(), n=1, quiet=T)
if (data.table_check) {
    nod3d <- data.table::fread(paste0(meshdir, "/nod3d.out"), skip=1, showProgress=F)
    nod3d <- as.matrix(nod3d)
    aux3d <- data.table::fread(paste0(meshdir, "/aux3d.out"), skip=1, nrows=n2*nlev_total, 
                               na.strings="-999", showProgress=F)
    aux3d <- matrix(aux3d$V1, nrow=nlev_total, ncol=n2)
} else { # use base::scan
    nod3d <- base::scan(paste0(meshdir, "/nod3d.out"), skip=1, quiet=T)
    nod3d <- matrix(nod3d, nrow=n3, byrow=T)
    aux3d <- base::scan(paste0(meshdir, "/aux3d.out"), skip=1, nlines=n2*nlev_total, 
                        na.strings="-999", quiet=T)
    aux3d <- matrix(aux3d, nrow=nlev_total, ncol=n2)
}
depth <- drop(nod3d[,4])
depth <- abs(unique(depth)) # model depths in m; negative downwards 
message("--> n2 = ", n2, "\n",
        "--> n3 = ", n3, "\n",
        "--> nlev_total = ", nlev_total, "\n",
        "--> depths = ", paste(depth, collapse=", "))

# check sellevel
if (!is.null(sellevel)) {
    message("\ncheck `sellevel` = \"", sellevel, "\" ...")
    sellevel_method <- "select" # default
    if (grepl(",", sellevel)) {
        sellevel <- strsplit(sellevel, ",")
    } else if (grepl("/", sellevel)) {
        sellevel <- strsplit(sellevel, "/")
        sellevel_method <- "range"
    }
    if (length(sellevel) != 1) stop("sellevel argument must be of form `sellevel=100`, `sellevel=100,1337.8,1338` or `sellevel=1000/1338` in meter (positive depth)") 
    sellevel <- sellevel[[1]]
    sellevel <- unique(sellevel)
    if (sellevel_method == "range" && length(sellevel) == 1) {
        message("depth range is wanted but both are levels are the same. continue with ", sellevel[1], " m ...")
        sellevel_method <- "select"
    }
    if (sellevel_method == "range") {
        if (length(sellevel) != 2) stop("if depth range is wanted, `sellevel` must contain two numbers separated by one \"/\"")
    }
    warn <- options()$warn
    options(warn=2) # stop on warning
    sellevel <- as.numeric(sellevel)
    options(warn=warn)
    if (sellevel_method == "select") {
        message("--> ", length(sellevel), " depths wanted: ", paste(sellevel, collapse=", "), " m")
    } else if (sellevel_method == "range") {
        message("--> all depths between ", sellevel[1], " and ", sellevel[2], " m wanted")
    }
} else { # sellevel argument not provided
    sellevel_method <- "range" # all
    sellevel <- range(depth)   # levels
} # sellevel argument provided or not

if (sellevel_method == "range") { 
    inds <- c(which.min(abs(depth - sellevel[1])),
              which.min(abs(depth - sellevel[2])))
    inds <- seq(inds[1], inds[2], b=1L)
    tmp <- depth[inds]
    if (sellevel[1] < min(tmp)) tmp <- c(sellevel[1], tmp)
    if (sellevel[2] > max(tmp)) tmp <- c(tmp, sellevel[2])
    sellevel <- tmp
}
sellevel_li <- as.list(sellevel)
for (li in seq_along(sellevel)) {
    if (sellevel[li] < min(depth) || sellevel[li] > max(depth)) {
        stop("wanted level ", li, "/", length(sellevel), ": ", sellevel[li], "m is not within model depths")
    }
    ind <- which.min(abs(depth - sellevel[li]))[1]
    interp <- F # default
    if (depth[ind] != sellevel[li]) { # if wanted depth is one of model depths
        if (depth[ind] < sellevel[li]) {
            ind <- c(ind, ind+1)
        } else if (depth[ind] > sellevel[li]) {
            ind <- c(ind-1, ind)
        }
        interp <- T
    }
    sellevel_li[[li]] <- list(depth=sellevel[li], inds=ind, depths=depth[ind], interp=interp)
} # for li
cat(capture.output(str(sellevel_li, digits=10)), sep="\n")
sellevel_needed <- unique(as.vector(unlist(lapply(sellevel_li, "[[", "depths"))))
sellevel_needed_inds <- unique(as.vector(unlist(lapply(sellevel_li, "[[", "inds"))))
sellevel_interp <- unique(as.vector(unlist(lapply(sellevel_li, "[[", "interp"))))
if (any(sellevel_interp)) {
    sellevel_interp <- T
} else {
    sellevel_interp <- F
}
nlev_needed <- length(sellevel_needed)
message("--> nlev_needed = ", nlev_needed)
if (sellevel_interp) {
    message("some vertical interpolation necessary --> check cdo ... ", appendLF=F)
    cdo <- Sys.which("cdo")
    if (cdo == "") stop("could not find cdo")
    message("ok")
}

# prepare output
n2_dim <- ncdf4::ncdim_def(name="ncells", units="", vals=seq_len(n2), create_dimvar=F) # from spheRlab::sl.grid.FESOM3Ddata1Dto2D.R
depth_dim <- ncdf4::ncdim_def(name="depth", units="m", vals=sellevel_needed)

message("\nconvert ", length(files), " files from nod3d to levelwise ...\n")
elapsed <- c()
cnt <- 0
n3dimname <- NULL
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
            ofile <- paste0(ofile, paste(sellevel, collapse="_"))
        } else if (sellevel_method == "range") {
            ofile <- paste0(ofile, sellevel[1], "-", sellevel[length(sellevel)])
        }
        ofile <- paste0(ofile, "m.", file_ext)
        if (file.exists(ofile)) { # levelwise does already exist 
            message("ofile ", ofile, " already exists. skip")
        
        } else { # levelwise does not exist

            # get nod3d dimension name
            if (is.null(n3dimname)) {
                message("first file: get nod3d dimname ... ", appendLF=F)
                n3dimname <- paste0(ncks, " -m ", files[fi]) # like ncdump -h
                n3dimname <- system(n3dimname, intern=T) # e.g. "    nodes_3d = 3668773 ;"
                ind <- grep(paste0(" = ", n3, " ;"), n3dimname)
                if (length(ind) != 1) {
                    stop("--> did not find pattern \" = ", n3, " ;\" once in `ncks -m` of this file, i.e. no dimension with number of 3d nodes")
                }
                n3dimname <- n3dimname[ind] # "    nodes_3d = 3668773 ;"
                n3dimname <- strsplit(n3dimname, " = ")[[1]][1] # "    nodes_3d"
                n3dimname <- trimws(n3dimname) # "nodes_3d"
                message(n3dimname)
            } # if is.null(n3dimname)

            # apply timstat before
            if (!is.null(timstat)) {
                message("\n`timstat` = \"", timstat, "\"")
                cmd <- paste0(cdo, " -", timstat)
                if (!is.null(shifttime)) cmd <- paste0(cmd, " -shifttime,", shifttime)
                tmp_files_timstat[fi] <- paste0(ofile, "_", timstat)
                cmd <- paste0(cmd, " ", files[fi], " ", tmp_files_timstat[fi])
                message("run `", cmd, "` ...")
                check <- system(cmd)
                if (check != 0) stop("cmd failed")
                files[fi] <- tmp_files_timstat[fi] # continue with timstat file
            } # if shifttime 
            
            # step 1: select nodes from current level; they are all non-NA and of different length per level
            message("\nstep 1: select data from ", nlev_needed, " levels ...")
            ofiles_lev_woutNA <- rep(NA, t=length(sellevel_needed))
            for (li in seq_along(sellevel_needed)) {
                inds <- aux3d[sellevel_needed_inds[li],]
                message("level ", li, "/", nlev_needed, ": ", sellevel_needed_inds[li], " (", sellevel_needed[li], "m) ...")
                if (all(is.na(inds))) {
                    message("--> all NA. skip")
                } else { # some non-NA values
                    ofiles_lev_woutNA[li] <- paste0(tools::file_path_sans_ext(ofile), 
                                                    "_lev", sellevel_needed_inds[li], "_", 
                                                    sellevel_needed[li], "m_woutNA_pid",
                                                    Sys.getpid(), "_",format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), ".nc")
                    if (file.exists(ofiles_lev_woutNA[li])) stop("this should not happen")
                    cmd <- paste0(ncks, " -O", # -O: overwrite
                                  " -d ", n3dimname, ",", min(inds, na.rm=T)-1, ",", max(inds, na.rm=T)-1, # -1 since ncks starts counting from zero
                                  " ", files[fi], " ", ofiles_lev_woutNA[li])
                    message("--> run `", cmd, "` ...")
                    check <- system(cmd) # todo: what happens if file has no variable with nod3d dim
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
                    if (is.null(has3D) ||            # 1st level 
                        (!is.null(has3D) && has3D)) { # all other levels
                        nonNAind <- nonNAinds[li]
                        message("level ", li, "/", length(nonNAinds), " ", sellevel_needed_inds[nonNAind], 
                                " (", sellevel_needed[nonNAind], "m): open ", ofiles_lev_woutNA[nonNAind], " ...")
                        nc <- ncdf4::nc_open(ofiles_lev_woutNA[nonNAind])
                        if (li == 1) { # create once new file that saves levelwise data on all depths of all nod3d-variables
                            ncvars <- vector("list", l=nc$nvars)
                            for (vi in seq_len(nc$nvars)) {
                                nod_ind <- which(sapply(nc$var[[vi]]$dim, "[[", "name") == n3dimname)
                                if (length(nod_ind) == 1) { # current variable has nod3d dimi
                                    ncdims <- c(list(n2_dim), list(depth_dim), nc$var[[vi]]$dim[-nod_ind]) # n2, depth, all other dims
                                    ncvars[[vi]] <- ncdf4::ncvar_def(name=names(nc$var)[vi], units=nc$var[[vi]]$units, dim=ncdims, missval=NA)
                                }
                            } # for vi
                            inds <- which(sapply(ncvars, is.null))
                            if (length(inds) > 0) ncvars <- ncvars[-inds] # remove all non-nod3d vars
                            if (length(ncvars) == 0) {
                                message("not a single variable has nod3d dimname = \"", n3dimname, "\". skip to next input file")
                                has3D <- F
                            } else {
                                has3D <- T
                                message("current level is first level --> create ofile = ", ofile, " ...")
                                ncout <- ncdf4::nc_create(ofile, ncvars, force_v4=T)
                                ncdf4::ncatt_put(ncout, "depth", "positive", "down")
                                ncdf4::ncatt_put(ncout, "depth", "axis", "Z")
                            }
                        } # if li == 1
                        if (has3D) { # necessary for first level
                            okinds <- which(!is.na(aux3d[sellevel_needed_inds[nonNAind],])) # e.g. 1:119130
                            for (vi in seq_along(ncvars)) {
                                message("var ", vi, "/", length(ncvars), ": ", ncvars[[vi]]$name, " ", appendLF=F) 
                                arr_woutNA <- ncdf4::ncvar_get(nc, ncvars[[vi]]$name, collapse_degen=F) # load data of current level wout NA; keep dims of length 1
                                var_dims <- dim(arr_woutNA) # e.g. (119130,12); n2=126859
                                nod_ind <- which(var_dims == length(okinds)) # in this loop all variables have nod3d dim
                                if (length(nod_ind) == 0) stop("this should not happen")
                                new_dims <- c(nod2=n2, depth=1, var_dims[-nod_ind]) # n2, 1 placeholder for depth, all other dims
                                arr_wNA <- array(NA, dim=new_dims)
                                lhsinds <- rep(",", t=length(new_dims))
                                lhsinds[nod_ind] <- "okinds"
                                lhsinds <- paste(lhsinds, collapse="")
                                cmd <- paste0("arr_wNA[", lhsinds, "] <- arr_woutNA") 
                                eval(parse(text=cmd)) # e.g. "arr_wNA[okinds,] <- arr_woutNA" or "arr_wNA[okinds,,,] <- arr_woutNA" depending on input dims
                                start <- c(nod2=1,  depth=li, rep(1, t=length(var_dims)-1)) # nod2, depth, all_other_dims_without_input_nod3_dim
                                count <- c(nod2=n2, depth=1,  rep(-1, t=length(var_dims)-1)) # -1 for complete dim
                                #stop("asd")
                                ncdf4::ncvar_put(ncout, ncvars[[vi]], vals=arr_wNA, start=start, count=count)
                                if (li == 1) {
                                    varatts <- ncdf4::ncatt_get(nc, ncvars[[vi]]$name) # original atts
                                    for (ai in seq_along(varatts)) {
                                        if (names(varatts)[ai] == "_FillValue" && varatts[[ai]] >= 1e30) {
                                            # todo:
                                            # "Error in ncatt_put, while writing attribute _FillValue with value 1.00000001504747e+30"
                                            # Error in ncatt_put_inner(idobj$group_id, idobj$id, attname, attval, prec = prec,  : 
                                            #  Error return from C call R_nc4_put_att_double for attribute _FillValue
                                        } else {
                                            ncdf4::ncatt_put(ncout, ncvars[[vi]], names(varatts)[ai], varatts[[ai]])
                                        }
                                    } # for ai
                                    if (!any(names(varatts) == "grid_type")) {
                                        ncdf4::ncatt_put(ncout, ncvars[[vi]], "grid_type", "unstructured")
                                    }
                                } # if li == 1
                                if (vi == length(ncvars)) message() # last
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

                # apply vertical interpolation
                cmd <- cdo
                if (!is.null(shifttime) && is.null(timstat)) cmd <- paste0(cmd, " -shifttime,", shifttime) # apply shifttime if not already done before
                if (sellevel_interp) cmd <- paste0(cmd, " -intlevel,", paste(sellevel, collapse=",")) # apply vertical interpolation
                if (cmd != cdo) {
                    cmd <- paste0(cmd, " ", ofile, " ", ofile, "_tmp && mv ", ofile, "_tmp ", ofile)
                    message("\nrun `", cmd, "` ...")
                    check <- system(cmd)
                    if (check != 0) stop("cmd failed")
                } # if vertical interpolation is necessary

            } # if some non-NA data
        } # if fout already exists or not
    } # if fin exists or not
} # for fi

message("\nfinished\n")

