#!/usr/bin/env Rscript

# convert fesom1 3D variables saved non-levelwise to levelwise using ncks  

rm(list=ls()); graphics.off()

if (interactive()) {
    me <- "fesom1_nod3d_levelwise_fast.r"
    if (F) {
        args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
                  "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/levelwise",
                  #"shifttime=-1day",
                  #"/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/thetao_fesom_20130101.nc")
                  "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/uo_fesom_20130101.nc")
    } else if (T) {
        args <- c("meshdir=/pool/data/AWICM/FESOM1/MESHES/core",
                  "outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/post/recom",
                  "shifttime=-1mon",
                  "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_wout_talk_rest2/outdata/recom/bgc22_fesom_39440101.nc")
    } else if (F) {
        args <- c("meshdir=/work/ollie/cdanek/mesh/fesom/CbSCL",
                  "outdir=~/test",
                  "~/test/temp.nc")
    }
} else { # if not interactive
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    #./fesom1_nod3d_levelwise_fast.r meshdir=/pool/data/AWICM/FESOM1/MESHES/core outdir=/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata shifttime=-1day /work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom/thetao_fesom_20140101.nc > levelwise.log 2>&1 &
}

usage <- paste0("\nUsage:\n $ ", me, 
                " meshdir=/path/to/mesh outdir=/path/to/save/result [shifttime=-1d] file1 [file2 filen]\n",
                "\n",
                "e.g. meshdir=/pool/data/AWICM/FESOM1/MESHES/core\n")

# check
if (length(args) < 3) { # meshdir, outdir, file1
    message(usage)
    quit()
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

if (length(args) == 0) {
    message(usage)
    quit()
}
files <- args
message("\n", length(files), " files:")
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
message("check cdo ... ", appendLF=F)
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")
message("ok")

warn <- options()$warn

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
nlev <- base::scan(paste0(meshdir, "/aux3d.out"), what=integer(), n=1, quiet=T)
if (data.table_check) {
    nod3d <- data.table::fread(paste0(meshdir, "/nod3d.out"), skip=1, showProgress=F)
    nod3d <- as.matrix(nod3d)
    aux3d <- data.table::fread(paste0(meshdir, "/aux3d.out"), skip=1, nrows=n2*nlev, 
                               na.strings="-999", showProgress=F)
    aux3d <- matrix(aux3d$V1, nrow=nlev, ncol=n2)
} else { # use base::scan
    nod3d <- base::scan(paste0(meshdir, "/nod3d.out"), skip=1, quiet=T)
    nod3d <- matrix(nod3d, nrow=n3, byrow=T)
    aux3d <- base::scan(paste0(meshdir, "/aux3d.out"), skip=1, nlines=n2*nlev, 
                        na.strings="-999", quiet=T)
    aux3d <- matrix(aux3d, nrow=nlev, ncol=n2)
}
depth <- drop(nod3d[,4])
depth <- abs(unique(depth)) # model depths in m; negative downwards 
message("--> n2 = ", n2, "\n",
        "--> n3 = ", n3, "\n",
        "--> nlev = ", nlev, "\n",
        "--> depths = ", paste(depth, collapse=", "))

# prepare output
n2_dim <- ncdf4::ncdim_def(name="ncells", units="", vals=seq_len(n2), create_dimvar=F) # from spheRlab::sl.grid.FESOM3Ddata1Dto2D.R
depth_dim <- ncdf4::ncdim_def(name="depth", units="m", vals=depth)

message("\nconvert ", length(files), " files from nod3d to levelwise ...\n")
elapsed <- c()
cnt <- 0
n3dimname <- NULL
for (fi in seq_along(files)) {
    message("******************************************************\n",
            "file ", fi, "/", length(files), ": ", files[fi])
    if (!file.exists(files[fi])) {
        message("file does not exist. skip")
    } else {
        suffix <- tools::file_path_sans_ext(basename(files[fi]))
        file_ext <- tools::file_ext(files[fi])
        ofile <- paste0(outdir, "/", suffix, "_levelwise.", file_ext)
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
            
            # step1: select nodes from current level; they are all non-NA and of different length per level
            message("\nstep1: select and save nodes from ", nlev, " levels ...")
            ofiles_lev_woutNA <- rep(NA, t=nlev)
            for (li in seq_len(nlev)) {
                inds <- aux3d[li,]
                #print(summary(inds))
                if (all(is.na(inds))) {
                    message("all nodes of level ", li, "/", nlev, " are NA. skip")
                } else { # some non-NA values
                    ofiles_lev_woutNA[li] <- paste0(tools::file_path_sans_ext(ofile), "_lev_", li, "_woutNA.nc")
                    cmd <- paste0(ncks, 
                                  " -d ", n3dimname, ",", min(inds, na.rm=T)-1, ",", max(inds, na.rm=T)-1, # -1 since ncks starts counting from zero
                                  " ", files[fi], " ", ofiles_lev_woutNA[li])
                    message("lev ", li, "/", nlev, ": run `", cmd, "` ...")
                    system(cmd) # todo: what happens if file has no variable with nod3d dim
                } # if current level has non-NA values
            } # for li nlev
            
            # step2: load all non-NA nodes of current level and save as array of dim (n2,ndepth,all other dims), 
            # filling additional values in n2-dim with NA
            nonNAinds <- which(!is.na(ofiles_lev_woutNA))
            if (length(nonNAinds) != length(depth)) stop("this should not happen")
            message("\nstep2: load all non-NA nodes of ", length(nonNAinds),
                    " non-NA levels and save on ", n2, " nodes per level ...")
            for (li in seq_along(nonNAinds)) {
                ind <- nonNAinds[li]
                nc <- ncdf4::nc_open(ofiles_lev_woutNA[ind])
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
                    if (length(ncvars) == 0) stop("not a single variable has nod3d dim. thists
                                                  case is not implemented")
                    ncout <- ncdf4::nc_create(ofile, ncvars, force_v4=T)
                    ncdf4::ncatt_put(ncout, "depth", "positive", "down")
                    ncdf4::ncatt_put(ncout, "depth", "axis", "Z")
                } # if li == 1
                okinds <- which(!is.na(aux3d[ind,])) # e.g. 1:119130
                for (vi in seq_along(ncvars)) {
                    if (li == 1) message("var ", vi, "/", length(ncvars), ": ", ncvars[[vi]]$name, "\nlev ", appendLF=F) 
                    message(li, " ", appendLF=F)
                    arr_woutNA <- ncdf4::ncvar_get(nc, ncvars[[vi]]$name, collapse_degen=F) # load data of current level wout NA; keep dims of length 1
                    var_dims <- dim(arr_woutNA) # e.g. (119130,12); n2=126859
                    nod_ind <- which(var_dims == length(okinds)) # in this loop all variables have nod3d dim
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
                    ncdf4::ncvar_put(ncout, ncvars[[vi]], vals=arr_wNA, 
                                     start=start, count=count)
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
                    if (li == length(nonNAinds)) message()
                } # for vi
                ncdf4::nc_close(nc)
            } # for li depth
            message("\nsave ", ofile, " ...")    
            ncdf4::nc_close(ncout)

            # rm tmp files
            invisible(file.remove(ofiles_lev_woutNA))

        } # if fout already exists or not
    } # if fin exists or not
} # for fi

message("\nfinished\n")

