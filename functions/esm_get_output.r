#!/usr/bin/env Rscript
    
rm(list=ls()); graphics.off()

models <- libpaths <- NULL

if (interactive()) {
    me <- "esm_get_output.r"
    #expidpath <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/hist"
    #outname <- "pi_old.ods"
    #expidpath <- "/work/ab0246/a270073/awicm-CMIP6/restart_from_restart_from_hu_oceanonly"
    #outname <- "restart_from_restart_from_hu_oceanonly.ods"
    #expidpath <- "/work/ab0246/a270073/awicm-CMIP6/PI-CTRL"
    #expidpath <- "/work/ab0246/a270073/awicm-CMIP6/PI-CTRL_nodynveg"
    #outname <- "pi_output.txt"
    #outname <- "pi_output.ods"
    #expidpath <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL4"
    #outname <- "PI-CTRL4_output.ods"
    #expidpath <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6"
    #outname <- "PI-CTRL6_output.ods"
    #expidpath <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/hist"
    #expidpath <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical"
    #outname <- "hist_output.txt"
    #outname <- "awi-esm-1-1-lr_historical_output.ods"
    #expidpath <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/hist2"
    #outname <- "hist2_output.txt"
    #outname <- "hist2_output.ods"
    #expidpath <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL5"
    #outname <- "PI-CTRL5_output.ods"
    #expidpath <- "/work/ollie/cdanek/out/awicm-1.0-recom/test4"
    #outname <- "output_awicm-recom.ods"
    #expidpath <- "/work/ollie/cdanek/out/echam-6.3.04p1/pictrl-grb"
    #outname <- "pictrl-grb.ods"
    #outname <- "pictrl-grb.txt"
    #expidpath <- "/work/ollie/cdanek/out/echam-6.3.04p1/pictrl-spinup-grb"
    #outname <- "pictrl-spinup-grb.ods"
    #outname <- "pictrl-spinup-grb.txt"
    #expidpath <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/hist_an"
    #outname <- "~/awicm1-recom_output_hist.ods"
    #expidpath <- "/work/ba1103/a270094/AWIESM/test"
    #outname <- "~/awicm1-recom_piControl_og_output.ods"
    #expidpath <- "/work/ba1103/a270073/out/mpiesm-s/test"
    #year <- 1860
    #outname <- "~/mpiesm-s_output.ods"
    expidpath <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl"
    year <- 2850
    outname <- "~/awicm1-recom_piControl_output.ods"

    #models <- c("echam6", "jsbach", "fesom", "recom") # recom and fesom double
    #models <- c("echam6", "jsbach", "fesom")
    #models <- "echam6"
    #models <- "fesom"
    models <- "jsbach"
    #models <- "recom"
    #models <- c("echam6", "jsbach")
    #models <- c("fesom", "recom")

# not interactive
} else {
    
    #./get_esm_output_vars.r /work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/hist ~/cmip6/hist_output.ods > log 2>&1 &
    
    me <- basename(sub(pattern=".*=", replacement="", commandArgs()[4]))
    usage <- paste0("\nUsage: ", me, " arg1 arg2 [--models=] [--libpaths=] or\n",
                    "       ", me, " arg1 arg2 [--models=] [--libpaths=] > output.log 2>&1 &\n",
                    "\n",
                    "   unnamed arg1: /path/that/contains/outdata/dir\n",
                    "   unnames arg2: year\n",
                    "   unnamed arg3: filename/of/output_table.ods (must have ending \".ods\", \".xlsx\" or \".txt\")\n",
                    "   optional named arg --models=models,to,include,string,seperated,by,comma (e.g. echam6,jsbach,fesom,recom)\n",
                    "   optional named arg --libpaths=/add/path/where/R/packages/are/installed,/separated/by/comma/if/more/than/one\n")
    args <- commandArgs(trailingOnly=T)
    if (length(args) < 2) {
        message(usage)
        quit()
    }
    expidpath <- args[1]
    year <- args[2]
    outname <- args[3]
    
    # check models if provided
    if (any(grepl("--models", args))) { # user provided models
        models <- sub("--models=", "", args[grep("--models=", args)])
        models <- strsplit(models, ",")[[1]]
    }

    # check libpaths if provided 
    if (any(grepl("--libpaths", args))) {
        libpaths <- sub("--libpaths=", "", args[grep("--libpaths=", args)])
        libpaths <- strsplit(libpaths, ",")[[1]]
    }

} # if interactive or not

## defaults
cdo_silent <- F
check_nml <- T

# known models
known_models <- c("echam", "jsbach", "fesom", "recom") # known models so far

# known variables to throw out
known_rm_vars <- vector("list", l=length(known_models))
names(known_rm_vars) <- known_models
known_rm_vars[["echam"]] <- c("hyai", "hyam", "hybi", "hybm")
known_rm_vars[["jsbach"]] <- known_rm_vars[["echam"]]

# known dimensions
known_dims <- vector("list", l=length(known_models))
names(known_dims) <- known_models
known_dims[["echam"]] <- c("stream", "operator", "nml_entry", "code", "table", "time", "lon", "lat", "lev", "plev", "nsp", "nc2", "soil_layer")
known_dims[["jsbach"]] <- c("stream", "operator", "nml_entry", "code", "table", "time", "lon", "lat", "depth", "lev", "tiles", "soil_layer", "belowsurface")
known_dims[["fesom"]] <- c("time", "nodes", "nodes_2d", "nodes_3d", "depth")
known_dims[["recom"]] <- known_dims[["fesom"]]
# add common dimnames
for (di in seq_along(known_dims)) {
    known_dims[[di]] <- c("interval", known_dims[[di]], "longname", "unit", "file")
}
known_monthly_intervals <- data.frame(rbind(c(1, "mon"),
                                            c(28, "day"), 
                                            c(29, "day"),
                                            c(30, "day"),
                                            c(31, "day"),
                                            c(112, "6hr"),
                                            c(116, "6hr"),
                                            c(120, "6hr"),
                                            c(124, "6hr"),
                                            c(248, "3hr"),
                                            c(672, "hr"),
                                            c(696, "hr"),
                                            c(720, "hr"),
                                            c(743, "hr"),
                                            c(744, "hr")),
                                      stringsAsFactors=F)
colnames(known_monthly_intervals) <- c("ntime", "interval")
known_annual_intervals <- data.frame(rbind(c(1, "yr"),
                                           c(12, "mon"),
                                           c(365, "day"),
                                           c(366, "day"),
                                           c(1459, "6hr"),
                                           c(1460, "6hr"),
                                           c(1463, "6hr"),
                                           c(1464, "6hr"),
                                           c(2919, "3hr"),
                                           c(2920, "3hr"),
                                           c(2927, "3hr"),
                                           c(2928, "3hr"),
                                           c(8759, "hr"),
                                           c(8760, "hr"),
                                           c(8783, "hr"),
                                           c(8784, "hr")),
                                     stringsAsFactors=F)
colnames(known_annual_intervals) <- c("ntime", "interval")
known_intervals <- data.frame("interval"=unique(c(known_monthly_intervals$interval,
                                                  known_annual_intervals$interval)),
                              stringsAsFactors=F)
known_intervals$order <- rep(NA, t=length(known_intervals$interval))
known_intervals$echam6_time <- rep(NA, t=length(known_intervals$interval))
known_intervals$echam6_interval <- rep(NA, t=length(known_intervals$interval))
for (i in seq_along(known_intervals$interval)) {
    if (known_intervals$interval[i] == "hr") {
        known_intervals$order[i] <- 1
        known_intervals$echam6_time[i] <- "1"
        known_intervals$echam6_interval[i] <- "hours"
    } else if (known_intervals$interval[i] == "3hr") {
        known_intervals$order[i] <- 2
        known_intervals$echam6_time[i] <- "3"
        known_intervals$echam6_interval[i] <- "hours"
    } else if (known_intervals$interval[i] == "6hr") {
        known_intervals$order[i] <- 3
        known_intervals$echam6_time[i] <- "6"
        known_intervals$echam6_interval[i] <- "hours"
    } else if (known_intervals$interval[i] == "day") {
        known_intervals$order[i] <- 4
        known_intervals$echam6_time[i] <- "1"
        known_intervals$echam6_interval[i] <- "days"
    } else if (known_intervals$interval[i] == "mon") { 
        known_intervals$order[i] <- 5
        known_intervals$echam6_time[i] <- "1"
        known_intervals$echam6_interval[i] <- "months"
    } else if (known_intervals$interval[i] == "yr") { 
        known_intervals$order[i] <- 6
        known_intervals$echam6_time[i] <- "1"
        known_intervals$echam6_interval[i] <- "years"
    } else {
        stop("output interval \"", known_intervals$interval[i], "\" not defined")
    }
}
# sort intervals
known_intervals <- known_intervals[sort(known_intervals$order, index.return=T, decreasing=F)$ix,]
                
cdo_known_codetables <- c("echam4", "echam5", "echam6", "mpiom1", "ecmwf", "remo",
                          "cosmo002", "cosmo201", "cosmo202", "cosmo203", "cosmo205", "cosmo250")

## checks

# dependency: ~/scripts/r/myfunctions.r:cdo_get_filetype()
if (!file.exists("~/scripts/r/functions/myfunctions.r")) {
    stop("dependency ~/scripts/r/functions/myfunctions.r not found")
} else {
    source("~/scripts/r/functions/myfunctions.r")
}

expidpath <- normalizePath(expidpath, mustWork=T) # error if not exist
if (!dir.exists(paste0(expidpath, "/outdata"))) {
    stop("directory `expidpath`/outdata = \"", expidpath, "/outdata\" does not exist")
}
expid <- basename(expidpath)

if (is.null(models)) { # not provided by user
    message("`models` not provided --> use dirs in `expidpath`/outdata ...")
    avail_models <- list.dirs(paste0(expidpath, "/outdata"), full.names=F, recursive=F) # only basenames
    models <- avail_models
}

# check models
if (any(is.na(match(models, known_models)))) {
    inds <- which(is.na(match(models, known_models)))
    warning("`models` = \"", paste(models[inds], collapse="\", \""), "\" unknown")
    models <- models[-inds]
}
if (length(models) == 0) {
    stop("found zero known models in `expidpath`/outdata = \"", expidpath, "/outdata\"")
}

if (!is.null(libpaths)) { # check user provided libpaths
    for (i in seq_along(libpaths)) {
        if (!dir.exists(libpaths[i])) {
            warning("provided libpath \"", libpaths[i], "\" does not exist. skip")
            libpaths[i] <- NA
        }
    }
    libpaths <- libpaths[!is.na(libpaths)]
    if (length(libpaths) > 0) { # put user paths before system paths
        libpaths <- c(libpaths, .libPaths())
    } else {
        libpaths <- .libPaths() # default
    }
} else {
    libpaths <- .libPaths() # default
}

outpath <- dirname(outname)
if (file.access(outpath, mode=0) == -1) { # not existing
    message("outpath = dirname(outname) = \"", outpath, "\" not existing --> try to create")
    dir.create(outpath, recursive=T, showWarnings=F)
}
if (file.access(outpath, mode=2) == -1) { # not writable
    stop("outpath = dirname(outname) = \"", outpath, "\" not writeable")
}
outpath <- normalizePath(outpath)
outname <- basename(outname)

namelists <- list("namelist.echam"=paste0(expidpath, "/config/echam/namelist.echam"))
op_patterns <- list(detect=c( "mean", "inst", "min", "max", "asis", "none", "sqrmean"),
                    meaning=c("mean", "inst", "min", "max", "asis", "none", "sqrmean"))
not_mentioned_nml_entry <- "not mentioned"

# Start
tic <- Sys.time()
message("##### ", me, " started #####")
message("Now its ", tic)
message("models = ", paste(models, collapse=", "))
message("expidpath = ", expidpath)
message("expid = ", expid)
message("year = ", year)
message("outpath = ", outpath)
message("outname = ", outname)
message("libpaths = ", paste(libpaths, collapse=", "))

# check if already existing output table should be replaced
outputfile_names <- rep(NA, t=length(models))
for (i in seq_along(models)) {
    outputfile_names[i] <- paste0(outpath, "/", 
                                  tools::file_path_sans_ext(outname), "_", 
                                  models[i], ".", 
                                  tools::file_ext(outname))
    if (file.exists(outputfile_names[i])) {
        message("outputfile = \"", outputfile_names[i], "\" already exists.")
        cmd <- paste0("rm ", outputfile_names[i])
        message(cmd, " ...")
        system(cmd)
    } # output name already exits
} # for i models
message("--> outputfiles to be created:\n",
        paste(paste0("   ", outputfile_names), collapse="\n"))

# load packages
message("load package ncdf.tools ... ", appendLF=F)
ret <- suppressMessages(suppressWarnings(require(ncdf.tools, lib=libpaths)))
if (ret == F) {
    message()
    stop("package \"ncdf.tools\" not installed in\n",
         paste(paste0("   ", libpaths), collapse="\n"), "\n",
         "rerun the script with arg3=/absolute/path/where/R/package/ncdf.tools/is/installed\n",
         "or type `install.packages(\"ncdf.tools\")` within an open R session and then rerun this script")
} else {
    message("ok")
}
ncdf.tools_tag <- T
ncdf4_tag <- F # not yet implemented

if (tools::file_ext(outname) == "ods") {
    pkg <- "readODS"
} else if (tools::file_ext(outname) == "xlsx") {
    pkg <- "xlsx"
} else if (tools::file_ext(outname) == "txt") {
    pkg <- "gdata"
} else {
    stop("arg2 must have file extension \".ods\", \".xlsx\" or \".txt\"")
}
message("outname = \"", outname, "\" --> load package ", pkg, " ... ", appendLF=F)
ret <- suppressMessages(suppressWarnings(require(pkg, lib=libpaths, character.only=T)))
if (ret == F) {
    message()
    stop("package \"", pkg, "\" not installed in\n",
         paste(paste0("   ", libpaths), collapse="\n"), "\n",
         "rerun the script with arg3=/absolute/path/where/R/package/", pkg, "/is/installed\n",
         "or type `install.packages(\"", pkg, "\")` within an open R session and then rerun this script")
} else {
    message("ok")
}

table_list <- vector("list", l=length(models))
names(table_list) <- models
message("*****************")

for (i in seq_along(models)) {

    options(width=80) # default
    
    # model output path
    path <- paste0(expidpath, "/outdata/", models[i])
    message("\n*****************************************************\n",
            "model ", i, "/", length(models), ": ", models[i], "\n",
            "*****************************************************")
    
    # find model output files of year
    outfiles <- NULL 
    message("\ngrep files for `", path, "/*_", year, "*` ...")
    outfiles <- list.files(path=path, pattern=paste0("_", year), full.names=T)
    
    if (length(outfiles) > 0) { # found files
        # keep only files with ".nc", ".grb" or "" extensions
        exts <- tools::file_ext(outfiles)
        message("check ", length(outfiles), " found files for \"nc\", \"grb\" or \"\" extensions ...") 
        keepinds <- which(exts == "nc" | exts == "grb" | exts == "")
        
        if (length(keepinds) > 0) { # found valid files
            message("--> ", length(keepinds), " files have valid extensions")
            rminds <- seq_along(outfiles)[-keepinds]
            if (length(rminds) > 0) {
                message("remove ", length(rminds), " files having neither \"nc\", \"grb\" or \"\" file extensions ...")
                outfiles <- outfiles[-rminds]
            }
        
        } else if (length(keepinds) == 0) { # no valid files found
            message("--> found zero files with valid extensions --> skip to next model")
        }

    } else if (length(outfiles) == 0) { # no files found
        message("--> no files found --> skip to next model")
    }

    if (is.null(outfiles)) next # model i

    message("\n--> found ", length(outfiles), " files")
    print(head(outfiles, n=30))
    print("...")
    print(tail(outfiles, n=30))

    # filenames:
    # echam: hist_echam6_tdiagmon_185012.nc, hist_echam6_tdiagmon_185002.nc -> monthly
    # jsbach: hist_jsbach_yassoyr_185005.nc -> monthly 
    # fesom: hist_fesom_wo_18500101.nc -> annual
    
    # first check if monthly files
    message("\ncheck if model files are monthly or annual ...")
    message("check if there are monthly files with pattern \"", year, "01\", \"",
            year, "02\", etc. ...")
    inds <- vector("list", l=12)
    for (mi in seq_len(12)) {
        inds[[mi]] <- which(grepl(paste0("_", year, sprintf("%02i", mi)), outfiles))
    }

    # found same number of january and february files -> monthly
    if (length(inds[[1]]) > 0 &&
        length(inds[[1]]) == length(inds[[2]])) {
        message("found same number of files with \"01", year, "\" and \"02", year, "\" patterns")
        file_interval <- "monthly"
        # continue with december files in case of annual variables in monthly files
        outfiles <- outfiles[inds[[12]]] 
    } else { # else assume annual files
        if (length(inds[[1]]) == 0) {
            message("did not find a single file with \"01", year, "\" pattern")
        } else {
            message("did not found same number of files with \"01", year, "\" and \"02", year, "\" patterns")
        }
        file_interval <- "annual"
    }
    message("--> assume ", file_interval, " files --> is this interval correct!?")
    #stop("asd")

    # if fesom, check if doubled links exist
    # older esm versions:       'varname_fesom_date.nc'
    # newer esm versions: 'expid_varname_fesom_date.nc'
    if (length(unique(outfiles)) != length(outfiles)) {
        message("found filenames more than once, remove double entries ...")
        outfiles <- unique(outfiles)
    } # if fesom
    
    # checks finished
    dirnames <- dirname(outfiles)
    outfiles <- basename(outfiles)
    message("\nproceed with ", length(outfiles), " files")
    print(outfiles)

    options(width=1000) # increase width per line for print
    for (j in seq_along(outfiles)) {

        path <- dirnames[j]
        file <- outfiles[j]
        message("\n############################################################\n",
                models[i], " file ", j, "/", length(outfiles), ": ", file, "\n",
                "n############################################################")
        
        # try to determine stream if echam or jsbach
        stream <- NULL
        inds <- gregexpr(models[i], c("echam", "jsbach")) # add models strings that use streams here if necessary
        if (any(sapply(inds, ">", -1))) {

            message("\ncurrent model \"", models[i], "\" uses streams --> try to determine stream ...")
            # e.g. hist_echam6_accw_201412.grb 
            #      test4_195012.01_aclcim.nc 
            #      test_jsbach_jsbach_268512.grb
            #      test_jsbach_driving_1860.grb
            message("file = \"", file, "\"")
            stream <- tools::file_path_sans_ext(file) # remove ".<extension>" (no effect if no extension)
            stream <- sub(paste0(expid, "_"), "", stream) # remove "<expid>_" only once
            if (grepl(paste0(models[i], "_"), stream)) {
                stream <- sub(paste0(models[i], "_"), "", stream) # remove "<model>_" only once
                # accw_201412
                # 195012.01_aclcim
                # jsbach_268512
                # driving_1860
            }

            # try different patterns to remove
            pattern_to_remove <- paste0(year, "12.01_") # case: test4_195012.01_aclcim.nc 
            if (grepl(pattern_to_remove, file)) { 
                stream <- sub(pattern_to_remove, "", stream)
                # aclcim
            }
            pattern_to_remove <- paste0("_", year, "12") # case: hist_echam6_accw_201412.grb
            if (grepl(pattern_to_remove, file)) { 
                stream <- sub(pattern_to_remove, "", stream)
                # accw
                # jsbach
            }
            pattern_to_remove <- paste0("_", year) # case: test_jsbach_driving_1860.grb
            if (grepl(pattern_to_remove, file)) { 
                stream <- sub(pattern_to_remove, "", stream)
                # driving
            }

            message("--> stream = \"", stream, "\" ", appendLF=F)
            if (nchar(stream) == 0 || length(stream) == 0) {
                message()
                warning("could not determine stream of file \"", file, "\" (stream = \"", stream, "\"). skip.")
            } else {
                if (grepl("[[:punct:]]", stream)) { # still some special symbols left
                    # case: hist_echam6_ATM_mm_201412.nc --> echam6_ATM_mm
                    #stop("continue")
                }
                message("--> is this stream correct!?")
            }
        } # if echam jsbach
        
        # determine file type 
        message("\nget filetype ...")
        convert_to_nc_tag <- F # default
        #if (tools::file_ext(file) == "grb") { # does not cover cases of file names without ending
        ftype <- cdo_get_filetype(fin=paste0(path, "/", file))
        
        # convert from grb to nc if necessary
        if (ftype$file_type == "non-nc") {
            
            # check if cdo module is loaded
            cdo_check <- system("cdo -V", ignore.stderr=T) == 0
            if (!cdo_check) {
                cmd <- "module load cdo"
                message(cmd, " ...")
                system(cmd)
            }

            # construct cdo grb->nc conversion command 
            cmd <- "cdo "
            if (cdo_silent) cmd <- paste0(cmd, "-s ")
            
            ## find and apply code table if present
            # try 1/3: new esm tools: codes file same pattern as data file
            code_pattern <- paste0(file, ".codes") 
            message("\ncheck for .codes file with pattern \"", path, "/", code_pattern, "\" ... ", appendLF=F)
            codesfile <- list.files(path, pattern=code_pattern, full.names=T)
            message("found ", length(codesfile), " files")
            if (length(codesfile) == 0) {
                if (!is.null(stream)) { 
                    # try 2/3: old esm tools (echam/jsbach only)
                    code_pattern <- paste0("*", year, "12.01_", stream, ".codes")
                    message("check for .codes file with pattern \"", path, "/", code_pattern, "\" ... ", appendLF=F)
                    codesfile <- list.files(path, pattern=code_pattern, full.names=T)
                    message("found ", length(codesfile), " files")
                    if (length(codesfile) == 0) {
                        # try 3/3: mpi-esm standard: logs/test_jsbach_driving.codes
                        code_pattern <- paste0(expid, "_*", stream, ".codes")
                        tmppath <- paste0(dirname(dirname(path)), "/log")
                        message("check for .codes file with pattern \"", path, "/", code_pattern, "\" ... ", appendLF=F)
                        codesfile <- list.files(tmppath, pattern=glob2rx(code_pattern), full.names=T)
                        message("found ", length(codesfile), " files")
                    }
                }
            }
            if (length(codesfile) > 1) {
                warning(paste(codesfile, collapse="\n"), 
                        "\ndont know which to use --> use first --> is this first file correct?!?!")
                codesfile <- codesfile[1]
            }
            if (length(codesfile) == 1) { # 1 codes file found
                message("--> use codefile \"", codesfile, "\" for grb->nc conversion ...")
                cmd <- paste0(cmd, "-t ", codesfile, " ")
            } else if (length(codesfile) == 0) {
                if (any(models[i] == cdo_known_codetables)) {
                    message("current model is \"", models[i], " --> use its default code table")
                    cmd <- paste0(cmd, "-t ", models[i])
                } else {
                    warning("neither found .codes-file nor is models[", i, "] = ", 
                            models[i], " included in cdo default codetabels ", 
                            paste(cdo_known_codetables, collapse=", "), "\n",
                            "--> conversion to nc will probably yield silly files")
                }
            }
            
            # apply conversion
            cmd <- paste0(cmd, "-f nc copy ", path, "/", file, " ", 
                          outpath, "/", file, ".nc")
            
            # run final cdo conversion command
            message("\nconvert ", 
                    utils:::format.object_size(file.info(paste0(path, "/", file))$size, "Mb"), 
                    " file to nc ...")
            message("run `", cmd, "` ...")
            if (grepl("mlogin", Sys.info()["nodename"])) {
                stop("cannot run conversion command on mlogin. switch to mistralpp and rerun the script")
            }
            system(cmd)
            
            # new file and path names for current file
            file <- paste0(file, ".nc")
            path <- outpath
            convert_to_nc_tag <- T
        
        } # first convert to nc if .grb file

        # try to get meta info of file file
        if (ncdf.tools_tag) {
            dims <- ncdf.tools::infoNcdfDims(paste0(path, "/", file))
            vars <- ncdf.tools::infoNcdfVars(paste0(path, "/", file))
        } else if (!ncdf.tools_tag && ncdf4_tag) {
            stop("not yet")
        }
        ndims <- dim(dims)[1]
        nvars <- dim(vars)[1]
        if (ndims == 0 || nvars == 0) {
            msg <- paste0("could not determine any dim or variable name from file ", 
                          path, "/", file, ". skip to next file")
            warning(msg, .immediate=T)
            warning(msg, .immediate=F)
            message("vars:")
            print(vars)
            message("dims:")
            print(dims)
            message("skip ...")
            next # file
        }
        message("\nFound ", ndims, " dims:")
        print(dims)
        message("\nFound ", nvars, " vars:")
        print(vars)
        
        # throw out some variables
        if (any(!is.na(match(vars[,"name"], known_rm_vars[[models[i]]])))) {
            message("\nthrow out some known ", models[i], " variables to remove ...")
            for (vari_to_rm in known_rm_vars[[models[i]]]) {
                if (any(vars[,"name"] == vari_to_rm)) {
                    inds <- which(vars[,"name"] == vari_to_rm)
                    message("   \"", vari_to_rm, "\"")
                    vars <- vars[-inds,]
                }
            }
            nvars <- dim(vars)[1]
        } # if variale was found to remove
        
        # array to save all meta data per variable of file file
        tmp <- data.frame(array(NA, c(nvars, length(known_dims[[models[i]]]) + 1)), stringsAsFactors=F)
        colnames(tmp) <- c("name", known_dims[[models[i]]])

        # found variables
        tmp[,"name"] <- vars[,"name"]
        tmp[,"unit"] <- vars[,"unit"]
        tmp[,"file"] <- outfiles[j] # original, not possibly grb->nc converted
        varids <- vars[,"id"]
        if (!is.null(stream)) tmp[,"stream"] <- stream # only echam/jsbach

        # try to determine long name and code of every variable
        message("\nTry do determine variable long name and code if possible ...")
        for (k in seq_along(varids)) {
            message("var ", k, ": ", vars[k,"name"], " atts:")
            atts <- ncdf.tools::infoNcdfAtts(paste0(path, "/", file), var.id=varids[k])
            print(atts)
            ## longname
            # try "long_name"
            longname_ind <- which(atts[,"name"] == "long_name")
            if (length(longname_ind) != 0) {
                tmp[k,"longname"] <- atts[longname_ind,"value"]
                message()
                #message("found \"long_name\" attribute: ", tmp[k,"longname"])
            } else {
                # try "description"
                message("no attribute named \"long_name\" found. try \"description\" ...")
                longname_ind <- which(atts[,"name"] == "description")
                if (length(longname_ind) != 0) {
                    tmp[k,"longname"] <- atts[longname_ind,"value"]
                    #message("found \"description\" attribute: ", tmp[k,"longname"])
                } else {
                    message("no attribute named either \"long_name\" or \"description\" found for variable ", 
                            vars[k,"name"], " of outfiles[", j, "] = ", file, ". skip ...\n")
                }
            }
            # code
            code_ind <- which(atts[,"name"] == "code")
            if (length(code_ind) != 0) {
                tmp[k,"code"] <- as.integer(atts[code_ind,"value"])
            }
            # table
            table_ind <- which(atts[,"name"] == "table")
            if (length(table_ind) != 0) {
                tmp[k,"table"] <- as.integer(atts[table_ind,"value"])
            }
        }
        
        if (F) {
            ff = "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/hist/outdata/fesom/hist_fesom_wo_18500101.nc"
            fdims <- ncdf.tools::infoNcdfDims(ff)
            fvars <- ncdf.tools::infoNcdfVars(ff)
            fatts <- ncdf.tools::infoNcdfAtts(ff, var.id=1)
        }

        # try to determine dimensions of every variable
        message("\nTry to determine dims of every var ...")
        for (k in seq_len(nvars)) {
            ndims_per_var <- vars[k,"n.dims"]
            for (l in seq_len(ndims_per_var)) {
                dim_id_per_var <- vars[k,paste0("dim.id.", l)] # e.g. "time"
                dim_per_var <- dims[which(dims[,"id"] == dim_id_per_var),"name"]
                if (!is.na(dim_per_var)) {
                    if (!any(colnames(tmp) == dim_per_var)) {
                        msg <- paste0(models[i], " model dimname \"", 
                                      dim_per_var, "\" is not known yet. skip.")
                        warning(msg, .immediate=T)
                        warning(msg, .immediate=F)
                    } else {
                        if (F) message("found dim ", dim_per_var)
                        tmp[k,dim_per_var] <- dims[dims[,"name"] == dim_per_var,"length"]
                    }
                } else {
                    if (F) message("dim_per_var is NA")
                }
            } # for all l dims per var
        }
        print(tmp)

        # try to determine output interval of every variable
        message("\nTry to determine output interval of every var ...")
        for (k in seq_len(nvars)) {
            ntime_per_var <- as.integer(tmp[k,"time"])
            if (is.na(ntime_per_var)) { 
                # in this case, the saved variable in the nc file has no time dimension
                # this should not happen but it does ...
                message("variable ", vars[k,"name"], 
                        " has no time dim, e.g. X=X(lon,lat) -> only 1 step per file -> set ntime_per_var to 1")
                ntime_per_var <- 1
                tmp[k,"time"] <- 1
            }
            if (file_interval == "monthly") {
                ind <- which(as.integer(known_monthly_intervals$ntime) == ntime_per_var)
            } else if (file_interval == "annual") {
                ind <- which(as.integer(known_annual_intervals$ntime) == ntime_per_var)
            }
            if (length(ind) == 1) {
                if (file_interval == "monthly") {
                    tmp[k,"interval"] <- known_monthly_intervals$interval[ind]
                } else if (file_interval == "annual") {
                    tmp[k,"interval"] <- known_annual_intervals$interval[ind]
                }
            # output interval is not known yet
            } else {
                stop(file_interval, " output interval '", tmp[k,"time"], "' is unknown. skip.")
            }
        }
        message("determined output intervals of ", file_interval, " file:")
        print(tmp[,c("time", "interval")])

        # try to determine operator (mean, inst, min, max, etc.) of echam6 or jsbach variable based on the namelist
        if (!is.null(stream)) {
            if (check_nml) {
                message("\nstream = ", stream, " is not NULL and `check_nml` is true ...")
                if (any(models[i] == c("echam", "jsbach"))) {
                    nml_to_check <- "namelist.echam"
                } else {
                    stop("not yet")
                }
                message("--> try to get stream infos based on models[", i, "] = ", models[i], 
                        " namelist mapping name \"", nml_to_check, "\" ...")
                nmlind <- which(names(namelists) == nml_to_check)
                
                # no namelist found
                if (length(nmlind) == 0) {
                    warning("could not find this namelist mapping name \"", nml_to_check, "\" in `namelists`. skip.")
                
                # namelist found
                } else {
                    
                    # if special stream
                    if (grepl("[[:punct:]]", stream)) { # still some special symbols left
                        message("ignore this task for special stream \"", stream, "\"")
                    
                    # if not special stream
                    } else {

                        if (file.exists(namelists[[nmlind]])) {
                            
                            message("read namelist ", namelists[[nmlind]], " ...")
                            nml <- readLines(namelists[[nmlind]])
                            
                            # for all variables in file
                            for (vari in seq_len(nvars)) {
                                
                                # check for so far known cases:
                                var2check <- tmp[vari,"name"]
                                code2check <- tmp[vari,"code"]
                                var_patterns <- c(paste0("'", var2check, "'"), # "'temp2'"
                                                  paste0("'", var2check, ":"), # "'temp2:"
                                                  paste0(">", var2check, "=")) # ">temp2="
                                determined_varpatterns <- rep(F, t=length(var_patterns))
                                determined_codepatterns <- determined_varpatterns
                                names(determined_varpatterns) <- var_patterns
                                code_patterns <- c(paste0("'", code2check, "'"), # "'temp2'"
                                                   paste0("'", code2check, ":"), # "'temp2:"
                                                   paste0(">", code2check, "=")) # ">temp2="
                                names(determined_codepatterns) <- code_patterns

                                message("\nTry do find stream \"", stream, "\" variable ", vari, "/", nvars, ": \"", 
                                        var2check, "\" (interval: ", tmp[vari,"interval"], ", code: ", code2check, 
                                        ") in whole nml based on var_patterns:")
                                
                                for (varpatterni in seq_along(var_patterns)) {
                                    message("   ", varpatterni, "/", length(var_patterns), ": \"", 
                                            var_patterns[varpatterni], "\" ", appendLF=F)
                                    if (any(regexpr(var_patterns[varpatterni], nml) != -1)) {
                                        message("-> yes")
                                        determined_varpatterns[varpatterni] <- T
                                    } else {
                                        message("-> no")
                                    }
                                }

                                # none of the above cases were found
                                if (all(determined_varpatterns == F)) {
                                    if (!is.na(code2check)) {
                                        message("None of the var_patterns \"", 
                                                paste0(var_patterns, collapse=paste0("\", \"")), 
                                                "\" were found in the namelist\n",
                                                namelists[[nmlind]], "\n",
                                                "Try with code_patterns ...")
                                        for (codepatterni in seq_along(code_patterns)) {
                                            message(codepatterni, "/", length(code_patterns), ": \"", 
                                                    code_patterns[codepatterni], "\" ", appendLF=F)
                                            if (any(regexpr(code_patterns[codepatterni], nml) != -1)) {
                                                message("-> yes")
                                                determined_codepatterns[codepatterni] <- T
                                            } else {
                                                message("-> no")
                                            }
                                        }
                                    }
                                }

                                # none of the above var and code cases were found
                                if (all(determined_varpatterns == F) && all(determined_codepatterns == F)) {
                                    # try special case: the nml entries "'temp2', 'temp2:inst'" yield the 2 variables "temp2" and "temp2_2"
                                    if (substr(var2check, nchar(var2check) - 1, nchar(var2check)) == "_2") {
                                        message("Found special case: the last 2 characters of var2check are \"_2\".\n",
                                                "This was done automatically by echam6 if, e.g., the nml entry \"'temp2', 'temp2:inst'\"",
                                                " was present in the nml.\n",
                                                "Adopt the var_patterns ...")
                                        var2check <- substr(var2check, 1, nchar(var2check)-2)
                                        var_patterns <- c(paste0("'", var2check, "'"), # "'temp2'"
                                                          paste0("'", var2check, ":"), # "'temp2:"
                                                          paste0(">", var2check, "=")) # ">temp2="
                                        names(determined_varpatterns) <- var_patterns
                                        for (varpatterni in seq_along(var_patterns)) {
                                            message(varpatterni, "/", length(var_patterns), ": \"", 
                                                    var_patterns[varpatterni], "\" ", appendLF=F)
                                            if (any(regexpr(var_patterns[varpatterni], nml) != -1)) {
                                                message("-> yes")
                                                determined_varpatterns[varpatterni] <- T
                                            } else {
                                                message("-> no")
                                            }
                                        }
                                    }
                                } # special "'temp2', 'temp2:inst'" --> "temp2" "temp2_2" case

                                # none of the above var and code cases were found
                                if (all(determined_varpatterns == F) && all(determined_codepatterns == F)) {
                                    message("Found not a single pattern match in whole nml\n",
                                            " -> could not determine operator.\n",
                                            "assume nml_entry = \"", not_mentioned_nml_entry, "\"\n",
                                            "Skip to next variable ...")
                                    tmp[vari,"nml_entry"] <- not_mentioned_nml_entry
                                
                                # any of "runoff'" "'runoff:" ">runoff=" was found 
                                } else if (any(determined_varpatterns)) {

                                    # loop through all found patterns
                                    found_varpatterns <- determined_varpatterns[-which(determined_varpatterns == F)]
                                    op_per_pattern <- rep(NA, t=length(found_varpatterns))
                                    nml_entry_per_pattern <- op_per_pattern
                                    for (patterni in seq_along(found_varpatterns)) {
                                        
                                        # check stupid case
                                        #if (tmp[vari,"name"] == "lsp_2" && tmp[vari,"interval"] == "6hr") stop("asd")
                                        if (paste0(var2check, "_2") == paste0(tmp[vari,"name"])) { # lsp_2 was replaced by lsp
                                            if (length(found_varpatterns) == 2) { # e.g. 'lsp' 'lsp:
                                                if (patterni == 1) {
                                                    message("Special case for var \"", var2check, "\"",
                                                            " (actually tmp[", vari, ",\"name\"] = \"", tmp[vari,"name"], "\").\n",
                                                            "Use var_pattern ", patterni+1, " \"", 
                                                            names(found_varpatterns)[patterni+1], "\" instead of the current var_pattern \"",
                                                            names(found_varpatterns)[patterni], "\".")
                                                    next # use the pattern
                                                }
                                            } # only 2 cases defined yet
                                        } # special case
                                        
                                        var_pattern <- names(found_varpatterns)[patterni]
                                        #if (tmp[vari,"name"] == "lsp_2" && tmp[vari,"interval"] == "6hr") stop("asd")
                                        time_echam <- known_intervals[which(known_intervals[,"interval"] == 
                                                                            tmp[vari,"interval"]),"echam6_time"]
                                        interval_echam <- known_intervals[which(known_intervals[,"interval"] == 
                                                                                tmp[vari,"interval"]),"echam6_interval"]
                                        interval_pattern <- paste0(time_echam, ",'", interval_echam, "',") # 6, 'hours',
                                        stream_pattern <- paste0("'", stream, "'")

                                        # find all occurences of found pattern
                                        var_pattern_inds <- grep(var_pattern, nml)
                                        for (linei in seq_along(var_pattern_inds)) {

                                            if (!is.na(op_per_pattern[patterni])) {
                                                break # this loop trying to find matching var_patterns in nml blocks
                                            }

                                            # find all arguments of current block in the namelist: 
                                            # -> find first occurences of "&" before and "/" after current line containing the pattern "temp2"
                                            upper_et_ind <- lower_slash_ind <- NA
                                            cnt <- 0
                                            while (is.na(upper_et_ind)) {
                                                ind <- var_pattern_inds[linei] - cnt # also include same line
                                                if (F) message("nml[ind] = \"", nml[ind], "\"")
                                                if (length(grep("&", nml[ind])) == 1) {
                                                    upper_et_ind <- ind
                                                }
                                                # if not found and top of nml reached:
                                                if (ind == 1 && is.na(lower_slash_ind)) {
                                                    lower_slash_ind <- 1
                                                }
                                                cnt <- cnt + 1
                                            }
                                            cnt <- 0
                                            while (is.na(lower_slash_ind)) {
                                                ind <- var_pattern_inds[linei] + cnt # also include same line
                                                if (F) message("nml[ind] = \"", nml[ind], "\"")
                                                if (substr(nml[ind], 1, 1) == "/") {
                                                    lower_slash_ind <- ind
                                                }
                                                # if not found and end of nml reached:
                                                if (ind == length(nml) && is.na(lower_slash_ind)) {
                                                    lower_slash_ind <- length(nml)
                                                }
                                                cnt <- cnt + 1
                                            }

                                            # check if found nml block begins with &mvstreamctl
                                            if (trimws(substr(nml[upper_et_ind], 1, 12), which="left") == "&mvstreamctl") {

                                                # check if the current nml block is the correct one depending on
                                                # current variable and its output frequency
                                                current_block <- nml[upper_et_ind:lower_slash_ind]
                                                message("\nCheck if current &mvstreamctl nml block nml[", 
                                                        upper_et_ind, ":", lower_slash_ind, "] containing var_pattern ", 
                                                        patterni, "/", length(found_varpatterns), ": \"", var_pattern, "\" also contains\n",
                                                        "the interval_pattern \"", interval_pattern, "\"\n",
                                                        "basename(outfiles[", j, "]) = ", basename(outfiles[j]), "...")
                                                message(paste(current_block, collapse="\n"))
                                              
                                                # 1st check: current interval_pattern not found in current nml block
                                                interval_inds <- regexpr(interval_pattern, gsub(" ", "", current_block))
                                                if (!any(interval_inds != -1)) {
                                                    message("interval_pattern \"", interval_pattern, 
                                                            "\" not found in this var_pattern ", patterni, "/", 
                                                            length(found_varpatterns), " \"", var_pattern, "\" nml block.")
                                                    if (linei == length(var_pattern_inds)) {
                                                        message("Reached end of nml.")
                                                        message("Did not find the combination of \"", stream, "\" \"", 
                                                                tmp[vari,"interval"], "\" var_pattern ", patterni, "/", 
                                                                length(found_varpatterns), ": \"", 
                                                                var_pattern, "\" in the whole nml.\n")
                                                        if (patterni == length(found_varpatterns)) {
                                                            message("Reached end of found_varpatterns\n",
                                                                    "Assume nml_entry = \"", not_mentioned_nml_entry, "\"")
                                                            tmp[vari,"nml_entry"] <- not_mentioned_nml_entry
                                                        }
                                                    }

                                                # current interval_pattern found in current nml block
                                                } else {

                                                    # 2nd check: current stream_pattern not found in current nml block
                                                    stream_inds <- regexpr(stream_pattern, gsub(" ", "", current_block))
                                                    # apply this condition only in special cases
                                                    #if (stream == "echamdaymax") stop("asd")
                                                    if (any(stream == c("echamdaymin", "echamdaymax")) && 
                                                        !any(stream_inds != -1)) { 
                                                        message("Found but apply special condition: stream_pattern \"", stream_pattern, 
                                                                "\" not found in this var_pattern ", patterni, "/", 
                                                                length(found_varpatterns), " \"", var_pattern, "\" nml block.")
                                                        if (linei == length(var_pattern_inds)) {
                                                            message("Reached end of nml.")
                                                            message("Did not find the combination of \"", stream, "\" \"", 
                                                                    tmp[vari,"stream"], "\" var_pattern ", patterni, "/",
                                                                    length(found_varpatterns), ": \"", 
                                                                    var_pattern, "\" in the whole nml. Skip to next variable.")
                                                        }
                                            
                                                    # current stream_pattern found in current nml block
                                                    } else { 

                                                        message("Success\n",
                                                                "-> Assume that this is the correct \"", 
                                                                tmp[vari,"interval"], "\" \"", stream, "\" \"", var2check, 
                                                                "\" block in the nml -> is this output frequency-varname-stream block correct?")

                                                        
                                                        op <- NA # default in this block

                                                        #if (stream == "echamdaymax") stop("asd")

                                                        # case1: "'runoff'" 
                                                        if (var_pattern == paste0("'", var2check, "'")) {
                                                            
                                                            testpattern <- paste0("'", var2check, "'")
                                                            message("check case 1 testpattern = \"", testpattern, "\" Found!")
                                                            nml_entry <- testpattern
                                                            op <- "mean"

                                                        # case 2: "'runoff:" "'temp2:inst"
                                                        } else if (var_pattern == paste0("'", var2check, ":")) {

                                                            # check operator cases
                                                            for (opi in seq_along(op_patterns$detect)) {
                                                                testpattern <- paste0(var_pattern, op_patterns$detect[opi]) 
                                                                message("check case 2 testpattern = \"", testpattern, "\" ... ", appendLF=F)
                                                                testpattern_ind <- which(regexpr(testpattern, current_block) != -1)
                                                                if (length(testpattern_ind) == 1) {
                                                                    nml_entry <- current_block[which(regexpr(testpattern, current_block) != -1)]
                                                                    # e.g. "\t'aps:inst', 'geosp', 'lsp', 'lsp:inst', 'q2m:inst', 'q:inst',"
                                                                    nml_entry_fromind <- regexpr(testpattern, nml_entry)
                                                                    # find first "'" after nml_entry_ind
                                                                    prime_inds <- gregexpr("'", nml_entry)[[1]]
                                                                    if (!any(prime_inds != -1)) stop("this should not happen")
                                                                    nml_entry_toind <- which(prime_inds > nml_entry_fromind)
                                                                    nml_entry_toind <- prime_inds[nml_entry_toind[1]]
                                                                    nml_entry <- substr(nml_entry, nml_entry_fromind, nml_entry_toind)
                                                                    message("Found!")
                                                                    op <- op_patterns$meaning[opi]
                                                                    break # out of this testpattern for opi loop
                                                                } else {
                                                                    message("Not found. ", appendLF=F)
                                                                }
                                                                if (opi == length(op_patterns$detect)) {
                                                                    message("Reached end of op_patterns$detect to check.")
                                                                } else {
                                                                    message("Try next pattern ...")
                                                                }
                                                            }

                                                        # case 3: ">runoff=" 'temp2:inst>temp2_pt=167'
                                                        } else if (var_pattern == paste0(">", var2check, "=")) {
                                                               
                                                            # get detected nml entry with testpattern
                                                            testpattern <- var_pattern 
                                                            nml_entry <- current_block[which(regexpr(testpattern, current_block) != -1)]
                                                            # e.g. "\tvariables = 'dmc', 'irld>rld=3', 'irldcs>rldcs=7', 'irlu>rlu=1',"
                                                            nml_entry_ind <- regexpr(testpattern, nml_entry)
                                                            # find first "'" before and after nml_entry_ind
                                                            prime_inds <- gregexpr("'", nml_entry)[[1]]
                                                            if (!any(prime_inds != -1)) stop("this should not happen")
                                                            nml_entry_fromind <- which(prime_inds <= nml_entry_ind)
                                                            nml_entry_fromind <- prime_inds[nml_entry_fromind[length(nml_entry_fromind)]]
                                                            nml_entry_toind <- which(prime_inds >= nml_entry_ind)
                                                            nml_entry_toind <- prime_inds[nml_entry_toind[1]]
                                                            nml_entry <- substr(nml_entry, nml_entry_fromind, nml_entry_toind)

                                                            # if any ":mean>aclcov=" ":inst>aclcov=" ":min>aclcov=" ":max>aclcov="
                                                            # else: ">aclcov=" -> mean
                                                            message("check case 3 nml_entry = \"", nml_entry, "\" for ") 
                                                            for (opi in seq_along(op_patterns$detect)) {
                                                                testpattern2 <- paste0(":", op_patterns$detect[opi], testpattern)
                                                                message("check case 3 testpattern2 = \"", testpattern2, "\" ... ", appendLF=F)
                                                                if (regexpr(testpattern2, nml_entry) != -1) {
                                                                    message("Found!")
                                                                    op <- op_patterns$meaning[opi]
                                                                    break # out of this testpattern2 for opi loop
                                                                } else {
                                                                    message("Not found. ", appendLF=F)
                                                                    if (opi == length(op_patterns$detect)) {
                                                                        message("Reached end of op_patterns$detect to check.")
                                                                        message(" -> Assume operator is \"mean\"")
                                                                        op <- "mean"
                                                                    } else {
                                                                        message("Try next pattern ...")
                                                                    }
                                                                }
                                                            }
                                                        } # which case

                                                        if (is.na(op)) {
                                                            stop("None of the above testpatterns were detected for this var_pattern ", 
                                                                    patterni, "/", length(var_patterns), ": \"", var_pattern, "\" nml block.")
                                                        } else {
                                                            message("Determined nml_entry: \"", 
                                                                    nml_entry, "\" -> is this nml entry correct?")
                                                            message("Determined op: \"", 
                                                                    op, "\" -> is this operation correct?")
                                                            nml_entry_per_pattern[patterni] <- nml_entry
                                                            op_per_pattern[patterni] <- op
                                                        }
                                                    } # if current stream_pattern was found in current nml block
                                                } # if current interval_pattern was found in current nml block 
                                            }  # if &mvstreamctl nml block
                                        } # for all found linei var2check "temp2" occurences in whole nml
                                    } # for all found patterni var_patterns, e.g. "'lsp'" "'lsp:"
                                    #if (stream == "echamdaymax") stop("asd")

                                    message()
                                    
                                    # any testpattern was successful
                                    if (any(!is.na(op_per_pattern))) {
                                        
                                        # if there are more than 1 testpatterns associated with the current variable 
                                        if (length(which(!is.na(op_per_pattern))) > 1) {

                                            message("There are ", length(which(!is.na(op_per_pattern))), 
                                                    " found var_pattern for the \"", tmp[vari,"interval"], 
                                                    "\" \"", stream, "\" variable \"", var2check, "\".")
                                            message("nml_entry_per_pattern:")
                                            print(nml_entry_per_pattern)
                                            message("op_per_pattern:")
                                            print(op_per_pattern)
                                            message("vars[,\"name\"]:")
                                            print(vars[,"name"])
                                            
                                            # special case: nml_entry "'tau_aero_550:mean>tau_aero_550=11'"
                                            # was found more than once in one nml block due to two patterns
                                            # were found
                                            if (length(unique(nml_entry_per_pattern)) == 1) {
                                                message("\nAll found nml entries are the same")
                                                op_ind <- 1

                                            # decide about special cases when the same variable was
                                            # written into the same file with different operators (e.g. mean and inst)
                                            } else { 

                                                # find all occurences of varname in file
                                                varinds_per_file <- grep(var2check, tmp[,"name"])
                                                # more than 1 nml entries and more than 1 fitting variables in file
                                                if (length(varinds_per_file) > 1) {
                                                    # e.g. "lsp" "lsp_2"
                                                    vars_per_file <- tmp[,"name"][varinds_per_file]
                                                    current_var_ind <- which(vars_per_file == var2check)
                                                    if (length(current_var_ind) == 1) {
                                                        message("\nCurrent var2check is \"", var2check, "\": identified the ", 
                                                                current_var_ind, "th case:")
                                                        op_ind <- current_var_ind
                                                    } else {
                                                        stop("Could not identify which variable is the current one. ",
                                                             "Solve this case by hand.")
                                                    }

                                                # more than 1 nml entries but only 1 fitting variable in file
                                                } else {
                                                    message("\nHowever, there is only 1 entry of var2check \"", 
                                                            var2check, "\" in the current file.\n",
                                                            "Try to find special operator patterns that match ...")
                                                   
                                                    var2check_with_op <- rep(F, t=length(op_patterns$detect))
                                                    for (opi in seq_along(op_patterns$detect)) {
                                                        if (regexpr(op_patterns$detect[opi], var2check) != -1) {
                                                            message("found op ", op_patterns$detect[opi], 
                                                                    " in var2check ", var2check, ".") 
                                                            var2check_with_op[opi] <- T
                                                        }
                                                    }
                                                    nml_entry_per_pattern_with_op <- vector("list", l=length(nml_entry_per_pattern))
                                                    names(nml_entry_per_pattern_with_op) <- nml_entry_per_pattern
                                                    for (nml_entry_i in seq_along(nml_entry_per_pattern_with_op)) {
                                                        tmp2 <- rep(F, t=length(op_patterns$detect))
                                                        for (opi in seq_along(op_patterns$detect)) {
                                                            if (regexpr(op_patterns$detect[opi], nml_entry_per_pattern[nml_entry_i]) != -1) {
                                                                tmp2[opi] <- T
                                                            }
                                                        }
                                                        nml_entry_per_pattern_with_op[[nml_entry_i]] <- tmp2
                                                    }
                                                    # if no ops were found in var2check (e.g. "rh2m") but in found 
                                                    # nml_entry_per_pattern (e.g. "'rh2m'" "'rh2m:max'")
                                                    # -> remove the one with the op_pattern match
                                                    if (!any(var2check_with_op) &&
                                                        any(sapply(nml_entry_per_pattern_with_op, any))) {
                                                        found_op_at_entries <- which(sapply(nml_entry_per_pattern_with_op, any))
                                                        nml_entry_per_pattern2 <- nml_entry_per_pattern
                                                        for (foundi in seq_along(found_op_at_entries)) {
                                                            which_found_op <- which(nml_entry_per_pattern_with_op[[found_op_at_entries[foundi]]])
                                                            message("found op modes\n", 
                                                                    "\"", 
                                                                    paste0(op_patterns$detect[which_found_op], collapse=paste0("\", \"")),
                                                                    "\"\n", 
                                                                    "in nml_entry ", found_op_at_entries[foundi], " \"", 
                                                                    nml_entry_per_pattern[found_op_at_entries[foundi]], ". Remove ...")
                                                            nml_entry_per_pattern2 <- nml_entry_per_pattern[-found_op_at_entries[foundi]]
                                                        }
                                                    }
                                                    if (length(nml_entry_per_pattern2) == 1) {
                                                        # successfully reduced the possible matches to 1
                                                        op_ind <- which(nml_entry_per_pattern == nml_entry_per_pattern2)
                                                    } else {
                                                        stop("omgggg")
                                                    }
                                                
                                                } # 
                                            }

                                        # default case: only one pattern was found and successfully matched with testpattern
                                        } else {
                                            op_ind <- which(!is.na(op_per_pattern))
                                        }

                                        tmp[vari,"nml_entry"] <- nml_entry_per_pattern[op_ind]
                                        tmp[vari,"operator"] <- op_per_pattern[op_ind]
                                        message("Summary variable ", vari, "/", nvars, ": \"", 
                                                tmp[vari,"interval"], "\" \"", var2check, "\" (tmp[", vari, ",\"name\"] = ", 
                                                tmp[vari,"name"], "):\n",
                                                "nml_entry: \"", tmp[vari,"nml_entry"], "\" -> is this nml_entry correct?\n",
                                                "op: \"", tmp[vari,"operator"], "\" -> is this operation correct?")
                                    
                                    # not a single testpattern could be matched to current variable
                                    } else {

                                        message("Could not match a single testpattern.\n",
                                                "Assume that the variable was not mentioned in the respective nml block\n",
                                                " -> assume nml_entry = \"", not_mentioned_nml_entry, "\" -> is this assumed nml_entry correct?")
                                        tmp[vari,"nml_entry"] <- not_mentioned_nml_entry
                                        
                                    } # if any patterns were found in the correct nml block and 1 or more testpatterns were also found

                                # not any of var_patterns was detected
                                } else {

                                    stop("not any of var_patterns was detected.")
                                    
                                } # if any var_patterns detected

                            } # for all vars

                        } else { # if namelist file does not exists
                            message("namelist file ", namelists[[nmlind]], " does not exist. skip.")
                        }
                    } # not special stream
                } # mapping namelist defined for current model
            } # if nml_check
        } # if echam6 or jsbach
        
        # append new entry to table
        if (j == 1) {
            table <- tmp
        } else {
            table <- rbind(table, tmp)
        }

        # remove temporary nc file from grb convert 
        if (convert_to_nc_tag) {
            cmd <- paste0("rm ", outpath, "/", file)
            message("\n", cmd, " ...")
            system(cmd)
        }

        #if (j == 1) stop("asd")

    } # for f in outfiles
    #stop("asd")

    # remove any columns with all NA
    inds <- apply(table, 2, function(x) all(is.na(x)))
    if (any(inds)) {
        message("All ", paste0(names(which(inds)), collapse=", "), 
                " are NA for ", models[i], " model. Remove columns ...")
        table <- table[,-which(inds)]
    }

    # sort table by interval
    message("\nSort table by known_intervals ...")
    for (ii in seq_along(known_intervals$interval)) {
        int <- known_intervals$interval[ii]
        message(ii, ": ", int)
        inds <- which(table["interval"] == int)
        if (length(inds) != 0) {
            if (!exists("table_tmp")) {
                table_tmp <- table[inds,]
            } else {
                table_tmp <- rbind(table_tmp, table[inds,])
            }
        }
    }
    table <- table_tmp
    rm(table_tmp)
    message("Done.")
    
    # add index as first column and put to front
    table$no <- 1:dim(table)[1]
    table <- table[c("no", colnames(table)[-which(colnames(table) == "no")])]

    # add to global list
    table_list[[i]] <- table

    #stop("asd")

} # for i nmodels

message("\n******* done with files **********")
toc <- Sys.time()
message("Now its ", toc)
elapsed <- toc - tic
message("Elapsed: ", elapsed, " ", attributes(elapsed)$units)

# check if any model survived
if (all(sapply(table_list, length) == 0)) {
    stop("found zero files for models ", paste(models, collapse=", "))
}

# save table
for (i in seq_along(models)) {
    
    if (!is.null(table_list[[i]])) {

        message("\nSave ", outputfile_names[i], " ...")
        
        # .ods
        if (regexpr(".ods", outname) != -1) {
            Sys.setenv("R_ZIPCMD"=system("which zip", intern=T)) # necessary for utils::zip(..., zip=Sys.getenv("R_ZIPCMD", "zip"))
            readODS::write_ods(table_list[[i]], path=outputfile_names[i])
        
        # .xlsx
        } else if (regexpr(".xlsx", outname) != -1) {
            xlsx::write.xlsx(table_list[[i]], outputfile_names[i], row.names=F, sheetName=models[i])
        
        # .txt    
        } else {
            if (dim(table_list[[i]])[1] == 1) {
                max_nchars <- apply(table_list[[i]], 2, nchar)
            } else {
                max_nchars <- apply(apply(table_list[[i]], 2, nchar), 2, max, na.rm=T)
            }
            gdata::write.fwf(table_list[[i]], file=outputfile_names[i], width=max_nchars, colnames=T, sep=";")
        }

    } else { # if current model is not NULL
        warning("found no output for model ", models[i]) 
    }

} # for i

options(width=80) # default
toc <- Sys.time()
message("\nNow its ", toc)
elapsed <- toc - tic
message("Elapsed: ", elapsed, " ", attributes(elapsed)$units)
message("##### ", me, " finished #####")


