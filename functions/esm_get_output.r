#!/usr/bin/env Rscript

if (interactive()) {
    rm(list=ls()); graphics.off()
    me <- "esm_get_output.r"
    #esmpath <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/old/hist"
    #outname <- "pi_old.ods"
    #esmpath <- "/work/ab0246/a270073/awicm-CMIP6/restart_from_restart_from_hu_oceanonly"
    #outname <- "restart_from_restart_from_hu_oceanonly.ods"
    #esmpath <- "/work/ab0246/a270073/awicm-CMIP6/PI-CTRL"
    #esmpath <- "/work/ab0246/a270073/awicm-CMIP6/PI-CTRL_nodynveg"
    #outname <- "pi_output.txt"
    #outname <- "pi_output.ods"
    #esmpath <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL4"
    #outname <- "PI-CTRL4_output.ods"
    #esmpath <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6"
    #outname <- "PI-CTRL6_output.ods"
    #esmpath <- "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/hist"
    #esmpath <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical"
    #outname <- "hist_output.txt"
    #outname <- "awi-esm-1-1-lr_historical_output.ods"
    #esmpath <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/hist2"
    #outname <- "hist2_output.txt"
    #outname <- "hist2_output.ods"
    #esmpath <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL5"
    #outname <- "PI-CTRL5_output.ods"
    #esmpath <- "/work/ollie/cdanek/out/awicm-1.0-recom/test4"
    #outname <- "output_awicm-recom.ods"
    #esmpath <- "/work/ollie/cdanek/out/echam-6.3.04p1/pictrl-grb"
    #outname <- "pictrl-grb.ods"
    #outname <- "pictrl-grb.txt"
    esmpath <- "/work/ollie/cdanek/out/echam-6.3.04p1/pictrl-spinup-grb"
    #outname <- "pictrl-spinup-grb.ods"
    outname <- "pictrl-spinup-grb.txt"
    
    models <- c("echam", "jsbach", "fesom", "recom")
    #models <- "echam"
    #models <- "fesom"
    #models <- "jsbach"
    #models <- "recom"
    #models <- c("echam", "jsbach")
    #models <- c("fesom", "recom")
    
    libpaths <- .libPaths()

# not interactive
} else {
    
    #./get_esm_output_vars.r /work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/hist ~/cmip6/hist_output.ods > log 2>&1 &
    
    me <- sub(pattern=".*=", replacement="", commandArgs()[4])
    usage <- paste0("\nUsage: ", me, " arg1 arg2 [--models=] [--libpaths=] or\n",
                    "       ", me, " arg1 arg2 [--models=] [--libpaths=] > output.log 2>&1 &\n",
                    "\n",
                    "   unnamed arg1: /path/to/esm-experiment (must contain the directories \"scripts/\" and \"outdata/\")\n",
                    "   unnamed arg2: filename/of/output_table.ods (must have ending \".ods\", \".xlsx\" or \".txt\")\n",
                    "   optional named arg --models=models,to,include,string,seperated,by,comma (e.g. echam,jsbach,fesom,recom)\n",
                    "   optional named arg --libpaths=/path/where/R/packages/are/installed,/separated/by/comma/if/more/than/one\n")
    args <- commandArgs(trailingOnly=T)
    if (length(args) < 2) {
        message(usage)
        quit()
    }
    esmpath <- args[1]
    outname <- args[2]
    
    # check models if provided
    models <- c("echam", "jsbach", "fesom", "recom") # known models so far
    if (any(grepl("--models", args))) {
        models <- sub("--models=", "", args[grep("--models=", args)])
        models <- strsplit(models, ",")[[1]]
    } else {
        models <- models
    }
    if (any(is.na(match(models, models)))) {
        inds <- which(is.na(match(models, models)))
        stop("models ", paste(models[inds], collapse=", "), " unknown")
    }

    # check libpaths if provided 
    libpaths <- .libPaths()
    if (any(grepl("--libpaths", args))) {
        libpaths <- sub("--libpaths=", "", args[grep("--libpaths=", args)])
        libpaths <- strsplit(libpaths, ",")[[1]]
        for (i in seq_along(libpaths)) {
            if (!dir.exists(libpaths[i])) {
                msg <- paste0("provided libpath \"", libpaths[i], "\" does not exist. skip")
                warning(msg, .immediate=T)
                warning(msg, .immediate=F)
                libpaths[i] <- NA
            }
        }
        if (!all(is.na(libpaths))) {
            libpaths <- c(libpaths, .libPaths())
        }
    }

} # if interactive or not

# dependency: ~/scripts/r/myfunctions.r:cdo_get_filetype()
if (!file.exists("~/scripts/r/functions/myfunctions.r")) {
    stop("dependency ~/scripts/r/functions/myfunctions.r not found")
} else {
    source("~/scripts/r/functions/myfunctions.r")
}

# defaults
cdo_silent <- F
check_nml <- T
namelists <- list("namelist.echam"=paste0(esmpath, "/config/echam/namelist.echam"))
op_patterns <- list(detect=c( "mean", "inst", "min", "max", "asis", "none", "sqrmean"),
                    meaning=c("mean", "inst", "min", "max", "asis", "none", "sqrmean"))
not_mentioned_nml_entry <- "not mentioned"

# known variables to throw out
known_rm_vars <- vector("list", l=length(models))
known_rm_vars[["echam"]] <- c("hyai", "hyam", "hybi", "hybm")
known_rm_vars[["jsbach"]] <- known_rm_vars[["echam"]]

# known dimensions
known_dims <- vector("list", l=length(models))
names(known_dims) <- models
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
known_intervals$echam_time <- rep(NA, t=length(known_intervals$interval))
known_intervals$echam_interval <- rep(NA, t=length(known_intervals$interval))
for (i in seq_along(known_intervals$interval)) {
    if (known_intervals$interval[i] == "hr") {
        known_intervals$order[i] <- 1
        known_intervals$echam_time[i] <- "1"
        known_intervals$echam_interval[i] <- "hours"
    } else if (known_intervals$interval[i] == "3hr") {
        known_intervals$order[i] <- 2
        known_intervals$echam_time[i] <- "3"
        known_intervals$echam_interval[i] <- "hours"
    } else if (known_intervals$interval[i] == "6hr") {
        known_intervals$order[i] <- 3
        known_intervals$echam_time[i] <- "6"
        known_intervals$echam_interval[i] <- "hours"
    } else if (known_intervals$interval[i] == "day") {
        known_intervals$order[i] <- 4
        known_intervals$echam_time[i] <- "1"
        known_intervals$echam_interval[i] <- "days"
    } else if (known_intervals$interval[i] == "mon") { 
        known_intervals$order[i] <- 5
        known_intervals$echam_time[i] <- "1"
        known_intervals$echam_interval[i] <- "months"
    } else if (known_intervals$interval[i] == "yr") { 
        known_intervals$order[i] <- 6
        known_intervals$echam_time[i] <- "1"
        known_intervals$echam_interval[i] <- "years"
    } else {
        stop("output interval \"", known_intervals$interval[i], "\" not defined")
    }
}
# sort intervals
known_intervals <- known_intervals[sort(known_intervals$order, index.return=T, decreasing=F)$ix,]

# Start
tic <- Sys.time()
message("##### ", me, " started #####")
message("Now its ", tic)
message("models = ", paste0(models, collapse=", "))
esmpath <- normalizePath(esmpath)
message("esmpath = ", esmpath)
expname <- basename(esmpath)
message("--> expname = ", expname)
test <- list.files(paste0(esmpath, "/scripts"), pattern="*date*", full.names=T)
if (length(test) != 1) {
    stop("could not found ", esmpath, "/scripts/*date")
}
last_output_year <- scan(test, what="char", quiet=T)
last_output_year <- as.numeric(substr(last_output_year[1], 1, 4))
message("--> last_output_year = ", last_output_year)
outpath <- normalizePath(dirname(outname))
message("outpath = ", outpath)
outname <- basename(outname)
message("outname = ", outname)
if (file.access(outpath, mode=2) == -1) { # not writable
    stop("outpath = ", outpath, " not writeable")
}

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
    stop("package \"", pkg, "\" not installed in\n",
         paste(paste0("   ", libpaths), collapse="\n"), "\n",
         "rerun the script with arg3=/absolute/path/where/R/package/", pkg, "/is/installed\n",
         "or type `install.packages(\"", pkg, "\")` within an open R session and then rerun this script")
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

    options("width"=80) # default

    message("find files model ", i, "/", length(models), ": ", models[i], " ...")

    # find model output of last_output_year
    outfiles <- list.files(path=paste0(esmpath, "/outdata/", models[i], "/"),
                           pattern=paste0(last_output_year), full.names=T)
    
    # remove *.codes
    outfiles <- outfiles[!base::endsWith(outfiles, ".codes")]
    
    if (length(outfiles) == 0) {
        message("no files found, skip to next model ...")
        next # model
    }
    message("found ", length(outfiles), " files")
    print(head(outfiles))
    print("...")
    print(tail(outfiles))

    # filenames:
    # echam: hist_echam6_tdiagmon_185001.nc, hist_echam6_tdiagmon_185002.nc -> monthly
    # jsbach: hist_jsbach_yassoyr_185005.nc -> monthly 
    # fesom: hist_fesom_wo_18500101.nc -> annual
    
    # first check if monthly files
    message("check if there are files with pattern \"", last_output_year, "01\" \"",
            last_output_year, "02\" ...")
    #inds_01 <- which(regexpr("01", basename(outfilesp)) != -1)
    #inds_02 <- which(regexpr("02", basename(outfilesp)) != -1)
    inds_01 <- which(grepl(paste0(last_output_year, "01"), outfiles))
    inds_02 <- which(grepl(paste0(last_output_year, "02"), outfiles))

    # found same number of january and february files -> monthly
    if (length(inds_01) > 0 &&
        length(inds_01) == length(inds_02)) {
        outfiles <- outfiles[inds_01]
        message("found same number of files -> assume monthly files")
        file_interval <- "monthly"
    
    # else assume annual files
    } else {
        message("found different number of files -> assume annual files")
        file_interval <- "annual"
    }
    #stop("asd")

    # if fesom, check if doubled links exist
    # older esm versions:       'varname_fesom_date.nc'
    # newer esm versions: 'expid_varname_fesom_date.nc'
    if (length(unique(outfiles)) != length(outfiles)) {
        message("Found filenames more than once, remove double entries ...")
        outfiles <- unique(outfiles)
    } # if fesom
    
    # checks finished
    dirnames <- dirname(outfiles)
    outfiles <- basename(outfiles)
    message("proceed with ", length(outfiles), " files")
    print(outfiles)

    options("width"=1000) # increase width per line for print
    for (j in seq_along(outfiles)) {

        path <- dirnames[j]
        file <- outfiles[j]
        message("\n", models[i], " file ", j, "/", length(outfiles), ": ", file, " ...")

        # first convert from grb to nc if necessary
        convert_to_nc_tag <- F # default
        #if (tools::file_ext(file) == "grb") { # does not cover cases of file names without ending
        ftype <- cdo_get_filetype(fin=paste0(path, "/", file))
        if (ftype$file_type == "non-nc") {

            # check if cdo module is loaded
            cdo_check <- system("cdo -V", ignore.stderr=T) == 0
            if (!cdo_check) {
                cmd <- "module load cdo"
                message(cmd, " ...")
                system(cmd)
            }

            # grb to nc
            cmd <- "cdo "
            if (cdo_silent) {
                cmd <- paste0(cmd, "-s ")
            }
            if (models[i] == "echam") {
                cmd <- paste0(cmd, "-t echam6 ")
            }
            cmd <- paste0(cmd, "-f nc copy ", path, "/", file, " ", 
                          outpath, "/", file, ".nc")
            message("convert to nc:")
            message(cmd, " ...")
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
        message("Found ", ndims, " dims:")
        print(dims)
        message("Found ", nvars, " vars:")
        print(vars)
        
        # throw out some variables
        if (any(!is.na(match(vars[,"name"], known_rm_vars[[models[i]]])))) {
            message("\nthrow out some variables ...")
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

        # try to determine long name and code of every variable
        message("\nTry do determine variable long name and code if possible ...")
        for (k in seq_along(varids)) {
            message("var ", k, ": ", vars[k,"name"], " ... ", appendLF=F)
            atts <- ncdf.tools::infoNcdfAtts(paste0(path, "/", file), var.id=varids[k])
            message("atts:")
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
                            vars[k,"name"], " of outfiles[", j, "] = ", file, ". skip ...")
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
        
        # try to determine stream if echam or jsbach
        if (any(models[i] == c("echam", "jsbach"))) {

            message("\ncurrent model is ", models[], " --> try do determine stream ...")
            # e.g. hist_echam6_accw_201401.grb test4_195001.01_aclcim.nc
            # --> everything that is not expnbame and date and ending
            stream <- tools::file_path_sans_ext(file)
            stream <- sub(paste0(expname, "_"), "", stream)
            if (file_interval == "monthly") {
                pattern <- paste0(last_output_year, "01") # case: hist_echam6_accw_201401.grb
                if (grepl(paste0(pattern, ".01"), file)) { # case: test4_195001.01_aclcim.nc
                    pattern <- paste0(pattern, ".01")
                }
            } else if (file_interval == "annual") {
                stop("not yet")
            }
            stream <- sub(paste0(pattern, "_"), "", stream)
            if (nchar(stream) == 0) {
                warning("could not determine stream of file ", file, ". skip.", immediate.=T)
                warning("could not determine stream of file ", file, ". skip.", immediate.=F)
            } else {
                # if special post processing streams
                if (grepl("_mm", stream)) { # case hist_echam6_ATM_mm_201401.nc
                    stop("continue")
                }
                message("stream: \"", stream, "\" (is this correct?)")
                tmp[,"stream"] <- stream
            }
        } # if echam jsbach

        # if there are any variable names containing the pattern "varX", with X being any number
        if (any(regexpr(pattern="var[[:digit:]]", vars[,"name"]) != -1)) {
            message("\n", 
                    "Some of these variables contain \"varX\", with X being a number.",
                    " Try to find fitting code table ...")

            # possibly existing code table
            # e.g.: hist_echam6_accw_185001.codes
            #       PI-CTRL_296901.01_accw.codes
            cmd <- paste0("ls ", dirnames[j], "/*", last_output_year, "*.codes") # use original file name here
            message(cmd, " ...")
            codes_files <- system(cmd, intern=T)
            if (length(codes_files) == 0) {
                    message("Could not find a .codes file containing the pattern \"", 
                            last_output_year, "\". Cannot resolve \"varX\" varnames.")
            } else {
                stream_inds <- which(regexpr(stream, basename(codes_files)) != -1)
                if (length(stream_inds) == 0) {
                    message("Could not find a .codes file containing the patterns \"",
                            last_output_year, " and \"", stream, "\". Cannot resolve \"varX\" varnames.")
                } else {
                    codes_file <- codes_files[stream_inds[1]]
                    message(".codes file\n",
                            codes_file)
                    message("found. Try to resolve \"varX\" varnames ...")
                    vars_tmp <- readLines(codes_file)
                    vars_tmp <- gsub("\\s+", " ", vars_tmp) # replace many " " by one " "
                    for (k in seq_along(vars_tmp)) {
                        infos <- strsplit(vars_tmp[k], "\\s+")[[1]] # seperate by spaces
                        codenumber_codes <- infos[2] # e.g. 160
                        codenumber_ind_nc <- which(sub("var", "", vars[,"name"]) == codenumber_codes)
                        # if codenumber of .codes file was found in .nc file (the default case)
                        if (length(codenumber_ind_nc) != 0) {
                            message("Found code number ", codenumber_codes, 
                                    " from the .nc file in the .codes file:")
                            print(vars_tmp[k])
                            if (any(colnames(tmp) == "name")) {
                                tmp[codenumber_ind_nc,"name"] <- infos[4]
                            } else {
                                stop("\"name\" is not a column name of tmp")
                            }
                            if (any(colnames(tmp) == "code")) {
                                tmp[codenumber_ind_nc,"code"] <- infos[2]
                            } else {
                                stop("\"code\" is not a column name of tmp")
                                                            }
                            if (any(colnames(tmp) == "lev")) {
                                tmp[codenumber_ind_nc,"lev"] <- infos[3]
                            } else {
                                stop("\"lev\" is not a column name of tmp")
                            }
                            if (any(colnames(tmp) == "longname")) {
                                tmp[codenumber_ind_nc,"longname"] <- paste0(infos[7:(length(infos) - 1)], collapse=" ")
                            } else {
                                stop("\"longname\" is not a column name of tmp")
                            }
                            if (any(colnames(tmp) == "unit")) {
                                tmp[codenumber_ind_nc,"unit"] <- infos[length(infos)]
                            } else {
                                stop("\"unit\" is not a column name of tmp.")
                            }
                        # else if codenumber of .codes file was not found in .nc file (strange case)
                        } else {
                            message("code number ", codenumber_codes, " from the .codes file\n",
                                    codes_file, "\n",
                                    "was not given in .nc file\n", 
                                    file, ". skip this code number.")
                        } # if code number from .codes table was also found in .nc file
                    } # for all k entries in .codes table
                } # if .codes file with pattern last_output_year and stream exists
            } # if .codes file with pattern last_output_year exists
        } # if any variable contains pattern "varX", with X being any number
       
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

        # try to determine operator (mean, inst, min, max, etc.) of echam or jsbach variable based on the namelist
        if (any(models[i] == c("echam", "jsbach"))) {
            if (check_nml) { # TODO
                message("\ncheck_nml is true ...")
                if (any(models[i] == c("echam", "jsbach"))) {
                    nml_to_check <- "namelist.echam"
                } else {
                    stop("not yet")
                }
                nmlind <- which(names(namelists) == nml_to_check)
                
                # no namelist found
                if (length(nmlind) == 0) {
                    message("could not find a \"", nml_to_check, "\". skip.")
                
                # namelist found
                } else {
                    
                    # if special *_mm stream
                    if (length(grep("_mm", stream)) != 0) {
                        message("ignore this task from this special \"", stream, "\" stream. skip.")
                    
                    # if not special *_mm stream
                    } else {
                        message("Try to find more infos about all current variables from this namelist:")
                        message(namelists[[nmlind]], " ...")
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
                                            "This was done automatically by echam if, e.g., the nml entry \"'temp2', 'temp2:inst'\"",
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
                                                                        tmp[vari,"interval"]),"echam_time"]
                                    interval_echam <- known_intervals[which(known_intervals[,"interval"] == 
                                                                            tmp[vari,"interval"]),"echam_interval"]
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
                                                            "\" block in the nml -> Is this correct?")

                                                    
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
                                                                nml_entry, "\" -> Is this correct?")
                                                        message("Determined op: \"", 
                                                                op, "\" -> Is this correct?")
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
                                            message("All found nml entries are the same")
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
                                                    message("Current var2check is \"", var2check, "\": identified the ", 
                                                            current_var_ind, "th case:")
                                                    op_ind <- current_var_ind
                                                } else {
                                                    stop("Could not identify which variable is the current one. ",
                                                         "Solve this case by hand.")
                                                }

                                            # more than 1 nml entries but only 1 fitting variable in file
                                            } else {
                                                message("However, there is only 1 entry of var2check \"", 
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
                                            "nml_entry: \"", tmp[vari,"nml_entry"], "\" -> Is this correct?\n",
                                            "op: \"", tmp[vari,"operator"], "\" -> Is this correct?")
                                
                                # not a single testpattern could be matched to current variable
                                } else {

                                    message("Could not match a single testpattern.\n",
                                            "Assume that the variable was not mentioned in the respective nml block\n",
                                            " -> assume nml_entry = \"", not_mentioned_nml_entry, "\" -> Is this correct?")
                                    tmp[vari,"nml_entry"] <- not_mentioned_nml_entry
                                    
                                } # if any patterns were found in the correct nml block and 1 or more testpatterns were also found

                            # not any of var_patterns was detected
                            } else {

                                stop("not any of var_patterns was detected.")
                                
                            } # if any var_patterns detected

                        } # for all vars
                    } # not _mm stream
                } # nml was found
            } # if nml_check
        } # if echam or jsbach
        
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

options("width"=80) # default
toc <- Sys.time()
message("\nNow its ", toc)
elapsed <- toc - tic
message("Elapsed: ", elapsed, " ", attributes(elapsed)$units)
message("##### ", me, " finished #####")


