# r version of generate_climate_for_jsbalone.ksh

# 7 must-have `output`: tmin, tmax, precip, wspeed, swdown, lwdown, mpot
# 2 optional `output`: qair, co2
         
# input must be defined on both land and ocean; missvals not allowed

rm(list=ls()); graphics.off()

cdo_silent <- ""
overwrite_existing <- F
clean <- T

if (T) { # awi-esm-1-1-lr_kh800 piControl
    inpath <- "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata"
    outpath <- "/work/ba1103/a270073/forcing/jsbalone/climate/awi-esm-1-1-lr_kh800_piControl"
    outprefix <- "climate_awi-esm-1-1-lr_kh800"
    remote <- ""
    from <- 2803
    to <- 2900
    input <- list(# instantaneous large scale precipitation
                  aprl=list(path=paste0(inpath, "/echam"),
                            fpattern="piControl_<YYYY><MM>.01_echamday",
                            select="-dayavg -select,code=142"),
                  # instantaneous convective precipitation
                  aprc=list(path=paste0(inpath, "/echam"),
                            fpattern="piControl_<YYYY><MM>.01_echamday",
                            select="-dayavg -select,code=143"),
                  # instantaneous net solar rad. flux clear sky at surface
                  srafs=list(path=paste0(inpath, "/jsbach"),
                             fpattern="piControl_<YYYY><MM>.01_jsbalone",
                             select="-dayavg -select,code=185"),
                  # instantaneous net solar rad. flux at surface
                  srads=list(path=paste0(inpath, "/echam"),
                             fpattern="piControl_<YYYY><MM>.01_echamday",
                             select="-dayavg -select,code=176"),
                  # instantaneous upward solar rad. flux at surface
                  sradsu=list(path=paste0(inpath, "/echam"),
                              fpattern="piControl_<YYYY><MM>.01_echamday",
                              select="-dayavg -select,code=204"),
                  # instantaneous net thermal rad. flux at surface
                  trads=list(path=paste0(inpath, "/echam"),
                             fpattern="piControl_<YYYY><MM>.01_echamday",
                             select="-dayavg -select,code=177"),
                  # instantaneous upward thermal rad. flux at surface
                  tradsu=list(path=paste0(inpath, "/echam"),
                              fpattern="piControl_<YYYY><MM>.01_echamday",
                              select="-dayavg -select,code=205"),
                  # 2m temperature [K]
                  t2min=list(path=paste0(inpath, "/echam"),
                             fpattern="piControl_<YYYY><MM>.01_echamdaymin",
                             select="-daymin -select,code=202"),
                  # 2m temperature [K]
                  t2max=list(path=paste0(inpath, "/echam"),
                             fpattern="piControl_<YYYY><MM>.01_echamdaymax",
                             select="-daymax -select,code=201"),
                  # instantaneous 10 metre wind [m/s]
                  wind10=list(path=paste0(inpath, "/echam"),
                                    fpattern="piControl_<YYYY><MM>.01_echamday",
                                    select="-dayavg -select,code=171"),
                  # 2m specific humidity
                  q2m=list(path=paste0(inpath, "/echam"),
                                    fpattern="piControl_<YYYY><MM>.01_echamday",
                                    select="-dayavg -select,code=54")
                  )
    output <- list(tmin=c("<cdo> -setname,tmin -setctomiss,-273.15 -subc,273.15 t2min",
                          "<ncatted> -a units,tmin,o,c,\"deg C\""),
                   tmax=c("<cdo> -setname,tmax -setctomiss,-273.15 -subc,273.15 t2max",
                          "<ncatted> -a units,tmax,o,c,\"deg C\""),
                   precip=c("<cdo> -setname,precip -setcode,4 -add aprl aprc",
                            "<ncatted> -a longname,precip,o,c,\"precipitation\"",
                            "<ncatted> -a units,precip,o,c,\"kg/m**2s\""),
                   swdown=c("<cdo> -setname,shortwave -sub srads sradsu",
                            "<ncatted> -a longname,shortwave,o,c,\"surface downward shortwave radiation\"",
                            "<ncatted> -a units,shortwave,o,c,\"W/m**2\""),
                   lwdown=c("<cdo> -setname,longwave -sub trads tradsu",
                            "<ncatted> -a longname,longwave,o,c,\"surface downward longwave radiation\"",
                            "<ncatted> -a units,longwave,o,c,\"W/m**2\""),
                   wspeed=c("<cdo> -setname,wspeed wind10"),
                   mpot=c("<cdo> -setname,mpot srafs",
                          "<ncatted> -a longname,mpot,o,c,\"net surface solar radiation (clear sky)\"",
                          "<ncatted> -a units,mpot,o,c,\"W/m**2\""),
                   qair=c("<cdo> -setname,qair q2m",
                          "<ncatted> -a units,qair,o,c,\"kg/kg\"")
                   )

} # which setting

######################################

# check must-have output
must_have_output <- c("tmin", "tmax", "precip", "swdown", "lwdown", "wspeed", "mpot") 
if (any(is.na(match(must_have_output, names(output))))) {
    inds <- which(is.na(must_have_output, match(names(output))))
    stop("variable definitions missing for ", length(inds), " variables: ", 
         paste(must_have_output[inds], collapse=", "))
}

# check outpath
if (!dir.exists(outpath)) {
    dir.create(outpath, recursive=T, showWarnings=F)
}
if (!dir.exists(outpath)) {
    stop("`outpath` = \"", outpath, "\" does not exist and could not be created")
}
outpath <- normalizePath(outpath, mustWork=T)

# check external programs
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")
ncdump <- Sys.which("ncdump")
if (ncdump == "") stop("could not find ncdump")
if (any(grepl("<ncatted>", unlist(output)))) {
    ncatted <- Sys.which("ncatted")
    if (ncatted == "") stop("could not find ncatted")
}
    
path <- getwd()

# do for all years
years <- from:to
for (yeari in seq_along(years)) {
    message("\n**************************************************\n",
            "year ", yeari, "/", length(years), ": ", years[yeari])

    # construct filenames to ln/copy
    for (vi in seq_along(input)) {
        fpattern <- paste0(input[[vi]]$path, "/", input[[vi]]$fpattern)
        fpattern <- sub("<YYYY>", years[yeari], fpattern)
        if (!grepl("<MM>", fpattern)) {
            files <- fpattern # annual files
        } else { # monthly files
            files <- rep(fpattern, t=12)
            for (mi in seq_len(12)) {
                files[mi] <- sub("<MM>", sprintf("%02i", mi), fpattern)
            }
        }
        input[[vi]]$infiles <- files
    } # for vi
    
    # link/copy all needed files
    infiles <- unique(as.vector(sapply(input, "[[", "infiles")))
    message("\nget ", length(infiles), " input files ...")
    for (fi in seq_along(infiles)) {
        fin <- infiles[fi]
        fout <- paste0(outpath, "/", basename(fin))
        if (file.exists(fout) && overwrite_existing) file.remove(fout)
        if (!file.exists(fout)) {
            if (remote != "") {
                stop("not yet")
            } else {
                message("ln -s ", fin, " ", fout, " ...")
                file.symlink(fin, fout)
            }
        } else {
            message("input file ", fout, " already exists. set `overwrite_existing`=T if you want to overwrite")
        }
        # replace original input with cp/ln
        for (vi in seq_along(input)) {
            inds <- which(input[[vi]]$infiles == fin)
            if (length(inds) > 0) {
                input[[vi]]$infiles[inds] <- fout
            }
        }
    } # for fi
    # clean original input files object
    
    # update infiles 
    infiles <- unique(as.vector(sapply(input, "[[", "infiles")))

    # check if cp/ln was successfull
    if (any(!file.exists(infiles))) {
        warning("file cp/ln did not work. skip to next year")
        next # year
    }

    # get file format nc/grb
    message("\ndetermine input format ...")
    cmd <- paste0(ncdump, " -k ", input[[1]]$infiles[1], " 2>/dev/null") 
    if (T) message("run `", cmd, "` ...")
    format <- suppressWarnings(system(cmd, intern=T))
    convert <- F
    if (!is.null(attributes(format))) {
        if (attributes(format)$status == 1) { # no success --> file is not nc
            convert <- T
        }
    }
    if (convert) {
        message("--> input files not of type netcdf --> apply conversion to nc")
    } else {
        message("--> input files of type netcdf --> conversion to nc not necessary")
    }

    # cdo selection of input
    message("\ncdo select of ", length(input), " input files ...")
    fselects <- rep(NA, t=length(input))
    for (vi in seq_along(input)) {
        message("var ", vi, "/", length(input), ": ", names(input)[vi])
        fselect <- paste0(outpath, "/", names(input)[vi])
        fselects[vi] <- fselect
        if (file.exists(fselect) && overwrite_existing) file.remove(fselect)
        if (!file.exists(fselect)) {
            cmd <- paste0(cdo, " ", cdo_silent)
            if (convert) { # add grb-nc conversion if necessary
                cmd <- paste0(cmd, " -f nc")
            }
            if (substr(input[[vi]]$select, 1, 1) != "-") {
                cmd <- paste0(cmd, " -")
            } else {
                cmd <- paste0(cmd, " ")
            }
            cmd <- paste0(cmd, input[[vi]]$select, " <files> ", fselect)
            if (T) message("run `", cmd, "` ...")
            cmd <- sub("<files>", paste(input[[vi]]$infiles, collapse=" "), cmd) 
            system(cmd)
        } else {
            message("selection file ", fselect, " already exists. set `overwrite_existing`=T if you want to overwrite")
        }
    } # for vi
    
    # calculate output
    message("\ncalculate ", length(output), " output files ...")
    setwd(outpath)
    fouts <- rep(NA, t=length(output))
    for (vi in seq_along(output)) {
        message("var ", vi, "/", length(output), ": ", names(output)[vi])
        fout <- names(output)[vi]
        fouts[vi] <- fout
        if (file.exists(fout) && overwrite_existing) file.remove(fout)
        if (!file.exists(fout)) {
            for (ci in seq_along(output[[vi]])) {
                cmd <- output[[vi]][ci]
                cmd <- sub("<cdo>", cdo, cmd)
                cmd <- sub("<ncatted>", ncatted, cmd)
                if (grepl("<", cmd)) stop("there are unknown commands in `output[[", vi, "]][", ci, "]` = \"", cmd, "\"")
                cmd <- paste0(cmd, " ", fout)
                if (T) message("run `", cmd, "` ...")
                system(cmd)
            } # for ci
        } else {
            message("calculates ouptut file ", fout, " already exists. set `overwrite_existing`=T if you want to overwrite")
        }
    } # for vi

    # merge results
    message("\nmerge output ...")
    cmd <- paste0(cdo, " ", cdo_silent, " merge ", paste(fouts, collapse=" "), " ", outprefix, "_", years[yeari], ".nc") 
    if (T) message("run `", cmd, "` ...")
    system(cmd)

    # clean
    if (clean) {
        message("\n`clean`=T --> clean tmp files ...")
        invisible(file.remove(fselects))
        invisible(file.remove(fouts))
        invisible(file.remove(infiles)) # not original input but cp/ln
    }

    setwd(path) # back to where script started

} # for yeari

warnings <- warnings()
if (length(warnings) > 0) {
    message("\n", length(warnings), " warnings:")
    print(warnings)
} else {
    message("\nno warnings")
}
message("\nfinished\n")

