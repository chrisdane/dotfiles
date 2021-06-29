## R

rm(list=ls()); graphics.off()
library(tools) # file_path_sans_ext

allowed_exts <- c("nc", "grb")
verbose <- 2 # [1,2]

#outdata_path <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_default/outdata"
#result_fname <- "~/cmip6/output_lists/CMIP6/CMIP_PMIP/dynveg_true/historical_default.txt"
#ls_year <- 1850

#outdata_path <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_pure_mkexp/outdata"
#result_fname <- "~/cmip6/output_lists/CMIP6/CMIP_PMIP/dynveg_true/historical_pure_mkexp.txt"
#ls_year <- 1850

#outdata_path <- "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/historical_test4/outdata"
#result_fname <- "~/cmip6/output_lists/CMIP6/CMIP_PMIP/dynveg_true/historical_test4.txt"
#ls_year <- 1850

outdata_path <- "/work/ollie/cdanek/out/awicm-1.0-recom/test4/outdata"
result_fname <- "output_short_awicm-recom.txt"
ls_year <- 1950

############################

result_path <- dirname(result_fname)
# 0 existence, 1 execute, 2 write, 4 read
if (file.access(result_fname, 0) != -1) {
    cmd <- paste0("rm ", result_fname)
    message(cmd)
    system(cmd)
} else {
    if (file.access(result_path, 0) == -1) dir.create(result_path, recursive=T)
}
    
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/Sys.readlink.html
is.symlink <- function(paths) isTRUE(nzchar(Sys.readlink(paths), keepNA=T))

## start
message("find year ", ls_year, " model data from ", outdata_path, " ...")

## 1: find available models
models <- list.dirs(outdata_path, full.names=F)
models <- models[-1] # remove empty string
models2 <- models
for (i in 1:length(models)) { # check if empty model dir
    if (length(list.files(paste0(outdata_path, "/", models[i]))) == 0) {
        models2[i] <- F
    }
}
models <- models[models2 != "FALSE"]
if (length(models) == 0) {
    stop("not a single non-empty sub directory in ", outdata_path)
}
rm(models2)

## 2: check available models 
modlist <- vector("list", l=length(models))
names(modlist) <- models
for (modi in 1:length(models)) {
    message()
    message("********** ", models[modi], " ***********")
    outf <- list.files(paste0(outdata_path, "/", models[modi]), pattern=paste0("*", ls_year, "*"))
    exts <- file_ext(paste0(outdata_path, "/", models[modi], "/", outf))
    inds <- exts %in% allowed_exts
    outf <- outf[inds]
    exts <- exts[inds]
    # check if output is link of original output (avoid treating the same data twice, e.g. fesom links)
    test_symlink <- rep(NA, t=length(outf))
    for (fi in 1:length(outf)) {
        test_symlink[fi] <- is.symlink(paste0(outdata_path, "/", models[modi], "/", outf[fi]))
    }
    # remove all links from files
    if (any(test_symlink)) {
        outf <- outf[-which(test_symlink)]
        exts <- exts[-which(test_symlink)]
    }
    nf <- length(outf)
    if (nf != 0) {
        message("check output of ", nf, " ", 
                paste0(allowed_exts, collapse=" or "), 
                " files in ", outdata_path, "/", models[modi], " ...")
        varlist <- vector("list", l=nf)
        names(varlist) <- outf
        for (fi in 1:nf) {
            
            fileext <- file_ext(outf[fi]) 
            if (any(fileext == c("nc", "grb"))) {
                fname <- paste0(outdata_path, "/", models[modi], "/", outf[fi])
                
                # get number of times
                cmd <- paste0("cdo -s ntime ", fname)
                if (verbose > 1) message(cmd)
                npf <- system(cmd, intern=T)
                
                # if npf is 0, no variables are saved in nc file, skip file
                if (length(npf) == 0) {
                    message("File has zero variables, continue with next file ...")
                    break
                }

                # get variable names
                cmd <- paste0("cdo -s pardes ", fname)
                if (verbose > 1) message(cmd)
                pardes <- system(cmd, intern=T)
                if (verbose > 2) { message("pardes"); cat(capture.output(str(pardes)), sep="\n") }
                varcode_inds <- which(regexpr("var[0-9]", pardes) != -1)
                if (verbose > 2) { message("varcode_inds"); cat(capture.output(str(varcode_inds)), sep="\n") }
                # if cdo pardes yields e.g. var161
                if (length(varcode_inds) != 0) {
                    # determine codes. assumptions: pardes result is " 160  var160      "
                    varcodes <- as.numeric(substr(pardes[varcode_inds], 2, 4))
                    if (verbose > 2) { message("varcodes"); cat(capture.output(str(varcodes)), sep="\n") }
                    if (any(!is.numeric(varcodes)) || any(is.na(varcodes))) {
                        stop("this should not happen with the varcodes")
                    }
                    # try to find code table
                    codetables <- list.files(paste0(outdata_path, "/", models[modi]), pattern="*codes*") 
                    if (verbose > 2) { message("codetables"); cat(capture.output(str(codetables)), sep="\n") }
                    if (length(codetables) != 0) { # found codetale
                        if (verbose > 2) message("*** Codetable found ***")
                        varnames <- rep(NA, t=length(varcodes))
                        longnames <- varnames
                        for (codetablei in 1:length(codetables)) {
                            codetable_fname <- paste0(outdata_path, "/", models[modi], "/", codetables[codetablei])
                            if (verbose > 2) { message("codetable_fname"); cat(capture.output(str(codetable_fname)), sep="\n") }
                            # 0 existence, 1 execute, 2 write, 4 read
                            if (file.access(codetable_fname, 0) != -1) {
                                codetable <- readLines(codetable_fname)
                                if (verbose > 2) { message("codetable"); cat(capture.output(str(codetable)), sep="\n") }
                                # assumption: codetables are " 160  1  runoff  0.00  1.00  Surface Runoff and Drainage (acc+weighted) [kg/m^2s]
                                tmp <- strsplit(codetable, "\\s+")
                                if (verbose > 2) { message("tmp"); cat(capture.output(str(tmp)), sep="\n") }
                                codetable <- as.numeric(sapply(tmp, "[", 2))
                                if (verbose > 2) { message("codetable"); cat(capture.output(str(codetable)), sep="\n") }
                                if (any(!is.numeric(codetable)) || any(is.na(codetable))) {
                                    stop("this should not happen with the codetable")
                                }
                                codetable_names <- sapply(tmp, "[", 4) 
                                if (verbose > 2) { message("codetable_names"); cat(capture.output(str(codetable_names)), sep="\n") }
                                # check all varcodes of file
                                if (any(varcodes %in% codetable)) {
                                    matchinds <- which(varcodes %in% codetable)
                                    if (verbose > 2) { message("matchinds"); cat(capture.output(str(matchinds)), sep="\n") }
                                    varnames[matchinds] <- codetable_names[matchinds]
                                    if (verbose > 2) { message("varnames"); cat(capture.output(str(varnames)), sep="\n") }
                                    for (matchi in 1:length(matchinds)) {
                                        longnames[matchinds[matchi]] <- paste0(tmp[[matchinds[matchi]]][7:length(tmp[[matchinds[matchi]]])], collapse=" ")
                                    }
                                    if (verbose > 2) { message("longnames"); cat(capture.output(str(longnames)), sep="\n") }
                                } else {
                                    message("Could not find any of ", paste0(varcodes, collapse=","), " in\n", codetable_fname)
                                }
                            } else {
                                message("Could not read codetable file ", codetable_fname, ". Try another one ...")
                            }
                        } # for codetablei
                    
                    } else { # no codetable found

                        message("Could not find and *codes* files in\n", paste0(outdata_path, "/", models[modi]))
                        varnames <- paste0("var", varcodes)
                        if (verbose > 2) { message("varnames"); cat(capture.output(str(varnames)), sep="\n") }

                    } # if there are codetables
                
                } else { # if cdo pardes yields variable names
                    
                    if (verbose > 2) message("*** No codetable found ***")
                    tmp <- strsplit(pardes, "\\s+")
                    if (verbose > 2) { message("tmp"); cat(capture.output(str(tmp)), sep="\n") }
                    varnames <- sapply(tmp, "[", 3) 
                    if (verbose > 2) { message("varnames"); cat(capture.output(str(varnames)), sep="\n") }
                    longnames <- rep(NA, t=length(varnames))
                    # fesom cdo pardes is only ""     "-1"   "evap"
                    if (length(tmp[[1]]) > 3) {
                        for (vari in 1:length(varnames)) {
                            longnames[vari] <- paste0(tmp[[vari]][4:length(tmp[[vari]])], collapse=" ")
                        }
                    }
                    if (verbose > 2) { message("longnames"); cat(capture.output(str(longnames)), sep="\n") }

                } # if cdo pardes yields var161 or variable names

            } else { # not nc or grb

                # not defined yet
            
            } # which file extension of the allowed ones

            #if (fi == 2 && models[modi] == "fesom") stop("asd")
            varlist[[fi]] <- data.frame(varname=varnames, 
                                        npf=rep(npf, t=length(varnames)),
                                        longname=longnames,
                                        fname=outf[fi])
            
            # delete before next file for better debugging
            if (T) {
                if (any(fileext == c("nc", "grb"))) { 
                   rm(varnames, npf, longnames,
                      fname, cmd, pardes, varcode_inds)
                } else {
                
                }
            } # remove before next file for better debugging

        } # for fi nf

    } else { # if nf == 0
        message("Zero files of year ", ls_year, " with allowed file extensions ", 
                paste0(allowed_exts, collapse=" or "), " found. Continue with next model ...")
    } # if nf != 0 or not

    modlist[[modi]] <- varlist

} # for modi models

## save result as table
message()
message("Save result to ", result_fname, " ...")
write("Output of experiment", file=result_fname, append=F)
write(outdata_path, file=result_fname, append=T)
write(paste0("year ", ls_year), file=result_fname, append=T)

nvar_per_model <- rep(NA, t=length(modlist))
for (modi in 1:length(modlist)) {
    nvar_per_model[modi] <- sum(sapply(sapply(modlist[[modi]], "[", "varname"), length))
}
nvar_tot <- sum(nvar_per_model)

cnt_tot <- 0
for (modi in 1:length(modlist)) {
    write("", file=result_fname, append=T)
    write(paste0(sprintf(paste0("%", nchar(length(modlist)), "i"), modi), "/", length(modlist), 
                 " ", names(modlist)[modi]), 
          file=result_fname, append=T)
    # append every variable of model
    cnt_per_model <- 0
    for (fi in 1:length(modlist[[modi]])) {
        for (vari in 1:length(modlist[[modi]][[fi]]$varname)) {
            cnt_tot <- cnt_tot + 1
            cnt_per_model <- cnt_per_model + 1
            write(paste0("  ",
                         sprintf(paste0("%", nchar(nvar_per_model[modi]), "i"), cnt_per_model), "/", nvar_per_model[modi], " ",
                         sprintf(paste0("%", nchar(nvar_tot), "i"), cnt_tot), "/", nvar_tot, " ",
                         modlist[[modi]][[fi]]$npf[vari], " ",
                         modlist[[modi]][[fi]]$varname[vari], " ", 
                         modlist[[modi]][[fi]]$fname[vari], " ",
                         modlist[[modi]][[fi]]$longname[vari]),
                  file=result_fname, append=T)
        } # for all vars of file of model
    } # for all files of model
} # for all models


