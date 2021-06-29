# create links of esm model output
# create recursive links: cp -rs filenames_to_link_*pattern* links_to_create_*pattern*

options(warn=2) # stop on warnings
rm(list=ls()); graphics.off()

ht <- function(d, n=7) {
    print(head(d, n))
    message(system('printf "   \u22ee"', intern=T))
    print(tail(d, n))
}

dry <- T
remove_existing_links_in_input_dirs <- T

# which esm experiments
# =================================================
if (T) { # awi-esm-1-1-lr piControl
    exps_in <- list(runids=c("restart_from_hu_oceanonly",
                             "restart_from_restart_from_hu_oceanonly",
                             "PI-CTRL",
                             "PI-CTRL2",
                             "PI-CTRL3",
                             "PI-CTRL4",
                             "PI-CTRL6"),
                    models=c("echam", "fesom", "jsbach"),
                    outdatapaths=c("/work/ab0246/a270073/awicm-CMIP6/restart_from_hu_oceanonly/outdata",
                                   "/work/ab0246/a270073/awicm-CMIP6/restart_from_restart_from_hu_oceanonly/outdata",
                                   "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL/outdata",
                                   "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL2/outdata",
                                   "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL3/outdata",
                                   "/work/ba0989/a270077/CMIP6_PMIP4/a270073/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL4/outdata",
                                   "/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/PI-CTRL6/outdata"),
                    froms=c(2701, 2703, 2713, 2870, 2900, 2911, 1855),
                    tos=c(2702, 2712, 2869, 2899, 2910, 2999, 1954),
                    fpatterns=list(echam=rep("<runid_in>_echam6_<stream>_<YYYY><MM>.grb", t=7),
                                   fesom=c(rep("<variable>_fesom_<YYYY>0101.nc", t=6), 
                                           "<runid_in>_fesom_<variable>_<YYYY>0101.nc"), 
                                   jsbach=rep("<runid_in>_jsbach_<stream>_<YYYY><MM>.grb", t=7)))
    exps_out <- list(runids=rep("piControl", t=7),
                     outdatapath=rep("/work/ab0246/a270073/awicm-test/CMIP6/CMIP_PMIP/dynveg_true/piControl/outdata", t=7),
                     froms=c(1543, 1545, 1555, 1712, 1742, 1753, 1842),
                     tos=c(1544, 1554, 1711, 1741, 1752, 1841, 1941), 
                     fpatterns=list(echam=rep("<runid_out>_echam6_<stream>_<YYYY><MM>.grb", t=7),
                                    fesom=rep("<runid_out>_fesom_<variable>_<YYYY>0101.nc", t=7),
                                    jsbach=rep("<runid_out>_jsbach_<stream>_<YYYY><MM>.grb", t=7)))
}

# =================================================

nrunids <- length(exps_in$runids)
nmodels <- length(exps_in$models)
finout_all <- vector("list", l=nrunids)
names(finout_all) <- exps_in$runids
for (runi in 1:nrunids) {

    message("\n", "*****************************************")
    message("process experiment ", runi, " \"", exps_in$runids[runi], "\" ...")
        
    years_in <- exps_in$froms[runi]:exps_in$tos[runi]
    years_out <- exps_out$froms[runi]:exps_out$tos[runi]
    
    tmp <- vector("list", l=nmodels)
    names(tmp) <- exps_in$models
    for (modi in 1:nmodels) {
    
        message("\n", "=====================================")
        message("model ", modi, " \"", exps_in$models[modi], "\" ...")
        
        # model data to link
        fin_wpath <- list.files(paste0(exps_in$outdatapaths[runi], "/", exps_in$models[modi]), full.names=T)
        fin <- basename(fin_wpath)

        # check if there are already existing links
        if (remove_existing_links_in_input_dirs) {
            if (any(Sys.readlink(fin_wpath) != "")) {
                link_inds <- which(Sys.readlink(fin_wpath) != "")
                message("\n", "throw out ", length(link_inds), " links. fin[link_inds]:")
                ht(fin[link_inds], n=3)
                fin <- fin[-link_inds]
                fin_wpath <- fin_wpath[-link_inds]
            }
        }
        
        # only the files with correct extensions
        # --> assumption: extension = substring from last dot in file name until the end
        fpattern_in <- exps_in$fpatterns[[exps_in$models[modi]]][runi]
        ext_in <- substr(fpattern_in, regexpr("\\.[^\\.]*$", fpattern_in) + 1, nchar(fpattern_in)) 
        wrong_ext_inds <- which(substr(fin, nchar(fin) - nchar(ext_in) + 1, nchar(fin)) != ext_in)
        if (length(wrong_ext_inds) > 0){
            if (length(wrong_ext_inds) == length(fin)) {
                stop("\n", "all fin do not have a different extension than the one given in fpattern_in = \"", 
                     fpattern_in, "\". stop here.")
            }
            message("\n", "remove ", length(wrong_ext_inds), " files whose extension is not \"", ext_in, "\":")
            message("fin[wrong_ext_inds]:")
            ht(fin[wrong_ext_inds], n=3)
            fin <- fin[-wrong_ext_inds]
            fin_wpath <- fin_wpath[-wrong_ext_inds]
        }
        
        # throw out zero size files
        if (any(file.size(fin_wpath) == 0)) {
            zero_size_file_inds <- which(file.size(fin_wpath) == 0)
            message("\n", "remove ", length(zero_size_file_inds), " zero size files:")
            ht(fin[zero_size_file_inds], n=3)
            fin <- fin[-zero_size_file_inds]
            fin_wpath <- fin_wpath[-zero_size_file_inds]
        }

        # input files:
        message("\n", "process these fin:")
        ht(fin, n=3)

        # identify correct YYYY, MM, etc. in found files
        # assumption: count <YYYY> (and <MM>) tags backward from end of file name
        message("\n", "determine YYYY ...")
        if (grepl("<YYYY>", fpattern_in)) {
            yyyyinds_fromend <- regexpr("<YYYY>", fpattern_in)
            yyyyinds_fromend <- nchar(fpattern_in) - yyyyinds_fromend - 2 # minus 2: <> from <YYYY>
        } else {
            stop("<YYYY> should be included in fpattern_in = ", fpattern_in, " !?")
        }
        if (grepl("<MM>", fpattern_in)) {
            mminds_fromend <- regexpr("<MM>", fpattern_in)
            mminds_fromend <- nchar(fpattern_in) - mminds_fromend - 2 # minis 2: <> from <MM>
            MM_in <- substr(fin, 
                            nchar(fin) - mminds_fromend,
                            nchar(fin) - mminds_fromend + 1)
            yyyyinds_fromend <- yyyyinds_fromend - 2 # minus 2: <> from <MM>
        }
        YYYY_in <- substr(fin,
                          nchar(fin) - yyyyinds_fromend,
                          nchar(fin) - yyyyinds_fromend + 3)

        # check if any derived year is out of given years
        if (any(is.na(match(YYYY_in, years_in)))) {
            out_of_given_years <- which(is.na(match(YYYY_in, years_in)))
            message("file names contain ", length(out_of_given_years), 
                    " files whose years are out of range of given years ", 
                    min(years_in), "-", max(years_in), ":\n",
                    paste0(unique(YYYY_in[out_of_given_years]), collapse=","), ".\n",
                    "remove them fin[out_of_given_years]:")
            ht(fin[out_of_given_years], n=3)
            fin <- fin[-out_of_given_years]
            fin_wpath <- fin_wpath[-out_of_given_years]
            YYYY_in <- YYYY_in[-out_of_given_years]
            if (exists("MM_in")) MM_in <- MM_in[-out_of_given_years]
        }

        # link names (default: same as input filenames)
        fout <- fin

        # replace years if necessary
        if (exps_in$froms[runi] != exps_out$froms[runi]) {
            message("\n", "change years ...")
            if (length(years_out) != length(years_in)) {
                stop("not implemented")
            }
            if (any(diff(years_in) != 1 || diff(years_out) != 1)) {
                stop("not implemented too")
            }
            
            # replace year labels in filenames
            offset <- unique(years_out - years_in)
            YYYY_out <- as.character(as.integer(YYYY_in) + offset)
            message("year offset = ", offset, " years --> YYYY_in = ", 
                    min(YYYY_in), "-", max(YYYY_in), " -> YYYY_out = ", 
                    min(YYYY_out), "-", max(YYYY_out), " ...")
            substr(fout, nchar(fout) - yyyyinds_fromend, nchar(fout) - yyyyinds_fromend + 3) <- YYYY_out
            message("fout:")
            ht(fout, n=3)
            #if (runi == 3 && modi == 1) stop("asd")

            # for next model
            rm(yyyyinds_fromend)
            if (exists("mminds_fromend")) rm(mminds_fromend)
         
        } # if change year labels

        # modify output link filename structure if necessry
        fpattern_out <- exps_out$fpatterns[[exps_in$models[modi]]][runi]
        if (fpattern_in != fpattern_out) {
            message("\n", "fpattern_in  = \"", fpattern_in, "\"\n",
                    "is different from output\n",
                    "fpattern_out = \"", fpattern_out, "\"\n",
                    " -> ", "modify output link filename structure ...")
       
            ## quick and dirty: consider individual cases
            
            # case 1
            if (fpattern_in == "<variable>_fesom_<YYYY>0101.nc" &&
                fpattern_out == "<runid_out>_fesom_<variable>_<YYYY>0101.nc") {

                message("\n", "special case 1")
                # remove "fesom" which is on wrong position
                fout <- gsub("fesom_", "", fout)
                # put runid and "fesom" in front
                fout <- paste0(exps_in$runids[runi], "_fesom_", fout)
               
            } # which mismatch fpattern_in fpattern_out case

            # finally replace <runid_in> with <runid_out> if necessary
            if (exps_in$runids[runi] != exps_out$runids[runi]) {
                message("\n", "replace <runid_in> with <runid_out> ...")
                fout <- gsub(exps_in$runids[runi], exps_out$runids[runi], fout)
            }

            message("finished modifying output link filename structure. fout:")
            ht(fout, n=3)
        
        } # modify output link filename structure

        message("\n", "fout:")
        ht(fout, n=3)
        tmp[[modi]]$fin <- paste0(exps_in$outdatapaths[runi], "/", exps_in$models[modi], "/", fin)
        tmp[[modi]]$fout <- paste0(exps_out$outdatapath, "/", exps_in$models[modi], "/", fout)

    } # for modi nmodels

    finout_all[[runi]]$files <- tmp
    finout_all[[runi]]$years <- list(years_in=years_in, years_out=years_out)

} # for runi nrunids

# check if there are double entries
if (F) {
    for (runi in 1:nrunids) {
        
        message("\n", "*****************************************")
        message("link files experiment \"", exps_in$runids[runi], "\" ...")
        
        for (modi in 1:nmodels) {
            
            message("\n", "model \"", exps_in$models[modi], "\" ...")

        } # for modi nmodels
    } # for runi runids
}

# link the files
ntotal <- 0
for (runi in 1:nrunids) {
    
    message("\n", "*****************************************")
    message("link files experiment ", runi, " \"", exps_in$runids[runi], "\" ...")
    
    for (modi in 1:nmodels) {
        
        message("\n", "=====================================")
        message("model ", modi, " \"", exps_in$models[modi], "\" ...")

        nf <- length(finout_all[[runi]]$files[[modi]]$fin)
        for (fi in 1:nf) {
            
            # check if output dir exists
            if (fi == 1) {
                if (file.access(dirname(finout_all[[runi]]$files[[modi]]$fout[fi]), 0) == -1) {
                    dir.create(dirname(finout_all[[runi]]$files[[modi]]$fout[fi]), recursive=T, showWarnings=F)
                }
            }
            # link command
            cmd <- paste0("ln -s ", finout_all[[runi]]$files[[modi]]$fin[fi], " ",
                          finout_all[[runi]]$files[[modi]]$fout[fi])
            if (fi == 1) message(cmd)
            cat("\r", fi, "/", nf, sep="")
            if (fi == nf) {
                message()
                message(cmd)
            }
            if (!dry) system(cmd)

        } # for fi nf

        ntotal <- ntotal + nf

    } # for modi nmodels
} # for runi runids

message("\n", "finished: created ", ntotal, " links")

