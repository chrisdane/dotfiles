#!/usr/bin/env Rscript

if (!interactive()) {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    usage <- paste0("\nUsage:\n $ ", me, " namelist1 namelist2\n\n")

    # check
    if (length(args) != 2) {
        cat(usage)
        quit()
    }

} else { # interactive session
    rm(list=ls())
    args <- c("/work/ba1103/a270094/AWIESM/test/work/namelist.recom", # old
              "/work/ba1103/a270073/out/awicm-1.0-recom/test/run_19500101-19500131/work/namelist.recom") # new
    args <- c("/work/ollie/cdanek/out/awicm-1.0-recom/from_oezguer/namelist.recom", # old
              "/work/ollie/cdanek/out/awicm-1.0-recom/test4/run_19500101-19500131/work/namelist.recom") # new
    args <- c("/home/ollie/cdanek/esm/esm_tools/namelists/echam/6.3.04p1/PI-CTRL/first/namelist.echam",
              "/home/ollie/cdanek/esm/esm_tools/namelists/echam/6.3.04p1/PI-CTRL/last/namelist.echam")
    args <- c("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr/esm-piControl_oezguer_nml/namelist.echam",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr/esm-piControl/run_19500101-19500131/work/namelist.echam")
    args <- c("/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl_oezguer_nml/namelist.echam",
              "/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-piControl/run_19500101-19501231/work/namelist.echam")
}

# check
if (!file.exists(args[1])) stop("file ", args[1], " does not exist")
if (!file.exists(args[2])) stop("file ", args[2], " does not exist")
args <- normalizePath(args)

options(width=3000) # increase length per print line from default 80

if (T) {
    #install.packages("devtools")
    #devtools::install_github("jsta/nml")
    library(nml) # https://github.com/jsta/nml
} else {
    source("~/scripts/r/packages/src/nml/R/utils.R")
    source("~/scripts/r/packages/src/nml/R/read.R")
}
cat("read nml1 ", args[1], " ...\n", sep="")
nml1 <- suppressWarnings(read_nml(args[1])) # suppress non-".nml" file ending warning
cat("read nml2 ", args[2], " ...\n", sep="")
nml2 <- suppressWarnings(read_nml(args[2])) # nml::read_nml() takes care of multi-line entries and white spaces etc.

# lower all capitals for better comparison
names(nml1) <- tolower(names(nml1))
names(nml2) <- tolower(names(nml2))
for (i in seq_along(nml1)) {
    names(nml1[[i]]) <- tolower(names(nml1[[i]]))
}
for (i in seq_along(nml2)) {
    names(nml2[[i]]) <- tolower(names(nml2[[i]]))
}

# compare chapter-wise
chapters1 <- names(nml1)
chapters2 <- names(nml2)
chapters_unique <- unique(c(chapters1, chapters2))

# check all nml1 variables if they also occur in nml2 (case 1/3) or if they are unique to nml1 (case 2/3)
nml1_and_nml2 <- nml1_but_not_nml2 <- nml2_but_not_nml1 <- list()
for (i in seq_along(chapters_unique)) { # loop through all unique chapter names
#for (i in 5) { 

    chapter <- chapters_unique[i] # name of current chapter
    inds1 <- which(chapters1 == chapter)
    inds2 <- which(chapters2 == chapter)
    
    # nml1 doesnt have current chapter; must come from nml2
    if (length(inds1) == 0) { 
        for (ch2i in seq_along(inds2)) {
            cat("************************** detected unique chapter **************************\n",
                "nml2 chapter \"", chapters2[inds2[ch2i]], "\" not in nml1:\n", sep="")
            cat(capture.output(str(nml2[inds2[ch2i]])), sep="\n")
            nml2_but_not_nml1[length(nml2_but_not_nml1)+1] <- nml2[inds2[ch2i]]
            names(nml2_but_not_nml1)[length(nml2_but_not_nml1)] <- chapter
        }
    
    # nml2 doesnt have current chapter; must come from nml1
    } else if (length(inds2) == 0) { 
        for (ch1i in seq_along(inds1)) {
            cat("************************** detected unique chapter **************************\n",
                "nml1 chapter \"", chapters1[inds1[ch1i]], "\" not in nml2:\n", sep="") 
            cat(capture.output(str(nml1[inds1[ch1i]])), sep="\n")
            nml1_but_not_nml2[length(nml1_but_not_nml2)+1] <- nml1[inds1[ch1i]]
            names(nml1_but_not_nml2)[length(nml1_but_not_nml2)] <- chapter
        }
    
    # both nml have current chapter
    } else if (length(inds1) > 0 && length(inds2) > 0) {
        
        if (length(inds1) != length(inds2)) {
            cat("chapter \"", chapter, "\" occurs ", length(inds1), " times in nml1 and ",
                length(inds2), " times in nml2\n", sep="")
        }

        # try to find chapter2 that matches current chapter1
        for (ch1i in seq_along(inds1)) {
            cha1 <- nml1[[inds1[ch1i]]]
            keys1 <- names(cha1)

            for (ch2i in seq_along(inds2)) {
                cha2 <- nml2[[inds2[ch2i]]]
                keys2 <- names(cha2)

                # special case
                if (chapter == "mvstreamctl") {
                    
                    # check for unknown keys 
                    if (all(is.na(match(c("source", "interval", "target", "filetag",
                                          "variables", "meannam", "sqrmeannam"),
                                        keys1)))) {
                        stop("unknown \"", chapter, "\" keys: ", paste(keys1, collapse=", "))
                    }
                    if (all(is.na(match(c("source", "interval", "target", "filetag",
                                          "variables", "meannam", "sqrmeannam"),
                                        keys2)))) {
                        stop("unknown \"", chapter, "\" keys: ", paste(keys2, collapse=", "))
                    }
                    
                    # source
                    if (!any(keys1 == "source") || !any(keys2 == "source")) {
                        stop("this never happened")
                    } else {
                        source1 <- cha1$source
                        source2 <- cha2$source
                    }
                    
                    # interval
                    # echam docu p 201: interval:
                    # "time averaging interval"
                    # default: interval=`putdata`            if `default_output`=false
                    #          interval=1,’months’,’first’,0 if `default_output`=true (= every month)
                    # e.g. `1,’steps’,’first’,0` (every step) or `6,’hours’,’first’,0` (6-hourly)
                    # echam docu p 37: putdata:
                    # "time interval at which output data are written to output files
                    # default: 12,’hours’,’first’,0
                    if (any(keys1 == "interval")) { 
                        interval1 <- cha1$interval # e.g. "1,months,first,0"
                    } else {
                        stop("never happened")
                    }
                    if (any(keys2 == "interval")) {
                        interval2 <- cha2$interval # e.g. "1,months,first,0"
                    } else {
                        stop("never happened")
                    }
                    
                    # target
                    target1 <- target2 <- NA # default: not given
                    if (any(keys1 == "target")) target1 <- cha1$target
                    if (any(keys1 == "target")) target2 <- cha2$target

                    # filetag
                    filetag1 <- filetag2 <- NA # default: not given
                    if (any(keys1 == "filetag")) filetag1 <- cha1$filetag
                    if (any(keys2 == "filetag")) filetag2 <- cha2$filetag
                    
                    # variables
                    if (any(keys1 == "variables")) { 
                        variables1 <- strsplit(cha1$variables, ",")[[1]] # e.g. "st:mean" or "tslm1" or "irsucs:inst>irsucs_pt=6"
                    } else {
                        variables1 <- NA
                    }
                    if (any(keys2 == "variables")) { 
                        variables2 <- strsplit(cha2$variables, ",")[[1]] # e.g. "st:mean" or "tslm1" or "irsucs:inst>irsucs_pt=6"
                    } else {
                        variables2 <- NA
                    }
                    
                    # meannam
                    if (any(keys1 == "meannam")) { 
                        meannam1 <- strsplit(cha1$meannam, ",")[[1]] # e.g. "st:mean" or "tslm1" or "irsucs:inst>irsucs_pt=6"
                    } else {
                        meannam1 <- NA
                    }
                    if (any(keys2 == "meannam")) { 
                        meannam2 <- strsplit(cha2$meannam, ",")[[1]] # e.g. "st:mean" or "tslm1" or "irsucs:inst>irsucs_pt=6"
                    } else {
                        meannam2 <- NA
                    }
                    
                    # sqrmeannam
                    if (any(keys1 == "sqrmeannam")) { 
                        sqrmeannam1 <- strsplit(cha1$sqrmeannam, ",")[[1]] # e.g. "st:mean" or "tslm1" or "irsucs:inst>irsucs_pt=6"
                    } else {
                        sqrmeannam1 <- NA
                    }
                    if (any(keys2 == "sqrmeannam")) { 
                        sqrmeannam2 <- strsplit(cha2$sqrmeannam, ",")[[1]] # e.g. "st:mean" or "tslm1" or "irsucs:inst>irsucs_pt=6"
                    } else {
                        sqrmeannam2 <- NA
                    }
                    
                    # get well structured data frames from input chapter
                    mvstream1_df <- mvstream2_df <- data.frame()
                    
                    # add cha1 "variables"
                    for (vi in seq_along(variables1)) {
                        if (all(is.na(variables1))) {
                            name_type <- c(NA, NA)
                        } else {
                            name_type <- strsplit(variables1[vi], ":")[[1]]
                            if (length(name_type) > 1) { # case 1/3 "st:mean" or "irsucs:inst>irsucs_pt=6"
                                if (any(grepl(">", name_type))) { # case 2/3 "irsucs:inst>irsucs_pt=6"
                                    name_type <- c(name_type[1], strsplit(name_type[2], ">")[[1]][1])
                                }
                            } else {
                                name_type <- c(name_type, "mean") # case 3/3 "tslm1" --> mean
                            }
                        }
                        row <- data.frame(variables=name_type[1], type=name_type[2], 
                                          meannam=NA, sqrmeannam=NA, 
                                          source=source1, target=target1, filetag=filetag1, 
                                          interval=interval1, orig=variables1[vi], 
                                          stringsAsFactors=F)
                        mvstream1_df <- rbind(mvstream1_df, row)
                    } # for vi in variables1

                    # add cha1 "meannam"
                    for (vi in seq_along(meannam1)) {
                        if (all(is.na(meannam1))) {
                            meannam <- NA
                        } else {
                            meannam <- strsplit(meannam1[vi], "[[:punct:]]")[[1]] # any special characters
                            if (length(meannam) > 1) {
                                stop("never happened")
                            }
                        }
                        row <- data.frame(variables=NA, type=NA,
                                          meannam=meannam, 
                                          sqrmeannam=NA, 
                                          source=source1, target=target1, filetag=filetag1, 
                                          interval=interval1, orig=meannam1[vi], 
                                          stringsAsFactors=F)
                        mvstream1_df <- rbind(mvstream1_df, row)
                    } # for vi in meannam1
                        
                    # add cha1 "sqrmeannam"
                    for (vi in seq_along(sqrmeannam1)) {
                        if (all(is.na(sqrmeannam1))) {
                            sqrmeannam <- NA
                        } else {
                            sqrmeannam <- strsplit(sqrmeannam1[vi], "[[:punct:]]")[[1]] # any special characters
                            if (length(sqrmeannam) > 1) {
                                stop("never happened")
                            }
                        }
                        row <- data.frame(variables=NA, type=NA,
                                          meannam=NA, 
                                          sqrmeannam=sqrmeannam, 
                                          source=source1, target=target1, filetag=filetag1, 
                                          interval=interval1, orig=sqrmeannam1[vi], 
                                          stringsAsFactors=F)
                        mvstream1_df <- rbind(mvstream1_df, row)
                    } # for vi in sqrmeannam1                     
                  
                    # add cha2 "variables"
                    for (vi in seq_along(variables2)) {
                        if (all(is.na(variables2))) {
                            name_type <- c(NA, NA)
                        } else {
                            name_type <- strsplit(variables2[vi], ":")[[1]]
                            if (length(name_type) > 1) { # case 1/3 "st:mean" or "irsucs:inst>irsucs_pt=6"
                                if (any(grepl(">", name_type))) { # case 2/3 "irsucs:inst>irsucs_pt=6"
                                    name_type <- c(name_type[1], strsplit(name_type[2], ">")[[1]][1])
                                }
                            } else {
                                name_type <- c(name_type, "mean") # case 3/3 "tslm1" --> mean
                            }
                        }
                        row <- data.frame(variables=name_type[1], type=name_type[2], 
                                          meannam=NA, sqrmeannam=NA, 
                                          source=source2, target=target2, filetag=filetag2, 
                                          interval=interval2, orig=variables2[vi], 
                                          stringsAsFactors=F)
                        mvstream2_df <- rbind(mvstream2_df, row)
                    } # for vi in variables2

                    # add cha2 "meannam"
                    for (vi in seq_along(meannam2)) {
                        if (all(is.na(meannam2))) {
                            meannam <- NA
                        } else {
                            meannam <- strsplit(meannam2[vi], "[[:punct:]]")[[1]] # any special characters
                            if (length(meannam) > 1) {
                                stop("never happened")
                            }
                        }
                        row <- data.frame(variables=NA, type=NA,
                                          meannam=meannam, 
                                          sqrmeannam=NA, 
                                          source=source2, target=target2, filetag=filetag2, 
                                          interval=interval2, orig=meannam2[vi], 
                                          stringsAsFactors=F)
                        mvstream2_df <- rbind(mvstream2_df, row)
                    } # for vi in meannam2
                        
                    # add cha2 "sqrmeannam"
                    for (vi in seq_along(sqrmeannam2)) {
                        if (all(is.na(sqrmeannam2))) {
                            sqrmeannam <- NA
                        } else {
                            sqrmeannam <- strsplit(sqrmeannam2[vi], "[[:punct:]]")[[1]] # any special characters
                            if (length(sqrmeannam) > 1) {
                                stop("never happened")
                            }
                        }
                        row <- data.frame(variables=NA, type=NA,
                                          meannam=NA, 
                                          sqrmeannam=sqrmeannam, 
                                          source=source2, target=target2, filetag=filetag2, 
                                          interval=interval2, orig=sqrmeannam2[vi], 
                                          stringsAsFactors=F)
                        mvstream2_df <- rbind(mvstream2_df, row)
                    } # for vi in sqrmeannam2         
                   
                    # sort alphabetically
                    if (!all(is.na(mvstream1_df$variables))) {
                        mvstream1_df <- mvstream1_df[sort(mvstream1_df$variables, index.return=T)$ix,]
                    } else if (!all(is.na(mvstream1_df$meannam))) {
                        mvstream1_df <- mvstream1_df[sort(mvstream1_df$meannam, index.return=T)$ix,]
                    } else if (!all(is.na(mvstream1_df$sqrmeannam))) {
                        mvstream1_df <- mvstream1_df[sort(mvstream1_df$sqrmeannam, index.return=T)$ix,]
                    } else {
                        # none of "variables", "meannam", "sqrmeannam" given in current mvstreamctl chapter
                    }
                    if (!all(is.na(mvstream2_df$variables))) {
                        mvstream2_df <- mvstream2_df[sort(mvstream2_df$variables, index.return=T)$ix,]
                    } else if (!all(is.na(mvstream2_df$meannam))) {
                        mvstream2_df <- mvstream2_df[sort(mvstream2_df$meannam, index.return=T)$ix,]
                    } else if (!all(is.na(mvstream2_df$sqrmeannam))) {
                        mvstream2_df <- mvstream2_df[sort(mvstream2_df$sqrmeannam, index.return=T)$ix,]
                    } else {
                        # none of "variables", "meannam", "sqrmeannam" given in current mvstreamctl chapter
                    }
                    
                    # add number column 
                    mvstream1_df <- cbind(no=seq_len(dim(mvstream1_df)[1]), mvstream1_df)
                    mvstream2_df <- cbind(no=seq_len(dim(mvstream2_df)[1]), mvstream2_df)
                    
                    # compare mvstreamctl blocks 
                    if (identical(mvstream1_df, mvstream2_df)) { # mvstreamctl blocks are identical
                        nml1_and_nml2[length(nml1_and_nml2)+1] <- list(list(nml1=cha1, nml2=cha2,
                                                                            nml1_df=mvstream1_df, 
                                                                            nml2_df=mvstream2_df))
                        names(nml1_and_nml2)[length(nml1_and_nml2)] <- chapter
                        
                    } else { # mvstreamctl blocks differ
                        # check different levels of similarity of mvstreamctl blocks
                        # approach here: do they have the same source or variables? if yes, print them
                        if (identical(mvstream1_df$source, mvstream2_df$source)) { # same source
                            if (identical(mvstream1_df$variables, mvstream2_df$variables)) { # same vars
                                cat("************************** detected diffs in similar mvstreamctl chapters **************************\n",
                                    "nml1 \"", chapter, "\" chapter ", ch1i, "/", 
                                    length(inds1), " and nml2 \"", chapter, "\" chapter ", 
                                    ch2i, "/", length(inds2), " share the same \"source\" and variables but differ:\n", sep="")
                                print(mvstream1_df, row.names=F)
                                print(mvstream2_df, row.names=F)
                            }
                        }
                        nml1_but_not_nml2[length(nml1_but_not_nml2)+1] <- list(list(nml1=cha1, nml1_df=mvstream1_df))
                        names(nml1_but_not_nml2)[length(nml1_but_not_nml2)] <- chapter
                        nml2_but_not_nml1[length(nml2_but_not_nml1)+1] <- list(list(nml2=cha2, nml2_df=mvstream2_df))
                        names(nml2_but_not_nml1)[length(nml2_but_not_nml1)] <- chapter
                    
                    } # are mvstreamctl blocks are identical or not

                # special case
                } else if (chapter == "set_stream") {
                    
                    # check for unknown keys 
                    if (all(is.na(match(c("stream", "lpost", "lrerun"), keys1)))) {
                        stop("unknown \"", chapter, "\" keys: ", paste(keys1, collapse=", "))
                    }
                    if (all(is.na(match(c("stream", "lpost", "lrerun"), keys2)))) {
                        stop("unknown \"", chapter, "\" keys: ", paste(keys2, collapse=", "))
                    }
                    
                    # stream
                    if (!any(keys1 == "stream") || !any(keys2 == "stream")) {
                        stop("this never happened")
                    } else {
                        stream1 <- cha1$stream
                        stream2 <- cha2$stream
                    }

                    # lpost
                    lpost1 <- lpost2 <- NA # default: not given
                    if (any(keys1 == "lpost")) lpost1 <- cha1$lpost
                    if (any(keys1 == "lpost")) lpost2 <- cha2$lpost
                    
                    # lrerun
                    lrerun1 <- lrerun2 <- NA # default: not given
                    if (any(keys1 == "lrerun")) lrerun1 <- cha1$lrerun
                    if (any(keys1 == "lrerun")) lrerun2 <- cha2$lrerun

                    stream1_df <- data.frame(stream=stream1, lpost=lpost1, lrerun=lrerun1, stringsAsFactors=F)
                    stream2_df <- data.frame(stream=stream2, lpost=lpost2, lrerun=lrerun2, stringsAsFactors=F)
                        
                    # sort alphabetically
                    stream1_df <- stream1_df[sort(stream1_df$stream, index.return=T)$ix,]
                    stream2_df <- stream2_df[sort(stream2_df$stream, index.return=T)$ix,]
                    stream1_df <- cbind(no=seq_len(dim(stream1_df)[1]), stream1_df)
                    stream2_df <- cbind(no=seq_len(dim(stream2_df)[1]), stream2_df)
                    
                    # compare set_stream blocks 
                    if (identical(stream1_df, stream2_df)) { #  blocks are identical
                        nml1_and_nml2[length(nml1_and_nml2)+1] <- list(list(nml1=cha1, nml2=cha2,
                                                                            nml1_df=stream1_df, 
                                                                            nml2_df=stream2_df))
                        names(nml1_and_nml2)[length(nml1_and_nml2)] <- chapter
                        
                    } else { # set_stream blocks differ
                        # check different levels of similarity of set_stream blocks
                        # approach here: do they have the same stream? if yes, print them
                        if (identical(stream1_df$stream, stream2_df$stream)) { # same stream
                            cat("************************** detected diffs in similar set_stream chapters **************************\n",
                                "nml1 \"", chapter, "\" chapter ", ch1i, "/", 
                                length(inds1), " and nml2 \"", chapter, "\" chapter ", 
                                ch2i, "/", length(inds2), " share the same \"stream\" but differ:\n", sep="")
                            print(stream1_df, row.names=F)
                            print(stream2_df, row.names=F)
                        }
                        nml1_but_not_nml2[length(nml1_but_not_nml2)+1] <- list(list(nml1=cha1, nml1_df=stream1_df))
                        names(nml1_but_not_nml2)[length(nml1_but_not_nml2)] <- chapter
                        nml2_but_not_nml1[length(nml2_but_not_nml1)+1] <- list(list(nml2=cha2, nml2_df=stream2_df))
                        names(nml2_but_not_nml1)[length(nml2_but_not_nml1)] <- chapter
                    
                    } # are set_stream blocks are identical or not

                # special case
                } else if (chapter == "set_stream_element") {
                    
                    # check for unknown keys 
                    if (all(is.na(match(c("stream", "name", "code", "lpost"), keys1)))) {
                        stop("unknown \"", chapter, "\" keys: ", paste(keys1, collapse=", "))
                    }
                    if (all(is.na(match(c("stream", "name", "code", "lpost"), keys2)))) {
                        stop("unknown \"", chapter, "\" keys: ", paste(keys2, collapse=", "))
                    }
                    
                    # stream
                    if (!any(keys1 == "stream") || !any(keys2 == "stream")) {
                        stop("this never happened")
                    } else {
                        stream1 <- cha1$stream
                        stream2 <- cha2$stream
                    }

                    # name
                    name1 <- name2 <- NA # default: not given
                    if (any(keys1 == "name")) name1 <- cha1$name
                    if (any(keys1 == "name")) name2 <- cha2$name
                    
                    # code
                    code1 <- code2 <- NA # default: not given
                    if (any(keys1 == "code")) code1 <- cha1$code
                    if (any(keys1 == "code")) code2 <- cha2$code
                    
                    # lpost
                    lpost1 <- lpost2 <- NA # default: not given
                    if (any(keys1 == "lpost")) lpost1 <- cha1$lpost
                    if (any(keys1 == "lpost")) lpost2 <- cha2$lpost

                    stream_elem1_df <- data.frame(stream=stream1, name=name1, code=code1, lpost=lpost1, stringsAsFactors=F)
                    stream_elem2_df <- data.frame(stream=stream2, name=name2, code=code2, lpost=lpost2, stringsAsFactors=F)
                        
                    # sort alphabetically
                    stream_elem1_df <- stream_elem1_df[sort(stream_elem1_df$stream, index.return=T)$ix,]
                    stream_elem2_df <- stream_elem2_df[sort(stream_elem2_df$stream, index.return=T)$ix,]
                    stream_elem1_df <- cbind(no=seq_len(dim(stream_elem1_df)[1]), stream_elem1_df)
                    stream_elem2_df <- cbind(no=seq_len(dim(stream_elem2_df)[1]), stream_elem2_df)
                    
                    # compare set_stream_element blocks 
                    if (identical(stream_elem1_df, stream_elem2_df)) { #  blocks are identical
                        nml1_and_nml2[length(nml1_and_nml2)+1] <- list(list(nml1=cha1, nml2=cha2,
                                                                            nml1_df=stream_elem1_df, 
                                                                            nml2_df=stream_elem2_df))
                        names(nml1_and_nml2)[length(nml1_and_nml2)] <- chapter
                        
                    } else { # set_stream_element blocks differ
                        # check different levels of similarity of set_stream_element blocks
                        # approach here: do they have the same stream? if yes, print them
                        if (identical(stream_elem1_df$stream, stream_elem2_df$stream)) { # same stream
                            cat("************************** detected diffs in similar similar set_stream_element chapters **************************\n",
                                "nml1 \"", chapter, "\" chapter ", ch1i, "/", 
                                length(inds1), " and nml2 \"", chapter, "\" chapter ", 
                                ch2i, "/", length(inds2), " share the same \"stream\" but differ:\n", sep="")
                            print(stream_elem1_df, row.names=F)
                            print(stream_elem2_df, row.names=F)
                        }
                        nml1_but_not_nml2[length(nml1_but_not_nml2)+1] <- list(list(nml1=cha1, nml1_df=stream_elem1_df))
                        names(nml1_but_not_nml2)[length(nml1_but_not_nml2)] <- chapter
                        nml2_but_not_nml1[length(nml2_but_not_nml1)+1] <- list(list(nml2=cha2, nml2_df=stream_elem2_df))
                        names(nml2_but_not_nml1)[length(nml2_but_not_nml1)] <- chapter
                    
                    } # are set_stream_element blocks are identical or not

                # default case
                } else { 
                    
                    # for better comparison, make df out of list
                    # --> NA becomes "NA" 
                    cha1_df <- cha2_df <- data.frame()
                    for (j in seq_along(cha1)) { # for every entry in current chapter1
                        name <- names(cha1)[j]
                        # have to convert multi-element list entries to string of length 1
                        # -> e.g. 6-element list entry `$dt_start = 2285   12   31   23   52   30` to "2285 12 31 23 30"
                        val <- paste(cha1[[j]], collapse=" ")
                        row <- data.frame(name=name, val=val, stringsAsFactors=F)
                        cha1_df <- rbind(cha1_df, row)
                    }
                    for (j in seq_along(cha2)) { # repeat for chapter2
                        name <- names(cha2)[j]
                        val <- paste(cha2[[j]], collapse=" ")
                        row <- data.frame(name=name, val=val, stringsAsFactors=F)
                        cha2_df <- rbind(cha2_df, row)
                    }

                    # sort alphabetically
                    cha1_df <- cha1_df[sort(cha1_df$name, index.return=T)$ix,]
                    cha2_df <- cha2_df[sort(cha2_df$name, index.return=T)$ix,]
                    cha1_df <- cbind(no=seq_len(dim(cha1_df)[1]), cha1_df)
                    cha2_df <- cbind(no=seq_len(dim(cha2_df)[1]), cha2_df)

                    # check if both chapters are identical
                    if (identical(cha1_df$name, cha2_df$name) &&
                        identical(cha1_df$val, cha2_df$val)) {
                        nml1_and_nml2[length(nml1_and_nml2)+1] <- list(list(nml1=cha1, nml2=cha2,
                                                                            nml1_df=cha1_df, nml2_df=cha2_df))
                        names(nml1_and_nml2)[length(nml1_and_nml2)] <- chapter
                    
                    } else {
                        cat("************************** detected diffs default chapter **************************\n",
                            "nml1 \"", chapter, "\" chapter ", ch1i, "/", 
                            length(inds1), " and nml2 \"", chapter, "\" chapter ", 
                            ch2i, "/", length(inds2), " differ:\n", sep="")
                        keys_unique <- unique(c(cha1_df$name, cha2_df$name))
                        for (keyi in seq_along(keys_unique)) {
                            key <- keys_unique[keyi]
                            keyinds1 <- which(cha1_df$name == key)
                            keyinds2 <- which(cha2_df$name == key)
                            if (length(keyinds1) > 1) {
                                stop("key \"", key, "\" occurs ", length(keyinds1), " times in chapter \"", 
                                     chapter, "\" of nml1. this chapter is not defined as special case ",
                                     "and so every key must occur not or once only.")
                            }
                            if (length(keyinds2) > 1) {
                                stop("key \"", key, "\" occurs ", length(keyinds2), " times in chapter \"", 
                                     chapter, "\" of nml2. this chapter is not defined as special case ",
                                     "and so every key must occur not or once only.")
                            }
                            if (length(keyinds1) == 0) { # key occurs in nml2 only
                                cat("   nml2 key \"", key, "\" = \"", cha2_df$val[keyinds2], "\" does not occur in nml1\n", sep="")
                            }
                            if (length(keyinds2) == 0) { # key occurs in nml1 only
                                cat("   nml1 key \"", key, "\" = \"", cha1_df$val[keyinds1], "\" does not occur in nml2\n", sep="")
                            }
                            if (length(keyinds1) == 1 && length(keyinds2) == 1) { # default case: one entry per chapter per nml
                                if (cha1_df$val[keyinds1] != cha2_df$val[keyinds2]) {
                                    cat("   nml1 key \"", key, "\" = \"", cha1_df$val[keyinds1], "\"\n",
                                        "   nml2 key \"", key, "\" = \"", cha2_df$val[keyinds2], "\"\n", sep="")
                                }
                            }
                        
                        } # for keyi in keys
                        
                        nml1_but_not_nml2[length(nml1_but_not_nml2)+1] <- list(list(nml1=cha1, nml1_df=cha1_df))
                        names(nml1_but_not_nml2)[length(nml1_but_not_nml2)] <- chapter
                        nml2_but_not_nml1[length(nml2_but_not_nml1)+1] <- list(list(nml2=cha2, nml2_df=cha2_df))
                        names(nml2_but_not_nml1)[length(nml2_but_not_nml1)] <- chapter

                    } # are current nml1 and nml2 chapters are identical or not

                } # default nml block or one of specail cases (mvstreamctl, set_stream, set_stream_element)
            
            } # for ch2i
        } # for ch1i
    
    } # how often current chapter occurs in nml1 and nml2

} # for i in chapters_unique

# do stuff with `nml1_and_nml2` `nml1_but_not_nml2` `nml2_but_not_nml1`
# ...

options(width=80) # restore default

