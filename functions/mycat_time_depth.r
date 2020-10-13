#!/sw/rhel6-x64/r/r-3.3.3-gcc48/bin/Rscript --vanilla

args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
args <- commandArgs(trailingOnly=T)

verbose <- F
remove_temporary_files <- T

if (length(args) == 0 ||
    (length(args) != 0 && length(args) < 4)) { # 1: -o, 2: <outname> 3: f1, 4: f2, ...

    message("\nUsage:\n",
            " $ ", me, " -timeinterval months -timeorigin 1850-01-01 -o <outname> <files_to_cat_seperated_by_space>\n")
    quit()
    
} else if (length(args) != 0 && length(args) >= 4) {

    # get time interval
    timeinterval <- which(args == "-timeinterval")
    if (length(timeinterval) == 0) {
        stop("provide timeinterval with e.g. `-timeinterval monthly`.")
    }
    timeinterval <- as.character(args[timeinterval + 1])
    if (timeinterval != "months") {
        stop("timeinterval \"", timeinterval, "\" not supported yet.")
    }
    
    # get time origin
    timeorigin <- which(args == "-timeorigin")
    if (length(timeorigin) == 0) {
        stop("provide timeorigin with e.g. `-timeorigin 1850-01-01`.")
    }
    timeorigin <- as.character(args[timeorigin + 1])
    
    # get time shift
    timeshift <- which(args == "-timeshift")
    if (length(timeshift) == 1) {
        timeshift <- as.numeric(args[timeshift + 1])
    } else {
        timeshift <- NA
    }
    
    outname <- which(args == "-o")
    if (length(outname) == 0) stop("provide outfile with `-o <outfile>`")
    fs <- args[(outname + 2):length(args)] # files to cat
    outname <- args[outname + 1]
    
    if (verbose) {
        cat("******************\n")
        cat(paste0("timeinterval=", timeinterval), "\n")
        cat(paste0("timeorigin=", timeorigin), "\n")
        if (!is.na(timeshift)) cat(paste0("timeshift=", timeshift), "\n")
        cat(paste0("outname=", outname), "\n")
        for (i in 1:length(fs)) {
            cat(paste0("   ", i, "/", length(fs), ": ", dirname(fs[i]), "/", basename(fs[i])), "\n")
        }
        print(args)
        #stop("asd")
    }

    cat("******************\n")
   
    # reorder dimensions from depth,time --> time,depth
    if (T) { 
        # without does not work
        # depth,time does not work
        # time,depth works but repeats time
        message("reorder dimensions ...")
        for (i in 1:length(fs)) {
            cmd <- paste0("ncpdq -a time,depth ",
                          dirname(fs[i]), "/", basename(fs[i]), " ",
                          dirname(fs[i]), "/r_", basename(fs[i]))
            cat(paste0(i, "/", length(fs), ": ", cmd, " ..."), "\n")
            system(cmd)
        }
    }

    # make time a record dimension
    message("make time to record dimensions ...")
    for (i in 1:length(fs)) {
        cmd <- paste0("ncks -O -4 --mk_rec_dmn time ", 
                      dirname(fs[i]), "/r_", basename(fs[i]), " ", 
                      dirname(fs[i]), "/r_", basename(fs[i]))
        cat(paste0(i, "/", length(fs), ": ", cmd, " ..."), "\n")
        system(cmd)
    } # for i all files

    # cat together all the files
    message("cat ", length(fs), " files together ...")
    cmd <- paste0("ncrcat ", 
                  paste0(dirname(fs), "/r_", basename(fs), collapse=" "), " ",
                  dirname(outname), "/r_", basename(outname))
    cat(paste0(cmd, " ..."), "\n")
    system(cmd)
        
    # remove temporary files
    if (remove_temporary_files) {
        message("remove ", length(fs), " temporary files ...")
        for (i in 1:length(fs)) {
            file.remove(paste0(dirname(fs[i]), "/r_", basename(fs[i])))
        }
    }

    # redo the time dimension from record to normal dimension
    message("make record to time dimension  ...")
    cmd <- paste0("ncks -O --fix_rec_dmn time ", 
                  dirname(outname), "/r_", basename(outname), " ", 
                  dirname(outname), "/", basename(outname))
    cat(paste0(cmd, " ..."), "\n")
    system(cmd)

    # remove temporary catted file 
    if (remove_temporary_files) {
        message("remove temporary file ...")
        file.remove(paste0(dirname(outname), "/r_", basename(outname)))
    }

    # reorder dimensions back from time,depth --> depth,time
    if (T) {
        # without does not work
        # time,depth works for ncview; output ncdump : variable(time, depth)
        message("reorder dimensions ...")
        cmd <- paste0("ncpdq -O -a depth,time ",
                      dirname(outname), "/", basename(outname), " ",
                      dirname(outname), "/", basename(outname))
        cat(paste0(cmd, " ..."), "\n")
        system(cmd)
    }

    # correct new time in units seconds from 1st time point of 1st catted file
    # assumption: no gaps in data gaps
    if (length(timeinterval) != 0 && length(timeorigin) != 0) {
        message("adjust time values of catted file ...")
        cmd <- paste0("ncks -s \"%f\\n\" -H -C -v time ", dirname(outname), "/", basename(outname))
        message(cmd)
        time <- system(cmd, intern=T)
        if (any(time == "")) { # remove blank lines
            time <- time[-which(time == "")]
        }
        time <- as.numeric(time)
        ntime <- length(time)
        if (T) {
            message("ntime=", ntime)
            message("time=", min(time), " to ", max(time))
            message("timeorigin=", timeorigin)
            message("timeinterval=", timeinterval)
            if (!is.na(timeshift)) message("timeshift=", timeshift)
        }
        ## construct new time of catted data
        originlt <- as.POSIXlt(time[1], origin=timeorigin, tz="UTC")
        message("input originlt=", originlt)
        originlist <- unclass(originlt)
        # set day of month to 15 to avoid month jumps due to irregular month lenghts
        # not tested for different time intervals yet!!!
        originlist$mday <- 15 
        # apply time shift if wanted
        if (!is.na(timeshift)) {
            if (T) message("shift years by ", timeshift)
            print(str(originlist))
            originlist$year <- originlist$year + timeshift
            print(str(originlist))
        }
        class(originlist) <- class(originlt)
        originlt <- originlist
        message("output originlt=", originlt)
        # new time
        timelt <- seq(originlt, l=ntime, b=timeinterval)
        timeseconds <- as.numeric(timelt)
        timeseconds_sprintf <- sprintf("%f", timeseconds)
        if (T) {
            message("timelt=", min(timelt), " to ", max(timelt))
            message("timeseconds=", min(timeseconds), " to ", max(timeseconds))
            message("timeseconds_sprintf=", timeseconds_sprintf[1], " to ", timeseconds_sprintf[ntime])
            timeltback <- as.POSIXlt(timeseconds, origin="1970-01-01", tz="UTC")
            message("timeltback=", min(timeltback), " to ", max(timeltback))
        }

        # temporary runscript
        cmdtime <- c(paste0("time[time]={", 
                            paste(timeseconds_sprintf, collapse=","), # need the sprintf here!!! 
                            "};"),
                     "time@units=\"seconds since 1970-01-01\";")
        cmdfile <- paste0("cmd_", Sys.getpid())
        writeLines(text=cmdtime, con=cmdfile)
        
        # run ncap2 command 
        cmd <- paste0("ncap2 -O -S ", cmdfile, " ",
                      dirname(outname), "/", basename(outname), " ",
                      dirname(outname), "/", basename(outname))
        message(cmd)
        system(cmd)

        # delete temporary runscript
        if (remove_temporary_files) file.remove(cmdfile)
    
    } # correct time dimension of catted result file

    cat("******************\n")

}

