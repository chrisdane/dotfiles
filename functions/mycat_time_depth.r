#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
args <- commandArgs(trailingOnly=T)

remove_temporary_files <- T

if (length(args) < 4) {
    message("\nUsage:\n",
            " $ ", me, 
            " --timeinterval=monthly ",
            "--newtimeorigin=1850-1-1 ",
            "--out=<outname> ",
            "<files_to_cat_seperated_by_space>\n")
    quit()
}
    
# get time interval
if (any(grepl("--timeinterval", args))) {
    timeinterval <- sub("--timeinterval=", "", args[grep("--timeinterval=", args)])
} else {
    stop("provide timeinterval with e.g. `--timeinterval=monthly`")
}
if (timeinterval == "monthly") {
    timeinterval_posix <- "1 mon"
} else {
    stop("timeinterval \"", timeinterval, "\" not supported yet.")
}

# get time origin
if (any(grepl("--newtimeorigin", args))) {
    newtimeorigin <- sub("--newtimeorigin=", "", args[grep("--newtimeorigin=", args)])
} else {
    stop("provide new timeorigin with e.g. `--newtimeorigin=1850-01-01`")
}

# get outname
if (any(grepl("--out", args))) {
    outname <- sub("--out=", "", args[grep("--out=", args)])
} else {
    stop("provide outfile with `--out=<outfile>`")
}

# get files to cat 
fs <- args[(grep("--out", args)+1):length(args)] # rest of args

# checks done 
if (T) {
    cat("\ninput:\n")
    cat(paste0("timeinterval  = ", timeinterval, " --> timeinterval_posix = ", timeinterval_posix), "\n")
    cat(paste0("newtimeorigin = ", newtimeorigin), "\n")
    cat(paste0("outname       = ", outname), "\n")
    for (i in 1:length(fs)) {
        cat(paste0("file ", i, "/", length(fs), " = ", dirname(fs[i]), "/", basename(fs[i])), "\n")
    }
    #print(args)
    #stop("asd")
}

# reorder dimensions from depth,time --> time,depth
if (T) { 
    # without does not work
    # depth,time does not work
    # time,depth works but repeats time
    message("\nstep 1: reorder dimensions (depth,time) --> (time,depth) ...")
    for (i in 1:length(fs)) {
        cmd <- paste0("ncpdq -a time,depth ",
                      dirname(fs[i]), "/", basename(fs[i]), " ",
                      dirname(fs[i]), "/r_", basename(fs[i]))
        cat(paste0(i, "/", length(fs), ": ", cmd, " ..."), "\n")
        system(cmd)
    }
}

# make time a record dimension
message("\nstep 2: make \"time\"-dim to record dim ...")
for (i in 1:length(fs)) {
    cmd <- paste0("ncks -O -4 --mk_rec_dmn time ", 
                  dirname(fs[i]), "/r_", basename(fs[i]), " ", 
                  dirname(fs[i]), "/r_", basename(fs[i]))
    cat(paste0(i, "/", length(fs), ": ", cmd, " ..."), "\n")
    system(cmd)
} # for i all files

# cat together all the files
message("\nstep 3: cat ", length(fs), " files together ...")
cmd <- paste0("ncrcat ", 
              paste0(dirname(fs), "/r_", basename(fs), collapse=" "), " ",
              dirname(outname), "/r_", basename(outname))
cat(paste0(cmd, " ..."), "\n")
system(cmd)
    
# remove temporary files
if (remove_temporary_files) {
    message("\n`remove_temporary_files`=T --> remove ", length(fs), " temporary files ...")
    for (i in 1:length(fs)) {
        check <- file.remove(paste0(dirname(fs[i]), "/r_", basename(fs[i])))
        if (!check) stop("should not happen")
    }
}

# redo the time dimension from record to normal dimension
message("\nstep 4: revert \"record\"-dim back to time dimension  ...")
cmd <- paste0("ncks -O --fix_rec_dmn time ", 
              dirname(outname), "/r_", basename(outname), " ", 
              dirname(outname), "/", basename(outname))
cat(paste0(cmd, " ..."), "\n")
system(cmd)

# remove temporary catted file 
if (remove_temporary_files) {
    message("\n`remove_temporary_files`=T -- remove temporary catted file ...")
    check <- file.remove(paste0(dirname(outname), "/r_", basename(outname)))
    if (!check) stop("should not happen")
}

# reorder dimensions back from time,depth --> depth,time
if (T) {
    # without does not work
    # time,depth works for ncview; output ncdump : variable(time, depth)
    message("\nstep 5: reorder dimensions back (time,depth) --> (depth,time) ...")
    cmd <- paste0("ncpdq -O -a depth,time ",
                  dirname(outname), "/", basename(outname), " ",
                  dirname(outname), "/", basename(outname))
    cat(paste0(cmd, " ..."), "\n")
    system(cmd)
}

# correct new time in units seconds from 1st time point of 1st catted file
# assumption: no gaps in data gaps
message("\nstep 6: adjust time values of catted file ...")
cmd <- paste0("ncks -s \"%f\\n\" -H -C -v time ", dirname(outname), "/", basename(outname)) # cdo too unflexible
message(cmd)
time <- system(cmd, intern=T)
if (any(time == "")) { # remove blank lines
    time <- time[-which(time == "")]
}
time <- as.numeric(time)
ntime <- length(time)
message("--> ntime = ", ntime)

# construct new time of catted data
message("make POSIX from newtimeorigin = ", newtimeorigin, " ...")
originlt <- as.POSIXlt(newtimeorigin, o="1970-1-1", tz="UTC") # error if not successful
#message("--> output originlt    = ", originlt)

if (timeinterval == "monthly") {
    # set day of month to 15 to avoid month jumps due to irregular month lenghts
    originlt$mday <- 15 
} else {
    warning("timeinterval \"", timeinterval, "\" not tested here", .immediate=T)
}

# new time
message("make new time of length ", ntime, " from ", originlt, " by ", timeinterval_posix, "\n",
        "--> assumption: no temporal gaps")
timect <- seq.POSIXt(originlt, l=ntime, b=timeinterval_posix)
if (length(timect) != ntime) stop("new time is of length ", length(timect), " but ntime = ", ntime)
timeseconds <- as.numeric(timect)
timeseconds_sprintf <- sprintf("%f", timeseconds)
if (F) {
    message("timect              = ", min(timect), " to ", max(timect))
    message("timeseconds         = ", min(timeseconds), " to ", max(timeseconds))
    message("timeseconds_sprintf = ", timeseconds_sprintf[1], " to ", timeseconds_sprintf[ntime])
    timectback <- as.POSIXlt(timeseconds, origin="1970-01-01", tz="UTC")
    message("timectback          = ", min(timectback), " to ", max(timectback))
}

# apply new time with ncap2
cmdfile <- paste0("cmd_", Sys.getpid()) # temporary runscript
message("apply new time to catted file with ncap2 via temporary runscript ", cmdfile, "\n",
        "--> if you want to keep this runscript, rerun the script with `remove_temporary_files`=F")
cmdtime <- c(paste0("time[time]={", 
                    paste(timeseconds_sprintf, collapse=","), # need the sprintf here!!! 
                    "};"),
             "time@units=\"seconds since 1970-01-01\";")
writeLines(text=cmdtime, con=cmdfile)

# run ncap2 command 
cmd <- paste0("ncap2 -O -S ", cmdfile, " ",
              dirname(outname), "/", basename(outname), " ",
              dirname(outname), "/", basename(outname))
message(cmd)
system(cmd)

# delete temporary runscript
if (remove_temporary_files) {
    message("\n`remove_temporary_files`=T -- remove temporary runscript ...")
    check <- file.remove(cmdfile)
    if (!check) stop("should not happen")
}

cat("\n******** finished **********\n")

