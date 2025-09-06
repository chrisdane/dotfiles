#!/usr/bin/env Rscript

# summarize ifs.stat
# ifs-source/arpifs/utility/opdis.F90

rm(list=ls()); graphics.off()

if (interactive()) {
    me <- "oifs_post_ifs.stat.r"
    #args <- "/work/ab1095/a270073/out/awiesm3-develop-cc/5yr2/run_19000101-19041231/work/ifs.stat"
    args <- c("/work/ab1095/a270073/out/awiesm3-develop-cc/5yr2/run_19000101-19041231/work/ifs.stat", "/work/ab1095/a270073/out/awiesm3-develop-cc/5yr2/run_19000101-19041231/work/NODE.001_01") 

} else { # if not interactive
    args <- commandArgs(trailingOnly=F) # internal and user args
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T) # user args only
    if (F) {
        print(args)
        if (!isatty(stdout())) message("Output is being piped (possibly to less).")
        print(stdout())
        stop("asd")
    }
} # if interactive or not 

help <- paste0("\nUsage:\n $ ", me, " ifs.stat [NODE.001_01] [| less]\n")

# stop if help
if (length(args) == 0 || length(args) > 2) {
    message(help)
    if (interactive()) {
        stop("stop interactive call")
    } else {
        quit()
    }
}

fstat <- args[1]
if (!file.exists(fstat)) stop("fstat ", fstat, " does not exist")
fstat <- normalizePath(fstat)
fnode <- NULL
if (length(args) == 2) {
    fnode <- args[2]
    if (!file.exists(fnode)) stop("fnode ", fnode, " does not exist")
    fnode <- normalizePath(fnode)
}

#############################################

# read ifs.stat
message("read ", fstat, " ...")
stat <- utils::read.table(fstat)
#!     CDCONF: current configuration flags
#!     CDCALLER: name of calling routine
#!     PCP:   cp time of last step
#!     PVCP:  vector cp time of last step
#!     PRT:   real time of last step
#!     PACP:  accumulated cp time
#!     PART:  accumulated real time
#!     KSTEP : model time step
# CLCP: IMCP:ISCP # todo: cp =? cumulative process
# CLTIME: IMIT:ISIT # todo: i ?= instantaneous
# AVNRMDIV: i guess it's average of norm of divergence
# IGETHWM: !-- Approximate max heap usage on *this* MPI task
# IGETSTK: !-- Approximate max stack size on *this* MPI task's master thread
# IGETVMP: !-- Virtual memory peak on *this* MPI-task 
# IENERGY: ! In kJoules (values are per node -- not per task [Cray])
if (ncol(stat) == 15) {
                       # 09:09:36    000000000   CNT3        -999     17.115  17.115 19.011 0:00    0:01      0.00000000000000E+00 898MB      5772KB     2256MB     0kJ        0W
                       # 09:09:37    AAAA00AAA   STEPO       0        18.997  18.997 19.942 0:19    0:21      0.21381939665703E-04 1361MB     5772KB     2783MB     0kJ        0W
                       # 09:09:37    FULLPOS-B   DYNFPOS     0        0.284   0.284  0.183  0:19    0:22      0.21381939665703E-04 1361MB     5772KB     2783MB     0kJ        0W
                       # 09:09:37    FULLPOS-S   DYNFPOS     0        0.239   0.239  0.035  0:19    0:22      0.21381939665703E-04 1361MB     5772KB     2783MB     0kJ        0W
                       # 09:09:37    0AAA00AAA   STEPO       1        2.248   2.248  0.357  0:22    0:22      0.19570333363538E-04 1452MB     5772KB     2810MB     0kJ        0W
                       # 09:09:39    0AAA00AAA   STEPO       2        2.752   2.752  1.477  0:24    0:23      0.18531185315336E-04 1473MB     5772KB     2896MB     0kJ        0W
                       # 09:09:39    0AAA00AAA   STEPO       3        0.582   0.582  0.077  0:25    0:23      0.17834519571469E-04 1473MB     5772KB     2896MB     0kJ        0W
                       # 09:09:39    0AAA00AAA   STEPO       4        0.527   0.527  0.072  0:26    0:24      0.17256366275926E-04 1473MB     5772KB     2896MB     0kJ        0W
                       # 09:09:39    0AAA00AAA   STEPO       5        0.555   0.555  0.071  0:26    0:24      0.16806435157483E-04 1473MB     5772KB     2896MB     0kJ        0W
                       # 09:09:39    0AAA00AAA   STEPO       6        0.715   0.715  0.095  0:27    0:24      0.16487179782098E-04 1473MB     5772KB     2896MB     0kJ        0W
                       # 09:09:39    0AAA00AAA   STEPO       7        0.491   0.491  0.062  0:27    0:24      0.16230124426488E-04 1473MB     5772KB     2896MB     0kJ        0W
                       # 09:09:39    0AAA00AAA   STEPO       8        0.445   0.445  0.057  0:28    0:24      0.16019346330884E-04 1473MB     5772KB     2896MB     0kJ        0W
                       # ...
                       # 11:15:09    0AAA00AAA   STEPO       35       0.896   0.896  0.473  1:00    0:46      0.14642773474595E-04 1467MB     5772KB     2878MB     0kJ        0W
                       # 11:15:10    FAAA00000   STEPO       36       1.403   1.403  0.698  1:01    0:47      0.14598975499009E-04 1467MB     5772KB     2878MB     0kJ        0W
                       # 11:15:11    FULLPOS-B   DYNFPOS     36       0.581   0.581  0.567  1:02    0:47      0.14598975499009E-04 1467MB     5772KB     2878MB     0kJ        0W
                       # 11:15:11    FULLPOS-S   DYNFPOS     36       0.132   0.132  0.019  1:02    0:47      0.14598975499009E-04 1467MB     5772KB     2878MB     0kJ        0W
                       # 11:15:11    000000000   CNT0        36       1.342   1.342  0.899  1:03    0:48      0.14598975499009E-04 1467MB     5772KB     2878MB     0kJ        0W
    colnames(stat) <- c("CLTIMEOD", "CDCONF",   "CDCALLER", "JSTEP", "PVCP", "PCP", "PRT", "CLCP", "CLTIME", "AVNRMDIV",          "IGETHWM", "IGETSTK", "IGETVMP", "IENERGY", "IPOWER")
} else if (ncol(stat) == 17) { # LDNHDYN
    colnames(stat) <- c("CLTIMEOD", "CDCONF",   "CDCALLER", "JSTEP", "PVCP", "PCP", "PRT", "CLCP", "CLTIME", "AVNRMDIV", "AVNRMVD", "AVNRMPD", "IGETHWM", "IGETSTK", "IGETVMP", "IENERGY", "IPOWER")
} else {
    stop("not implemented")
}
# --> there are several lines per time step JSTEP (FULLPOS-{B,S})
# --> PVCP = PCP
# --> CDCONF = AAAA00AAA takes longer than 0AAA00AAA on average
if (isatty(stdout())) {
    print(head(stat, n=30), width=300)
    message("...")
    print(tail(stat, n=30), width=300)
}

# read NODE.001_01
if (!is.null(fnode)) {
    # get start time --> todo: which one?
    if (F) { # The initial date of the run is : 1900  1  1
        cmd <- paste0("grep \"^ The initial date of the run is  \" ", fnode)
        message("\nrun `", cmd, "` ...")
        time_init <- suppressWarnings(system(cmd, intern=T)) # " The initial date of the run is : 1900  1  1"
        if (length(time_init) == 0) stop("could not find this line in this NODE file. never happened before")
        time_init <- strsplit(time_init, " : ")[[1]][2] # " The initial date of the run is" "1900  1  1"
        time_init <- strsplit(time_init, "\\s+")[[1]] # "1900" "1"    "1"
        time_init <- as.POSIXct(paste0(time_init[1], "-", time_init[2], "-", time_init[3]))
    } else if (T) { # XIOSFPOS: TIME_ORIGIN IS 1900-01-01 00:00:00
        cmd <- paste0("grep \"^ XIOSFPOS: TIME_ORIGIN IS \" ", fnode)
        message("\nrun `", cmd, "` ...")
        time_init <- suppressWarnings(system(cmd, intern=T)) # " XIOSFPOS: TIME_ORIGIN IS 1900-01-01 00:00:00 "
        if (length(time_init) == 0) stop("could not find this line in this NODE file. never happened before")
        time_init <- substr(time_init, 27, nchar(time_init)-1) # "1900-01-01 00:00:00"
        time_init <- as.POSIXct(time_init)
    } else if (F) { # XIOSFPOS: START_TIME IS 1900-01-01 00:00:00
        stop("asdsada")
    }
    time_init_lt <- as.POSIXlt(time_init)
    message("--> ", time_init, " ", time_init_lt$zone)

    # get dt
    if (T) { #  XIOSFPOS: TIME_STEP IS 2400s
        cmd <- paste0("grep \"^ XIOSFPOS: TIME_STEP IS \" ", fnode)
        message("\nrun `", cmd, "` ...")
        dt_sec <- suppressWarnings(system(cmd, intern=T)) # " XIOSFPOS: TIME_STEP IS 2400s               "
        if (length(dt_sec) == 0) stop("could not find this line in this NODE file. never happened before")
        dt_sec <- substr(dt_sec, 25, nchar(dt_sec)) # "2400s               "
        dt_sec <- strsplit(dt_sec, "s")[[1]][1] # "2400"            "               "
    } else {
        stop("asdasd")
    }
    message("--> ", dt_sec, " --> convert to numeric ...")
    warn <- options()$warn; dt_sec <- as.numeric(dt_sec); options(warn=warn)
    message("--> ", dt_sec)

} # if fnode

# check input
inds <- which(is.na(match(stat$CDCONF, c("000000000", "0AAA00AAA", "AAAA00AAA", "FULLPOS-B", "FULLPOS-S"))))
if (length(inds) > 0) stop("unknown CDCONF: ", paste(unique(stat$CDCONF[inds]), collapse="\n"))
inds <- which(is.na(match(stat$CDCALLER, c("CNT3", "CNT0", "DYNFPOS", "STEPO"))))
if (length(inds) > 0) stop("unknown CDCALLER: ", paste(unique(stat$CDCALLER[inds]), collapse="\n"))

# remove header/footer lines: 000000000 CNT3, 000000000 CNT0 
inds <- which(stat$CDCONF == "000000000")
if (length(inds) > 0) stat <- stat[-inds,]

# throw out duplicated timesteps JSTEP: FULLPOS-{B,S} DYNFPOS
inds <- which(duplicated(stat$JSTEP))
if (length(inds) > 0) {
    message("remove ", length(inds), "/", nrow(stat), " = ", round(length(inds)/nrow(stat)*100, 2), "% duplicated JSTEP rows ...")
    stat <- stat[-inds,]
}

# convert CLTIME string "min:sec" to numeric
tmp <- strsplit(stat$CLTIME, ":")
CLTIME_num <- as.numeric(sapply(tmp, "[[", 1)) + as.numeric(sapply(tmp, "[[", 2))/60
rm(tmp)

# todo: cumsum(PCP) != CLCP, cumsum(PRT) != CLTIME_num
PCP_cs <- cumsum(stat$PCP)
PRT_cs <- cumsum(stat$PRT)

# add calendar dt if dt_sec given
if (!is.null(fnode)) {
    dt_calendar_sec <- stat$JSTEP*dt_sec
    dt_calendar_min <- dt_calendar_sec/60L
    dt_calendar_hour <- dt_calendar_min/60L
    dt_calendar_day <- dt_calendar_hour/24L
    dt_calendar_yr_365 <- dt_calendar_day/365
    dt_calendar_yr_36525 <- dt_calendar_day/365.25
    time <- time_init + dt_calendar_sec
}

# summary
if (is.null(fnode)) {
    df <- data.frame(JSTEP=stat$JSTEP, PRT=stat$PRT, CLTIME=stat$CLTIME, CLTIMEMOD=stat$CLTIMEOD)
} else {
    df <- data.frame(JSTEP=stat$JSTEP, time=time, PRT=stat$PRT, CLTIME=stat$CLTIME, 
                     dt_min=dt_calendar_min, dt_hour=dt_calendar_hour, dt_day=dt_calendar_day, dt_yr_365=dt_calendar_yr_365, dt_yr_36525=dt_calendar_yr_36525,
                     CLTIMEMOD=stat$CLTIMEOD)
}
if (isatty(stdout())) {
    message("\n--> summary:")
    print(head(df, n=30), row.names=F, width=300)
    message("...")
    print(tail(df, n=30), row.names=F, width=300)
}

if (!isatty(stdout())) { # piped function call
    print(df, row.names=F, width=300, max=.Machine$integer.max) # Inf
}

# plot step time
if (isatty(stdout())) {
    message("\nplot PRT per JSTEP ...")
    plotname <- "~/models/oifs/ifs.stat"
    if (!dir.exists(plotname)) dir.create(plotname, recursive=T, showWarnings=F)
    if (!dir.exists(plotname)) stop("could not create dir ", plotname)
    title <- strsplit(fstat, "/")[[1]] 
    # e.g.:
    # [1] ""                      "work"                  "ab1095"               
    # [4] "a270073"               "out"                   "awiesm3-develop-cc"   
    # [7] "5yr2"                  "run_19000101-19041231" "work"                 
    #[10] "ifs.stat"
    title <- paste(title[(length(title) - 4):(length(title) - 2)], collapse="_")
    plotname <- paste0(plotname, "/ifs.stat_PRT_", title, "_max_JSTEP_", max(stat$JSTEP))
    if (!is.null(fnode)) plotname <- paste0(plotname, "_dt_sec_", dt_sec, ".png")
    message("--> ", plotname)

    steps <- stat$JSTEP
    xat <- pretty(steps, n=40)

    # make irregular y-axis to bring very short (seconds) and very large (hours) queue times onto one y-axis
    y <- stat$PRT # seconds
    yat <- ylab <- c(min(y, na.rm=T), quantile(y, probs=seq(0.1, 0.9, by=0.1), na.rm=T), max(y, na.rm=T))
    names(yat)[c(1, length(yat))] <- c("min", "max")
    ylab <- paste0(names(yat), ": ", ylab)

    # linearly interpolate actual step times to irregular y-axis levels
    n_interp <- 20
    yat_plot <- seq(1, by=n_interp, length.out=length(yat)) # sec
    y_plot <- rep(NA, times=length(y)) # sec
    for (yi in seq_len(length(yat)-1)) {
        if (yi == length(yat)-1) { # last
            inds <- which(y >= yat[yi] & y <= yat[yi+1])
            ytmp <- seq(yat[yi], yat[yi+1], length.out=n_interp+1) # linearly interpolate secs between two wanted y-levels
        } else { # all others
            inds <- which(y >= yat[yi] & y < yat[yi+1])
            ytmp <- seq(yat[yi], yat[yi+1], length.out=n_interp) # linearly interpolate secs between two wanted y-levels
        }
        if (length(inds) != 0) {
            for (yj in seq_along(inds)) {
                y_plot[inds[yj]] <- which.min(abs(y[inds[yj]] - ytmp)) + (yi-1)*n_interp
            } # for yj
        }
    } # for yi

    # plot
    png(plotname, width=4/3*1500, height=1500, res=200, family="Droid Sans")
    par(mar=c(7.1, 7.1, 2.1, 4.1))
    plot(steps, y_plot, type="n",
         xaxt="n", yaxt="n",
         xlab="", ylab="",
         main=title)
    axis(1, at=xat, labels=F)
    text(x=xat, y=yat_plot[1] - 0.66*yat_plot[2], labels=xat, adj=1, srt=90, xpd=T)
    mtext("JSTEP", side=1, line=4)
    axis(2, at=yat_plot, labels=ylab, las=2)
    mtext("PRT: real time of last step [sec]", side=2, line=6)
    abline(h=yat_plot, col="gray", lwd=0.5)
    points(steps, y_plot, pch=".")
    #lines(steps, y_plot)

    # add CLTIME
    ycumsum <- CLTIME_num # min
    ycumsum_unit <- "min"
    if (max(ycumsum) <= 60) { # total runtime <= 60 min -> sec
        ycumsum <- ycumsum*60
        ycumsum_unit <- "secs"
    } else if (max(ycumsum) > 60) { # total runtime > 60 min -> hours
        ycumsum <- ycumsum/60
        ycumsum_unit <- "hours"
    }
    par(new=T)
    plot(steps, ycumsum, type="l", col=2, axes=F, xlab="", ylab="")
    axis(4, pretty(ycumsum, n=20), las=2, col=2, col.ticks=2, col.axis=2)
    mtext(paste0("CLTIME [", ycumsum_unit, "]"), side=4, line=3, col=2)

    # add mean step time PRT
    y_mean <- mean(y, na.rm=T) # sec
    y_mean_unit <- "sec"
    y_median <- median(y, na.rm=T)
    y_median_unit <- "sec"
    legend("topleft", 
           paste0(c("mean", "median"), " PRT: ", format(c(y_mean, y_median), digits=3), " ", c(y_mean_unit, y_median_unit)),
           col=NA, lty=NA, pch=NA, lwd=NA, bty="n", x.intersp=-2)
    invisible(dev.off())

} # if if (isatty(stdout())) {

