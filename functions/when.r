#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()

if (interactive()) {
    me <- "when.r"
    #args <- c(as.character(Sys.time()), "Europe/Berlin")
    #args <- c(as.character(Sys.time()), "Asia/Seoul")
    #args <- c(as.character(Sys.time()), "accra")
    #args <- c(as.character(Sys.time()), "KST")
    args <- c("2022-5-12 14:00", "KST", "Berlin")
    #args <- c("2022-5-12 14:00", "KST", "CET")
    #args <- c("2022-5-12 14:00", "KST", "CEST")
    #args <- c("2022-5-12 14:00", "KST", "UTC")
    #args <- c("2022-5-12 14:00", "LMT")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage:\n $ ", me, " datein [tzin] tzout\n",
                "\ne.g.\n",
                "  ", me, " now UTC\n",
                "  ", me,  " tomorrow Africa # list all available time zones of Africa\n",
                "  ", me, " \"next Thursday\" Honolulu # non-default but allowed time zone\n",
                "  ", me, " \"9:00 next Fri\" Asia/Seoul\n",
                "  ", me, " \"9:00 next Fri\" KST # non-default but allowed time zone abbreviation (of Asia/Seoul)\n",
                "  ", me, " \"2022-05-12 14:00\" Berlin KST\n",
                "  ", me, " \"2022-05-12 14:00\" KST Berlin\n",
                "  ", me, " \"2024-5-14 20:00\" UTC Berlin\n",
                "\nSee `man date` for allowed formats of `datein`. Available time zones are all upper case files in\n",
                "/usr/share/zoneinfo (recursive; except \"SECURITY\"):\n",
                "  `find /usr/share/zoneinfo -type f -printf '%P\\n' | grep ^[ABCDEFGHIJKLMNOPQRSTUVWXYZ] | grep -v SECURITY | sort`\n",
                "or run `timedatectl list-timezones` or `tzselect`.\n")

# check
if (length(args) < 2 || length(args) > 3) {
    if (interactive()) {
        stop(usage)
    } else {
        message(usage)
        quit()
    }
}

# check input date
datein <- args[1]
cmd <- paste0("date -d \"", datein, "\" +\"%Y-%m-%d %H:%M:%S\"")
message("check 1/3 date: run `", cmd, "` ... ", appendLF=F) 
warn <- options()$warn
options(warn=2) # stop on warning
datein <- system(cmd, intern=T)
options(warn=warn)
message("ok")

# check time zones
if (length(args) == 2) {
    tzin <- Sys.timezone()
    message("`tzin` not given --> use system time zone \"", tzin, "\"")
    tzout <- args[2]
} else if (length(args) == 3) {
    tzin <- args[2]
    tzout <- args[3]
}
tzs <- base::OlsonNames() # all available time zones
tzdata_file <- "/usr/share/zoneinfo/tzdata.zi"
for (i in seq_len(2)) { # for input and output tz
    if (i == 1) tz <- c("tzin"=tzin)
    if (i == 2) tz <- c("tzout"=tzout)
    message("check ", i+1, "/3 ", names(tz), ": \"", tz, "\" ... ", appendLF=F)
    inds_default <- match(tz, tzs) # exact match
    if (!is.na(inds_default) == 1) { # success
        message("ok")
    } else { # no exact match
        inds_default <- grep(tz, tzs, ignore.case=T) # grep pattern
        if (length(inds_default) == 1) {
            tmp <- tzs[inds_default]
            names(tmp) <- names(tz)
            tz <- tmp; rm(tmp)
            if (i == 1) tzin <- tz
            if (i == 2) tzout <- tz
            message("found grep pattern once in default time zones --> continue with ", 
                    names(tz), " \"", tz, "\"")
        } else if (length(inds_default) > 1) { # found multiple
            message("pattern \"", tz, "\" was found ", length(inds_default), " times in default time zones:\n",
                    paste(tzs[inds_default], collapse=" "))
            stop("rerun with one of the time zones above.\n", usage)
        } else if (length(inds_default) == 0) { # find neither exact match nor grep pattern in default time zones
            # scan /usr/share/zoneinfo/tzdata.zi and try to determine tz
            pattern <- paste0(" ", tz, " ")
            message("this time zone is not one of the ", length(tzs), " default time zones\n",
                    "--> scan ", tzdata_file, " for pattern \"", pattern, 
                    "\" ...")
            if (!file.exists(tzdata_file)) {
                stop("could not find file ", tzdata_file, ".\n", usage)
            } else {
                tzdata <- readLines(tzdata_file)
            }
            inds_tzdata <- grep(pattern, tzdata)
            if (length(inds_tzdata) == 0) {
                stop("did not find pattern \"", trimws(pattern), "\" in file ", tzdata_file, "\n", usage)
            } else {
                zones <- rep(NA, t=length(inds_tzdata))
                for (j in seq_along(inds_tzdata)) {
                    # block from line starting with "Z" until line with grep pa tern 
                    ok <- F; cnt <- 0
                    while(!ok) {
                        if (grepl("^Z ", tzdata[inds_tzdata[j] - cnt])) {
                            ok <- T
                        } else {
                            cnt <- cnt + 1
                        }
                    }
                    ind <- inds_tzdata[j] - cnt
                    if (F) { # debug
                        message("************************* entry ", j, "/", length(inds_tzdata), ":")
                        if (F) { # complete block
                            inds2 <- seq(ind, inds_tzdata[j], b=1)
                        } else if (T) { # first and last lines of block
                            inds2 <- c(ind, inds_tzdata[j])
                        }
                        print(data.frame(ind=inds2, block=tzdata[inds2]))
                    }
                    zones[j] <- tzdata[ind] # e.g. "Z Asia/Seoul 8:27:52 - LMT 1908 Ap"
                } # for j
                zones <- strsplit(zones, " ")
                zones <- sapply(zones, "[[", 2) # e.g "Asia/Seoul"
                zones <- unique(zones)
                zones <- sort(zones)
                dates <- vector("list", l=length(zones))
                time <- Sys.time() # dummy time
                for (zi in seq_along(zones)) { # check if times of found time zones differ
                    tmp <- time
                    attr(tmp, "tzone") <- zones[zi] # stolen from lubridate:::.with_tz
                    tmp <- as.POSIXlt(tmp) 
                    dates[[zi]] <- list(posixlt=tmp, tz_abb=tmp$zone)
                }
                dateslt <- lapply(dates, "[[", "posixlt") 
                message("determined ", length(zones), " time zones potentially fitting to wanted tz \"", tz, "\":")
                print(cbind(tz=zones, tz_abb=sapply(dates, "[[", "tz_abb"),
                            example_time=sapply(dateslt, as.character)))
                dates_utc <- dateslt
                for (zi in seq_along(zones)) {
                    dates_utc[[zi]] <- as.POSIXct(as.character(dateslt[[zi]]), tz="UTC")
                }
                ndifferent_zones <- length(unique(unlist(dates_utc)))
                if (ndifferent_zones != 1) {
                    message()
                    stop("--> those time zones have ", ndifferent_zones, 
                         " different times. dont know which time zone to use. rerun with valid time zone.\n", usage)
                } else if (ndifferent_zones == 1) {
                    message(" --> those time zones have an identical time --> continue with the first: ", 
                            names(tz), " = \"", zones[1], "\"")
                    if (i == 1) tzin <- zones[1]
                    if (i == 2) tzout <- zones[1]
                }
            } # if pattern was found
        }
    } # if tz was found or not
} # for i

# from here, both start and out time zones are valid

# check input datein
datein <- as.POSIXct(datein, tz=tzin) # error if no success
datein_lt <- as.POSIXlt(datein)
tzin_abb <- datein_lt$zone

# calc target datein
dateout <- datein
attr(dateout, "tzone") <- tzout # stolen from lubridate:::.with_tz
dateout_lt <- as.POSIXlt(dateout)
tzout_abb <- dateout_lt$zone

# calc time diff
datein_utc <- as.POSIXct(as.character(datein), tz="UTC")
dateout_utc <- as.POSIXct(as.character(dateout), tz="UTC")
if (F) { # debug
    print(t(data.frame(datein=paste0(datein, " ", tzin), 
                       dateout=paste0(dateout, " ", tzout),
                       datein_utc=paste0(datein_utc, " UTC"),
                       dateout_utc=paste0(dateout_utc, " UTC"))))
}
dt <- difftime(dateout_utc, datein_utc, unit="hour")

msg <- paste0("--> ", datein, " in ", tzin)
if (!is.null(tzin_abb)) msg <- paste0(msg, " (\"", tzin_abb, "\")")
msg <- paste0(msg, " is ", dateout, " in ", tzout)
if (!is.null(tzout_abb)) msg <- paste0(msg, " (\"", tzout_abb, "\")")
if (dt == 0) {
    msg <- paste0(msg, ", i.e. they are identical")
} else {
    msg <- paste0(msg, ", i.e. ", tzout, " is ", abs(dt), " hour", ifelse(abs(dt) > 1, "s", ""), " ", 
                  ifelse(dt < 0, "earlier", "later"))
}
message(msg)

