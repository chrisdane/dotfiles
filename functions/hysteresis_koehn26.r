#!/usr/bin/env Rscript

# calc hysteresis area H(y) (eq. 1), min-max normalized Hn(y) (eq. 2), and
# sign-aware Hs(y) (eqs. 3/4) of Koehn et al. 2026, Nature Climate Change,
# https://doi.org/10.1038/s41558-026-02715-9 ("Persistence of Arctic Ocean
# acidification under negative emissions"), Methods section "Hysteresis
# analysis". Unlike the paper (fixed protocol peak, year 140 of an idealized
# 1pctCO2-cdr run), the ramp-up/ramp-down split and integration bounds are
# determined individually from `fin_co2`'s own peak.
#
# - H (eq. 1) — unsigned loop area between the ramp-up and ramp-down branches. Always ≥ 0.
# - Hn (eq. 2) — H normalized by y's full min-max range; a dimensionless fraction of that range enclosed by the loop. Always ≥ 0.
# - Hs (eqs. 3/4) — signed: mean(y) on the ramp-down branch minus mean(y) on the ramp-up branch, at matched CO2 levels.
#   - Hs > 0: y stays elevated as CO2 falls — a lagging/incomplete recovery.
#   - Hs < 0: y is lower on the way down than on the way up — an overshoot, or the loop traversed in the opposite sense.

# `fin_co2`: a global/single-point atmospheric CO2 time series, e.g. files
#   in /work/ab1095/a270073/post/echam6/fldint/co2_burden (kg m-2) or
#   /work/ab1095/a270073/post/recom/select/aCO2 (uatm/ppm-like); its "units"
#   attribute is carried through as-is to the output, not converted.
# `fin_y`: the spatial variable to evaluate hysteresis for, e.g. files in
#   /work/ba1103/a270073/post/recom/select/pCO2s. Dims other than time
#   (unstructured, e.g. "nodes_2d", or regular "lon"/"lat") are treated
#   generically as space, in whatever number/order they come in.

if (interactive()) {
    me <- "hysteresis_koehn26.r"
    if (T) {
        args <- c("--fin_co2=/home/a/a270073/work/ab1095/a270073/bc/echam/input/r0008/greenhouse_ssp126.nc",
                  "--fin_y=/home/a/a270073/work/ab1095/a270073/post/AWI-ESM-1-REcoM/spco2/yearmean/spco2_mon_yearmean_AWI-ESM-1-REcoM_ssp126_r1i1p1f1_gr1_2015-2100.nc",
                  "--from_co2=2015",
                  "--to_co2=2100")
    }
} else { # if not interactive
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

#print(args)
#options(warn=2)

# check
usage <- paste0("\nUsage:\n ", me, " ",
                "--fin_co2=<provide CO2 time series filename> ",
                "--fin_y=<provide spatial y filename> ",
                "--dry=F ",
                "--outdir=`dirname fin_y` ",
                "--varname_co2=`cdo showname fin_co2` ",
                "--varname_y=`cdo showname fin_y` ",
                "--from_co2=<optional, restrict fin_co2 to time >= the start of this date, \"YYYY[-MM[-DD[ HH:MM:SS]]]\"> ",
                "--to_co2=<optional, restrict fin_co2 to time <= the end of this date, e.g. \"1910\" means through 1910-12-31 23:59:59> ",
                "--from_y=<optional, restrict fin_y to time >= the start of this date> ",
                "--to_y=<optional, restrict fin_y to time <= the end of this date>\n",
                "\n",
                "with e.g.\n",
                "(levante)  --fin_co2=/pool/data/ECHAM6/input/r0008/greenhouse_ssp126.nc\n",
                "           --fin_co2=/work/bb0519/foci_input2/ECHAM6_GENERAL/ECHAM6/input/r0008/greenhouse_ssp534os.nc\n")
if (length(args) == 0) {
    message(usage)
    quit()
}

# check fin_co2
if (!any(grepl("--fin_co2", args))) {
    stop("must provide --fin_co2=<fin_co2> argument", usage)
} else {
    fin_co2 <- sub("--fin_co2=", "", args[grep("--fin_co2=", args)])
    message("fin_co2 = ", fin_co2)
    if (!file.exists(fin_co2)) stop("fin_co2 = \"", fin_co2, "\" does not exist")
}

# check fin_y
if (!any(grepl("--fin_y", args))) {
    stop("must provide --fin_y=<fin_y> argument", usage)
} else {
    fin_y <- sub("--fin_y=", "", args[grep("--fin_y=", args)])
    message("fin_y = ", fin_y)
    if (!file.exists(fin_y)) stop("fin_y = \"", fin_y, "\" does not exist")
}

# check varname_co2
if (!any(grepl("--varname_co2", args))) {
    cmd <- paste0("cdo -s showname ", fin_co2)
    message("varname_co2 not provided. run `", cmd, "` ... ", appendLF=F)
    varname_co2 <- trimws(system(cmd, intern=T))
    varname_co2 <- strsplit(varname_co2, " ")[[1]][1] # take 1st in case of several
    message("\"", varname_co2, "\"")
} else {
    varname_co2 <- sub("--varname_co2=", "", args[grep("--varname_co2=", args)])
}

# check varname_y
if (!any(grepl("--varname_y", args))) {
    cmd <- paste0("cdo -s showname ", fin_y)
    message("varname_y not provided. run `", cmd, "` ... ", appendLF=F)
    varname_y <- trimws(system(cmd, intern=T))
    varname_y <- strsplit(varname_y, " ")[[1]][1] # take 1st in case of several
    message("\"", varname_y, "\"")
} else {
    varname_y <- sub("--varname_y=", "", args[grep("--varname_y=", args)])
}
message("--> varname_co2 = \"", varname_co2, "\"")
message("--> varname_y = \"", varname_y, "\"")

# check --from_co2/--to_co2/--from_y/--to_y (all optional, default: no
# restriction): kept as raw strings here, parsed below once it's known
# whether each file's time converts to POSIX
get_arg_str <- function(argname) {
    full <- paste0("--", argname, "=")
    if (!any(grepl(full, args, fixed=T))) return(NA_character_)
    sub(full, "", args[grep(full, args, fixed=T)])
}
from_co2_str <- get_arg_str("from_co2")
to_co2_str   <- get_arg_str("to_co2")
from_y_str   <- get_arg_str("from_y")
to_y_str     <- get_arg_str("to_y")
for (n in c("from_co2_str", "to_co2_str", "from_y_str", "to_y_str")) {
    v <- get(n)
    if (!is.na(v)) message("--> ", sub("_str$", "", n), " = \"", v, "\"")
}

# check outdir
if (!any(grepl("--outdir", args))) { # outdir not provided
    outdir <- dirname(fin_y)
    message("outdir not provided. use `dirname(fin_y)` = \"", outdir, "\"")
} else { # outdir provided
    outdir <- sub("--outdir=", "", args[grep("--outdir=", args)])
}
if (file.access(outdir, mode=0) == -1) { # not existing
    message("outdir = \"", outdir, "\" does not exist. try to create ... ", appendLF=F)
    dir.create(outdir, recursive=T)
    if (!file.exists(outdir)) {
        stop("not successful. error msg:")
    } else {
        message("success")
    }
} else { # outdir exists
    if (file.access(outdir, mode=2) == -1) { # not writable
        stop("provided outdir = \"", outdir, "\" not writeable.")
    }
}
outdir <- normalizePath(outdir)
message("--> outdir = \"", outdir, "\"")

# check fout
fout <- paste0(outdir, "/", sub(varname_y, paste0("hysteresisK26_", varname_y), basename(fin_y)))
message("\n--> fout:")
print(fout)
if (file.exists(fout)) stop("fout ", fout, " already exists")

# safety check: `sub()` above no-ops if `varname_y` isn't literally in
# `basename(fin_y)`, which would make `fout` == `fin_y` and risk deleting
# the input instead of an old output
if (normalizePath(fout, mustWork=F) == normalizePath(fin_y, mustWork=F)) {
    stop("`fout` came out identical to `fin_y` (\"", fout, "\"), which means varname_y = \"",
         varname_y, "\" was not found in basename(fin_y) = \"", basename(fin_y), "\" for the ",
         "substitution to act on. Refusing to continue, since the next step would otherwise ",
         "delete/overwrite the input file. Provide a differently-named fin_y, or point --outdir ",
         "elsewhere, or rename the output manually afterwards.")
}

# check dry
dry <- F
if (any(args == "--dry")) {
    message("argument `--dry` provided --> dry run")
    dry <- T
}

# check ncdf4
message("\nload ncdf4 ...")
library(ncdf4)

##################################################################################

# from R > 3.2
trimws <- function (x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
{
    which <- match.arg(which)
    mysub <- function(re, x) sub(re, "", x, perl = TRUE)
    switch(which, left = mysub(paste0("^", whitespace, "+"),
        x), right = mysub(paste0(whitespace, "+$"), x), both = mysub(paste0(whitespace,
        "+$"), mysub(paste0("^", whitespace, "+"), x)))
}

# `order(co2_leg)` always returns co2_leg's ascending-sort permutation, so
# comparing it to a fixed reference index (`1:n` or `n:1`) is unreliable: a
# mismatch there is a *value-rank* position, not a *time* position, so it
# doesn't say *when* co2 was actually out of order (worst for a ramp-down
# leg, whose expected rank order is time-reversed); and a mere *tie* in
# co2_leg needs no resorting at all, yet R's tie-break rarely reproduces a
# naive `1:n`/`n:1` reference exactly, so that comparison flags perfectly
# fine legs too. This instead works directly in time order: position i is
# a genuine violation only if `co2_leg[i]` -> `co2_leg[i+1]` runs the
# *wrong* way for the leg's own overall trend (increasing if the leg ends
# higher than it starts, decreasing otherwise); a tie is never a
# violation. For every violation (+/-3 points of context), shows the date
# at that position (column 1) next to the date `order(co2_leg)` moved
# there instead (column 2, `dates[ord]`). `co2_leg`/`dates` are the leg's
# own (already `ok`-filtered) vectors in chronological order; a leg with
# no violation, or a missing `dates`, prints nothing.
report_ord_mismatch <- function(co2_leg, dates, ord, label) {
    if (is.null(dates)) return(invisible())
    n <- length(co2_leg)
    if (n < 2) return(invisible())
    d <- diff(co2_leg)
    viol <- if (co2_leg[n] >= co2_leg[1]) which(d < 0) else which(d > 0) # index i: co2_leg[i] -> co2_leg[i+1] runs the wrong way
    if (length(viol) == 0) return(invisible())
    diffidx <- sort(unique(c(viol, viol + 1L))) # both endpoints of each violating step
    idx <- sort(unique(unlist(lapply(diffidx, function(i) max(1, i-3):min(n, i+3)))))
    warning("--> warning: co2 is not monotonic within the ", label, " leg (", length(diffidx), "/", n,
            " point(s) out of chronological order) -- order() had to resort. Rows within +/-3 time points ",
            "of every mismatch (input date vs. the date order() put there instead):")
    print(data.frame(date_input=dates[idx], date_ord=dates[ord][idx]))
} # report_ord_mismatch

hysteresis_koehn2026 <- function(co2, y, dates=NULL) {
    # `co2`: atmospheric CO2 series, in fin_co2's own units (carried
    #   through as-is, not converted)
    # `y`: variable to evaluate hysteresis for; same length/time axis as `co2`
    # `dates`: optional, same length as `co2`/`y`; if given, used only for
    #   `report_ord_mismatch()`'s diagnostic below (not for any calculation)
    # returns a list with the hysteresis area `H` (eq. (1)), the min-max
    # normalized hysteresis area `Hn` (eq. (2)), the sign-aware hysteresis
    # area `Hs` (eqs. (3)/(4)), the individually determined integration
    # bounds `co2_min`/`co2_max`, the index of the individual peak
    # `ind_peak` used to split ramp-up/-down, and the common co2 grid with
    # both branches evaluated on it (for plotting)
    stopifnot(length(co2) == length(y))
    if (!is.null(dates)) stopifnot(length(dates) == length(co2))
    if (F) { # debug
        y <- ydata_mat[spacei,]
        dates <- time_vals_co2
    }
    ok <- is.finite(co2) & is.finite(y)
    co2 <- co2[ok]; y <- y[ok]
    if (!is.null(dates)) dates <- dates[ok]
    if (length(co2) < 3) {
        return(list(H=NA, Hn=NA, Hs=NA, co2_min=NA, co2_max=NA, ind_peak=NA))
    }

    # individual peak CO2 --> defines the ramp-up/ramp-down split
    # (replaces koehn et al.'s fixed year-140 split of the idealized
    # 1pctCO2-cdr protocol)
    ind_peak <- which.max(co2)
    if (ind_peak == 1 || ind_peak == length(co2)) {
        return(list(H=NA, Hn=NA, Hs=NA, co2_min=NA, co2_max=NA, ind_peak=ind_peak))
    }
    co2_up <- co2[1:ind_peak]; y_up <- y[1:ind_peak] # ramp-up: start --> individual peak
    co2_down <- co2[ind_peak:length(co2)]; y_down <- y[ind_peak:length(co2)] # ramp-down: individual peak --> end
    dates_up <- if (!is.null(dates)) dates[1:ind_peak] else NULL
    dates_down <- if (!is.null(dates)) dates[ind_peak:length(co2)] else NULL

    if (F) { # debug
        plot(dates, co2, type="o")
        points(dates_up, co2_up, col=2, cex=0.5)
        points(dates_down, co2_down, col=4, cex=0.5)
    }

    # `y_up(co2)`/`y_down(co2)`: y as a function of co2 (eq. (1)
    # integrates over dCO2, not dt); sort each leg by co2 first since
    # co2(t) need not be perfectly monotonic within a leg (interannual
    # noise on top of the forced trend)
    ord_up <- order(co2_up)
    report_ord_mismatch(co2_up, dates_up, ord_up, "ramp-up")
    co2_up <- co2_up[ord_up]; y_up <- y_up[ord_up]
    ord_down <- order(co2_down)
    report_ord_mismatch(co2_down, dates_down, ord_down, "ramp-down")
    co2_down <- co2_down[ord_down]; y_down <- y_down[ord_down]

    # individual integration bounds: replace koehn et al.'s fixed
    # [CO2^piC, 4*CO2^piC] by this run's own ramp-up/-down overlap,
    # i.e. only the co2 range where both branches can be evaluated
    co2_min <- max(min(co2_up), min(co2_down))
    co2_max <- min(max(co2_up), max(co2_down))
    if (!(co2_max > co2_min)) {
        return(list(H=NA, Hn=NA, Hs=NA, co2_min=co2_min, co2_max=co2_max, ind_peak=ind_peak))
    }

    # common co2 grid on which to evaluate |y_down(co2) - y_up(co2)|.
    # ties="ordered": `x` values are assumed already sorted (done above);
    # avoids `ties=mean`'s averaging of y over very similar x values during
    # a flat peak-CO2 stretch
    nco2 <- max(length(unique(co2_up)), length(unique(co2_down)))
    co2_grid <- seq(co2_min, co2_max, length.out=nco2)
    y_up_i <- stats::approx(co2_up, y_up, xout=co2_grid, ties="ordered")$y
    y_down_i <- stats::approx(co2_down, y_down, xout=co2_grid, ties="ordered")$y

    # co2-normalized trapezoidal integral of `yvals` over `co2_grid` (the
    # continuous "mean" eq. (4) uses) -- reused for eq. (1)/(3)'s integral
    # too, so both stay exactly consistent
    trapz_mean <- function(yvals, xvals) {
        sum((yvals[-1] + yvals[-length(yvals)])/2*diff(xvals)) / (xvals[length(xvals)] - xvals[1])
    }

    # eq. (1): trapezoidal integration of |y_down(co2) - y_up(co2)|
    # over [co2_min, co2_max], normalized by the co2 range
    dy <- abs(y_down_i - y_up_i)
    H <- trapz_mean(dy, co2_grid)

    # eq. (2): min-max normalized hysteresis area. `y` here is still the
    # full (finite-filtered) ramp-up+ramp-down series from above (not
    # `y_up`/`y_down` individually), i.e. max/min across the full cycle,
    # as in the paper
    y_range <- range(y)
    Hn <- if (diff(y_range) == 0) NA else H/diff(y_range)

    # eqs. (3)/(4): sign-aware hysteresis area (eq. (1) without abs()),
    # implemented as eq. (4)'s difference of means -- by linearity of the
    # trapezoidal rule exactly equal to eq. (3)'s signed-difference integral
    Hs <- trapz_mean(y_down_i, co2_grid) - trapz_mean(y_up_i, co2_grid)

    list(H=H, Hn=Hn, Hs=Hs, co2_min=co2_min, co2_max=co2_max, ind_peak=ind_peak,
         co2_grid=co2_grid, y_up=y_up_i, y_down=y_down_i, dy=dy)

} # hysteresis_koehn2026

##################################################################################

# identify a variable's time dim among `known_time_dimnames` (not assumed
# to literally be "time" -- some files here use "mon" instead)
known_time_dimnames <- c("time", "Time", "TIME", "mon")
find_time_dim <- function(vardims, filelabel) {
    time_dim_ind <- which(!is.na(match(vardims, known_time_dimnames)))
    if (length(time_dim_ind) != 1) {
        stop("could not uniquely identify the time dimension of ", filelabel, " among dims: ", paste(vardims, collapse=", "),
             " (looked for one of: ", paste(known_time_dimnames, collapse=", "), ")")
    }
    time_dim_ind
}

# a dimension's coordinate values + units (`nc_open()` fills `vals` with
# 1:len if there's no actual coordinate variable of that name)
time_dim_vals <- function(nc, dimname) {
    d <- nc$dim[[dimname]]
    if (is.null(d$vals) || length(d$vals) != d$len) {
        stop("dimension \"", dimname, "\" has no usable coordinate values (length ", length(d$vals), " != dim length ", d$len, ")")
    }
    list(vals=d$vals, units=if (is.null(d$units)) "" else d$units)
}

# convert a CF time coordinate's raw numeric `vals` ("<unit> since
# <origin>" `units`, e.g. "days since 1850-01-01", + optional `calendar`)
# to POSIX time, so --from_*/--to_* work as real dates and 2 files' time
# axes are comparable regardless of units/origin/calendar. Returns NULL
# (caller falls back to raw time values) if `units` isn't of that form,
# its origin can't be parsed, or its unit isn't a recognized CF one.
# "months"/"years since" step by calendar month/year, not a fixed length;
# a non-Gregorian `calendar` is only warned about, not corrected for.
# parse "YYYY[-MM[-DD[ HH:MM[:SS]]]]" (or with "T") into POSIXct, padding
# missing trailing components explicitly rather than relying on
# `strptime()`'s own (system-date-dependent, not 0/1-defaulting) handling
# of a short format. `end_of_period=F` (default; CF origin, --from_*):
# missing fields default to that component's start. `end_of_period=T`
# (--to_*): missing fields default to its end, e.g. "1910" -> 1910-12-31
# 23:59:59, so --to_co2=1910 includes all of 1910.
parse_date_flexible <- function(str, end_of_period=F) {
    str <- sub("T", " ", trimws(str), fixed=T)
    precision <- if (grepl("^[0-9]+$", str)) "year"
                 else if (grepl("^[0-9]+-[0-9]{1,2}$", str)) "month"
                 else if (grepl("^[0-9]+-[0-9]{1,2}-[0-9]{1,2}$", str)) "day"
                 else if (grepl("^[0-9]+-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}$", str)) "minute"
                 else "full"
    padded <- switch(precision, year=paste0(str, "-01-01 00:00:00"), month=paste0(str, "-01 00:00:00"),
                      day=paste0(str, " 00:00:00"), minute=paste0(str, ":00"), full=str)
    start <- as.POSIXct(strptime(padded, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
    if (is.na(start) || !end_of_period || precision == "full") return(start)

    # end of the given precision's period = 1 second before the start of
    # the *next* one (POSIXlt correctly rolls e.g. December -> next year)
    lt <- as.POSIXlt(start)
    if (precision == "year") lt$year <- lt$year + 1
    else if (precision == "month") lt$mon <- lt$mon + 1
    else if (precision == "day") lt$mday <- lt$mday + 1
    else if (precision == "minute") lt$min <- lt$min + 1
    as.POSIXct(lt) - 1
} # parse_date_flexible

cf_time_to_posix <- function(vals, units, calendar, filelabel) {
    # ECHAM6/MPI-ESM idealized-forcing convention (e.g.
    # /pool/data/ECHAM6/input/r0008/greenhouse_ssp126.nc): the value
    # itself directly *is* the (possibly fractional) year, e.g. 1850.5 =
    # halfway through year 1850
    if (grepl("^\\s*year\\s+as\\s+%Y\\.%f\\s*$", units, ignore.case=T)) {
        yr <- floor(vals)
        frac <- vals - yr
        start_of_year <- as.POSIXct(paste0(sprintf("%04d", yr), "-01-01"), tz="UTC")
        end_of_year <- as.POSIXct(paste0(sprintf("%04d", yr + 1), "-01-01"), tz="UTC")
        return(start_of_year + frac*as.numeric(difftime(end_of_year, start_of_year, units="secs")))
    }

    m <- regmatches(units, regexec("^\\s*([[:alpha:]]+)\\s+since\\s+(.+?)\\s*$", units))[[1]]
    if (length(m) != 3) {
        message("--> warning: ", filelabel, "'s time \"units\" = \"", units, "\" is not of the form \"<unit> since <origin>\" -- ",
                "cannot convert to POSIX time; using its own raw time values instead")
        return(NULL)
    }
    unit <- tolower(m[2])
    origin_str <- m[3]

    origin <- parse_date_flexible(origin_str)
    if (is.na(origin)) {
        message("--> warning: could not parse ", filelabel, "'s time origin \"", origin_str, "\" (from units \"", units, "\") -- ",
                "cannot convert to POSIX time; using its own raw time values instead")
        return(NULL)
    }

    if (!is.null(calendar) && nzchar(calendar) && !(tolower(calendar) %in% c("standard", "gregorian", "proleptic_gregorian"))) {
        message("--> warning: ", filelabel, "'s time \"calendar\" = \"", calendar, "\" is not exactly representable by POSIXct ",
                "(always the ordinary Gregorian calendar) -- converted dates are an approximation")
    }

    if (unit %in% c("second", "seconds", "sec", "secs", "s")) {
        return(origin + vals)
    } else if (unit %in% c("minute", "minutes", "min", "mins")) {
        return(origin + vals*60)
    } else if (unit %in% c("hour", "hours", "hr", "hrs", "h")) {
        return(origin + vals*3600)
    } else if (unit %in% c("day", "days", "d")) {
        return(origin + vals*86400)
    } else if (unit %in% c("month", "months", "mon", "mons")) {
        lt <- as.POSIXlt(origin)
        out <- vapply(vals, function(v) {
            lt2 <- lt; total_mon <- lt2$mon + v
            lt2$year <- lt2$year + floor(total_mon/12); lt2$mon <- total_mon %% 12
            as.numeric(as.POSIXct(lt2))
        }, numeric(1))
        return(as.POSIXct(out, origin="1970-01-01", tz="UTC"))
    } else if (unit %in% c("year", "years", "yr", "yrs")) {
        lt <- as.POSIXlt(origin)
        out <- vapply(vals, function(v) { lt2 <- lt; lt2$year <- lt2$year + v; as.numeric(as.POSIXct(lt2)) }, numeric(1))
        return(as.POSIXct(out, origin="1970-01-01", tz="UTC"))
    }
    message("--> warning: ", filelabel, "'s time unit \"", unit, "\" (from units \"", units, "\") is not recognized -- ",
            "cannot convert to POSIX time; using its own raw time values instead")
    NULL
} # cf_time_to_posix

# parse 1 --from_*/--to_* value: as a date (if `use_posix`, i.e. that
# file's own time could be converted to POSIX time above) or as a plain
# number (the fallback, comparing against that file's own raw time
# values). NA (argument not given) passes through unchanged either way.
# `end_of_period`: passed straight through to `parse_date_flexible()` --
# T for a "to" bound (so e.g. --to_co2=1910 includes all of 1910), F
# (default) for a "from" bound.
parse_time_bound <- function(str, use_posix, argname, end_of_period=F) {
    if (is.na(str)) return(if (use_posix) as.POSIXct(NA) else NA_real_)
    if (use_posix) {
        v <- parse_date_flexible(str, end_of_period=end_of_period)
        if (is.na(v)) stop("--", argname, "=\"", str, "\" could not be parsed as a date (\"YYYY[-MM[-DD[ HH:MM[:SS]]]]\")")
        return(v)
    }
    v <- suppressWarnings(as.numeric(str))
    if (is.na(v)) {
        stop("--", argname, "=\"", str, "\" could not be parsed as a number (this file's time could not be converted to POSIX ",
             "time, so --from_*/--to_* are compared against its own raw, unit-specific time-coordinate values instead)")
    }
    v
} # parse_time_bound

# classify a POSIXct time axis's frequency from the median step between
# consecutive values: "year" (~300-400 days), "month" (~25-35 days), or
# "other". Used below to decide how coarsely 2 time axes must agree to
# count as aligned -- 2 genuinely monthly/yearly series may legitimately
# use different within-period timestamp conventions.
detect_time_freq <- function(vals) {
    if (length(vals) < 2) return("other")
    step_days <- stats::median(diff(as.numeric(vals)))/86400
    if (step_days >= 300 && step_days <= 400) "year"
    else if (step_days >= 25 && step_days <= 35) "month"
    else "other"
} # detect_time_freq

# translate an optional [from, to] time range (POSIXct, if that file's own
# time could be converted -- see `cf_time_to_posix()` -- else a plain
# number compared against its own raw time-coordinate values either way)
# into a contiguous `start`/`count` for that one dimension, for
# `ncvar_get()`. NA on either end means "from the very first/last value",
# i.e. no restriction on that end.
time_range_to_start_count <- function(vals, from, to, label) {
    idx <- seq_along(vals)
    if (!is.na(from)) idx <- idx[vals[idx] >= from]
    if (!is.na(to)) idx <- idx[vals[idx] <= to]
    if (length(idx) == 0) {
        stop(label, ": no time value(s) found in [", from, ", ", to, "] among the file's own time range [",
             min(vals), ", ", max(vals), "]")
    }
    if (!all(diff(idx) == 1)) stop(label, ": selected time indices are not contiguous; is the time axis sorted?")
    list(start=idx[1], count=length(idx), vals=vals[idx])
}

# `start`/`count` vectors covering all of a variable's dims (`dimnames`, in
# the variable's own dim order), restricting only its time dimension
# (`time_dim_ind`) to `[time_start, time_count]` and reading every other dim
# in full. Named by `dimnames` (rather than left as plain positional
# vectors) so e.g. `sc_co2$start`/`sc_co2$count` are self-explanatory when
# printed/debugged, matching what `ncvar_get(start=, count=)` expects.
full_start_count <- function(dimnames, time_dim_ind, time_start, time_count) {
    ndims <- length(dimnames)
    start <- rep(1, ndims); start[time_dim_ind] <- time_start
    count <- rep(-1, ndims); count[time_dim_ind] <- time_count
    names(start) <- names(count) <- dimnames
    list(start=start, count=count)
}

##################################################################################

# read co2 time series
message("\nread co2 = \"", varname_co2, "\" from fin_co2 ...")
nc_co2 <- ncdf4::nc_open(fin_co2)
if (is.null(nc_co2$var[[varname_co2]])) stop("variable \"", varname_co2, "\" not found in fin_co2. available: ", paste(names(nc_co2$var), collapse=", "))
vardims_co2 <- sapply(nc_co2$var[[varname_co2]]$dim, function(d) d$name)
time_dim_ind_co2 <- find_time_dim(vardims_co2, "fin_co2")
time_dimname_co2 <- vardims_co2[time_dim_ind_co2]
message("--> time dimension of \"", varname_co2, "\" in fin_co2: \"", time_dimname_co2, "\"")
tv_co2 <- time_dim_vals(nc_co2, time_dimname_co2)
posix_co2 <- cf_time_to_posix(tv_co2$vals, tv_co2$units, nc_co2$dim[[time_dimname_co2]]$calendar, "fin_co2")
use_posix_co2 <- !is.null(posix_co2)
time_axis_co2 <- if (use_posix_co2) posix_co2 else tv_co2$vals
from_co2 <- parse_time_bound(from_co2_str, use_posix_co2, "from_co2")
to_co2 <- parse_time_bound(to_co2_str, use_posix_co2, "to_co2", end_of_period=T)
tc_co2 <- time_range_to_start_count(time_axis_co2, from_co2, to_co2, "--from_co2/--to_co2")
sc_co2 <- full_start_count(vardims_co2, time_dim_ind_co2, tc_co2$start, tc_co2$count)
co2 <- as.vector(ncdf4::ncvar_get(nc_co2, varname_co2, start=sc_co2$start, count=sc_co2$count)) # dummy size-1 spatial dims are dropped automatically
time_vals_co2 <- tc_co2$vals
message("--> fin_co2 time: ", if (use_posix_co2) "converted to POSIX time" else "kept as raw, unit-specific values (see warning above)",
        "; using ", length(time_vals_co2), " step(s) from ",
        if (use_posix_co2) format(time_vals_co2[1], "%Y-%m-%d %H:%M:%S", tz="UTC") else time_vals_co2[1], " to ",
        if (use_posix_co2) format(time_vals_co2[length(time_vals_co2)], "%Y-%m-%d %H:%M:%S", tz="UTC") else time_vals_co2[length(time_vals_co2)])
# take whatever units `varname_co2` carries in fin_co2 as-is (ppm,
# kgCO2 kgAir-1, mol mol-1, PgC, ...) and carry them through to the
# output's attributes below, rather than assuming/enforcing one unit
units_co2_att <- suppressWarnings(ncdf4::ncatt_get(nc_co2, varname_co2, "units")) # ncdf4 bug: partial match of 'group' to 'groups'
units_co2 <- if (units_co2_att$hasatt) units_co2_att$value else "unknown"
if (!units_co2_att$hasatt) message("--> warning: \"", varname_co2, "\" in fin_co2 has no \"units\" attribute; using \"unknown\"")
ncdf4::nc_close(nc_co2)
message("--> length(co2) = ", length(co2), ", range = ", paste(round(range(co2, na.rm=T), 2), collapse=" to "), " ", units_co2)

# read y: identify time dim (via `known_time_dimnames`) vs (any number,
# any order of) spatial dims
message("\nread y = \"", varname_y, "\" from fin_y ...")
nc_y <- ncdf4::nc_open(fin_y)
if (is.null(nc_y$var[[varname_y]])) stop("variable \"", varname_y, "\" not found in fin_y. available: ", paste(names(nc_y$var), collapse=", "))
vardims <- sapply(nc_y$var[[varname_y]]$dim, function(d) d$name)
message("--> dims of \"", varname_y, "\" in fin_y: ", paste(vardims, collapse=", "))
time_dim_ind <- find_time_dim(vardims, "fin_y")
time_dimname_y <- vardims[time_dim_ind]
message("--> time dimension of \"", varname_y, "\" in fin_y: \"", time_dimname_y, "\"")
space_dim_inds <- setdiff(seq_along(vardims), time_dim_ind)
if (length(space_dim_inds) == 0) stop("\"", varname_y, "\" has no dimension besides \"", time_dimname_y, "\"")
space_dim_names <- vardims[space_dim_inds]
message("--> treating as spatial dims: ", paste(space_dim_names, collapse=", "))

tv_y <- time_dim_vals(nc_y, time_dimname_y)
posix_y <- cf_time_to_posix(tv_y$vals, tv_y$units, nc_y$dim[[time_dimname_y]]$calendar, "fin_y")
use_posix_y <- !is.null(posix_y)
time_axis_y <- if (use_posix_y) posix_y else tv_y$vals
from_y <- parse_time_bound(from_y_str, use_posix_y, "from_y")
to_y <- parse_time_bound(to_y_str, use_posix_y, "to_y", end_of_period=T)
tc_y <- time_range_to_start_count(time_axis_y, from_y, to_y, "--from_y/--to_y")
sc_y <- full_start_count(vardims, time_dim_ind, tc_y$start, tc_y$count)
# collapse_degen=F: ncvar_get() would otherwise silently drop any size-1
# dim (e.g. time narrowed to 1 step), desyncing `ydata`'s dims from
# `vardims`/`perm` below; _FillValue/missing_value -> NA either way
ydata <- ncdf4::ncvar_get(nc_y, varname_y, start=sc_y$start, count=sc_y$count, collapse_degen=F)
time_vals_y <- tc_y$vals
ncdf4::nc_close(nc_y)
message("--> fin_y time: ", if (use_posix_y) "converted to POSIX time" else "kept as raw, unit-specific values (see warning above)",
        "; using ", length(time_vals_y), " step(s) from ",
        if (use_posix_y) format(time_vals_y[1], "%Y-%m-%d %H:%M:%S", tz="UTC") else time_vals_y[1], " to ",
        if (use_posix_y) format(time_vals_y[length(time_vals_y)], "%Y-%m-%d %H:%M:%S", tz="UTC") else time_vals_y[length(time_vals_y)])

# move time to the last array dim, flatten all spatial dims into one
perm <- c(space_dim_inds, time_dim_ind)
ydata <- base::aperm(ydata, perm)
space_shape <- dim(ydata)[seq_along(space_dim_inds)]
nspace <- prod(space_shape)
ntime <- dim(ydata)[length(dim(ydata))]
message("--> nspace = ", nspace, " (", paste(space_shape, collapse=" x "), "), ntime = ", ntime)

# check that fin_co2's and fin_y's (possibly --from_*/--to_*-restricted)
# time axes actually *align*, not just happen to have the same length --
# same length alone is necessary but not sufficient (2 series covering
# different periods at the same frequency would pass a length-only check)
if (length(time_vals_co2) != length(time_vals_y)) {
    stop("length(co2) = ", length(time_vals_co2), " != ntime of \"", varname_y, "\" in fin_y = ", length(time_vals_y),
         ". check that fin_co2 and fin_y (and any --from_*/--to_* given) cover the same time period at the same frequency.")
}
if (use_posix_co2 && use_posix_y) {
    # both converted to calendar time -- directly comparable regardless of
    # each file's units/origin/calendar. Compare at whichever granularity
    # both series' frequency supports (year, or year+month) rather than
    # exact clock time, since same-frequency series may legitimately use
    # different within-period timestamp conventions; anything else needs
    # an exact match.
    freq_co2 <- detect_time_freq(time_vals_co2)
    freq_y <- detect_time_freq(time_vals_y)
    if (freq_co2 == "year" && freq_y == "year") {
        cmp_co2 <- format(time_vals_co2, "%Y", tz="UTC"); cmp_y <- format(time_vals_y, "%Y", tz="UTC")
        granularity <- "calendar year"
    } else if (freq_co2 == "month" && freq_y == "month") {
        cmp_co2 <- format(time_vals_co2, "%Y-%m", tz="UTC"); cmp_y <- format(time_vals_y, "%Y-%m", tz="UTC")
        granularity <- "calendar year+month"
    } else {
        cmp_co2 <- format(time_vals_co2, "%Y-%m-%d %H:%M:%S", tz="UTC"); cmp_y <- format(time_vals_y, "%Y-%m-%d %H:%M:%S", tz="UTC")
        granularity <- "exact POSIX time"
    }
    mismatch <- which(cmp_co2 != cmp_y)
    if (length(mismatch) > 0) {
        stop("fin_co2's and fin_y's time axis values do not align (compared as ", granularity, ") at ", length(mismatch), "/",
             length(time_vals_co2), " step(s), e.g. index ", mismatch[1], ": ", cmp_co2[mismatch[1]], " vs ", cmp_y[mismatch[1]])
    }
    message("--> fin_co2's and fin_y's time axis values align (", length(time_vals_co2), " steps, compared as ", granularity, ")")
} else if (identical(tv_co2$units, tv_y$units)) {
    # fallback: neither (or only 1) converted to POSIX time -- only
    # directly comparable if both happen to share the exact same raw units
    if (!isTRUE(all.equal(time_vals_co2, time_vals_y))) {
        mismatch <- which(abs(time_vals_co2 - time_vals_y) > 1e-6*pmax(abs(time_vals_co2), abs(time_vals_y), 1))
        stop("fin_co2's and fin_y's time axis values do not align (both in \"", tv_co2$units, "\") at ", length(mismatch), "/",
             length(time_vals_co2), " step(s), e.g. index ", mismatch[1], ": ", time_vals_co2[mismatch[1]], " vs ", time_vals_y[mismatch[1]])
    }
    message("--> fin_co2's and fin_y's time axis values align (", length(time_vals_co2), " steps, \"", tv_co2$units, "\")")
} else {
    message("--> warning: could not verify fin_co2's and fin_y's time axis values align (at least 1 could not be converted to ",
            "POSIX time, and their raw units differ: \"", tv_co2$units, "\" vs \"", tv_y$units, "\") -- only checked that both ",
            "have the same number of steps (", length(time_vals_co2), ")")
}

ydata_mat <- matrix(ydata, nrow=nspace, ncol=ntime)
rm(ydata)

# calc H, Hn, Hs at every spatial point against the one shared co2 series
message("\ncalc hysteresis at ", nspace, " spatial points ...")
H <- Hn <- Hs <- rep(NA, times=nspace)
ind_peak_all <- co2_min_all <- co2_max_all <- rep(NA, times=nspace) # for sanity-checking below; identical for every point since only co2-dependent
progress_every <- max(1, round(nspace/100)) # ~every 1%
for (spacei in seq_len(nspace)) {
    # 1 updated-in-place line: `message()` always appends "\n", so use
    # `cat()` (to stderr, matching this script's other output) with a
    # leading "\r" instead, flushed explicitly
    if (spacei == 1 || spacei %% progress_every == 0 || spacei == nspace) {
        cat("\r", round(100*spacei/nspace), "% (", spacei, "/", nspace, ")   ", sep="", file=stderr())
        flush(stderr())
    }
    #if (spacei == 4126) stop("debug")
    res <- hysteresis_koehn2026(co2, ydata_mat[spacei,], dates=time_vals_co2)
    H[spacei] <- res$H; Hn[spacei] <- res$Hn; Hs[spacei] <- res$Hs
    ind_peak_all[spacei] <- res$ind_peak; co2_min_all[spacei] <- res$co2_min; co2_max_all[spacei] <- res$co2_max
} # for spacei
cat("\n", file=stderr()) # end the progress line before the next `message()`
message("--> ", sum(is.na(H)), "/", nspace, " points returned NA (e.g. all-land/masked points, or points with too few finite values)")
# `ind_peak`/`co2_min`/`co2_max` only depend on `co2`, never `y` --> must
# be identical at every non-NA point. `ind_peak` is still returned even
# when co2's peak sits at the series' first/last index (no leg to
# compare), so it can be non-NA even when every H/Hn/Hs came out NA
ind_peak_uniq <- unique(ind_peak_all[!is.na(ind_peak_all)])
if (length(ind_peak_uniq) > 1) stop("this should not happen: `ind_peak` differs across spatial points despite depending only on the shared `co2`")
if (length(ind_peak_uniq) == 0) {
    message("--> could not determine an individual peak CO2 at all (co2 too short/all non-finite)")
} else if (all(is.na(co2_min_all))) {
    message("--> individual peak CO2 = ", round(co2[ind_peak_uniq], 2), " ", units_co2, " at ind ", ind_peak_uniq,
            " (at the very start/end of the series --> no ramp-up or ramp-down leg to compare; ",
            "H/Hn/Hs are NA at every point)")
} else {
    message("--> individual peak CO2 = ", round(co2[ind_peak_uniq], 2), " ", units_co2, " at ind ", ind_peak_uniq,
            ", integration range = [", round(min(co2_min_all, na.rm=T), 1), ", ", round(max(co2_max_all, na.rm=T), 1), "] ", units_co2)
}

H <- array(H, dim=space_shape)
Hn <- array(Hn, dim=space_shape)
Hs <- array(Hs, dim=space_shape)

##################################################################################

# write output: derive a spatial (time-collapsed) template from fin_y via
# NCO `ncwa -a <time_dimname_y>` (keeps fin_y's exact spatial dims/coord
# vars, whatever they are), then rename/add variables into it via ncdf4
cmd <- paste0("ncwa -O -a ", time_dimname_y, " -v ", varname_y, " ", fin_y, " ", fout)
message("run `", cmd, "` ...")
if (!dry) system(cmd)

if (!dry) {
    nc_out <- ncdf4::nc_open(fout, write=T)
    template_var <- nc_out$var[[varname_y]]

    # H: same units as y. `ncvar_rename()` only updates the *returned*
    # `nc_out` object's $var list (used by ncvar_put/ncatt_put) -- must reassign
    nc_out <- ncdf4::ncvar_rename(nc_out, varname_y, paste0("H_", varname_y))
    ncdf4::ncvar_put(nc_out, paste0("H_", varname_y), H)
    ncdf4::ncatt_put(nc_out, paste0("H_", varname_y), "long_name",
                      paste0("Koehn et al. 2026 eq. (1) hysteresis area of ", varname_y))
    ncdf4::ncatt_put(nc_out, paste0("H_", varname_y), "units", template_var$units)

    # Hn: unitless
    var_hn <- ncdf4::ncvar_def(paste0("Hn_", varname_y), units="1", dim=template_var$dim,
                                missval=template_var$missval,
                                longname=paste0("Koehn et al. 2026 eq. (2) min-max normalized hysteresis area of ", varname_y))
    nc_out <- suppressWarnings(ncdf4::ncvar_add(nc_out, var_hn))
    ncdf4::ncvar_put(nc_out, paste0("Hn_", varname_y), Hn)

    # Hs: same units as y
    var_hs <- ncdf4::ncvar_def(paste0("Hs_", varname_y), units=template_var$units, dim=template_var$dim,
                                missval=template_var$missval,
                                longname=paste0("Koehn et al. 2026 eqs. (3)/(4) sign-aware hysteresis area of ", varname_y))
    nc_out <- suppressWarnings(ncdf4::ncvar_add(nc_out, var_hs))
    ncdf4::ncvar_put(nc_out, paste0("Hs_", varname_y), Hs)

    ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_fin_co2", fin_co2)
    ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_varname_co2", varname_co2)
    ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_co2_units", units_co2)
    ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_time_dimname_co2", time_dimname_co2)
    ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_time_dimname_y", time_dimname_y)
    # POSIXct isn't a sensible netCDF attribute value -- format as ISO8601
    # instead (self-describing, unlike the raw-value fallback below)
    fmt_time_bound <- function(v, use_posix) if (use_posix) format(v, "%Y-%m-%dT%H:%M:%SZ", tz="UTC") else v
    if (!is.na(from_co2)) ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_from_co2", fmt_time_bound(from_co2, use_posix_co2))
    if (!is.na(to_co2))   ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_to_co2", fmt_time_bound(to_co2, use_posix_co2))
    if (!is.na(from_y))   ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_from_y", fmt_time_bound(from_y, use_posix_y))
    if (!is.na(to_y))     ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_to_y", fmt_time_bound(to_y, use_posix_y))
    ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_individual_peak_co2", co2[ind_peak_uniq])
    if (use_posix_co2) {
        ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_individual_peak_co2_time",
                          format(time_vals_co2[ind_peak_uniq], "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
    } else {
        ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_individual_peak_co2_time", time_vals_co2[ind_peak_uniq])
        ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_individual_peak_co2_time_units", tv_co2$units)
    }
    ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_individual_peak_ind", ind_peak_uniq)
    ncdf4::ncatt_put(nc_out, 0, "hysteresis_koehn26_reference", "Koehn et al. 2026, Nature Climate Change, https://doi.org/10.1038/s41558-026-02715-9")

    ncdf4::nc_close(nc_out)
    message("saved ", fout)

} # if !dry

