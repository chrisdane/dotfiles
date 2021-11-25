#!/usr/bin/env Rscript

# r

rm(list=ls()); graphics.off()

if (interactive()) {
    paths <- paste0("/mnt/lustre02/work/ik1017/CMIP6/data/CMIP6/CMIP/IPSL/IPSL-CM6A-LR/piControl/r1i1p1f1/",
                    c("AERmon", "AERmonZ", "Amon", "CFmon", "Emon", "EmonZ", "ImonAnt", "ImonGre", "LImon", "Lmon", "Omon", "SImon"))
} else {
    paths <- commandArgs(trailingOnly=T)
}

# checks
paths <- unique(paths)
for (path in paths) {
    if (!dir.exists(path)) {
        stop("path \"", path, "\" does not exist")
    }
}

if (Sys.which("grep") == "") stop("could not find grep")
ncdump <- Sys.which("ncdump")
if (ncdump == "") stop("could not find ncdump")

# start
if (interactive()) {
    cat("check ", length(paths), " paths for output ...\n", sep="")
}
files <- list()
for (pathi in seq_along(paths)) {
    tmp <- list.files(paths[pathi], full.names=F, recursive=T) # e.g. airmass/gr/v20181022/airmass_AERmon_IPSL-CM6A-LR_piControl_r1i1p1f1_gr_187001-234912.nc
    variables <- dirname(dirname(dirname(tmp))) 
    variables_unique <- unique(variables) # e.g. airmass o3 od870aer pfull phalf ps tatp ua va zg
    cnt <- length(files)
    for (vi in seq_along(variables_unique)) {
        cnt <- cnt + 1
        ind <- which(variables == variables_unique[vi])[1] # first file per variable
        files[[cnt]] <- list(variable=variables_unique[vi],
                             file=paste0(paths[pathi], "/", tmp[ind]))
    } # for vi
} # for pathi

# get variable attributes via ncdump
if (interactive()) {
    cat("\nget variable infos of ", length(files), " files via ncdump ...\n", sep="")
}
for (fi in seq_along(files)) {
    cmd <- paste0(ncdump, " -h ", files[[fi]]$file, " | grep ", files[[fi]]$variable, ":")
    if (F && interactive()) cat("run `", cmd, "` ...\n", sep="")
    infos <- system(cmd, intern=T)
    for (atti in seq_len(5)) {
        if (atti == 1) attname <- "long_name"
        if (atti == 2) attname <- "standard_name"
        if (atti == 3) attname <- "description"
        if (atti == 4) attname <- "units"
        if (atti == 5) attname <- "cell_methods"
        files[[fi]][attname] <- NA
        att <- infos[grep(attname, infos)]
        if (length(att) > 0) {
            att <- gsub("[\r\n\t\"]", "", att) # remove regular expressions
            att <- strsplit(att, " = ")[[1]][2] # e.g. "Vertically integrated mass content of air in layer ;"
            att <- substr(att, 1, nchar(att)-2)
            files[[fi]][attname] <- att
        }
    } # for atti
} # for fi

if (interactive()) {
    cat("\nfound ", length(files), " variables:\n", sep="")
}
nchar_max_number <- nchar(length(files))
nchar_max_variable <- max(nchar(sapply(files, "[[", "variable")))
nchar_max_long_name <- sapply(files, "[[", "long_name")
if (!all(is.na(nchar_max_long_name))) {
    nchar_max_long_name <- max(nchar(nchar_max_long_name))
} else {
    nchar_max_long_name <- NA
}
nchar_max_standard_name <- sapply(files, "[[", "standard_name")
if (!all(is.na(nchar_max_standard_name))) {
    nchar_max_standard_name <- max(nchar(nchar_max_standard_name))
} else {
    nchar_max_standard_name <- NA
}
nchar_max_description <- sapply(files, "[[", "description")
if (!all(is.na(nchar_max_description))) {
    nchar_max_description <- max(nchar(nchar_max_description))
} else {
    nchar_max_description <- NA
}
nchar_max_units <- sapply(files, "[[", "units")
if (!all(is.na(nchar_max_units))) {
    nchar_max_units <- max(nchar(nchar_max_units))
} else {
    nchar_max_units <- NA
}
nchar_max_cell_methods <- sapply(files, "[[", "cell_methods")
if (!all(is.na(nchar_max_cell_methods))) {
    nchar_max_cell_methods <- max(nchar(nchar_max_cell_methods))
} else {
    nchar_max_cell_methods <- NA
}
for (fi in seq_along(files)) {
    cat(sprintf(paste0("%", nchar_max_number, "i"), fi), ": ", 
        sprintf(paste0("%-", nchar_max_variable, "s"), files[[fi]]$variable), " ", sep="")
    if (!is.na(nchar_max_long_name)) {
        cat(sprintf(paste0("%-", nchar_max_long_name, "s"), files[[fi]]$long_name), " ", sep="")
    }
    if (!is.na(nchar_max_standard_name)) {
        cat(sprintf(paste0("%-", nchar_max_standard_name, "s"), files[[fi]]$standard_name), " ", sep="")
    }
    if (!is.na(nchar_max_description)) {
        cat(sprintf(paste0("%-", nchar_max_description, "s"), files[[fi]]$description), " ", sep="")
    }
    if (!is.na(nchar_max_units)) {
        cat(sprintf(paste0("%-", nchar_max_units, "s"), files[[fi]]$units), " ", sep="")
    }
    if (!is.na(nchar_max_cell_methods)) {
        cat(sprintf(paste0("%-", nchar_max_cell_methods, "s"), files[[fi]]$cell_methods), " ", sep="")
    }
    cat(files[[fi]]$file, "\n", sep="")
} # for fi


