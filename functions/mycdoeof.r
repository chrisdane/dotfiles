#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=F)
me <- basename(sub("--file=", "", args[grep("--file=", args)]))
args <- commandArgs(trailingOnly=T)

# from R > 3.2
trimws <- function (x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
{
    which <- match.arg(which)
    mysub <- function(re, x) sub(re, "", x, perl = TRUE)
    switch(which, left = mysub(paste0("^", whitespace, "+"),
        x), right = mysub(paste0(whitespace, "+$"), x), both = mysub(paste0(whitespace,
        "+$"), mysub(paste0("^", whitespace, "+"), x)))
}

# check
usage <- paste0("\nUsage:\n $ ", me, " args\n",
                " must have arg:\n",
                "   --anom_file=<anom_file>\n",
                " optional args:\n",
                "   --dry\n",
                "   --neof=3\n",
                "   --varname=`cdo showname anom_file`\n",
                "   --outdir=`dirname(anom_file)`\n",
                "   --method=eof (eof, eoftime, eofspatial)\n",
                "   --cdo_weight_mode=off\n",
                "   --max_jacobi_iter=100\n",
                "   --P=`min($nproc, 4)` (only applies if cdo version > 1.9.9, P=1 otherwise)\n")
if (length(args) == 0) {
    message(usage)
    quit()
}

# check if cdo exists
if (Sys.which("cdo") == "") stop("could not find cdo program")
cdo_version <- paste0("cdo --version 2>&1")
cdo_version <- system(cdo_version, intern=T)
cdo_version <- cdo_version[1] # e.g. "Climate Data Operators version 1.7.0 (http://mpimet.mpg.de/cdo)"
cdo_version <- strsplit(cdo_version, " ")[[1]]
cdo_version <- sapply(cdo_version, strsplit, split="\\.")
cdo_version <- suppressWarnings(lapply(cdo_version, as.numeric))
tmp <- sapply(cdo_version, is.na)
tmp <- lapply(tmp, "==", "FALSE")
tmp <- sapply(tmp, any)
if (any(tmp)) {
    if (length(which(tmp)) == 1) {
        cdo_version <- cdo_version[[which(tmp)]] # numeric vector of length 3; e.g.: 1 7 0
    } else {
        stop("the case of more than 1 as.numeric() of `cdo --version?` is not implemented here")
    }
} else {
    stop("the case of 0 as.numeric() of `cdo --version?` is not implemented here")
}
message("cdo_version = ", paste(cdo_version, collapse="."))

# check anom_file
if (!any(grepl("--anom_file", args))) {
    stop("must provide --anom_file=<anom_file> argument", usage)
} else {
    anom_file <- sub("--anom_file=", "", args[grep("--anom_file=", args)])
    message("anom_file = ", anom_file)
    if (!file.exists(anom_file)) stop("anom_file = \"", anom_file, "\" does not exist")
    indir <- normalizePath(dirname(anom_file))
}
# check if input is monthly or not
monthly <- F
if (grepl("Jan-Dec", anom_file)) monthly <- T

# check varname
if (!any(grepl("--varname", args))) {
    message("`--varname=<varname>` or `--varname=varname1,varname2` not provided --> run `cdo showname` ...") 
    if (Sys.which("cdo") == "") stop("did not find cdo command")
    cmd <- paste0("cdo -s showname ", anom_file)
    varnames <- system(cmd, intern=T)
    varnames <- strsplit(varnames, " ")[[1]]
} else {
    varnames <- sub("--varname=", "", args[grep("--varname=", args)])
    varnames <- strsplit(varnames, ",")[[1]]
}
if (any(varnames == "")) varnames <- varnames[-which(varnames == "")]
message("--> varnames = \"", paste(varnames, collapse="\", \""), "\"")
if (length(varnames) == 0) stop("found zero varnames")

# check method
if (any(grepl("--method", args))) {
    method <- sub("--method=", "", args[grep("--method=", args)])
    method <- as.character(method)
    if (!any(method == c("eof", "eoftime", "eofspatial"))) {
        stop("provided method = ", method, 
             " must be either \"eof\" (default), \"eoftime\" or \"eofspatial\"")
    } else {
        message("provided method = ", method)
    }
} else {
    method <- "eof"
    message("--method not provided. use default ", method) 
}

# check cdo_weight_mode
if (any(grepl("--cdo_weight_mode", args))) {
    cdo_weight_mode <- sub("--cdo_weight_mode=", "", args[grep("--cdo_weight_mode=", args)])
    cdo_weight_mode <- as.character(cdo_weight_mode)
    if (cdo_weight_mode != "on" && cdo_weight_mode != "off") {
        stop("provided cdo_weight_mode = ", cdo_weight_mode, 
             " must be either \"off\" (recommended) or \"on\"")
    } else {
        message("provided cdo_weight_mode = ", cdo_weight_mode)
    }
} else {
    cdo_weight_mode <- "off"
    message("--cdo_weight_mode not provided. use default ", cdo_weight_mode) 
}

# check max_jacobi_iter
if (any(grepl("--max_jacobi_iter", args))) {
    max_jacobi_iter <- sub("--max_jacobi_iter=", "", args[grep("--max_jacobi_iter=", args)])
    max_jacobi_iter <- as.numeric(max_jacobi_iter)
    message("provided max_jacobi_iter = ", max_jacobi_iter)
} else {
    max_jacobi_iter <- 100
    message("--max_jacobi_iter not provided. use default ", max_jacobi_iter) 
}

# check parallel processors P; only for cdo version >= 1.9.10
# if cdo version <= 1.9.9, --P must equal 1
if (any(grepl("--P", args))) { # --P is given
    nparallel <- sub("--P=", "", args[grep("--P=", args)])
    nparallel<- as.integer(nparallel)
    if (cdo_version[1] < 1 ||
        cdo_version[1] == 1 && cdo_version[2] < 9 || 
        cdo_version[1] == 1 && cdo_version[2] >= 9 && cdo_version[3] < 10) {
        message("--P = ", nparallel, " is given but cdo_version = ", cdo_version, 
                " < 1.9.10. there is a data race bug in `eof,neof --P`. set --P=1 ...")
        nparallel <- 1
    } 
    message("provided --P = ", nparallel)
} else { # --P not given
    if (cdo_version[1] < 1 ||
        cdo_version[1] == 1 && cdo_version[2] < 9 || 
        cdo_version[1] == 1 && cdo_version[2] >= 9 && cdo_version[3] < 10) { # cdo < 1.9.10
        nparallel <- 1
    } else { # cdo >= 1.9.10
        if (Sys.which("nproc") == "") stop("could not find nproc program")
        nproc <- as.integer(system("nproc", intern=T))
        nparallel <- min(nproc, 4) 
    }
    message("--P not provided. use default ", nparallel) 
}
if (nparallel > 1) {
    if (!exists("nproc")) {
        if (Sys.which("nproc") == "") stop("could not find nproc program")
        nproc <- as.integer(system("nproc", intern=T))
    }
    if (nparallel > nproc) {
        message("--P = ", nparallel, " > $(nproc) = ", nproc, 
                ". set --P=", nproc, " ...")
        nparallel <- nproc
    }
}

# check neof
if (any(grepl("--neof", args))) {
    neof <- sub("--neof=", "", args[grep("--neof=", args)])
    neof <- as.numeric(neof)
    if (!is.numeric(neof)) stop("provided neof=", neof, " is not numeric")
} else {
    neof <- 3
    message("--neof not provided. use default ", neof) 
}

# check dry
dry <- F
if (any(args == "--dry")) {
    message("--dry provided --> dry run")
    dry <- T
}

# check outdir
if (!any(grepl("--outdir", args))) { # outdir not provided
    outdir <- normalizePath(dirname(anom_file))
    message("outdir not provided. use default `dirname(anom_file)`")
} else { # outdir provided
    outdir <- sub("--outdir=", "", args[grep("--outdir=", args)])
}
if (file.access(outdir, mode=0) == -1) { # not existing
    message("outdir = \"", outdir, "\" does not exist. try to create ... ", appendLF=F)
    dir.create(outdir, recursive=T)
    if (!file.exists(outdir)) {
        stop("not successful. error msg:")
    } else {
        message("ok")
    }
} else { # outdir exists
    if (file.access(outdir, mode=2) == -1) { # not writable
        stop("provided outdir = \"", outdir, "\" not writeable.")
    }
}
outdir <- normalizePath(outdir)
message("--> outdir = \"", outdir, "\"")


# run annual and seasonal means if monthly before trend
fin_all <- c()
if (monthly) { # if input is monthly
    message("string \"Jan-Dec\" detected in anom_file --> calc annual and seasonal means ...")

    # calc annual mean
    fout <- sub("Jan-Dec", "annual", anom_file)
    if (file.exists(fout)) {
        message("file \"", fout, "\" already exists. skip cdo yearmean")
    } else {
        cmd <- paste0("cdo yearmean ", anom_file, " ", fout)
        if (file.access(dirname(anom_file), mode=2) == -1) {
            stop("input is monthly but cannot run\n`", cmd, "` since output dir is not writable.")
        }
        message("run `", cmd, "` ...")
        if (!dry) system(cmd)
    }
    fin_all <- c(fin_all, fout)

    # calc seasonal means and select seasons
    fout <- sub("Jan-Dec", "seasmean", anom_file)
    if (file.exists(fout)) {
        message("file \"", fout, "\" already exists. skip cdo seasmean")
    } else {
        cmd <- paste0("cdo seasmean ", anom_file, " ", fout)
        message("run `", cmd, "` ...")
        if (!dry) system(cmd)
    }
    seasons <- c("DJF", "MAM", "JJA", "SON")
    for (si in seq_along(seasons)) {
        fout_si <- sub("seasmean", paste0(seasons[si], "mean"), fout)
        if (file.exists(fout_si)) {
            message("file \"", fout_si, "\" already exists. skip cdo selseas,", seasons[si])
        } else {
            cmd <- paste0("cdo -selseas,", seasons[si], " ", fout, " ", fout_si)
            message("run `", cmd, "` ...")
            if (!dry) system(cmd)
        }
        fin_all <- c(fin_all, fout_si)
    } # for si

} else { # if input is not monthly
    fin_all <- c(fin_all, anom_file)

} # if monthly
    
# calc eof for all variables and files
for (vi in seq_along(varnames)) {

    message("***************** varname ", vi, "/", length(varnames), " ********************")
    
        for (fi in seq_along(fin_all)) {

        if (length(fin_all) > 1) message("***************** file ", fi, "/", length(fin_all), " ********************")

        # cdo eof
        fout_eigval <- paste0(fin_all[fi], "_", varnames[vi], "_eof_", neof, "_cdo", method, "_eigval.nc")
        fout_eigvec <- paste0(fin_all[fi], "_", varnames[vi], "_eof_", neof, "_cdo", method, "_eigvec.nc")
        cmd <- paste0("export CDO_WEIGHT_MODE=", cdo_weight_mode, "; ", 
                      "export MAX_JACOBI_ITER=", max_jacobi_iter, "; ", 
                      "cdo -v -L -P ", nparallel, " ", method, ",", neof, " -selvar,", varnames[vi], " ",
                      indir, "/", fin_all[fi], " ", 
                      outdir, "/", fout_eigval, " ", outdir, "/", fout_eigvec)
        message("run `", cmd, "` ...")
        if (!dry) {
            tic <- Sys.time()
            system(cmd)
            toc <- Sys.time()
            
            if (!file.exists(paste0(outdir, "/", fout_eigval))) {
                stop("result \"", outdir, "/", fout_eigval, "\" does not exist but should")
            }

            elapsed <- toc - tic
            elapsed <- paste0("\n--> `cdo ", method, "` call took ", round(elapsed), " ", attr(elapsed, "units"))
            message(elapsed)

            # set elapsed time as global nc attribute
            cmd <- paste0("ncatted -O -a cdo_", method, "_elapsed,global,c,c,\"", elapsed, "\" ",
                          outdir, "/", fout_eigval, " ", outdir, "/", fout_eigval)
            message("\nrun `", cmd, "` ...")
            system(cmd)
            cmd <- paste0("ncatted -O -h -a cdo_", method, "_elapsed,global,c,c,\"", elapsed, "\" ",
                          outdir, "/", fout_eigvec, " ", outdir, "/", fout_eigvec)
            message("run `", cmd, "` ...")
            system(cmd)
        } # if not dry
        
        # cdo eofcoeff
        fout_pc <- paste0(fin_all[fi], "_", varnames[vi], "_eof_", neof, "_cdo", method, "_pc.nc")
        cmd <- paste0("export CDO_FILE_SUFFIX=NULL; ",
                      "cdo -v eofcoeff ", outdir, "/", fout_eigvec, " ", indir, "/", fin_all[fi], " ",
                      outdir, "/", fout_pc)
        message("\nrun `", cmd, "` ...")
        if (!dry) system(cmd)

        #stop("asd")

        # rename the *nc00000, *.nc000001, ... files and calc normalized PC = pc_i/sqrt(eigenval_i)
        merge_files <- rep(NA, t=2*neof)
        cnt <- 0
        for (i in seq_len(neof)-1) { # 0, 1, 2, ...
            message("\nprocess pc ", i+1, ":")
            
            # rename
            fout_pc_i <- paste0(fin_all[fi], "_", varnames[vi], "_eof_", neof, "_cdo", method, "_pc", i+1, ".nc")
            cmd <- paste0("mv ", outdir, "/", fout_pc, sprintf("%05i", i), " ", 
                          outdir, "/", fout_pc_i)
            message("run `", cmd, "` ...")
            if (!dry) system(cmd)
            cnt <- cnt+1
            merge_files[cnt] <- paste0(outdir, "/", fout_pc_i)
            
            # setname
            cmd <- paste0("cdo -s setname,pc", i+1, " ", outdir, "/", fout_pc_i, " ", 
                          outdir, "/tmp && mv ", outdir, "/tmp", " ", outdir, "/", fout_pc_i)
            message("run `", cmd, "` ...")
            if (!dry) system(cmd)
            
            # normalized pc = pc/sqrt(eigenval); eq. 13.21 von stoch and zwiers 1999
            fout_normalized_pc_i <- paste0(fin_all[fi], "_", varnames[vi], "_eof_", neof, "_cdo", 
                                           method, "_normalized_pc", i+1, ".nc")
            cmd <- paste0("cdo -L -s -setname,pc", i+1, "_normalized -div ", outdir, "/", fout_pc_i, 
                          " -sqrt -seltimestep,", i+1, " ", outdir, "/", fout_eigval, " ",
                          outdir, "/", fout_normalized_pc_i)
            message("run `", cmd, "` ...")
            if (!dry) system(cmd)
            cnt <- cnt+1
            merge_files[cnt] <- paste0(outdir, "/", fout_normalized_pc_i)
        } # for i neof

        # merge default and normalized eigenvec PCs together
        message("\nmerge ", neof, " default and normalized PCs together ...")
        cmd <- paste0("cdo -s -O merge ", 
                      paste(merge_files, collapse=" "), " ",
                      outdir, "/", fout_pc)
        message("run `", cmd, "` ...")
        if (!dry) {
            system(cmd)
            file.remove(merge_files)
        }

        # setname eigenval and eigenvec
        cmd <- paste0("cdo -s --reduce_dim setname,eigenval_abs ", outdir, "/", fout_eigval, " ",
                      outdir, "/tmp && mv ", outdir, "/tmp ",
                      outdir, "/", fout_eigval)
        message("\nrun `", cmd, "` ...")
        if (!dry) system(cmd)
        cmd <- paste0("cdo -s setname,eigenvec ", outdir, "/", fout_eigvec, " ",
                      outdir, "/tmp && mv ", outdir, "/tmp ",
                      outdir, "/", fout_eigvec)
        message("run `", cmd, "` ...")
        if (!dry) system(cmd)

        # calc eigenval in percent
        fout_eigval_pcnt <- paste0(fin_all[fi], "_", varnames[vi], "_eof_", neof, "_cdo", method, "_eigval_pcnt.nc")
        cmd <- paste0("cdo -s -L -setname,eigenval_pcnt -mulc,100 -div ", outdir, "/", fout_eigval, " ",
                      "-timsum ", outdir, "/", fout_eigval, " ",
                      outdir, "/", fout_eigval_pcnt)
        message("\nrun `", cmd, "` ...")
        if (!dry) system(cmd)

        # merge eigenval abs and pcnt together
        message("\nmerge absolute eigenvals and in percent together ...")
        cmd <- paste0("cdo -s merge ", outdir, "/", fout_eigval, " ",
                      outdir, "/", fout_eigval_pcnt, " ",
                      outdir, "/tmp && mv ", outdir, "/tmp ",
                      outdir, "/", fout_eigval)
        message("run `", cmd, "` ...")
        if (!dry) {
            system(cmd)
            file.remove(paste0(outdir, "/", fout_eigval_pcnt))
        }

        # calc normalized eigenvec = sqrt(eigenval_eofi) * eigenvec_eofi; eq. 13.22 von stoch and zwiers 1999
        fout_sqrt_eigval <- fout_normalized_eigvec <- rep(NA, t=neof)
        for (i in seq_len(neof)) {
            message("\nprocess eigenvec ", i, " ...")
            
            fout_sqrt_eigval_i <- paste0(fin_all[fi], "_", varnames[vi], "_eof_", neof, "_cdo", 
                                         method, "_sqrt_eigval", i, ".nc")
            cmd <- paste0("cdo -s -L -setname,sqrt_eigenval -sqrt -seltimestep,", i, " -selvar,eigenval_abs ",
                          outdir, "/", fout_eigval, " ",
                          outdir, "/", fout_sqrt_eigval_i)
            message("run `", cmd, "` ...")
            if (!dry) system(cmd)
            fout_sqrt_eigval[i] <- fout_sqrt_eigval_i

            fout_normalized_eigvec_i <- paste0(fin_all[fi], "_", varnames[vi], "_eof", neof, "_cdo", 
                                               method, "_normalized_eigvec_", i, ".nc")
            cmd <- paste0("cdo -s -seltimestep,", i, " ", 
                          outdir, "/", fout_eigvec, " ",
                          outdir, "/", fout_normalized_eigvec_i)
            message("run `", cmd, "` ...")
            if (!dry) system(cmd)
            fout_normalized_eigvec[i] <- fout_normalized_eigvec_i

            cmd <- paste0("cdo -s merge ", outdir, "/", fout_sqrt_eigval_i, " ",
                          outdir, "/", fout_normalized_eigvec_i, " ",
                          outdir, "/tmp && mv ", outdir, "/tmp ", 
                          outdir, "/", fout_normalized_eigvec_i)
            message("run `", cmd, "` ...")
            if (!dry) system(cmd)

            cmd <- paste0("cdo -s -expr,normalized_eigenvec=\"eigenvec*sqrt_eigenval\" ", 
                          outdir, "/", fout_normalized_eigvec_i, " ",
                          outdir, "/tmp && mv ", outdir, "/tmp ", 
                          outdir, "/", fout_normalized_eigvec_i)
            message("run `", cmd, "` ...")
            if (!dry) system(cmd)
        } # for i neof

        # merge all normalized eigenvecs
        message("\nmerge ", neof, " normalized eigenvecs together ...")
        fout_eigvec_normalized <- paste0(fin_all[fi], "_", varnames[vi], "_eof_", neof, "_cdo", 
                                         method, "_normalized_eigvec.nc")
        cmd <- paste0("cdo -s -O mergetime ", paste(paste0(outdir, "/", fout_normalized_eigvec), collapse=" "), " ",
                      outdir, "/", fout_eigvec_normalized)
        message("run `", cmd, "` ...")
        if (!dry) system(cmd)
        
        # correct units
        if (!dry) {
            message("\ncorrect units ...")
            data_unit <- system(paste0("cdo showunit ", outdir, "/", fout_eigvec), intern=T)
            data_unit <- trimws(data_unit)
            cmd <- paste0("cdo setunit,\"", data_unit, "\" ", 
                          outdir, "/", fout_eigvec_normalized, " ",
                          outdir, "/tmp && mv ", outdir, "/tmp ",
                          outdir, "/", fout_eigvec_normalized)
            message("run `", cmd, "` ...")
            system(cmd)
            # delete old units attribute "<nco_ncatted> -O -a code,aprt_times_temp2,d,,",
            cmd <- paste0("ncatted -O -a units,,d,, ",
                          outdir, "/", fout_eigvec)#, " ",
                          #outdir, "/tmp && mv ", outdir, "/tmp ",
                          #outdir, "/", fout_eigvec)
            message("run `", cmd, "` ...")
            system(cmd)
        }

        # merge default and normalized eigenvecs
        message("\nmerge ", neof, " default and normalized eigenvecs together ...")
        cmd <- paste0("cdo -s merge ", outdir, "/", fout_eigvec, " ", 
                      outdir, "/", fout_eigvec_normalized, " ",
                      outdir, "/tmp && mv ", outdir, "/tmp ",
                      outdir, "/", fout_eigvec)
        message("run `", cmd, "` ...")
        if (!dry) {
            system(cmd)
            file.remove(paste0(outdir, "/", fout_sqrt_eigval))
            file.remove(paste0(outdir, "/", fout_normalized_eigvec))
            file.remove(paste0(outdir, "/", fout_eigvec_normalized))
        }


        # described variance
        if (!dry) {
            cmd <- paste0("ncdump -v eigenval_pcnt ", outdir, "/", fout_eigval)
            dump <- system(cmd, intern=T)
            dump <- dump[(grep("data:", dump)):(length(dump)-1)]
            dump <- dump[3:length(dump)]
            dump <- sub("eigenval_pcnt = ", "", dump)
            dump <- gsub(",", "", dump)
            dump <- gsub(";", "", dump)
            dump <- trimws(dump)
            dump <- paste(dump, collapse=" ")
            dump <- strsplit(dump, " ")[[1]]
            dump <- as.numeric(dump)
            message("\ndescribed variance:\n",
                    paste(paste0("   ", method, seq_len(neof), ": ", 
                                 round(dump[seq_len(neof)], 1), "%"), collapse="\n"))
        }

    } # for fi in fin_all
} # for vi in varnames


# transfer to other server if wanted
if (F && grepl("stan", Sys.info()["nodename"])) {
    message("##########################")
    outdir_paleosrv <- paste0("/isibhv/projects/paleo_work/cdanek/post", 
                              substr(outdir, regexpr("post/", outdir)+4, nchar(outdir)))
    cmd <- paste0("scp ", paste(paste0(outdir, "/", fout_all), collapse=" "), 
                  " cdanek@paleosrv1.awi.de:", outdir_paleosrv, "/.")
    message("run `", cmd, "` ...")
    if (!dry) system(cmd)
} # if on stan

message("\nfinished")

