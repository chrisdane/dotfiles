# r

# r_install_pkg.sh: part 1/2 of complicated r package installation
# r_install_pkg.r:  part 2/2

# part 2: called from part 1; set package-specific options and install packages

# set package-specific options
# -> see `./configure --help` of several packages at the bottom

args <- commandArgs(trailingOnly=F)
me <- normalizePath(sub("--file=", "", args[grep("--file=", args)]))
r_binary <- args[1]
pid <- Sys.getpid()
root_script <- "r_install_pkg.sh"
message("\n############## part 2/2: ", me, " ################\n\n",
        "run ", r_binary, "\n",
        "--> R pid = ", pid, "\n",
        "--> ", version["version.string"])

# check if R runs with --vanilla = --no-save, --no-restore, --no-site-file, --no-init-file and --no-environ
if (!any(grepl("--vanilla", args))) {
    stop("run R with `--vanilla` from ", root_script)
} else {
    message("--> R runs with `--vanilla`")
}
message("--> .libPaths() = \n",
        paste(paste0("    ", .libPaths()), collapse="\n"))

# evaluate trailing _named_ arguments to be available in r works space
args <- commandArgs(trailingOnly=T)
named_args <- args[which(substr(args, 1, 2) == "--")]
if (length(named_args) > 0) {
    message("--> evaluate provided named arguments:")
    for (argi in seq_along(named_args)) {
        arg <- named_args[argi]
        arg <- substr(arg, 3, nchar(arg)) # after leading "--"
        arg <- strsplit(arg, "=")[[1]] # "lhs" "rhs"
        if (length(arg) != 2) stop("provided argument \"", named_args[argi], "\" not interpretable")
        cmd <- paste0(arg[1], " <- \"", arg[2], "\"")
        message("    `", cmd, "`")
        eval(parse(text=cmd))
    } # for argi named_args
}

# check if this part 2 was properly called from part 1
if (!exists("root_pid")) {
    stop("run `./", root_script, "` with named argument `--root_pid`.")
} else {
    cmd <- paste0("ps f -p ", root_pid)
    #message("run `", cmd, "` ...")
    t1 <- suppressWarnings(try(system(cmd, ignore.stderr=T, ignore.stdout=T, intern=T)))
    if (!is.null(attributes(t1))) {
        atts <- attributes(t1)
        if (!any(names(atts) == "status")) stop("this never happened")
        if (atts$status == 1) {
            stop("This r process was not called from part 1. Edit and  run `./", root_script, "` instead.")
        }
    } else {
        message("--> R called from pid ", root_pid)
    }
}

# show paths
PATH <- Sys.getenv("PATH")
LD_LIBRARY_PATH <- Sys.getenv("LD_LIBRARY_PATH")
PKG_CONFIG_PATH <- Sys.getenv("PKG_CONFIG_PATH")
message("--> $PATH = \n",
        paste(paste0("    ", strsplit(PATH, ":")[[1]]), collapse="\n"))
message("--> $LD_LIBRARY_PATH = \n",
        paste(paste0("    ", strsplit(LD_LIBRARY_PATH, ":")[[1]]), collapse="\n"))
message("--> $PKG_CONFIG_PATH = \n",
        paste(paste0("    ", strsplit(PKG_CONFIG_PATH, ":")[[1]]), collapse="\n"))
if (exists("ldflags")) {
    message("--> ldflags =")
    ldflags_tmp <- strsplit(ldflags, ",")[[1]]
    for (i in seq_along(ldflags_tmp)) {
        ldflags_tmp2 <- strsplit(ldflags_tmp[i], ":")[[1]]
        message("    ", ldflags_tmp2[1])
        if (length(ldflags_tmp2) > 1) {
            for (j in 2:length(ldflags_tmp2)) {
                message("       ", ldflags_tmp2[j])
            }
        }
    } 
}

# check repos
repos <- "https://cloud.r-project.org"
message("--> repos = \"", repos, "\"")

# check packages
unnamed_args <- args[-which(substr(args, 1, 2) == "--")]
if (length(unnamed_args) == 0) {
    stop("no packages provided in ", root_script)
} else {
    pkgs <- unnamed_args
}
message("--> install ", length(pkgs), " packages = \"", paste(pkgs, collapse="\", \""), "\"")

# checks passed

# do for all packages
for (pkgi in seq_along(pkgs)) {
   
    pkg <- pkgs[pkgi]
    message("\n************ pkg ", pkgi, "/", length(pkgs), ": \"", pkg, "\" ************\n")
    
    # set paths back to default
    Sys.setenv(PATH=PATH)
    Sys.setenv(LD_LIBRARY_PATH=LD_LIBRARY_PATH)
    Sys.setenv(PKG_CONFIG_PATH=PKG_CONFIG_PATH)
    
    configure.args <- NULL # default

    if (pkg == "rgeos") {
        if (!exists("geos_path")) stop("package \"", pkg, "\" needs argument `geos_path` in ", root_script)
        configure.args <- paste0("--with-geos-config=", geos_path, "/bin/geos-config")# ",
                                 #"PKG_LIBS=$ldflags") # check
    
    } else if (pkg == "proj4") {
        if (!exists("proj_path")) stop("package \"", pkg, "\" needs argument `proj_path` and in ", root_script)
        if (!exists("sqlite_path")) stop("package \"", pkg, "\" needs argument `sqlite_path` and in ", root_script)
        configure.args <- paste0("PKG_CPPFLAGS=-I$", proj_path, "/include ",
                                 "PKG_LIBS='-L$", proj_path, "/lib ",
                                           "-L$", sqlite_path, "/lib")# ",
                                           # "$ldflags'") # check
    
    } else if (pkg == "sf") {
        if (!exists("udunits_path")) stop("package \"", pkg, "\" needs argument `udunits_path` and in ", root_script)
        if (!exists("gdal_path")) stop("package \"", pkg, "\" needs argument `gdal_path` and in ", root_script)
        if (!exists("geos_path")) stop("package \"", pkg, "\" needs argument `geos_path` and in ", root_script)
        if (!exists("proj_path")) stop("package \"", pkg, "\" needs argument `proj_path` and in ", root_script)
        if (!exists("sqlite_path")) stop("package \"", pkg, "\" needs argument `sqlite_path` and in ", root_script)
        message("add \"", udunits_path, "/lib\" to LD_LIBRARY_PATH")
        Sys.setenv(LD_LIBRARY_PATH=paste0(Sys.getenv("LD_LIBRARY_PATH"), ":", udunits_path, "/lib"))
        configure.args <- paste0("--with-gdal-config=${gdal_path}/bin/gdal-config ",
                                 "--with-geos-config=${geos_path}/bin/geos-config ",
                                 "--with-proj-lib=${proj_path}/lib ",
                                 "PKG_LIBS='-L${sqlite_path}/lib")# ",
                                           #"$ldflags'") # check

    } else if (pkg == "terra") {
        if (!exists("gdal_path")) stop("package \"", pkg, "\" needs argument `gdal_path` and in ", root_script)
        if (!exists("geos_path")) stop("package \"", pkg, "\" needs argument `geos_path` and in ", root_script)
        if (!exists("proj_path")) stop("package \"", pkg, "\" needs argument `proj_path` and in ", root_script)
        if (!exists("sqlite_path")) stop("package \"", pkg, "\" needs argument `sqlite_path` and in ", root_script)
        configure.args <- paste0("--with-gdal-config=${gdal_path}/bin/gdal-config ",
                                 "--with-proj-include=${proj_path}/include ",
                                 "--with-proj-lib=${proj_path}/lib ",
                                 "--with-proj-share=${proj_path}/share ",
                                 "--with-geos-config=${geos_path}/bin/geos-config ",
                                 "PKG_LIBS='-L${sqlite_path}/lib ",
                                           "$ldflags'")
    
    } else if (pkg == "units") {
        if (!exists("udunits_path")) stop("package \"", pkg, "\" needs argument `udunits_path` and in ", root_script)
        message("add \"", udunits_path, "/lib\" to LD_LIBRARY_PATH")
        Sys.setenv(LD_LIBRARY_PATH=paste0(Sys.getenv("LD_LIBRARY_PATH"), ":", udunits_path, "/lib"))
        # or withr::with_envvar(c("LD_LIBRARY_PATH"="${udunits_path}/lib"), install.packages(...))
        configure.args <- paste0("--with-udunits2-lib=", udunits_path, "/lib ",
                                 "--with-udunits2-include=", udunits_path, "/include")
    
    } else if (pkg == "oce") {
        message("add \"", udunits_path, "/lib\" to LD_LIBRARY_PATH")
        Sys.setenv(LD_LIBRARY_PATH=paste0(Sys.getenv("LD_LIBRARY_PATH"), ":", udunits_path, "/lib"))
           
    } else if (pkg == "bookdown") {
        stop("update")
        Sys.setenv(PATH="${pandoc_path}/bin:${PATH}")
         
    } # which package

    # check paths before package installation
    message("\n$PATH =")
    message(paste(paste0("   ", strsplit(Sys.getenv("PATH"), ":")[[1]]), collapse="\n"))
    message("$LD_LIBRARY_PATH =")
    message(paste(paste0("   ", strsplit(Sys.getenv("LD_LIBRARY_PATH"), ":")[[1]]), collapse="\n"))
    message("$PKG_CONFIG_PATH =")
    message(paste(paste0("   ", strsplit(Sys.getenv("PKG_CONFIG_PATH"), ":")[[1]]), collapse="\n"))
    
    # run installation
    cmd <- paste0("install.packages(\"", pkg, "\", lib=\"", lib, "\", repos=\"", 
                  repos, "\", configure.args=\"", configure.args, "\")")
    message("\nrun `", cmd, "` ...\n")
    eval(parse(text=cmd))
    
    message("\n$LD_LIBRARY_PATH =")
    message(paste(paste0("   ", strsplit(Sys.getenv("LD_LIBRARY_PATH"), ":")[[1]]), collapse="\n"))
    
    # check if package can be loaded with same LD_LIBRARY_PATH that was used for installation
    message("\ntry to load package \"", pkg, "\" with LD_LIBRARY_PATH (see potential warnings at the end)")
    tc <- tryCatch(suppressMessages(suppressWarnings(
                      library(pkg, lib=lib, character.only=T))),
                   error=function(e) e, warning=function(w) w)
    if (any(search() == paste0("package:", pkg))) {
        message("--> ok")
    } else {
        message("--> failed. see warning at the end")
        warning("warning of loading package \"", pkg, "\" with LD_LIBRARY_PATH:\n", tc$message)
    }

    # check if package can be loaded with blank LD_LIBRARY_PATH
    message("\ntry to load package \"", pkg, "\" without LD_LIBRARY_PATH (see potential warnings at the end)")
    Sys.setenv(LD_LIBRARY_PATH="")
    tc <- tryCatch(suppressMessages(suppressWarnings(
                      library(pkg, lib=lib, character.only=T))),
                   error=function(e) e, warning=function(w) w)
    if (any(search() == paste0("package:", pkg))) {
        message("--> ok")
    } else {
        message("--> failed. see warning at the end")
        warning("warning of loading package \"", pkg, "\" without LD_LIBRARY_PATH:\n", tc$message)
    }

    message()

} # for pkgi

## some `./configure --help` entries:
#
# sf:
# --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
# --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
# --with-gdal-config=GDAL_CONFIG
#                         the location of gdal-config
# --with-data-copy=yes/no local copy of data directories in package, default
#                         no
# --with-proj-data=DIR    location of PROJ data directory
# --with-proj-include=DIR location of proj header files
# --with-proj-api=yes/no  use the deprecated proj_api.h even when PROJ 6 is
#                         available; default no
# --with-proj-lib=LIB_PATH
#                         the location of proj libraries
# --with-proj-share=SHARE_PATH
#                         the location of proj metadata files
# --with-geos-config=GEOS_CONFIG
#                         the location of geos-config
#
# terra (needs raster, Rcpp):
# --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
# --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
# --with-gdal-config=GDAL_CONFIG
#                         the location of gdal-config
# --with-data-copy=yes/no local copy of data directories in package, default
#                         no
# --with-proj-data=DIR    location of PROJ data directory
# --with-proj-include=DIR location of proj header files
# --with-proj-api=yes/no  use the deprecated proj_api.h even when PROJ 6 is
#                         available; default no
# --with-proj-lib=LIB_PATH
#                         the location of proj libraries
# --with-proj-share=SHARE_PATH
#                         the location of proj metadata files
# --with-geos-config=GEOS_CONFIG
#                         the location of geos-config
# 
# rgdal:
# --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
# --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
# --with-gdal-config=GDAL_CONFIG
#                         the location of gdal-config
# --with-proj-include=DIR location of proj header files
# --with-proj-lib=LIB_PATH
#                         the location of proj libraries
# --with-proj_api=ARG        legacy proj_api.h while still available, or current proj.h (ARG="proj_api.h", "proj.h"(default))
# --with-proj-share=SHARE_PATH
#                         the location of proj metadata files
# --with-data-copy=yes/no local copy of data directories in package, default
#                         no
# --with-proj-data=DIR    location of PROJ.4 data directory
#
# proj4:
# --disable-option-checking  ignore unrecognized --enable/--with options
# --disable-FEATURE       do not include FEATURE (same as --enable-FEATURE=no)
# --enable-FEATURE[=ARG]  include FEATURE [ARG=yes]
# --enable-proj6          use the new PROJ6 API even if PROJ4 API is
#                         available. The default is [disable] since PROJ6 API
#                         has issues, use with caution.
# 
# rgeos:
# --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
# --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
# --with-geos-config=GEOS_CONFIG
#                         the location of geos-config
#
# raster:
#  no configure file in package source
#
# units:
# --with-PACKAGE[=ARG]    use PACKAGE [ARG=yes]
# --without-PACKAGE       do not use PACKAGE (same as --with-PACKAGE=no)
# --with-udunits2-include=DIR
#                         location of the udunits2 header files
# --with-udunits2-lib=DIR location of the udunits2 libraries
#


