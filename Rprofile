# r

# 0/1 = 0
# 1/0 = Inf; -1/0 = -Inf
# 0/0 = NaN
# options(warn=-1) # do not print warnings
# options(warn=0) # print warnings at the end
# options(warn=1) # print warnings as they occur
# options(warn=2) # print warnings as they occur and stop, i.e. turn warning into error
# options(error = recover)
# options(error = NULL)
# save output to file: sink("file.txt"); <do stuff>; sink()
# file.edit()
# options(prompt="R> ", digits=4, show.signif.stars=FALSE)
# getSrcDirectory
# remove all white space: gsub(" ", "", x, fixed=T)
# anyNA(x) is more efficient than any(is.na(x))
# which.min is more efficient than which(x == min(x)). ATTENTION: which.min(c(1,1,2)) = 1, i.e. the second (and 3rd,4th,...) minimum is negelected
# which(abs(zlevels - center_around) == min(abs(zlevels - center_around)))
# cols_rgb_p <- rgb(t(col2rgb(cols_p)/255), alpha=alpha_rgb)
# mean1 <- function(x) mean(x)
# mean2 <- function(x) sum(x) / length(x)
# --> mean2 is faster
# for 2D, rowSums(), colSums(), rowMeans(), and colMeans() are faster than apply()
# vapply() is faster than sapply()
# any(x == 10) is faster than 10 %in% x
# unlist(x, use.names=F) is faster than unlist(x)
# check stats:::t.test.default()
# SDMTools::grid.info(lats=lats, cellsize=diff(lons)[1])$area --> average 0.2% difference to `cdo gridarea`
# library(pryr) --> object_size(), mem_used(), mem_change() 
#file_sizes_byte <- file.size(files)
#file_sizes_pretty <- sapply(file_sizes_byte, utils:::format.object_size, "auto")
# strsplit(x, "\\s+")[[1]] arbitrary number of spaces blanks
# getAnywhere(objectname)
# check <- system(cmd)
# if (check != 0) stop("cmd failed")
# legend("topleft", "abc", col=1, lty=1, pch=22, pt.bg=col2rgba("gray", 0.3), pt.lwd=0, pt.cex=4, bty="n", cex=2)
# # system.file("extdata", "2012.csv", package = "testdat")
# options(menu.graphics=FALSE) #graphics dialogs always seem to crash R
# inds <- matrix(inds, nrow=1) # multi dim index
# order(x) = sort(x, index.return=T)$ix !!! pay attention to NA
# pdf.options(useDingbats = TRUE) https://yihui.name/knitr/demo/graphics/
# options("scipen"=100, "digits"=4): c(1.810032e+09, 4) --> 1810032000, 4
# list.files(pattern = glob2rx('*.tif'))
# list.files(pattern = '^.*\\.tif$')
# intersect(intersect(a,b),c) = Reduce(intersect, list(a,b,c))
# sprintf("%02i", 1) --> 01; sprintf("%2s", "a") --> " a"
# rJava pkg error: JDK is incomplete! Please make sure you have a complete JDK. JRE is *not* sufficient: sudo R CMD javareconf
# RColorBrewer::brewer.pal(n=RColorBrewer:::maxcolors["Spectral"], name="Spectral")
# cat(capture.output(str(dates_in_list)), sep="\n")
# return(as.list(environment())) # return everything defined in function
#‘isTRUE(x)’ is the same as ‘{ is.logical(x) && length(x) == 1 &&
#     !is.na(x) && x }’; ‘isFALSE()’ is defined analogously.
#     Consequently, ‘if(isTRUE(cond))’ may be preferable to ‘if(cond)’
#     because of ‘NA’s.
#me <- paste0(normalizePath(getSrcDirectory(read_gnip)), "/", getSrcFilename(read_gnip))
#remotes::install_github("nickmckay/LiPD-Utilities", subdir="R")
#pid=system("sleep 100 & echo $!", intern=T) leads to `sh <defunct>` zombie
#source(textConnection(file.lines.collapsed), ...)
#abline(a=0, b=1) 1:1 line
#ls("package:seacarb") # list all functions of package alphabetical
#base::getRversion() > "4.1.2"
# keep NULL as list element: 
# li[length(list)+1] <- list(NULL) # i.e. not `[[` in rhs!
# names(li)[lenght(li)] <- "myname"
# nc <- RNetCDF::open.nc(fname)
# RNetCDF::print.nc(nc)
# str(RNetCDF::file.inq.nc(nc))
#List of 6
# $ ndims     : int 6
# $ nvars     : int 11
# $ ngatts    : int 46
# $ unlimdimid: int 0
# $ format    : chr "classic4"
# $ libvers   : chr "4.8.1 of Feb 16 2022 20:56:37 $"# variable dims nc versus r:
#str(RNetCDF::att.inq.nc(nc, "NC_GLOBAL", 0)) # to 45
#List of 4
# $ id    : int 0
# $ name  : chr "Conventions"
# $ type  : chr "NC_CHAR"
# $ length: num 256
#str(RNetCDF::dim.inq.nc(nc, 0)) # to 5
#List of 4
# $ id    : int 0
# $ name  : chr "time"
# $ length: num 420
# $ unlim : logi TRUE
#str(RNetCDF::var.inq.nc(nc, 0)) # to 10
#List of 6
# $ id    : int 0
# $ name  : chr "time"
# $ type  : chr "NC_DOUBLE"
# $ ndims : int 1
# $ dimids: int 0
# $ natts : int 6
 #1
#ncdump: time, lat, lon
#     r: lon , lat, time; Chunking: [1440,720,1]
#2
#ncdump: lon,  lat, time
#     r: time, lat, lon; Chunking: [1,720,1440]
#3
#ncdump: lat, lon, time
#     r: time, lon, lat; Chunking: [1,1440,720]
# --> ncdf4 reverses variable dims
# --> ncdump -h header does NOT display the dim order
# get dims of var:
#dimids_nc <- sapply(nc$dim, "[[", "id")
#dimids_var <- nc$var[[varname]]$dimids # e.g. 2 3 0 = i j time
#dimids_var_nc_inds <- match(dimids_var, dimids_nc) # e.g. 3 4 1 = i j time
#dimnames_var <- names(dimids_nc)[dimids_var_nc_inds]
#names(dimids_var) <- names(dimids_var_nc_inds) <- dimnames_var
#########
#ncvars <- vector("list", l=length(clim))
#names(ncvars) <- names(clim)
#for (vi in seq_along(ncvars)) {
#    ncvars[[vi]] <- ncdf4::ncvar_def(name=names(ncvars)[vi], units="", dim=list(londim, latdim, mondim), missval=NaN)
#}
#outnc <- ncdf4::nc_create(fclim_out, vars=ncvars, force_v4=T)
#for (i in seq_along(ncvars)) {
#    ncdf4::ncvar_put(outnc, ncvars[[i]], clim[[i]])
#}
#ncdf4::nc_close(outnc)
#time <- as.POSIXlt(strsplit(trimws(system(paste0("cdo -s showdate ", nc$file), intern=T)), "  ")[[1]], tz="UTC")
#axis(4, pretty(diff(temp2), n=10), las=2, col=2, col.ticks=2, col.axis=2)
#mtext("annual temp change [C]", side=4, line=3, col=2)
# decrease space between labels and ticks:
#axis(2, at=yat, labels=F, tck=-0.03, las=2, cex.axis=0.9) # ticks only
#axis(2, at=yat, line=-0.5, lwd=0, las=2, cex.axis=0.9) # add labels
# compile bookdown:
# bookdown::render_book("index.Rmd", "bookdown::gitbook")

if (T) { # set F for blank .Rprofile

    if (interactive()) { 
        message(c(rep("*", t=(getOption("width")/2 - 6)),
                  " ~/.Rprofile ", 
                  rep("*", t=(getOption("width")/2 - 6))))
        message(R.version.string, " on ", 
                Sys.info()[4], " (PID ", Sys.getpid(), ")")
    }
    
    # R executable
    Rwrapper <- Sys.which("R")
    Rexe <- file.path(R.home(), "bin", "exec", "R")
    if (interactive()) {
        message("R wrapper = `which(\"R\")`        = ", Rwrapper, "\n",
                "R binary  = `R.home()`/bin/exec/R = ", Rexe)
    }

    # check LD_LIBRARY_PATH
    if (interactive()) {
        #Sys.setenv(LD_LIBRARY_PATH=paste0(Sys.getenv("LD_LIBRARY_PATH"), ":/sw/spack-rhel6/udunits-2.2.28-h6kiqa/lib"))
        message("LD_LIBRARY_PATH = ", appendLF=F)
        if (Sys.getenv("LD_LIBRARY_PATH") != "") {
            message("\n", paste(paste0("   ", strsplit(Sys.getenv("LD_LIBRARY_PATH"), ":")[[1]]), collapse="\n"))
        } else {
            message("<none set>")
        }
    }
    
    # which compiler was used to build this R 
    if (T) { 
        if (base::getRversion() >= "4.3.0") { # new function base::R_compiled_by() from in R >= 4.3.0
            compilers <- base::R_compiled_by()
            if (interactive()) message("`base::R_compiled_by()`:\n",
                                       paste(paste0("  ", names(compilers), ": ", compilers), collapse="\n"))
        } else { # manual way
            cmd <- "R CMD config CC"
            if (interactive()) message("`", cmd, "`: ", appendLF=F)
            Ccompiler <- system(cmd, intern=T)
            # e.g.: "gcc"
            #       "gcc -std=gnu99"
            #       "x86_64-conda_cos6-linux-gnu-cc"
            #       "gcc -m64 -std=gnu99"
            Ccompiler <- strsplit(Ccompiler, " ")[[1]][1] # first word
            if (interactive()) message("\"", Ccompiler, "\"")
            
            # C compiler version used to build this R
            # --> todo: no general solution found yet
            #cmd <- paste0("objdump -s --section .comment ", Rexe)
            #cmd <- paste0("readelf -p .comment ", Rexe) # readelf -S: show all sections
            cmd <- paste0("strings -a ", Rexe, " | grep CC:")
            #cmd <- paste0(elfinfo -l ", Rexe")
            #cmd <- paste0("ldd ", Rexe)
            if (F) { # --> all methods not reliable
                if (interactive()) message("`", cmd, "`:")
                Ccompiler_version <- system(cmd, intern=F, ignore.stdout=T) # check for error
                if (Ccompiler_version == 1) { # cmd not successfull
                    if (interactive()) message("   no success")
                } else if (Ccompiler_version == 0) { # cmd successfull
                    Ccompiler_version <- system(cmd, intern=T) # get return value
                    if (interactive()) {
                        for (i in seq_along(Ccompiler_version)) message("   \"", Ccompiler_version[i], "\"")
                    }
                    Ccompiler_version <- strsplit(Ccompiler_version[length(Ccompiler_version)], " ")[[1]]
                    Ccompiler_version <- Ccompiler_version[length(Ccompiler_version)] # last entry the version number so far
                    # e.g. "4.8.5-16)"

                    # replace special symbols by dots
                    Ccompiler_version <- gsub("[[:punct:]]", ".", Ccompiler_version)
                    
                    # only use version numbers up to 2 dots
                    Ccompiler_version_dotinds <- gregexpr("\\.", Ccompiler_version)[[1]]
                    if (all(Ccompiler_version_dotinds == -1)) { # no dots there
                        stop("case \"", Ccompiler_version, "\" not defined here")
                    } else {
                        if (length(Ccompiler_version_dotinds) > 2) {
                            Ccompiler_version <- substr(Ccompiler_version, 1, Ccompiler_version_dotinds[3] - 1)
                        }
                    }
                    if (interactive()) {
                        message("--> C compiler: \"", Ccompiler, " ", Ccompiler_version, "\"")
                    }
                } # cmd successfull or not
            } # if F
        } # R >= 4.3.0 or not
    } # which compiler was used to build this R?

    # where should new packages be installed?
    # - default: `Sys.getenv("R_LIBS_USER")` = e.g. "~/R/x86_64-pc-linux-gnu-library/3.6"
    # - package compiler needs to be the same as the compiler used to build r itself
    # - a specific compiler can be set while package installation, e.g.:
    #      withr::with_makevars(list(CXX11STD = "-std=c++11"), install.packages("s2"))
    # - within minor version changes, package re-compilation usually is not necessary, e.g. from "3.6.1" to "3.6.2"
    # - from e.g. "3.6.x" to "3.7.x", package re-compilation _may_ be necessary
    # --> no general rule found: e.g. r 3.5 packages may or may not work in r 3.6 and vice versa
    # --> minimum version-dependent package directory structure is 3.5, 3.6, etc.
    newLibPaths_main <- "~/scripts/r/packages/bin" # hardware-independent path where packages should be installed
    rversion_x.y <- paste0(version$major, ".", substr(version$minor, 1, 1)) # e.g. "3.6" and not "3.6.1"
    newLibPaths <- paste0(newLibPaths_main, "/r_", rversion_x.y) # currently running r version in format e.g. "3.6" and not "3.6.1"
    newLibPaths <- c(
                     # current r version path will be first entry of `.libPaths()` 
                     newLibPaths, 
                     # all other potentially available dirs with same major version, e.g. "3.5", "3.7", ...
                     # todo: increasing 3.5, 3.7 (default) or decreasing 3.7, 3.5?
                     list.files(newLibPaths_main, pattern=paste0("r_", version$major), full.names=T) 
                     )
    newLibPaths <- unique(newLibPaths)
    if (interactive()) message("Set .libPaths() ...")
    sapply(newLibPaths, function(x) dir.create(x, recursive=T, showWarnings=F))
    .libPaths(newLibPaths) # add my libpaths before system defaults
    if (interactive()) message(paste0("   ", .libPaths(), collapse="\n"))
    #Sys.setenv(R_LIBS_USER=paste(.libPaths(), collapse=":")) # this may be needed for package build
    
    # do rest only if interactive session
	if (T && interactive()) {

        # change R message language to english if not alread done by OS
        # LC_CTYPE: character type
		if (T && regexpr("en", Sys.getlocale("LC_MESSAGES")) == -1) {
			oldlocale <- Sys.getlocale("LC_MESSAGES")
			message("Set message language from \"", oldlocale, "\" to \"C\": `Sys.setlocale(\"LC_MESSAGES\", \"C\")`")
			Sys.setlocale("LC_MESSAGES", "C")
        }

        # add my functions to an environment so that they do not get removed on rm()
        # https://stackoverflow.com/questions/4828094/hiding-personal-functions-in-r
        if (T) {
            scripts <- paste0("~/scripts/r/functions/", 
                              c("myfunctions.r", "myccf.r", "myls.r", 
                                "package_functions.r", "mysetOutputColors.r", 
                                "myRPrompt.r"))
            for (i in seq_along(scripts)) {
                if (file.exists(scripts[i])) {
                    if (!exists("myEnv")) {
                        myEnv <- new.env()
                        message("Load default functions into environment \"myEnv\" ...")
                        message("   check with `ls(pos=which(search() == \"myEnv\"))`")
                    }   
                    cmd <- paste0("sys.source(\"", scripts[i], "\", envir=myEnv)")
                    message("   ", cmd)
                    eval(parse(text=cmd))
                }
            }
            if (exists("myEnv")) {
                base::attach(myEnv, warn.conflicts=F) # if masked functions exist
            }
        }

        # load default packages
        if (T) { 
            message("Load default packages ...")
            # `options(defaultPackages = c(getOption("defaultPackages"), "crayon"))` has not so good handling
            packages <- NULL # default: no packages
            packages <- c("ncdf4"
                          , "fields"
                          , "oce" 
                          #, "extrafont" 
                          #, "bookdown" 
                          #, "devtools"
                          #, "dtupdate" # for github_update
                          #, "data.table"
                          #, "forecast"
                          #, "ncdf.tools"
                          #, "crayon"
                          )
            if (Sys.getenv("TERM") == "xterm-256color") {
                packages <- c("colorout", packages) # try to load colorout package first
            } else {
                warning("$XTERM = \"", Sys.getenv("TERM"), "\" not defined")
            }
            npkg <- length(packages)
            nchar_no <- nchar(npkg)
            cnt <- 0
            nchar_pkg <- nchar(packages)
            failed <- NULL
            libpaths <- .libPaths()
            for (pkg in packages) {
                
                cnt <- cnt + 1
                message("   ", sprintf(paste0("%", nchar_no, "i"), cnt), "/", npkg, "  ", pkg, "  ", 
                        paste0(rep(" ", t=max(nchar_pkg) - nchar(pkg)), collapse=""), appendLF=F)
               
                # try to load package from all available .libPaths()
                for (libpathi in seq_along(libpaths)) {

                    # library() yields better error handling than require()
                    tc <- tryCatch(suppressMessages(suppressWarnings(
                                      library(pkg, lib=libpaths[libpathi], character.only=T))),
                                   error=function(e) e, warning=function(w) w)
                    
                    # check if package load was successfull
                    if (any(search() == paste0("package:", pkg))) {
                        checktext <- "ok"
                        break # for libpathi loop
                    } else {
                        checktext <- "failed"
                        failed <- c(failed, 
                                    paste0(pkg, " ", libpathi, ": ", tc$message),
                                    paste0(pkg, " ", libpathi, ": libpath = ", libpaths[libpathi])) 
                    }
                } # for libpathi
                
                # apply color to checktext if possible
                if (pkg == "colorout") {
                    if (any(search() == "package:colorout")) { # 1st try: colorout successfully loaded
                        if (exists("mysetOutputColors")) mysetOutputColors() # set my default colors; needs ~/scripts/r/functions/mysetOutputColors.r
                    } else { # 2nd try: colorout not loaded
                        if (!any(packages == "crayon")) { # try to load crayon 
                            #devtools::install_github("jalvesaq/colorout")
                            pkg <- "crayon" # overwrite colorout with crayon
                            packages[which(packages == "colorout")] <- pkg
                            nchar_pkg <- nchar(packages)
                            for (libpathi in seq_along(libpaths)) {
                                tc <- tryCatch(suppressMessages(suppressWarnings(
                                                  library(pkg, lib=libpaths[libpathi], character.only=T))),
                                               error=function(e) e, warning=function(w) w)
                                if (any(search() == "package:crayon")) break
                            } # for libpathi
                        } # if crayon not already loaded
                    } # if colorout could not be loaded
                } # if current pkg is colorout

                # apply colors to checktext if one of colorout or crayon was successfully loaded
                if (any(search() == "package:colorout")) {
                    if (checktext == "ok") {
                        if (exists("mysetOutputColors")) mysetOutputColors(stderror=40) # green
                    } else if (checktext == "failed") {
                        if (exists("mysetOutputColors")) mysetOutputColors(stderror=196) # red
                    }
                } else if (any(search() == "package:crayon")) {
                    if (checktext == "ok") {
                        fancy <- crayon::combine_styles(crayon::make_style("ivory"),
                                                        crayon::make_style("green", bg=T))
                    } else if (checktext == "failed") {
                        fancy <- crayon::combine_styles(crayon::make_style("ivory"),
                                                        crayon::make_style("red", bg=T))
                    }
                    checktext <- fancy(checktext)
                } # if colorout or crayon package is loaded

                message(checktext, appendLF=F) # show ok/failed of loading current pkg
                
                # switch off colors again
                if (any(search() == "package:colorout")) {
                    if (exists("mysetOutputColors")) mysetOutputColors()
                } else if (any(search() == "package:crayon")) {
                    checktext <- crayon::strip_style(checktext)
                }
                
                # add info if package load was successfull
                if (checktext == "ok") {
                    pkginfo <- suppressMessages(suppressWarnings(utils::packageDescription(pkg)))
                    # cran or git repo
                    if (any(names(pkginfo) == "RemoteType")) {
                        message("  ", pkginfo$RemoteType, appendLF=F) # = "github"
                    } else if (any(names(pkginfo) == "Repository")) {
                        message("  ", pkginfo$Repository, appendLF=F) # = "CRAN"
                        message("  ", appendLF=F) # add 2 more spaces
                    } else { # built from source
                        message("  source", appendLF=F) # does this cover all other cases?
                    }
                    # add version
                    message("  ", sprintf("%5s", pkginfo$Version), appendLF=F)
                    # add path from where package was loaded
                    tmp <- base::find.package(pkg)
                    if (substr(tmp, 1, nchar(normalizePath("~"))) == normalizePath("~")) {
                        tmp <- paste0("~", substr(tmp, nchar(normalizePath("~")) + 1, nchar(tmp)))
                    }
                    message("  ", tmp, appendLF=F)
                    # additional text
                    if (F && pkg == "oce") {
                        checktext <- paste0("data('coastlineWorld', package='", i, "')")
                        message("   ", checktext, appendLF=F)
                        eval(parse(text=checktext))
                    }
                } # if package load was successfull
                message("") # linebreak
            } # for pkg packages
        } # if load default packages

        # set some global options after packages loaded since some functions may need package functions
        if (T) {
            message("Set options ...")
            r <- getOption("repos")
            r["CRAN"] <- "https://cloud.r-project.org" # https and always-near-me mirror 
            message("   options(repos=c(CRAN=\"", r, "\"))")
            options(repos=r)
            message("   options(continue=\"   \")")
            options(continue="   ")
            message("   options(show.error.locations=T)")
            options(show.error.locations=T)
            #message("   options(stringsAsFactors=F)")
            #options(stringsAsFactors=F)
            message("   options(menu.graphics=F)")
            options(menu.graphics=F)
            message("   options(browser=\"firefox\")")
            options(browser="firefox")
            if (exists("~/.plotly")) {
                message("   Sys.setenv(\"plotly_username\"=\"...\")")
                message("   Sys.setenv(\"plotly_api_key\"=\"...\")")
                message("   Sys.setenv(\"MAPBOX_TOKEN\"=\"...\"")
                source("~/.plotly")
            }
            if (length(which(search() == "myEnv")) > 0) {
                if (any(ls(pos=which(search() == "myEnv")) == "myRPrompt")) {
                    message("   options(prompt=myRPrompt())   (setwd() is also changed)")
                    options(prompt=myRPrompt())
                } # if myRPrompt() is loaded
                
                if (any(ls(pos=which(search() == "myEnv")) == "myErrorFun")) { 
                    if (exists(".traceback")) { # todo: why should `base::.traceback` not exist? 
                        message("   options(error=myErrorFun)   (check with 'getOption(\"error\")')")
                        options(error=myErrorFun)
                    }
                } # if myErrorFun() is loaded
            } # if environment myEnv was loaded
        } # if set default options

        ## do stuff after loading packages/functions
        # paste commands that I always forget
        #if (any(ls(pos=which(search() == "myEnv")) == "myhelp")) myhelp()
       
        # set default plot fonts
        if (F && any(search() == "package:extrafont")) {
            #family <- "Droid Sans Mono" # "CM Roman"
            family <- "CM Roman"
            if (any(regexpr(family, extrafont::fonts()) != -1)) {
                cmd <- paste0("   pdf.options(family=", family, ")")
                message(cmd, " # and grDevcices::embedFonts() or extrafont::embed_fonts()")
                grDevices::pdf.options(family=family)
            } else {
                message("   family '", family, "' is not installed: extrafont::font_install(\"fontcm\")")
            }
        }
    
        if (length(which(search() == "myEnv")) > 0) {
            if (any(ls(pos=which(search() == "myEnv")) == "mynews")) {
                message("Run mynews() to see R ", version$major, ".", version$minor, " news ...")
            } # if mynews() is loaded
        } # if environment myEnv was loaded

        # show error message if package load failed
        if (T) {
            if (!is.null(failed)) {
                message("Messages of failed packages:")
                for (i in seq_along(failed)) message("   ", failed[i])
            } # if any packages failed
        } # if load default packages

        message(c(rep("*", t=(getOption("width")/2 - 6)),
                  " ~/.Rprofile ",
                  rep("*", t=(getOption("width")/2 - 6))))

    } # if interactive

    # modify a function from a package as soon as the package is loaded
    # somehow not needed anymore
	if (F) {
        setHook(hookName=packageEvent(pkgname="devtools", event="attach"),
                value=function(...) {
                          # save the function to be overwritten
                          original_install_github <- devtools::install_github
                          # define new function
                          my_install_github <- function(...) {
                              # first detach colorout if loaded
                              if (any(search() == "package:colorout")) {
                                  message(".Rprofile: Entering setHook() ... if you dont want that, use devtools::install_github(...) instead")
                                  message("setHook: colorout::noColorOut() ... ")
                                  noColorOut()
                                  message("setHook: devtools::install_github ...")
                              }
                              # run the original function
                              original_install_github(...)
                              # reload colorout package
                              if (any(search() == "package:colorout")) {
                                  message("setHook: my_setOutputColors() ... ")
                                  if (exists("mysetOutputColors")) mysetOutputColors()
                              }
                          }
                          # overwrite the original function of the package
                          unlockBinding("install_github", as.environment("package:devtools"))
                          assign("install_github", my_install_github, "package:devtools")
                      } # hook function
                ) # setHook
    } # F

    # clear work space
    rm(list=ls())

} # if T

