## R

# https://csgillespie.github.io/efficientR/3-3-r-startup.html#r-startup
# image white lines: http://cran.stat.auckland.ac.nz/doc/FAQ/R-FAQ.html#Why-are-there-unwanted-borders
## usefull commands
#options(warn=2) # stop on warnings
#options(warn=0) # back to default
# options(warn=-1) # turn off
# options(error = recover)
# options(error = NULL)
# save output to file: sink("file.txt"); <do stuff>; sink()
# file.edit()
# options(prompt="R> ", digits=4, show.signif.stars=FALSE)
# getSrcDirectory
# anyNA(x) is more efficient than any(is.na(x))
# which.min is more efficient than which(x == min(x)). ATTENTION: which.min(c(1,1,2)) = 1, i.e. the second (and 3rd,4th,...) minimum is negelected
#which(abs(zlevels - center_around) == min(abs(zlevels - center_around)))
# cols_rgb_p <- rgb(t(col2rgb(cols_p)/255), alpha=alpha_rgb)
# mean1 <- function(x) mean(x)
# mean2 <- function(x) sum(x) / length(x)
# --> mean2 is faster
# for 2D, rowSums(), colSums(), rowMeans(), and colMeans() are faster than apply()
# vapply() is faster than sapply()
# any(x == 10) is faster than 10 %in% x
# unlist(x, use.names=F) is faster than unlist(x)
# check stats:::t.test.default()
# library(pryr) --> object_size(), mem_used(), mem_change() 
#file_sizes_byte <- file.size(files)
#file_sizes_pretty <- sapply(file_sizes_byte, utils:::format.object_size, "auto")
# strsplit(x, "\\s+")[[1]] arbitrary number of spaces blanks
# getAnywhere(objectname)
# system.file("extdata", "2012.csv", package = "testdat")
# options(menu.graphics=FALSE) #graphics dialogs always seem to crash R
# sort(x, index.return=T)$ix
# pdf.options(useDingbats = TRUE) https://yihui.name/knitr/demo/graphics/
# options("scipen"=100, "digits"=4): c(1.810032e+09, 4) --> 1810032000, 4
# list.files(pattern = glob2rx('*.tif'))
# list.files(pattern = '^.*\\.tif$')
# sprintf("%02i", 1) --> 01
# rJava pkg error: JDK is incomplete! Please make sure you have a complete JDK. JRE is *not* sufficient: sudo R CMD javareconf
# cat(capture.output(str(dates_in_list)), sep="\n")
# return(as.list(environment())) # return everything defined in function
#‘isTRUE(x)’ is the same as ‘{ is.logical(x) && length(x) == 1 &&
#     !is.na(x) && x }’; ‘isFALSE()’ is defined analogously.
#     Consequently, ‘if(isTRUE(cond))’ may be preferable to ‘if(cond)’
#     because of ‘NA’s.
#me <- paste0(normalizePath(getSrcDirectory(read_gnip)), "/", getSrcFilename(read_gnip))
#remotes::install_github("nickmckay/LiPD-Utilities", subdir="R")
#pid=system("sleep 100 & echo $!", intern=T) leads to `sh <defunct>` zombie
#
if (T) { # set F for blank .Rprofile

    # hostname, e.g. mlogin101, stan1.awi.de, K
    host <- Sys.info()[4]

    if (interactive()) { 
        message(c(rep("*", t=(getOption("width")/2 - 6)),
                  " ~/.Rprofile ", 
                  rep("*", t=(getOption("width")/2 - 6))))
        message("This ", R.version.string, " runs on ", 
                Sys.info()[4], " with PID ", Sys.getpid())
    }
    
    # R executable
    Rwrapper <- Sys.which("R")
    Rexe <- file.path(R.home(), "bin", "exec", "R")
    if (interactive()) {
        message("R wrapper: ", Rwrapper)
        message("R executable: ", Rexe)
        message("numeric tolerance `sqrt(.Machine$double.eps)` = ", 
                sqrt(.Machine$double.eps))
    }

    # C compiler used to build this R 
    if (T) { # building a package needs same C compiler that was used for building r itself
        if (grepl("stan", host)) {
            Ccompiler_version <- "4.4.7"
        } else if (grepl("paleosrv", host)) {
            Ccompiler_version <- "5.4.0"
        } else {
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
            #cmd <- paste0("objdump -s --section .comment ", Rexe)
            #cmd <- paste0("readelf -p .comment ", Rexe) # readelf -S: show all sections
            cmd <- paste0("strings -a ", Rexe, " | grep CC:")
            if (interactive()) message("`", cmd, "`:")
            Ccompiler_version <- system(cmd, intern=T)
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
                message("--> C compiler version: ", Ccompiler_version)
            }
        } # find C compiler used to build this R
    } # which C compiler
    rm(Rwrapper, Rexe, host) 

    # where should new packages be installed?
    # default: `Sys.getenv("R_LIBS_USER")` = e.g. "~/R/x86_64-pc-linux-gnu-library/3.6"
    # --> within minor version changes, recompilation is not necessary, e.g. from 3.6.1 to 3.6.2  
    rversion_x.y <- paste0(version$major, ".", substr(version$minor, 1, 1)) # e.g. 3.6
    newLibPaths <- list.files("~/scripts/r/packages/bin", pattern=paste0("r_", version$major), full.names=T)
    if (length(newLibPaths) == 0) {
        newLibPaths <- paste0("~/scripts/r/packages/bin/r_", rversion_x.y)
    }
    if (interactive()) message("Set .libPaths() ...")
    sapply(newLibPaths, function(x) dir.create(x, recursive=T, showWarnings=F))
    .libPaths(newLibPaths)
    if (interactive()) message(paste0("   ", .libPaths(), collapse="\n"))
    
    #Sys.setenv(R_LIBS_USER=paste0(.libPaths(), collapse=":")) # this may be needed for package build
    rm(newLibPaths, rversion_x.y)
       
	if (interactive()) {

        # change R message language to english if not alread done by OS
        # LC_CTYPE: character type
		if (regexpr("en", Sys.getlocale("LC_MESSAGES")) == -1) {
			oldlocale <- Sys.getlocale("LC_MESSAGES")
			Sys.setlocale("LC_MESSAGES", "C")
			newlocale <- Sys.getlocale("LC_MESSAGES")
			message("   Set message language from \"", oldlocale, "\" to \"", newlocale, "\" ...")
			message("      Sys.setlocale(\"LC_MESSAGES\", \"C\")")
            rm(oldlocale, newlocale)
        }

        # add my functions to an environment so that they do not get removed on rm()
        # https://stackoverflow.com/questions/4828094/hiding-personal-functions-in-r
        scripts <- paste0("~/scripts/r/functions/", 
                          c("myfunctions.r", "myccf.r", "myls.r", 
                            "update.check.r", "mysetOutputColors.r", 
                            "myRPrompt.r", "label_function.r",
                            "leap_function.r"))
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
            attach(myEnv, warn.conflicts=F) # if masked functions exist
            rm(cmd,myEnv)
        }
        rm(scripts, i)

        # load default packages
		message("Load default packages ...")
		# actually: 
		# options(defaultPackages = c(getOption("defaultPackages"), "crayon"))
		# but this has not such a nice handling
        
        #packages <- NULL
        packages <- c("ncdf4", "fields", "oce" 
                      #, "extrafont" 
                      #, "bookdown" 
                      #, "devtools"
                      #, "dtupdate" # for github_update
                      )
        # "data.table", "forecast", "ncdf.tools", "crayon"
        if (Sys.getenv("TERM") == "xterm-256color") {
            packages <- c("colorout", packages) # try to load colorout package first
        }

        npkg <- length(packages)
        nchar_no <- nchar(npkg)
        cnt <- 0
        nchar_pkg <- nchar(packages)
		failed <- NULL
		for (pkg in packages) {
            
            cnt <- cnt + 1
            message("   ", sprintf(paste0("%", nchar_no, "i"), cnt), "/", npkg, "  ", pkg, "  ", 
                    paste0(rep(" ", t=max(nchar_pkg) - nchar(pkg)), collapse=""), appendLF=F)
           
            # try to load package from all available .libPaths()
            for (libpathi in seq_along(.libPaths())) {

                # library() yields better error handling than require()
                tc <- tryCatch(suppressMessages(suppressWarnings(
                                  library(pkg, lib=.libPaths()[libpathi], character.only=T))),
                               error=function(e) e, warning=function(w) w)
                
                # check if package load was successfull
                if (any(search() == paste0("package:", pkg))) {
                    checktext <- "ok"
                    break # for libpathi loop
                } else {
                    checktext <- "failed"
                    failed <- c(failed, 
                                paste0(pkg, ": ", tc$message),
                                paste0(pkg, ": libpath = ", .libPaths()[libpathi])) 
                }
            } # for libpathi
            
            # apply color to checktext if possible
            if (pkg == "colorout") {
                if (any(search() == "package:colorout")) { # 1st try: colorout successfully loaded
                    mysetOutputColors() # set my default colors; needs ~/scripts/r/functions/mysetOutputColors.r
                } else { # 2nd try: colorout not loaded
                    if (!any(packages == "crayon")) { # try to load crayon 
                        #devtools::install_github("jalvesaq/colorout")
                        pkg <- "crayon" # overwrite colorout with crayon
                        packages[which(packages == "colorout")] <- pkg
                        nchar_pkg <- nchar(packages)
                        for (libpathi in seq_along(.libPaths())) {
                            tc <- tryCatch(suppressMessages(suppressWarnings(
                                              library(pkg, lib=.libPaths()[libpathi], character.only=T))),
                                           error=function(e) e, warning=function(w) w)
                            if (any(search() == "package:crayon")) break
                        } # for libpathi
                    } # if crayon not already loaded
                } # if colorout could not be loaded
            } # if current pkg is colorout

            # apply colors to checktext if one of colorout or crayon was successfully loaded
            if (any(search() == "package:colorout")) {
                if (checktext == "ok") {
                    mysetOutputColors(stderror=40) # green
                } else if (checktext == "failed") {
                    mysetOutputColors(stderror=196) # red
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
                mysetOutputColors()
            } else if (any(search() == "package:crayon")) {
				checktext <- crayon::strip_style(checktext)
			}
            
            # show if cran or git package
            repo <- suppressMessages(suppressWarnings(utils::packageDescription(pkg)))
            if (any(names(repo) == "RemoteType")) {
                message("  ", repo$RemoteType, appendLF=F) # = "github"
            } else if (any(names(repo) == "Repository")) {
                message("  ", repo$Repository, appendLF=F) # = "CRAN"
                message("  ", appendLF=F) # add 2 more spaces
            }
			# show library path
			if (checktext == "ok") {
                tmp <- base::find.package(pkg)
                if (substr(tmp, 1, nchar(normalizePath("~"))) == normalizePath("~")) {
                    tmp <- paste0("~", substr(tmp, nchar(normalizePath("~")) + 1, nchar(tmp)))
                }
				message("  ", tmp, appendLF=F)
                rm(tmp)
			}
			# show additional text
			if (F && checktext == "ok" && pkg == "oce") {
				checktext <- paste0("data('coastlineWorld', package='", i, "')")
				message("   ", checktext, appendLF=F)
				eval(parse(text=checktext))
			}
            # linebreak
			message("")
		} # for pkg packages
		rm(cnt, libpathi, npkg, packages, tc, pkg, nchar_no, nchar_pkg, checktext, repo)
		if (exists("fancy")) rm(fancy)

        # set some global options after packages loaded since some functions may need package functions
        message("Set options ...")
        r <- getOption("repos")
        r["CRAN"] <- "https://cloud.r-project.org" # https and always-near-me mirror 
        message("   options(repos=c(CRAN=\"", r, "\"))")
        options(repos=r)
        rm(r)
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
                if (exists(".traceback")) { 
                    message("   options(error=myErrorFun)   (check with 'getOption(\"error\")')")
                    options(error=myErrorFun)
                }
            } # if myErrorFun() is loaded
        } # if environment myEnv was loaded

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
                rm(cmd)
            } else {
                message("   family '", family, "' is not installed: extrafont::font_install(\"fontcm\")")
            }
            rm(family)
        }
    
        if (length(which(search() == "myEnv")) > 0) {
            if (any(ls(pos=which(search() == "myEnv")) == "mynews")) {
                message("Run mynews() to see R ", version$major, ".", version$minor, " news ...")
            } # if mynews() is loaded
        } # if environment myEnv was loaded

        # show error message if package load failed
        if (!is.null(failed)) {
            message("Messages of failed packages:")
            for (i in seq_along(failed)) message("   ", failed[i])
            rm(i)
        } # if any packages failed
        rm(failed)

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
                                  mysetOutputColors()
                              }
                          }
                          # overwrite the original function of the package
                          unlockBinding("install_github", as.environment("package:devtools"))
                          assign("install_github", my_install_github, "package:devtools")
                      } # hook function
                ) # setHook
    } # F

} # if T

