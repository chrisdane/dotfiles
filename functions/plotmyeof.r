#!/usr/bin/env Rscript

plotmyeof <- function(...) {
    
    # close possibly plot devices; load helper functions
    graphics.off()
    library(ncdf4)
    library(fields)
    source("~/scripts/r/functions/myfunctions.r") # myDefaultPlotOptions()
    source("~/scripts/r/functions/image.plot.pre.r")
    source("~/scripts/r/functions/image.plot.nxm.r")
    
    # defaults
    neof <- 2
    loess_span <- 0.33
    neigval_plot <- 15
    plot_type <- "png"
   
    # get user input
    dots <- list(...)
    #print(dots)
    if (interactive()) { # this runs when this file is `source()`ed
        if (any(names(dots) == "settings")) {
            settings <- dots$settings
        } else {
            stop("must provide `settings` if `source()`ed\n", usage)
        }
        if (any(names(dots) == "neof")) neof <- dots$neof
        if (any(names(dots) == "loess_span")) loess_span <- dots$loess_span
        if (any(names(dots) == "neigval_plot")) neigval_plot <- dots$neigval_plot
        if (any(names(dots) == "plot_type")) plot_type <- dots$plot_type
    } else { # this runs when this file is `./`ed 
        dots <- dots[[1]]
        # capture special args
        if (any(grep("--neof=", dots))) {
            neof <- as.integer(sub("--neof=", "", dots[grep("--neof=", dots)]))
            dots <- dots[-grep("--neof=", dots)]
        }
        if (any(grep("--loess_span=", dots))) {
            loess_span <- as.numeric(sub("--loess_span=", "", dots[grep("--loess_span=", dots)]))
            dots <- dots[-grep("--loess_span=", dots)]
        }
        if (any(grep("--neigval_plot=", dots))) {
            neigval_plot <- as.integer(sub("--neigval_plot=", "", dots[grep("--neigval_plot=", dots)]))
            dots <- dots[-grep("--neigval_plot=", dots)]
        }
        if (any(grep("--plot_type=", dots))) {
            plot_type <- sub("--plot_type=", "", dots[grep("--plot_type=", dots)])
            dots <- dots[-grep("--plot_type=", dots)]
        }
        if (any(grep("--", dots))) {
            inds <- which(grepl("--", dots))
            message("provided argument", ifelse(length(inds) > 1, "s", ""), " \"", 
                    paste(dots[inds], collapse="\", \""), "\" not defined. continue without ...")
            dots <- dots[-inds]
        }
        nargs <- length(dots)
        settings <- vector("list", l=nargs)
        # construct same `settings` as if `source()`ed
        for (i in seq_len(nargs)) {
            #message("arg ", i, "/", nargs, ": ")
            #print(str(dots[[i]]))
            if (!grepl("=", dots[[i]])) stop("argument ", i, " has no \"=\".\n", usage)
            arg <- strsplit(dots[[i]], "=")[[1]]
            if (arg[1] == "") {
                stop("no <plotname> before \"=\" provided in arg ",
                     i, "/", nargs, ": \"", dots[[i]], "\"\n", usage)
            }
            files <-strsplit(arg[2:length(arg)], ",")[[1]]
            if (length(files) == 1) { # result of `myeof.r`
                if (is.na(files)) {
                    stop("no <filename> after \"=\" provided in arg ",
                         i, "/", nargs, ": \"", dots[[i]], "\"\n", usage)
                }
                names(files) <- "eof"
            } else if (length(files) == 3) { # result of `mycdeof.r`
                names(files) <- c("eigval", "eigvec", "pc")
            } else {
                stop("arg ", i, "/", nargs, ": \"", dots[[i]], "\" provided ", length(files), 
                     "files. must be 1 (result of `myeof.r`) or 3 (result of `mycdoeof.r`).\n", usage)
            }
            settings[[i]]$files <- files
            names(settings)[i] <- arg[1]
        }
        #print(str(settings))
    } # interactive or not

    message("loaded ", length(settings), " setting", 
            ifelse(length(settings) > 1, "s", ""), ":")
    cat(capture.output(str(settings)), sep="\n")

    # check existance of provided files
    for (i in seq_along(settings)) {
        for (fi in seq_along(settings[[i]])) { 
            if (!file.exists(settings[[i]]$files[fi])) {
                stop("file ", fi, "/", length(settings[[i]]$files), ": \"", 
                     settings[[i]]$files[fi], "\" of setting ", i, "/", 
                     length(settings), ": \"", names(settings)[i], 
                     "\" does not exist")
            }
        }
    }
   
    message("run ", me, " with neof = ", neof, ", loess_span = ", loess_span, 
            ", neigval_plot = ", neigval_plot, ", plot_type = \"", plot_type, "\" ...")

    # declare
    p <- myDefaultPlotOptions(plot_type=plot_type)
    nsettings <- length(settings)
    datas <- vector("list", l=nsettings)
    names(datas) <- names(settings)
    eigvals <- eigvecs <- pcs <- datas

    # load data
    for (i in seq_along(settings)) {

        files <- settings[[i]]$files
        message("\nload setting ", i, "/", nsettings, ": ", names(settings)[i])
        
        for (fi in seq_along(files)) {
            
            message("open file ", fi, "/", length(files), ": ", files[[fi]], " ...") 
            nc <- ncdf4::nc_open(files[[fi]])
            
            message("name of files[", fi, "] = ", names(files)[fi], " --> ", appendLF=F) 
            
            if (names(files)[fi] == "eof") { # r result
                message("load complete eof result ...")
                eigvals[[i]] <- list(eigenval_abs=ncvar_get(nc, "eigenval_abs"),
                                     eigenval_pcnt=ncvar_get(nc, "eigenval_pcnt"))
                eigvecs[[i]] <- list(eigenvec=ncvar_get(nc, "eigenvec"),
                                     eigenvec_normalized=ncvar_get(nc, "eigenvec_normalized"))
                eigvecs[[i]]$eigenvec_normalized_units <- nc$var[["eigenvec_normalized"]]$units
                eigvecs[[i]]$lon <- nc$dim$lon$vals
                eigvecs[[i]]$lat <- nc$dim$lat$vals
                tmp <- vector("list", l=neof)
                for (eofi in seq_len(neof)) {
                    tmp[[eofi]] <- list(ncvar_get(nc, paste0("pc", eofi)),
                                        ncvar_get(nc, paste0("pc", eofi, "_normalized")))
                    names(tmp[[eofi]]) <- c(paste0("pc", eofi), paste0("pc", eofi, "_normalized"))
                }
                pcs[[i]] <- tmp
                pcs[[i]]$time <- nc$dim$time$vals
                pcs[[i]]$time_units <- nc$dim$time$units

            } else if (names(files)[fi] == "eigval") { # cdo result
                message("load cdo eigenvals ...")
                eigvals[[i]] <- list(eigenval_abs=ncvar_get(nc, "eigenval_abs"),
                                     eigenval_pcnt=ncvar_get(nc, "eigenval_pcnt"))
            
            } else if (names(files)[fi] == "eigvec") { # cdo result
                message("load cdo eigenvecs ...")
                eigvecs[[i]] <- list(eigenvec=ncvar_get(nc, "eigenvec"),
                                     eigenvec_normalized=ncvar_get(nc, "normalized_eigenvec"))
                eigvecs[[i]]$eigenvec_normalized_units <- nc$var[["normalized_eigenvec"]]$units
                eigvecs[[i]]$lon <- nc$dim$lon$vals
                eigvecs[[i]]$lat <- nc$dim$lat$vals
            
            } else if (names(files)[fi] == "pc") { # cdo result
                message("load cdo principal components ...")
                tmp <- vector("list", l=neof)
                for (eofi in seq_len(neof)) {
                    tmp[[eofi]] <- list(ncvar_get(nc, paste0("pc", eofi)),
                                        ncvar_get(nc, paste0("pc", eofi, "_normalized")))
                    names(tmp[[eofi]]) <- c(paste0("pc", eofi), paste0("pc", eofi, "_normalized"))
                }
                pcs[[i]] <- tmp
                pcs[[i]]$time <- nc$dim$time$vals
                pcs[[i]]$time_units <- nc$dim$time$units
            
            } else { # anom data used for eof analysis
                message("load input data that was used for eof analysis ...")
                datas[[i]] <- list(data=ncvar_get(nc, names(files)[fi]),
                                   units=nc$var[[names(files)[fi]]]$units,
                                   lon=nc$dim$lon$vals,
                                   lat=nc$dim$lat$vals,
                                   time=nc$dim$time$vals,
                                   time_units=nc$dim$time$units)
            }
           

        } # for fi in seq_along(files)
        
    } # for i in seq_along(settings)

    # check loaded data
    for (i in seq_along(settings)) {

        message("\ncheck loaded ", names(settings)[i], " data ...")

        if (!is.null(datas[[i]])) {

            # check time of data
            if (!is.null(datas[[i]]$time)) {
                if (grepl(" since ", datas[[i]]$time_units)) {
                    time <- udunits2::ud.convert(datas[[i]]$time, data[[i]]$time_units, 
                                                 "seconds since 1970-1-1 00:00:00")
                    time <- as.POSIXct(time, tz="UTC", origin="1970-1-1 00:00:00")
                } else {
                    stop("not defined yet")
                }
                datas[[i]]$time <- as.POSIXlt(time)
                datas[[i]]$time_units <- NULL
                if (T) {
                    message("make my BP datas time ...")
                    time <- make_posixlt_origin((rev(datas[[i]]$time$year)+1900)*-1)
                    data[[i]]$time <- time
                }
            }

            # flip lats of datas if necessary
            if (any(diff(datas[[i]]$lat) < 0)) {
                message("flip datas latitudes ...")
                datas[[i]]$lat <- rev(eigvecs[[i]]$lat)
                datas[[i]]$data <- datas[[i]]$data[,rev(seq_along(data[[i]]$lat)),]
            }

        } # if data was loaded
            
        # flip lats of eigvecs if necessary
        if (!is.null(eigvecs[[i]])) { 
            if (any(diff(eigvecs[[i]]$lat) < 0)) {
                message("flip eigvecs latitudes ...")
                eigvecs[[i]]$lat <- rev(eigvecs[[i]]$lat)
                for (eofi in seq_len(neof)) {
                    eigvecs[[i]]$eigenvec[,,eofi] <- eigvecs[[i]]$eigenvec[,rev(seq_along(eigvecs[[i]]$lat)),eofi]
                    eigvecs[[i]]$eigenvec_normalized[,,eofi] <- eigvecs[[i]]$eigenvec_normalized[,rev(seq_along(eigvecs[[i]]$lat)),eofi]
                }
            }
        }
           
        # reorder PCs from single time series into array
        if (!is.null(pcs[[i]])) { 
            tmp <- tmp2 <- array(NA, dim=c(length(pcs[[i]][[1]][[1]]), neof))
            for (eofi in seq_len(neof)) {
                tmp[,eofi] <- pcs[[i]][[eofi]][[paste0("pc", eofi)]]
                tmp2[,eofi] <- pcs[[i]][[eofi]][[paste0("pc", eofi, "_normalized")]]
            }
            pcs[[i]] <- list(pc=tmp, pc_normalized=tmp2,
                             time=pcs[[i]]$time,
                             time_units=pcs[[i]]$time_units)
            if (grepl(" since ", pcs[[i]]$time_units)) {
                time <- udunits2::ud.convert(pcs[[i]]$time, pcs[[i]]$time_units, 
                                             "seconds since 1970-1-1 00:00:00")
                time <- as.POSIXct(time, tz="UTC", origin="1970-1-1 00:00:00")
            } else {
                stop("not defined yet")
            }
            pcs[[i]]$time <- as.POSIXlt(time)
            pcs[[i]]$time_units <- NULL
            if (T) {
                message("make my BP pcs time ...")
                time <- make_posixlt_origin((rev(pcs[[i]]$time$year)+1900)*-1)
                pcs[[i]]$time <- time            
            }
        }
            
    } # for i in settings

    # check normalized pc = pc_eofi / sqrt(eigenval_eofi)
    # --> var(normalized pc) should equal 1
    message("\ncheck normalized PCs ...")
    for (i in seq_along(pcs)) {
        if (!is.null(pcs[[i]]$pc_normalized)) {
            for (j in seq_len(dim(pcs[[i]]$pc_normalized)[2])) {
                var_of_pc_normalized <- var(pcs[[i]]$pc_normalized[,j])
                message(names(settings)[i], " var(pc_normalized ", j, ") = ", var_of_pc_normalized)
                if (round(var_of_pc_normalized) != 1) {
                    message("--> ", var_of_pc_normalized, " ~ ", round(var_of_pc_normalized), 
                            " not equal 1 but it should!")
                }
            }
        }
    }


    # apply mult fac
    for (i in seq_along(pcs)) {
        if (F && names(pcs)[i] == "cdo") {
            message("\nmult cdo*-1 ...")
            pcs[[i]]$pc <- -1*pcs[[i]]$pc 
            pcs[[i]]$pc_normalized <- -1*pcs[[i]]$pc_normalized
            eigvecs[[i]]$eigenvec <- -1*eigvecs[[i]]$eigenvec
            eigvecs[[i]]$eigenvec_normalized <- -1*eigvecs[[i]]$eigenvec_normalized
            names(pcs)[i] <- "cdo*-1"
            names(eigvecs)[i] <- "cdo*-1"
        }
    }

    ## calc stuff

    # smooth global fldmean of anom data
    message("\ncalc and smooth fldmean of datas ...")
    for (i in seq_along(settings)) {
        if (!is.null(datas[[i]])) {
            library(SDMTools)
            area_m2 <- SDMTools::grid.info(lats=data$lat, cellsize=diff(data$lon)[1])$area
            area_m2 <- replicate(area_m2, n=length(data$lon))
            area_m2 <- t(area_m2)
            area_m2_global_sum <- sum(area_m2)
            area_m2 <- replicate(area_m2, n=dim(datas[[i]]$data)[3])
            data_fldmean <- datas[[i]]$data * area_m2
            data_fldmean <- apply(data_fldmean, 3, sum)
            data_fldmean <- data_fldmean/area_m2_global_sum
            datas[[i]]$fldmean <- data_fldmean; rm(data_fldmean)

            tmp <- predict(loess(datas[[i]]$fldmean ~ seq_along(datas[[i]]$time), span=loess_span), se=T)
            datas[[i]]$fldmean_loess <- unname(tmp$fit)
            datas[[i]]$fldmean_loess_se <- unname(tmp$se.fit)
        }
    }

    # calc eigenval uncertainty after north et al. 1982 rulem
    message("\ncalc eigenval uncertainty after north et al. 1982 ...")
    for (i in seq_along(settings)) {
        if (!is.null(eigvals[[i]]$eigenval_pcnt)) {
            eigvals[[i]]$eigenval_pcnt_error <- north_etal_1982_rule(eigenval=eigvals[[i]]$eigenval_pcnt)
        }
    }

    # calc normalized pcs weighted by global mean of eigenvecs
    message("\ncalc weighted PCs = normalized PCs * fldmean of eigenvecs and sum ", 
            neof, " of these weighted normalized PCs up and smooth all time series ...")
    for (i in seq_along(settings)) {
        if (!is.null(eigvecs[[i]]) && !is.null(pcs[[i]])) {
            area_m2 <- SDMTools::grid.info(lats=eigvecs[[i]]$lat, cellsize=diff(eigvecs[[i]]$lon)[1])$area # nlat
            area_m2 <- replicate(area_m2, n=length(eigvecs[[i]]$lon)) # nlat,nlon
            area_m2 <- t(area_m2) # nlon,nlat
            eigenvec_fldmean <- eigenvec_normalized_fldmean <- rep(NA, t=neof)
            pc_loess <- pc_loess_se <- 
                pc_normalized_loess <- pc_normalized_loess_se <- 
                pc_weighted <- pc_weighted_loess <- pc_weighted_loess_se <- 
                pc_weighted_normalized <- pc_weighted_normalized_loess <- pc_weighted_normalized_loess_se <- 
                    array(NA, dim=dim(pcs[[i]]$pc_normalized))
            
            for (eofi in seq_len(neof)) {
                
                # calc fldmean of eigenvec
                eigvec_fldmean_eofi <- eigvecs[[i]]$eigenvec[,,eofi]*area_m2
                eigvec_fldmean_eofi <- sum(eigvec_fldmean_eofi)/sum(area_m2)
                eigenvec_fldmean[eofi] <- eigvec_fldmean_eofi
                message("fldmean of ", names(eigvecs)[i], " eigenvec ", eofi, 
                        " = ", eigvec_fldmean_eofi)
                
                # calc fldmean of normalized eigenvec
                normalized_eigvec_fldmean_eofi <- eigvecs[[i]]$eigenvec_normalized[,,eofi]*area_m2
                normalized_eigvec_fldmean_eofi <- sum(normalized_eigvec_fldmean_eofi)/sum(area_m2)
                eigenvec_normalized_fldmean[eofi] <- normalized_eigvec_fldmean_eofi
                message("fldmean of ", names(eigvecs)[i], " normalized eigenvec ", eofi, 
                        " = ", normalized_eigvec_fldmean_eofi, " ", eigvecs[[i]]$eigenvec_normalized_units)
                
                # calc weighted pc
                pc_weighted[,eofi] <- eigvec_fldmean_eofi * pcs[[i]]$pc[,eofi]
                
                # calc weighted normalized pc (has same units as original data used for eof analysis)
                pc_weighted_normalized[,eofi] <- normalized_eigvec_fldmean_eofi * pcs[[i]]$pc_normalized[,eofi]
                
                message("smooth pc ", eofi, " time series ...")

                # smooth pc
                tmp <- predict(loess(pcs[[i]]$pc[,eofi] ~ seq_along(pcs[[i]]$time), span=loess_span), se=T)
                pc_loess[,eofi] <- unname(tmp$fit)
                pc_loess_se[,eofi] <- unname(tmp$se.fit)
                
                # smooth normalized pc
                tmp <- predict(loess(pcs[[i]]$pc_normalized[,eofi] ~ seq_along(pcs[[i]]$time), span=loess_span), se=T)
                pc_normalized_loess[,eofi] <- unname(tmp$fit)
                pc_normalized_loess_se[,eofi] <- unname(tmp$se.fit)
                
                # smooth weighted pc
                tmp <- predict(loess(pc_weighted[,eofi] ~ seq_along(pcs[[i]]$time), span=loess_span), se=T)
                pc_weighted_loess[,eofi] <- unname(tmp$fit)
                pc_weighted_loess_se[,eofi] <- unname(tmp$se.fit)
                
                # smooth weighted normalized pc
                tmp <- predict(loess(pc_weighted_normalized[,eofi] ~ seq_along(pcs[[i]]$time), span=loess_span), se=T)
                pc_weighted_normalized_loess[,eofi] <- unname(tmp$fit)
                pc_weighted_normalized_loess_se[,eofi] <- unname(tmp$se.fit)
            
            } # for eofi
            eigvecs[[i]]$eigenvec_normalized_fldmean <- eigenvec_normalized_fldmean
            pcs[[i]]$pc_loess <- pc_loess
            pcs[[i]]$pc_loess_se <- pc_loess_se
            pcs[[i]]$pc_normalized_loess <- pc_normalized_loess
            pcs[[i]]$pc_normalized_loess_se <- pc_normalized_loess_se
            pcs[[i]]$pc_weighted <- pc_weighted
            pcs[[i]]$pc_weighted_loess <- pc_weighted_loess
            pcs[[i]]$pc_weighted_loess_se <- pc_weighted_loess_se
            pcs[[i]]$pc_weighted_normalized <- pc_weighted_normalized
            pcs[[i]]$pc_weighted_normalized_loess <- pc_weighted_normalized_loess
            pcs[[i]]$pc_weighted_normalized_loess_se <- pc_weighted_normalized_loess_se
            pcs[[i]]$pc_weighted_normalized_units <- eigvecs[[i]]$eigenvec_normalized_units
                
            # sum weighted pcs
            tmp <- rep(0, t=length(pcs[[i]]$pc_weighted[,1]))
            for (eofi in seq_len(neof)) {
                tmp <- tmp + pcs[[i]]$pc_weighted[,eofi]
            }
            pcs[[i]]$pc_weighted_sum <- tmp

            # smooth summed weighted pc
            tmp <- predict(loess(pcs[[i]]$pc_weighted_sum ~ seq_along(pcs[[i]]$time), span=loess_span), se=T)
            pcs[[i]]$pc_weighted_sum_loess <- unname(tmp$fit)
            pcs[[i]]$pc_weighted_sum_loess_se <- unname(tmp$se.fit)
            
            # sum weighted normalized PCs
            tmp <- rep(0, t=length(pcs[[i]]$pc_weighted_normalized[,1]))
            for (eofi in seq_len(neof)) {
                tmp <- tmp + pcs[[i]]$pc_weighted_normalized[,eofi]
            }
            pcs[[i]]$pc_weighted_normalized_sum <- tmp

            # smooth summed weighted normalized PC
            tmp <- predict(loess(pcs[[i]]$pc_weighted_normalized_sum ~ seq_along(pcs[[i]]$time), span=loess_span), se=T)
            pcs[[i]]$pc_weighted_normalized_sum_loess <- unname(tmp$fit)
            pcs[[i]]$pc_weighted_normalized_sum_loess_se <- unname(tmp$se.fit)
        
        } # if eigvecs and pcs are provided

    } # for i


    ## plots

    # prepare plot
    if (!all(sapply(pcs, is.null))) {
        tlim <- range(lapply(lapply(pcs, "[[", "time"), as.numeric))
        tlimlt <- as.POSIXlt(tlim, origin="1970-1-1", tz="UTC")
        tlimct <- as.POSIXct(tlimlt)
        tlablt <- as.POSIXlt(pretty(tlimlt, n=10)) # this does not work with large negative years, e.g. -800000 (800ka) 
        time_plot_unit <- "time"
        tatn <- as.numeric(tlablt)
        if (T) {
            message("\nmake my own BP labels ...")
            tlablt <- make_posixlt_origin(seq(-7000, 0, b=1000))
            tatn <- as.numeric(tlablt)
            tlablt <- abs(tlablt$year+1900)
            time_plot_unit <- "year before 1950"
        }
    }

    # plot eigenvals in percent
    if (!all(sapply(eigvals, is.null))) {
        message("\nplot ", neigval_plot, " eigenvals in percent ...")
        eigvals_error_plot <- lapply(eigvals, "[[", "eigenval_pcnt_error")
        eigvals_error_upper_plot <- lapply(eigvals_error_plot, "[[", "upper.lim")
        eigvals_error_lower_plot <- lapply(eigvals_error_plot, "[[", "lower.lim")
        eigvals_error_upper_plot <- lapply(eigvals_error_upper_plot, function(x) x[seq_len(neigval_plot)])
        eigvals_error_lower_plot <- lapply(eigvals_error_lower_plot, function(x) x[seq_len(neigval_plot)])
        ylim <- range(eigvals_error_upper_plot, eigvals_error_upper_plot)
        cols <- mycols(length(eigvals_error_upper_plot))
        plotname <- paste0(paste(names(settings), collapse="_vs_"), "_eof_", neigval_plot, "_eigenvals_pcnt.png")
        message("plot ", plotname, " ...")
        png(plotname, width=2666, height=2000, res=300)
        plot(seq_len(neigval_plot), seq_len(neigval_plot), 
             t="n", ylim=ylim, xaxt="n", yaxt="n" 
             , log="x",
             , xlab="Number of eigenvalue", ylab="Described variance [%]")
        axis(1, at=pretty(seq_len(neigval_plot), n=10))
        axis(2, at=pretty(ylim, n=10), las=2)
        for (i in seq_along(eigvals_error_upper_plot)) {
            polygon(c(seq_len(neigval_plot), rev(seq_len(neigval_plot))),
                    c(eigvals_error_upper_plot[[i]], rev(eigvals_error_lower_plot[[i]])),
                    col=col2rgba(cols[i], alpha=0.3), border=NA)
            lines(seq_len(neigval_plot), eigvals[[i]]$eigenval_pcnt[seq_len(neigval_plot)],
                      col=cols[i])
        }
        legend("topright", legend=names(eigvals_error_upper_plot), 
               lty=1, col=cols, bty="n", x.intersp=0.2)
        dev.off()
    }

    # plot normalized eigenvecs
    if (!all(sapply(eigvecs, is.null))) {
        message("\nplot normalized eigenvecs ...")
        for (eofi in seq_len(neof)) {
            plotname <- paste0(paste(names(settings), collapse="_vs_"), "_eof_normalized_eigenvec_", eofi, ".png")
            message("plot ", plotname, " ...")
            lons <- lats <- eigvecs_plot <- vector("list", l=length(eigvecs))
            for (i in seq_along(eigvecs)) {
                lons[[i]] <- eigvecs[[i]]$lon
                lats[[i]] <- eigvecs[[i]]$lat
                eigvecs_plot[[i]] <- eigvecs[[i]]$eigenvec_normalized[,,eofi]
                names(eigvecs_plot)[i] <- paste0("EOF", eofi, " ", names(eigvecs)[i], 
                                                   " (", round(eigvals[[i]]$eigenval_pcnt[eofi]), "%; µ=",
                                                   round(eigvecs[[i]]$eigenvec_normalized_fldmean[eofi], 3), 
                                                   " ", eigvecs[[i]]$eigenvec_normalized_units, ")")
            }
            nm <- image.plot.nxm(lons, lats, eigvecs_plot, dry=T)
            width_in <- p$inch #a4_width_in # maximum a4 width as threshold (8.26 in)
            message("width_in = ", width_in, " ", appendLF=F)
            asp_width_over_height <- (nm$ncol*p$map_width)/(nm$nrow*p$map_height)
            message("--> aspect ratio = ", round(asp_width_over_height, 3))
            height_in <- width_in/asp_width_over_height
            message("height_in = width_in/aspect_ratio = ", 
                    width_in, "/", round(asp_width_over_height, 3), " = ", 
                    round(width_in/asp_width_over_height, 4), " = ",
                    round(height_in, 4))
            if (height_in > p$a4_height_in) {
                height_in <- p$a4_height_in # take a4 maximum height as threshold (11.58 in)
                message("this is > ", p$a4_height_in, " --> height_in = ", height_in, 
                        " --> aspect ratio = ", round(width_in/height_in, 3))
            }
            pointsize <- p$pointsize*width_in/p$inch # multiple of default
            #pointsize <- 8
            if (p$plot_type == "pdf") {
                pdf(plotname, width=width_in, height=height_in, family=p$family_pdf,
                    pointsize=pointsize)
            } else if (p$plot_type == "png") {
                png(plotname, width=width_in, height=height_in, units="in", family=p$family_png, 
                    pointsize=pointsize, res=p$ppi)
            }
            if (F) {
                message("special: same limits as bader et al. 2020 ...")
                zlevels <- c(seq(-0.6, -0.2, b=0.1), 0, seq(0.2, 0.6, b=0.1))
                if (range(eigvecs_plot)[1] < min(zlevels)) zlevels <- c(range(eigvecs_plot)[1], zlevels)
                if (range(eigvecs_plot)[2] > max(zlevels)) zlevels <- c(zlevels, range(eigvecs_plot)[2])
                ip <- image.plot.pre(zlim=range(eigvecs_plot), 
                                     zlevels=zlevels, anom_colorbar=T, center_include=T)
            } else {
                ip <- image.plot.pre(range(eigvecs_plot))
            }
            addland_list <- lapply(vector("list", l=nsettings), 
                                   base::append, 
                                   list(data="world2", xlim="xlim", ylim="ylim"))
            image.plot.nxm(lons, lats, eigvecs_plot, n=nm$nrow, m=nm$ncol, ip=ip,
                           xlab="Longitude [°]", ylab="Latitude [°]", 
                           zlab=paste0("Normalized eigenvector [", eigvecs[[1]]$eigenvec_normalized_units, "]"),
                           addland_list=addland_list,
                           verbose=F)
            dev.off()
        } # for eofi
    } # if eigvecs

    # plot PCs
    if (!all(sapply(pcs, is.null))) {
        
        # plot normalized (var=1) pcs
        message("\nplot normalized PCs ...")
        plotname <- paste0(paste(names(settings), collapse="_vs_"), "_eof_", neof, 
                           "_normalized_var.eq.1_pc_loess_span_", loess_span, ".png")
        message("plot ", plotname, " ...")
        pcs_plot <- lapply(pcs, "[[", "pc_normalized_loess")
        pcs_plot <- lapply(pcs_plot, function(x) x[,seq_len(neof)])
        if (is.vector(pcs_plot[[1]])) {
            for (i in seq_along(pcs_plot)) {
                pcs_plot[[i]] <- as.matrix(pcs_plot[[i]])
            }
        }
        ylim <- range(pcs_plot)
        message("ylim ", neof, " normalized PCs: ", appendLF=F)
        dput(ylim)
        if (F) {
            message("special ylim : ", appendLF=F)
            ylim <- c(-1.07541310239326, 1.09568086282759)
            # holtx10: c(-0.566597936868442, 0.533824089742989)
            # holt: c(-1.07541310239326, 1.09568086282759)
            dput(ylim)
        }
        cols <- mycols(nsettings)
        png(plotname, width=2666, height=2000, res=300)
        plot(pcs[[1]]$time, seq_along(pcs[[1]]$time), 
             t="n", xlim=tlim, ylim=ylim, xaxt="n", yaxt="n", 
             xlab=time_plot_unit, ylab="normalized (var=1) PC")
        axis(1, at=tatn, labels=tlablt)
        axis(2, at=pretty(ylim, n=10), las=2)
        abline(h=0, col="gray")
        for (i in seq_along(pcs)) {
            for (eofi in seq_len(neof)) {
                lines(pcs[[i]]$time, pcs_plot[[i]][,eofi],
                      col=cols[i], lty=eofi)
            }
        }
        legend("topleft", legend=names(pcs), lty=1, col=cols,
               bty="n", x.intersp=0.2)
        legend("topright", legend=paste0("PC", seq_len(neof)), lty=seq_along(pcs), col="black",
               bty="n", x.intersp=0.2)
        dev.off()

        # plot weighted normalized pcs
        message("\nplot weigthed normalized PCs ...")
        plotname <- paste0(paste(names(settings), collapse="_vs_"), "_eof_", neof, 
                           "_weighted_normalized_pc_loess_span_", loess_span, ".png")
        message("plot ", plotname, " ...")
        pcs_plot <- lapply(pcs, "[[", "pc_weighted_normalized_loess")
        pcs_plot <- lapply(pcs_plot, function(x) x[,seq_len(neof)])
        if (is.vector(pcs_plot[[1]])) {
            for (i in seq_along(pcs_plot)) {
                pcs_plot[[i]] <- as.matrix(pcs_plot[[i]])
            }
        }
        ylim <- range(pcs_plot)
        message("ylim ", neof, " weighted normalized PCs: ", appendLF=F)
        dput(ylim)
        if (F) {
            message("special ylim : ", appendLF=F)
            ylim <- c(-0.192290662049971, 0.205080115577942)
            # holtx10: 3 eof: c(-0.126770348916235, 0.125147999164271)
            # holt: 3 eof: c(-0.192290662049971, 0.205080115577942)
            dput(ylim)
        }
        cols <- mycols(neof)
        png(plotname, width=2666, height=2000, res=300)
        plot(pcs[[1]]$time, seq_along(pcs[[1]]$time), 
             t="n", xlim=tlim, ylim=ylim, xaxt="n", yaxt="n", 
             xlab=time_plot_unit, 
             ylab=paste0("weighted normalized PC [", pcs[[1]]$pc_weighted_normalized_units, "]"),
            main="weighted normalized PC = fldmean(normalized eigenvec) * normalized PC")
        axis(1, at=tatn, labels=tlablt)
        axis(2, at=pretty(ylim, n=10), las=2)
        abline(h=0, col="gray")
        for (i in seq_along(pcs)) {
            for (eofi in seq_len(neof)) {
                lines(pcs[[i]]$time, pcs_plot[[i]][,eofi],
                      col=cols[i], lty=eofi)
            }
        }
        legend("topleft", legend=names(pcs), lty=seq_along(pcs), col=cols,
               bty="n", x.intersp=0.2)
        legend("topright", legend=paste0("PC", seq_len(neof)), lty=seq_len(neof), col="black",
               bty="n", x.intersp=0.2)
        dev.off()

        # plot sum of smoothed weighted normalized PCs
        plotname <- paste0(paste(names(settings), collapse="_vs_"), "_eof_sum_", 
                           neof, "_weighted_normalized_pc_loess_span_", loess_span, ".png")
        message("\nplot ", plotname, " ...")
        pcs_plot <- lapply(pcs, "[[", "pc_weighted_normalized_sum_loess")
        ylim <- range(pcs_plot)
        if (!all(sapply(datas, is.null))) {
            ylim <- range(ylim, lapply(datas, "[[", "fldmean_loess"))
        }
        message("ylim: ", appendLF=F)
        dput(ylim)
        if (F) {
            message("special ylim = ", appendLF=F)
            ylim <- c(-0.326267625632257, 0.370359071993717)
            # holtx10: c(-0.169200222485841, 0.16945167214843)
            # holt: c(-0.326267625632257, 0.370359071993717)
            dput(ylim)
        }
        cols <- mycols(nsettings)
        png(plotname, width=2666, height=2000, res=300)
        par(mar=c(5.1, 6.1, 4.1, 2.1) + 0.1)
        plot(pcs[[1]]$time, seq_along(pcs[[1]]$time), 
             t="n", xlim=tlim, ylim=ylim, xaxt="n", yaxt="n", 
             xlab=time_plot_unit, ylab=NA)
        axis(1, at=tatn, labels=tlablt)
        axis(2, at=pretty(ylim, n=10), las=2)
        mtext(text=paste0("sum of weighted normalized PC [", pcs[[1]]$pc_weighted_normalized_units, "]"), side=2, line=4)
        abline(h=0, col="gray")
        for (i in seq_along(pcs)) {
            lines(pcs[[i]]$time, pcs_plot[[i]], col=cols[i], lty=1)
        }
        legend("topleft", 
               legend=paste0(names(settings), " (EOF ", paste(seq_len(neof), collapse="+"), ")"),
               lty=1, col=cols, bty="n", x.intersp=0.2)
        # add anom data
        if (!all(sapply(datas, is.null))) {
            corrs <- vector("list", l=length(pcs))
            for (i in seq_along(datas)) {
                corrs[[i]] <- cor.test(data[[i]]$fldmean_loess, pcs_plot[[i]])
                message(names(settings)[i], " corr(anom data used for eof analysis, weighted normalized PC", 
                        paste(seq_len(neof), collapse="+"), " ", names(pcs)[i], ") = ", 
                        round(corrs[[i]]$estimate, 3))
                lines(datas[[i]]$time, datas[[i]]$fldmean_loess, col=cols[i], lty=2)
            }
            legend("topright",
                   legend=paste0(names(pcs), " data; r=", round(sapply(corrs, "[[", "estimate"), 3)), 
                   lty=2, col=cols,
                   bty="n", x.intersp=0.2)
        }
        dev.off()
    } # if pcs

    message("\nfinished")

} # plotmyeof() function


# define usage
if (interactive()) { # this runs when this file is `source()`ed
    me <- sys.frames()[[1]]$ofile # this is the script that is `source()`ed. not necessarily this one.

} else { # this runs when this file is `./`ed
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
}
usage <- paste0("\nUsage:\n",
                " $ ", me, " ",
                "<plotname>=<result_of_myeof.r> ",
                "[<plotname2>=<result_of_myeof.r>] [...]\nor\n",
                " $ ", me, " ", 
                "<plotname>=<eigval_result_of_mycdoeof.r>,<eigvec_result_of_mycdoeof.r>,<pc_result_of_mycdoeof.r> ",
                "[<plotname2>=<eigval_result_of_mycdoeof.r>,<eigvec_result_of_mycdoeof.r>,<pc_result_of_mycdoeof.r>] [...]\nor\n",
                " $ ", me, " ",
                "<plotname>=<result_of_myeof.r> ",
                "[<plotname2>=<eigval_result_of_mycdoeof.r>,<eigvec_result_of_mycdoeof.r>,<pc_result_of_mycdoeof.r>] [...]\n")

# run plotmyeof.r
if (interactive()) { # this runs when this file is `source()`ed
    # nothing to do
    # input for plotmyeof() function is supposed to be provided by other script

} else {
    args <- commandArgs(trailingOnly=T)
    if (length(args) == 0) {
        message(usage)
        quit()
    }
    plotmyeof(args)
}


