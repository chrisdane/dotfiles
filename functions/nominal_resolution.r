#!/usr/bin/env Rscript
   
# calc nominal resolution of model according to 
# https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit
# all in km
nominal_res_df <- data.frame(greater_equal=c(0   , 0.72, 1.6, 3.6, 7.2, 16, 36, 72 , 160, 360, 720 , 1600, 3600, 7200),
                             less_than=    c(0.72, 1.6 , 3.6, 7.3, 16 , 36, 72, 160, 360, 720, 1600, 3600, 7200, Inf),
                             nominal_res=  c(0.5 , 1   , 2.5, 5  , 10 , 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000))
clean <- T

if (interactive()) {
    me <- "fesom1_get_meshinfo.r"
    args <- "~/mlotst_fesom_18700101.nc"
    #args <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/CCCma/CanESM5/historical/r1i1p1f1/Omon/thetao/gn/v20190429/thetao_Omon_CanESM5_historical_r1i1p1f1_gn_185001-186012.nc"
    #args <- c("/work/ik1017/CMIP6/data/CMIP6/CMIP/CCCma/CanESM5/historical/r1i1p1f1/Omon/thetao/gn/v20190429/thetao_Omon_CanESM5_historical_r1i1p1f1_gn_185001-186012.nc",
    #          "thetao=/work/ik1017/CMIP6/data/CMIP6/CMIP/MIROC/MIROC-ES2L/historical/r1i1p1f2/Omon/thetao/gn/v20190823/thetao_Omon_MIROC-ES2L_historical_r1i1p1f2_gn_185001-201412.nc")
    #args <- "/work/ik1017/CMIP6/data/CMIP6/CMIP/CAS/FGOALS-f3-L/historical/r1i1p1f1/Omon/thetao/gn/v20191007/thetao_Omon_FGOALS-f3-L_historical_r1i1p1f1_gn_195001-201412.nc"
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
    #./fesom1_get_meshinfo.r /pool/data/AWICM/FESOM1/MESHES/core
}

usage <- paste0("\nUsage:\n",
                " $ ", me, " /path/to/nc/file/with/griddes/not/generic/and/not/unstructued\n",
                "   ", me, " varname=/path/to/nc/file/with/griddes/not/generic/and/not/unstructued\n",
                "   ", me, " f1 f2 ...\n",
                "   ", me, " var1=f1 var2=f2 ...\n", 
                "\n")

# check
if (length(args) == 0) {
    message(usage)
    quit()
}
tmp <- strsplit(args, "=")
fs <- rep(NA, t=length(tmp))
for (fi in seq_along(fs)) {
    if (length(tmp[[fi]]) == 1) { # only `filename` given
        fs[fi] <- tmp[[fi]][1]
        names(fs)[fi] <- ""
    } else if (length(tmp[[fi]]) == 2) { # `varname=filename` given
        fs[fi] <- tmp[[fi]][2]
        names(fs)[fi] <- tmp[[fi]][1]
    } else {
        stop("this should not happen")
    }
}
cdo <- Sys.which("cdo")
library(ncdf4)

if (cdo == "") stop("could not find cdo")

for (fi in seq_along(fs)) {

    # select var with correct grid
    # this may take some time for large file
    select <- ""
    varname <- names(fs)[fi]
    foutselect <- fs[fi]
    if (varname != "") {
        foutselect <- paste0("~/", basename(fs[fi]), "_select")
        if (file.exists(foutselect)) stop("foutselect ", foutselect, " already exists")
        cmd <- paste0(cdo, " -sellevidx,1 -seltimestep,1 -select,name=", varname, " ", fs[fi], " ", foutselect)
        message("run `", cmd, "` ... (this may take some time for a large file)")
        check <- system(cmd)
        if (check != 0) stop("error")
    }

    # get dx in m
    foutx <- paste0("~/", basename(fs[fi]), "_griddx")
    if (file.exists(foutx)) stop("foutx ", foutx, " already exists")
    cmd <- paste0(cdo, " -griddx ", select, " ", foutselect, " ", foutx)
    message("run `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) { # cdo griddx no success (e.g. unstructured grid)

        # get gridarea in m2
        foutarea <- paste0("~/", basename(fs[fi]), "_area")
        message("\n--> `cdo griddx` not supported for this grid --> try to approximate dmax from grid area ...")
        cmd <- paste0(cdo, " gridarea ", foutselect, " ", foutarea)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error")

        # calc resolution from Danilov 2022: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2022MS003177
        # --> dmax = sqrt(2*area_elem) ~ sqrt(area_node)
        # nodes vs elems (km):
        # core1     min   fldmean     max
        # node:   7.1100  76.5955  152.42
        # elem:  11.847   76.3513  146.62
        # core2     min   fldmean     max
        # node:   7.1114  76.5956  152.42
        # elem:  11.850   76.3513  146.61
        # LSea2     min   fldmean     max
        # node:   2.7470  45.1649  102.71
        # elem:   4.6788  45.2855  120.72
        foutdmax <- paste0("~/", basename(fs[fi]), "_dmax")
        if (file.exists(foutdmax)) stop("foutdmax ", foutdmax, " already exists")
        message("\napproximate resolution from Danilov 2022 (https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2022MS003177):\n",
                "\"A rather good estimate is provided by the square root of the area of unit\n",
                "cell (twice the triangle area or area of the dual cell) which is only 9%\n",
                "coarser than the real resolution.\"\n",
                "--> dmax = sqrt(2*area_elem) ~ sqrt(area_node)\n",
                "- quadrilateral vs triangular meshes: number of vertices\n",
                "- quadrilateral vs hexagonal meshes: number of cells")
        cmd <- paste0(cdo, " -setunit,km -expr,'dmax=sqrt(2*cell_area)/1e3' ", foutarea, " ", foutdmax)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error")
        
        if (clean) { # clean
            invisible(file.remove(foutarea))
        }

    } else { # cdo griddx success
    
        # get dy in m
        fouty <- paste0("~/", basename(fs[fi]), "_griddy")
        if (file.exists(fouty)) stop("fouty ", fouty, " already exists")
        cmd <- paste0(cdo, " -griddy ", select, " ", foutselect, " ", fouty)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error")

        # merge dx and dy
        foutxy <- paste0("~/", basename(fs[fi]), "_griddxy")
        if (file.exists(foutxy)) stop("foutxy ", foutxy, " already exists")
        cmd <- paste0(cdo, " merge ", foutx, " ", fouty, " ", foutxy)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error")

        # calc dmax = sqrt(dx^2 + dy^2)
        foutdmax <- paste0("~/", basename(fs[fi]), "_dmax")
        if (file.exists(foutdmax)) stop("foutdmax ", foutdmax, " already exists")
        message("\ncalc nominal resolution dmax = sqrt(dx^2 + dy^2) as in Taylor et al. ...")
        cmd <- paste0(cdo, " -setunit,km -divc,1e3 -expr,'dmax=sqrt(dx*dx + dy*dy)' ", foutxy, " ", foutdmax)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error")
    
        if (clean) { # clean
            invisible(file.remove(foutx, fouty, foutxy))
        }

    } # cdo griddx success or not
     
    # read dmax and print summary
    nc <- ncdf4::nc_open(foutdmax)
    dmax <- ncdf4::ncvar_get(nc, "dmax", collapse_degen=F)
    message("dmax (", paste(dim(dmax), collapse=","), "), n = ", length(dmax), ":")
    print(summary(as.vector(dmax))[-4]) # do not show mean as this is not spatially weighted

    # calc fldmean of dmax
    message("\nget global mean of dmax ...")
    cmd <- paste0(cdo, " -fldsum -gridarea ", foutdmax, " ", foutdmax, "_tmp")
    check <- system(cmd)
    if (check != 0) { # case 1: griddes not complete: cannot use fldmean --> get area by dx*dy
        foutarea <- paste0("~/", basename(fs[fi]), "_area")
        message("--> grid is not completely defined. workaround: get area=dx*dy ...")
        cmd <- paste0(cdo, " -expr,'area_m2=dx*dy' ", foutxy, " ", foutarea)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error")
        cmd <- paste0(cdo, " -output -fldsum ", foutarea)
        message("run `", cmd, "` ...")
        area_m2_fldsum <- trimws(system(cmd, intern=T))
        cmd <- paste0(cdo, " -output -divc,", area_m2_fldsum, " -fldsum [ -mul ", foutdmax, " ", foutarea, " ]")
    } else { # case 2: griddes complete: simply use fldmean
        foutarea <- NULL
        cmd <- paste0(cdo, " --pedantic -output -fldmean ", foutdmax)
    }
    message("run `", cmd, "` ...")
    dmax_km_fldmean <- trimws(system(cmd, intern=T))
    message("convert \"", dmax_km_fldmean, "\" to numeric ... ", appendLF=F)
    warn <- options()$warn; options(warn=2); dmax_km_fldmean <- as.numeric(dmax_km_fldmean); options(warn=warn)
    
    # get nominal resolution
    res_nominal <- nominal_res_df[which(dmax_km_fldmean >= nominal_res_df[,"greater_equal"] & 
                                        dmax_km_fldmean < nominal_res_df[,"less_than"]), "nominal_res"]
    message("--> average dmax = ", dmax_km_fldmean, " ~ ", round(dmax_km_fldmean), " km translates to nominal resolution of ", res_nominal, " km")

    if (clean) { # clean
        if (foutselect != fs[fi]) invisible(file.remove(foutselect))
        invisible(file.remove(foutdmax, paste0(foutdmax, "_tmp")))
        if (!is.null(foutarea)) invisible(file.remove(foutarea))
    }

} # for fi

message("\nfinished\n")

