#!/usr/bin/env Rscript

# map jsbach output from tiles to pft (plant functional type)
# - this is not straight forward since several pfts can be saved on one tile level
# - the mapping table is obtained from the `cover_type` variable nc attributes from the jsbach input file that must be provided  
# - run `./jsbach_map_file_to_pft.r` to display help

if (interactive()) { # test
    rm(list=ls()); graphics.off()
    me <- "jsbach_tile2pft.r"
    #args <- c("cover_type=/work/ba1103/a270094/AWIESM/test/input/jsbach/jsbach.nc",
    #          "cover_fract=/work/ba1103/a270073/post/jsbach/select/cover_fract/awi-esm-1-1-lr_kh800_piControl_jsbach_select_cover_fract_global_Jan-Dec_1950-1951.nc",
    #          "cover_fract_pft=/work/ba1103/a270073/post/jsbach/select/cover_fract_pft/test.nc")
    #args <- c("cover_type=/work/ba1103/a270094/AWIESM/test/input/jsbach/jsbach.nc",
    #          "cover_fract=/work/ba1103/a270073/post/jsbach/select/cover_fract/awi-esm-1-1-lr_kh800_piControl_jsbach_select_cover_fract_global_Jan-Dec_1950-1951.nc",
    #          "veg_ratio_max=/work/ba1103/a270073/post/jsbach/select/veg_ratio_max/awi-esm-1-1-lr_kh800_piControl_jsbach_select_veg_ratio_max_global_Jan-Dec_1950-1951.nc",
    #          "pft_fract_box=/work/ba1103/a270073/post/jsbach/select/pft_fract_box/test.nc")
    args <- c("cover_type=/work/ba1103/a270073/out/mpiesm-1.2.01p5/mpiesm-s/piControl_2801_ndepo_init_model_restart/work/jsbach.nc",
              "cover_fract=/work/ba1103/a270073/post/jsbach/select/cover_fract/jsbach-3.20p1_piControl_ndepo_init_restart_jsbach_select_cover_fract_global_Jan-Dec_2801-2802.nc",
              "veg_ratio_max=/work/ba1103/a270073/post/jsbach/select/veg_ratio_max/jsbach-3.20p1_piControl_ndepo_init_restart_jsbach_select_veg_ratio_max_global_Jan-Dec_2801-2802.nc",
              "pft_fract_box=/work/ba1103/a270073/post/jsbach/select/pft_fract_box/jsbach-3.20p1_piControl_ndepo_init_restart_jsbach_select_pft_fract_box_global_Jan-Dec_2801-2802.nc")

} else {

    # get args
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)

} # if interactive or not

options(warn=2) # stop on warnings    
remove_tmp_files <- T # F for debug 

helpstring <- paste(rep(" ", t=nchar(me)), collapse="")
help <- paste0("\nUsage: (order of args does not matter except last is taken as output)\n",
               " $ ", me, " cover_type=</path/to/jsbach/input/file/with/variable/cover_type>\n",
               "   ", helpstring, " <varname_in>=</path/to/file/with/variable/on/tiles>\n",
               "   ", helpstring, " <varname_out>=</path/to/file/with/variable/on/pfts>\n",
               "   ", helpstring, " optional:\n",
               "   ", helpstring, " slm=</path/to/jsbach/input/file/with/variable/slm> # default = file of `cover_type`\n",
               "\nSpecial cases:\n",
               "  1) If `cover_fract` and `veg_ratio_max` are provided and output is named `pft_fract_box`,\n",
               "     cover fraction with respect to the box area (and not vegetated area) is calculated and\n",
               "     saved per pft (not per tile).\n")

# checks
if (length(args) < 3) {
    message(help)
    quit()
}
host <- Sys.info()["nodename"]
if (grepl("mlogin", host)) stop("switch to mistralpp")
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")
ncatted <- Sys.which("ncatted")
if (ncatted == "") stop("could not find ncatted")

# collect user args
fcover_type <- fslm <- NULL
data <- list()
for (argi in seq_along(args)) {
    arg <- args[argi]
    #print(arg)
    if (!grepl("=", arg)) {
        stop("all arguments must be of form `varname=/path/to/file/with/variable/on/tiles`")
    }
    arg <- strsplit(arg, "=")[[1]] # len2: varname path
    if (length(arg) != 2) {
        stop("argument must be of form `varname=/path/to/file/with/variable/on/tiles`")
    }
    if (argi < length(args)) { # not last (output) file
        if (file.access(arg[2], mode=0) == -1) {
            stop("input file \"", arg[2], "\" does not exist")
        }
        if (file.access(arg[2], mode=4) == -1) {
            stop("input file \"", arg[2], "\" not readable")
        }
        if (arg[1] == "cover_type") {
            fcover_type <- arg[2]
        } else if (arg[1] == "slm") {
            fslm <- arg[2]
        } else {
            data[[length(data)+1]] <- list(filename=arg[2])
            names(data)[length(data)] <- arg[1]
        }
    } else { # last user arg: output
        varout <- arg[1]
        fout <- arg[2]
    }
} # for argi

# check user args
if (is.null(fcover_type)) {
    stop("must provide argument `cover_type=/path/to/jsbach/input/file/with/variable/cover_type`")
}
if (is.null(fslm)) {
    fslm <- fcover_type
}
outpath <- dirname(fout)
if (!dir.exists(outpath)) {
    dir.create(outpath, recursive=T)
    if (!dir.exists(outpath)) {
        stop("could not create path `dirname(fout)` = \"", outpath, "\"")
    }
}
if (file.access(outpath, mode=2) == -1) {
    stop("no permission to write in path `dirname(fout)` = \"", outpath, "\"")
}

# load necessary packages
message("load ncdf4 package ...")
library(ncdf4)


## start

# get cover_type and number of tiles
message("\nopen cover_type file ", fcover_type, " ...")
cover_type_nc <- ncdf4::nc_open(fcover_type)
message("get cover_type variable ...")
cover_type <- ncdf4::ncvar_get(cover_type_nc, "cover_type")
if (length(dim(cover_type)) != 3) {
    stop("dim(cover_type) = ", paste(dim(cover_type), collapse=", "), 
         " is of length ", length(dim(cover_type)), " but must be of length 3")
}

# get ntiles
cover_type_dimids <- sapply(cover_type_nc$var$cover_type$dim, "[[", "id")
names(cover_type_dimids) <- sapply(cover_type_nc$var$cover_type$dim, "[[", "name")
ntiles_dim_id <- cover_type_dimids["ntiles"]
ntiles_dim_ind <- ntiles_dim_id + 1 # +1 since nc ids count from 0 but r counts from 1
ntiles <- dim(cover_type)[ntiles_dim_ind]
message("--> ntiles = ", ntiles)

# get slm
if (fslm == fcover_type) {
    message("get slm variable ...")
    slm <- ncdf4::ncvar_get(cover_type_nc, "slm")
} else {
    message("open slm file ", fslm, " ...")
    slm_nc <- ncdf4::nc_open(fslm)
    message("get slm variable ...")
    slm <- ncdf4::ncvar_get(slm_nc, "slm")
}

# get number of land points from binary land sea mask `slm`
ntotal <- prod(dim(slm))
nland <- length(which(slm == 1))
nocean <- ntotal - nland
message("--> total grid cells = ", ntotal, "\n",
        "--> land grid cells = ", nland, " (", round(nland/ntotal*100, 2), "%)\n",
        "--> ocean grid cells = ", nocean, " (", round(nocean/ntotal*100, 2), "%)")

# get attributes of `cover_type` and generate mapping table tile <--> cover_type, i.e. lev <--> pft
cover_type_atts <- ncdf4::ncatt_get(cover_type_nc, "cover_type")
lcts <- names(cover_type_atts) # e.g. "lct01" or "lct21"
if (!any(grepl("^lct", lcts))) {
    stop("not any attribute of variable `cover_type` starts with \"lct\"")
}
lcts <- lcts[which(grepl("^lct", lcts))]
lct_vals <- lct_types <- rep(NA, t=length(lcts))
for (lcti in seq_along(lcts)) {
    lct_vals[lcti] <- as.integer(sub("^lct", "", lcts[lcti])) # strip "lct" from "lct01"
    lct_types[lcti] <- cover_type_atts[[lcts[lcti]]]
}
lct <- data.frame(cover_type=lct_vals, 
                  name=lct_types, stringsAsFactors=F)
message("--> based on the input file, there are ", length(lcts), 
        " possible `cover_type`", ifelse(length(lcts) > 1, "s", ""), ":")
print(lct)

# get all cover_types per level
# --> multiple `cover_type`s per tile possible
tiles <- list() 
for (tilei in seq_len(ntiles)) {
    cover_type_vals <- sort(unique(as.vector(cover_type[,,tilei])))
    if (any(cover_type_vals == 0)) { # remove 0 (=ocean)
        cover_type_vals <- cover_type_vals[-which(cover_type_vals == 0)]
    }
    names(cover_type_vals) <- lct$name[lct$cover_type == cover_type_vals] 
    tiles[[tilei]] <- cover_type_vals
    names(tiles)[tilei] <- tilei
}
message("--> of those, ", length(unlist(tiles)), " `cover_type`",
        ifelse(length(unlist(tiles)) > 1, "s", ""), " are saved in `ntiles`=", 
        ntiles, " levels:")
cat(capture.output(str(tiles)), sep="\n")

# get case based on user input
case <- 0 # default

# special case 1: calc cover fraction with respect to the box area (and not vegetated area)
if (length(data) == 2 &&
    any(grepl("cover_fract", names(data), ignore.case=T)) && 
    any(grepl("veg_ratio_max", names(data), ignore.case=T)) &&
    varout == "pft_fract_box") {
    message("\n*********************************************\n",
            "special case 1: calc pft fraction (not cover fraction) with respect to the box area (not vegetated area)\n")
    case <- 1
    outunit <- "fraction" # "fraction" or "area"
    ind <- which(grepl("veg_ratio_max", names(data), ignore.case=T))
    veg_ratio_max_varname <- names(data)[ind]
    fveg_ratio_max <- data[[ind]]$filename
    data[[ind]] <- NULL # keep only cover_fract for variable loop below
} # define special cases here

# bring data from lev --> pft
for (vari in seq_along(data)) {
    message("\n####################################################\n",
            "map input variable ", vari, "/", length(data), ": ", names(data)[vari], " from tiles to pfts ...")
    fouts <- list()
    cnt <- 0
    for (tilei in seq_len(ntiles)) {
        
        cover_types <- tiles[[tilei]]
        message("\n*********************************************\n",
                "tile ", tilei, "/", ntiles, " has ", 
                length(cover_types), " `cover_type`s:")
        print(cover_types)

        for (typei in seq_along(cover_types)) {

            # create mask file of current cover_type (=pft) of current tile
            pft_name <- gsub(" ", "_", names(cover_types)[typei])
            fmask <- paste0(outpath, "/mask_cover_type_", cover_types[typei], "_", pft_name, ".nc")
            if (vari == 1) {
                message("\ncreate mask file ", fmask, " ...")
                message("--> get location inds where `cover_type` ", typei, "/", length(seq_along(cover_types)), 
                        " = ", cover_types[typei], " (\"", names(cover_types)[typei], "\") ...")
                cmd <- rep(",", t=length(dim(cover_type)))
                cmd[ntiles_dim_ind] <- tilei
                cmd <- paste0("inds <- cover_type[", paste(cmd, collapse=""), ",drop=T] == ", cover_types[typei])
                eval(parse(text=cmd))
                if (!any(inds)) stop("this should not happen")
                ncells <- length(which(inds))
                ncells_rel <- ncells/nland*100
                message("--> ", ncells, " grid cells = ", round(ncells_rel, 2), " % of ", nland, " land cells") 
                if (tilei == 1 && typei == 1) {
                    lon_dim <- cover_type_nc$dim$lon
                    lat_dim <- cover_type_nc$dim$lat
                }
                mask_var <- ncdf4::ncvar_def(name=pft_name, units="", 
                                             dim=list(lon_dim, lat_dim), missval=0)
                mask_nc <- ncdf4::nc_create(filename=fmask, force_v4=T, vars=mask_var)
                ncdf4::ncvar_put(nc=mask_nc, varid=mask_var, vals=inds)
                ncdf4::nc_close(mask_nc)
            } # if vari == 1

            # construct temporary fout of current cover_type (=pft) of current tile
            cnt <- cnt + 1
            fouti <- paste0(outpath, "/", varout, "_", pft_name, ".nc")
            fouts[[cnt]] <- list(lev=cover_types[typei],
                                 cover_type=cover_types[typei],
                                 name=names(cover_types)[typei],
                                 fmask=fmask, fout=fouti, 
                                 ncells=ncells, ncells_rel=ncells_rel)
            
            # construct cdo command
            cmd <- paste0(cdo, 
                          " -setname,", varout, 
                          " -setlevel,", cover_types[typei], # pft number
                          " -mul ", fmask)

            # special case 1
            # calc cover fraction with respect to grid cell area and not vegetated area
            # = eq 1.5 jsbach docu = grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
            if (case == 1) { # data == cover_fract
                # todo: if (outunit == "area") cmd <- paste0(cmd, " -mul ", farea)
                if (names(cover_types)[typei] == "glacier") { 
                    # special case glacier: does not need multiplication with `veg_ratio_max` as
                    # "glaciers either cover a grid box fully or are completely absent." jsbach docu 1.3, p. 7
                    # --> `veg_ratio_max` is zero where `cover_type` = 1, i.e. glacier
                    cmd <- paste0(cmd, " -sellevel,", tilei, " -selname,", names(data)[vari], " ", data[[vari]]$filename)
                } else {
                    cmd <- paste0(cmd, " -mul -sellevel,", tilei, " -selname,", names(data)[vari], " ", data[[vari]]$filename, " -selname,veg_ratio_max ", fveg_ratio_max)
                }
            
            # default case
            } else { 
                cmd <- paste0(cmd, " -sellevel,", tilei, " -selname,", names(data)[vari], " ", data[[vari]]$filename)
            } # which case

            # add fout
            cmd <- paste0(cmd, " ", fouts[[cnt]]$fout)
            
            # run cdo command
            message("\nrun `", cmd, "` ...")
            system(cmd)

            # save non-glacier and ocean locations 
            # needed for e.g. bare land fraction calculation = `1 - veg_ratio_max`
            if (vari == 1) {
                if (tilei == 1 && typei == 1) { # declare everywhere
                    inds_non_glacier <- inds_ocean <- array(T, dim=dim(inds))
                }
                if (cover_types[typei] == 1) { # set glacier to false
                    inds_non_glacier[inds] <- F
                }
                inds_ocean[inds] <- F # set land to false 
            }

        } # for typei in cover_types of current tile

    } # for tilei ntiles
    
} # for vari

# special case 1
if (case == 1) {

    # calc bare land fraction
    message("\n*********************************************\n",
            "special case 1: calc bare land = `1 - veg_ratio_max` at _non-glacier locations_\n",
            "--> veg_ratio_max = 0 at glaciers --> 1 - 0 would yield 100% bare land at glacier locations although these are glaciers\n",
            "--> need to calc at non-glacier locations only")
    
    fmask <- paste0(outpath, "/mask_cover_type_non_glacier.nc")
    message("\ncreate mask file ", fmask, " ...")
    message("--> get non-glacier location inds ...")
    inds_non_glacier[inds_ocean] <- F # set ocean to NA
    ncells <- length(which(inds_non_glacier))
    ncells_rel <- ncells/nland*100
    message("--> ", ncells, " grid cells = ", round(ncells_rel, 2), " % of ", nland, " land cells") 

    # non-glacier mask file 
    mask_var <- ncdf4::ncvar_def(name="non_glacier", units="", 
                                 dim=list(lon_dim, lat_dim), missval=0)
    mask_nc <- ncdf4::nc_create(filename=fmask, force_v4=T, vars=mask_var)
    ncdf4::ncvar_put(nc=mask_nc, varid=mask_var, vals=inds_non_glacier)
    ncdf4::nc_close(mask_nc)
            
    # calc bare land
    fouti <- paste0(outpath, "/", varout, "_bare_land.nc")
    cnt <- cnt + 1
    fouts[[cnt]] <- list(lev=0, # bare land dummy level for cdo setlevel
                         cover_type=NA,
                         name=paste0("bare land (1-", veg_ratio_max_varname, ")"),
                         fmask=fmask, fout=fouti, 
                         ncells=ncells, ncells_rel=ncells_rel)
    cmd <- cdo
    # todo: if (outunit == "area") cmd <- paste0(cmd, " -mul ", farea)
    cmd <- paste0(cmd,
                  " -setname,", varout, 
                  " -mul ", fmask, 
                  " -expr,'", varout, "=1-", veg_ratio_max_varname, "'", # same name as other pfts to get merged
                  " -selname,", veg_ratio_max_varname, " ", fveg_ratio_max, 
                  " ", fouti)
    message("\nrun `", cmd, "` ...")
    system(cmd)
    
    # set bare land zaxis to get merged with other pfts along lev dimension
    message("\nget zaxis info ...")
    cmd <- paste0(cdo, " zaxisdes ", fouts[[cnt-1]]$fout, " > ", outpath, "/tmp_zaxisdes.txt")
    message("run `", cmd, "` ...")
    system(cmd)
    cmd <- paste0(cdo, 
                  " -setlevel,", fouts[[cnt]]$lev,
                  " -setzaxis,", outpath, "/tmp_zaxisdes.txt ", fouti, " ", 
                  outpath, "/tmp && mv ", outpath, "/tmp ", fouti)
    message("set zaxis info ...")
    message("run `", cmd, "` ...")
    system(cmd)
    invisible(file.remove(paste0(outpath, "/tmp_zaxisdes.txt")))

} # if case 1

# merge pfts together
message("\n**********************************\n",
        "merge ", length(fouts), " files together ...")
atts <- paste0("@lev", sapply(fouts, "[[", "lev"), "=\"",
               "name=", sapply(fouts, "[[", "name"), 
               "; cover_type=", sapply(fouts, "[[", "cover_type"), 
               "; ncells=", sapply(fouts, "[[", "ncells"), 
               "\"")
cmd <- paste0(cdo,
              " -setattribute,", varout, "@slm_nocean=", nocean,
              " -setattribute,", varout, "@slm_nland=", nland,
              " -setattribute,", varout, "@slm_ntotal=", ntotal,
              " ", paste(rev(paste0("-setattribute,", varout, atts)), collapse=" "), 
              " -setname,\"", varout, "\"", 
              " -merge ", paste(sapply(fouts, "[[", "fout"), collapse=" "), " ", fout)
message("run `", cmd, "` ...")
system(cmd)
                                                  
# remove old `code`
cmd <- paste0(ncatted, " -O -a code,", varout, ",d,, ", fout)
message("\nrun `", cmd, "` ...")
system(cmd)

# remove old `long_name`
cmd <- paste0(ncatted, " -O -a long_name,", varout, ",d,, ", fout)
message("\nrun `", cmd, "` ...")
system(cmd)

# remove old `units`
cmd <- paste0(ncatted, " -O -a units,", varout, ",d,, ", fout)
message("\nrun `", cmd, "` ...")
system(cmd)

# remove tmp files
if (remove_tmp_files) {
    message("\n`remove_tmp_files`=T --> rm merge files ...")
    invisible(file.remove(sapply(fouts, "[[", "fout")))
    invisible(file.remove(sapply(fouts, "[[", "fmask")))
    # todo: if (outunit == "area") file.remove(farea)
}

# finished
options(warn=0) # back to default
message("\nfinished\n")

# for reference:
#!-----------------------------------------------------------
#! calculate araes covered by grass, woods, deserts
#!-----------------------------------------------------------
# ALLOCATE (dummy(nlat))
# ALLOCATE (gauss_weights(nlat))
# CALL gauaw(dummy,gauss_weights,nlat,pi)
# ALLOCATE (grid_area(nlat))
# grid_area(:) = gauss_weights(:) * 2._dp * pi * radius_of_earth**2 / REAL(nlon)
# DEALLOCATE (dummy,gauss_weights)
# area_tropical_forest = 0._dp
# area_temperate_forest = 0._dp
# area_woody = 0._dp
# area_shrubs = 0._dp
# area_crops = 0._dp
# area_pasture = 0._dp
# area_grass = 0._dp
# area_all = 0._dp
# area_land = 0._dp
# area_land_tropics= 0._dp
# area_land_extratropics = 0._dp
# area_glac = 0._dp
# DO i = 1,nlon
# DO j = 1,nlat
#    area_all = area_all + grid_area(j)
#    IF (slm(i,j) > 0.5_dp) THEN                   ! land
#       area_land = area_land + grid_area(j)
#       DO k = 1,ntiles
#          IF (nlct == 11) THEN
#             IF (cover_type(i,j,k) == 1 .OR. cover_type(i,j,k) == 2) THEN
#                area_tropical_forest = area_tropical_forest + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             ELSE IF (cover_type(i,j,k) == 3 .OR. cover_type(i,j,k) == 4) THEN
#                area_temperate_forest = area_temperate_forest + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             ELSE IF (cover_type(i,j,k) == 5 .OR. cover_type(i,j,k) == 6) THEN
#                area_shrubs = area_shrubs + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             ELSE IF (cover_type(i,j,k) == 7 .OR. cover_type(i,j,k) == 8) THEN
#                area_grass = area_grass + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             ELSE IF (cover_type(i,j,k) == 9) THEN
#                area_crops = area_crops + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             ELSE IF (cover_type(i,j,k) == 10) THEN
#                area_pasture = area_pasture + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             END IF
#             IF (cover_type(i,j,k) < 7) THEN
#                area_woody = area_woody + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             END IF
#          ELSE IF (nlct == 14) THEN
#             IF (cover_type(i,j,k) == 1 .OR. cover_type(i,j,k) == 2) THEN
#                area_tropical_forest = area_tropical_forest + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             ELSE IF (cover_type(i,j,k) == 3 .OR. cover_type(i,j,k) == 4 &
#                 .OR. cover_type(i,j,k) == 5 .OR. cover_type(i,j,k) == 6) THEN
#                area_temperate_forest = area_temperate_forest + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             ELSE IF (cover_type(i,j,k) == 7 .OR. cover_type(i,j,k) == 8) THEN
#                area_shrubs = area_shrubs + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             ELSE IF (cover_type(i,j,k) == 9 .OR. cover_type(i,j,k) == 10) THEN
#                area_grass = area_grass + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             ELSE IF (cover_type(i,j,k) == 12 .OR. cover_type(i,j,k) == 13) THEN
#                area_crops = area_crops + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             END IF
#             IF (cover_type(i,j,k) < 9) THEN
#                area_woody = area_woody + &
#                     grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
#             END IF
#          END IF
#          IF (glac_dp(i,j) >= 0.5_dp) THEN
#             area_glac = area_glac + grid_area(j) * cover_fract(i,j,k)
#          ELSE IF (j <= nlat/3 .OR. j > 2*nlat/3) THEN
#             area_land_extratropics = area_land_extratropics &
#                  + grid_area(j) * cover_fract(i,j,k)
#          ELSE
#             area_land_tropics = area_land_tropics &
#                  + grid_area(j) * cover_fract(i,j,k)
#          END IF
#       END DO
#    END IF
# END DO 
# END DO
# DEALLOCATE (grid_area)
# IF (info) WRITE (*,*) ''
# IF (info) WRITE (*,*) 'The surface of the globe [km2]: ',area_all * 1.e-06
# IF (info) WRITE (*,*) 'Global land surface [km2]: ',area_land * 1.e-06
# IF (info) WRITE (*,*) 'Global glacier [km2]: ',area_glac * 1.e-06_dp
# IF (info) WRITE (*,*) 'Land tropics [km2]: ',area_land_tropics * 1.e-06
# IF (info) WRITE (*,*) 'Land extratropics [km2]: ',area_land_extratropics * 1.e-06
# IF (info) WRITE (*,*) 'nlat,nlat/3,2*nlat/3: ',nlat,nlat/3,2*nlat/3
# IF (info) WRITE (*,*) 'Area of tropical forest [km2]: ',area_tropical_forest * 1.e-06
# IF (info) WRITE (*,*) 'Area of temperate + boreal forest [km2]: ',area_temperate_forest * 1.e-06
# IF (info) WRITE (*,*) 'Area of crops [km2]: ',area_crops * 1.e-06
# IF (info) WRITE (*,*) 'Area of pasture [km2]: ',area_pasture * 1.e-06
# IF (info) WRITE (*,*) 'Area of all woody types [km2]: ',area_woody * 1.e-06
# IF (info) WRITE (*,*) 'Area of grass lands [km2]: ',area_grass * 1.e-06

