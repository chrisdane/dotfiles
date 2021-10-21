#!/usr/bin/env Rscript

# get jsbach PFT fraction or area with respect to grid area and not vegetated area (=`cover_fract`) via
# ```
# if `outunit` = "fraction"
#   if glacier (`cover_type`=1)
#       pft_fract_box(lon,lat,pftlev,time) = pft_mask(lon,lat,pftlev) * cover_fract(lon,lat,tilelev,time)
#   else 
#       pft_fract_box(lon,lat,pftlev,time) = pft_mask(lon,lat,pftlev) * cover_fract(lon,lat,tilelev,time) * veg_ratio_max(lon,lat,time)
# if `outunit` = "area"
#   same as in `outunit` = "fraction" case but RHS multiplied with `area_m2(lon,lat)`
# ```
# - run `jsbach_pft_wrt_box.r` in terminal to display help
# - simple cdo commands are not possible since mapping from `cover_type` to `cover_fract` is needed as indicated by the different `tilelev` and `pftlev` dimensions above (see reference at the end of script)
# - the warning `cdo(2) mul (Warning): Input parameters have different levels!` can be ignored
# - the lon,lat dims of jsbach variables `cover_fract` etc. refer to the binary land sea mask `slm` and not the fractional land sea mask `slf`
#   --> in T63, `cover_fract` at panama strait (`cdo remapnn,lon=279.375/lat=8.393669`) has no values, i.e. no vegetation although slf != 0
#   --> this is absolute bullshit and wrong
##########################################################################################

options(warn=2) # stop on warnings    

if (interactive()) { # test
    fjsbach <- "/mnt/lustre02/work/ba1103/a270094/AWIESM/test/input/jsbach/jsbach.nc"
    fcover_fract <- "/work/ba1103/a270073/post/jsbach/select/cover_fract/awi-esm-1-1-lr_kh800_piControl_og_jsbach_select_cover_fract_global_Jan-Dec_1950-1951.nc"
    cover_fract_varname <- "cover_fract"
    fveg_ratio_max <- "/work/ba1103/a270073/post/jsbach/select/veg_ratio_max/awi-esm-1-1-lr_kh800_piControl_og_jsbach_select_veg_ratio_max_global_Jan-Dec_1950-1951.nc"
    veg_ratio_max_varname <- "veg_ratio_max"
    fout <- "/work/ba1103/a270073/post/jsbach/select/pft_fract_box/awi-esm-1-1-lr_kh800_piControl_og_jsbach_select_pft_fract_box_global_Jan-Dec_1950-1951.nc"
    outunit <- "fraction"
    remove_tmp_files <- F
    #cmd <- paste0("jsbach_pft_wrt_box.r --jsbach=", fjsbach, " --cover_fract=", fcover_fract, " --veg_ratio_max=", fveg_ratio_max, " --fout=", fout)

} else {

    # get args
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    help <- paste0("\nUsage:\n",
                   " $ ", me, " ",
                   "--jsbach=/path/to/input/jsbach.nc ",
                   "--cover_fract=/path/to/cover_fract.nc ",
                   "--cover_fract_varname=cover_fract ",
                   "--veg_ratio_max=/path/to/veg_ratio_max.nc ",
                   "--veg_ratio_max_varname=veg_ratio_max ",
                   "--fout=/path/to/outputfile ",
                   "--outunit=fraction ",
                   "--remove_tmp_files=T",
                   "\n")

    # check args 
    args <- commandArgs(trailingOnly=T)
    if (length(args) < 4) {
        message(help)
        quit()
    }
    message("\nstart ", me, " ...\n")

    if (any(grepl("--jsbach", args))) {
        fjsbach <- sub("--jsbach=", "", args[grep("--jsbach=", args)])
    } else {
        stop("provide `--jsbach=/path/to/input/jsbach.nc`")
    }
    if (any(grepl("--cover_fract", args))) {
        fcover_fract <- sub("--cover_fract=", "", args[grep("--cover_fract=", args)])
    } else {
        stop("provide `--cover_fract=/path/to/cover_fract.nc`")
    }
    if (any(grepl("--cover_fract_varname", args))) {
        cover_fract_varname <- sub("--cover_fract_varname=", "", args[grep("--cover_fract_varname=", args)])
    } else {
        cover_fract_varname <- "cover_fract" # default
    }
    if (any(grepl("--veg_ratio_max", args))) {
        fveg_ratio_max <- sub("--veg_ratio_max=", "", args[grep("--veg_ratio_max=", args)])
    } else {
        stop("provide `--veg_ratio_max=/path/to/veg_ratio_max.nc`")
    }
    if (any(grepl("--veg_ratio_max_varname", args))) {
        veg_ratio_max_varname <- sub("--veg_ratio_max_varname=", "", args[grep("--veg_ratio_max_varname=", args)])
    } else {
        veg_ratio_max_varname <- "veg_ratio_max" # default
    }
    if (any(grepl("--fout", args))) {
        fout <- sub("--fout=", "", args[grep("--fout=", args)])
    } else {
        stop("provide `--fout=/path/to/fout.nc`")
    }
    if (any(grepl("--outunit", args))) {
        outunit <- sub("--outunit=", "", args[grep("--outunit=", args)])
    } else {
        outunit <- "fraction" # default; "fraction" or "area"
    }
    if (any(grepl("--remove_tmp_files", args))) {
        remove_tmp_files <- sub("--remove_tmp_files=", "", args[grep("--remove_tmp_files=", args)])
    } else {
        remove_tmp_files <- T # default
    }

} # if interactive or not

# checks
host <- Sys.info()["nodename"]
if (grepl("mlogin", host)) {
    stop("switch to mistralpp")
}
fnames <- c(fjsbach, fcover_fract, fveg_ratio_max)
for (fi in seq_along(fnames)) {
    if (file.access(fnames[fi], mode=0) == -1) {
        stop("input file \"", fnames[fi], "\" does not exist")
    }
}
for (fi in seq_along(fnames)) {
    if (file.access(fnames[fi], mode=4) == -1) {
        stop("input file \"", fnames[fi], "\" not readable")
    }
}
outpath <- dirname(fout)
if (!dir.exists(outpath)) {
    dir.create(outpath, recursive=T)
    if (!dir.exists(outpath)) {
        stop("could not create path of `fout` = \"", outpath, "\"")
    }
}
if (file.access(outpath, mode=2) == -1) {
    stop("no permission to write in path of `fout` = \"", outpath, "\"")
}

if (outunit == "fraction") {
    varout <- "pft_fract_box"
    varunit <- "fraction"
    long_name <- "pft fraction with respect to grid cell area and not vegetated area"
} else if (outunit == "area") {
    varout <- "pft_area_box"
    varunit <- "m2"
    long_name <- "pft area with respect to grid cell area and not vegetated area"
} else {
    stop("`outunit` = ", outunit, " not defined")
}

message("`which cdo` = ", appendLF=F)
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")
message(cdo)

message("`which ncatted` = ", appendLF=F)
ncatted <- Sys.which("ncatted")
if (ncatted == "") stop("could not find ncatted")
message(ncatted)

message("load ncdf4 package ...")
library(ncdf4)

## checks

# check varnames of cover_fract file 
message("\ncheck varnames of cover_fract file ... ")
cmd <- paste0(cdo, " showname ", fcover_fract)
message("run `", cmd, "` ...")
cover_fract_varnames <- strsplit(trimws(system(cmd, intern=T)), " ")[[1]]
if (!any(cover_fract_varnames == cover_fract_varname)) {
    stop("not any of \"", paste(cover_fract_varnames, collapse="\", \""), 
         "\" equals `cover_fract_varname` = \"", cover_fract_varname, "\"\n",
         "rerun script with `--cover_fract_varname=<correct_varname>`")
} else {
    message("--> found cover_fract varname = \"", cover_fract_varname)
}

# check varnames of veg_ratio_max file 
message("\ncheck varnames of veg_ratio_max file ... ")
cmd <- paste0(cdo, " showname ", fveg_ratio_max)
message("run `", cmd, "` ...")
veg_ratio_max_varnames <- strsplit(trimws(system(cmd, intern=T)), " ")[[1]]
if (!any(veg_ratio_max_varnames == veg_ratio_max_varname)) {
    stop("not any of \"", paste(veg_ratio_max_varnames, collapse="\", \""), 
         "\" equals `veg_ratio_max_varname` = \"", veg_ratio_max_varname, "\"\n",
         "rerun script with `--veg_ratio_max_varname=<correct_varname>`")
} else {
    message("--> found veg_ratio_max varname = \"", veg_ratio_max_varname)
}

# open jsbach forcing file
message("\nopen forcing file ", fjsbach, " ...")
jsbach_nc <- ncdf4::nc_open(fjsbach)

# check jsbach forcing file variables
jsbach_nc_vars <- names(jsbach_nc$var)
if (!any(!is.na(match(c("slm", "SLM"), jsbach_nc_vars)))) {
    stop("jsbach forcing file does not have \"slm\" or \"SLM\" variable")
}
if (!any(!is.na(match(c("cover_type"), jsbach_nc_vars)))) {
    stop("jsbach forcing file does not have \"cover_type\" variable")
}

## start

# get number of land points from binary land sea mask `slm`
slm_ind <- which(!is.na(match(names(jsbach_nc$var), c("slm", "SLM"))))
if (length(slm_ind) != 1) {
    stop("this forcing file has ", length(slm_ind), " vars named `slm` or `SLM`. must have 1")
}
slm <- ncdf4::ncvar_get(jsbach_nc, names(jsbach_nc$var)[slm_ind])
ntotal <- prod(dim(slm))
nland <- length(which(slm == 1))
nocean <- ntotal - nland
message("--> total grid cells = ", ntotal, "\n",
        "--> land grid cells = ", nland, " (", round(nland/ntotal*100, 2), "%)\n",
        "--> ocean grid cells = ", nocean, " (", round(nocean/ntotal*100, 2), "%)")

# get number of tiles
if (!any(names(jsbach_nc$dim) == "ntiles")) {
    stop("this forcing file does not have a `ntiles` dim")
}
ntiles <- jsbach_nc$dim$ntiles$len
message("--> ntiles = ", ntiles)

# get `cover_type` tile dim index
jsbach_nc_dims <- sapply(jsbach_nc$var$cover_type$dim, "[[", "id")
names(jsbach_nc_dims) <- sapply(jsbach_nc$var$cover_type$dim, "[[", "name")
ntiles_dim_id <- jsbach_nc_dims["ntiles"]
ntiles_dim_ind <- ntiles_dim_id + 1 # +1 since nc ids count from 0 but r counts from 1

# check if variable `cover_type` is available and get its attributes
if (!any(names(jsbach_nc$var) == "cover_type")) {
    stop("this forcing file does not have variable `cover_type`")
}
cover_type_atts <- ncdf4::ncatt_get(jsbach_nc, "cover_type")
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
message("--> based on the forcing file, there are ", length(lcts), 
        " possible `cover_type`", ifelse(length(lcts) > 1, "s", ""), ":")
print(lct)

# load variable `cover_type`
cover_type <- ncdf4::ncvar_get(jsbach_nc, "cover_type")
if (length(dim(cover_type)) != 3) {
    stop("dim(cover_type) = ", paste(dim(cover_type), collapse=", "), 
         " is of length ", length(dim(cover_type)), " but must be of length 3")
}
tiles <- list() # multiple `cover_type`s per tile possible -_-
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

# calc pft fraction with respect to grid area for all pfts
fouts <- list()
cnt <- 0
for (tilei in seq_len(ntiles)) {
    
    cover_types <- tiles[[tilei]]
    message("\n*********************************************\n",
            "tile ", tilei, "/", ntiles, " has ", 
            length(cover_types), " `cover_type`s:")
    print(cover_types)

    for (typei in seq_along(cover_types)) {

        # create mask file of current cover_type of current tile
        message("\nget location inds where `cover_type` = ", cover_types[typei], 
                " (\"", names(cover_types)[typei], "\") ...")
        cmd <- rep(",", t=length(dim(cover_type)))
        cmd[ntiles_dim_ind] <- tilei
        cmd <- paste0("inds <- cover_type[", paste(cmd, collapse=""), ",drop=T] == ", cover_types[typei])
        eval(parse(text=cmd))
        if (!any(inds)) stop("this should not happen")
        ncells <- length(which(inds))
        ncells_rel <- ncells/nland*100
        message("--> ", ncells, " grid cells = ", round(ncells_rel, 2), " % of ", nland, " land cells") 
        pft_name <- gsub(" ", "_", names(cover_types)[typei])
        fmask <- paste0(outpath, "/tmp_mask_cover_type_", cover_types[typei], "_", pft_name, ".nc")
        message("create mask file ", fmask, " ...")
        if (tilei == 1 && typei == 1) {
            lon_dim <- jsbach_nc$dim$lon
            lat_dim <- jsbach_nc$dim$lat
        }
        mask_var <- ncdf4::ncvar_def(name=paste0("mask_", pft_name), units="", 
                                     dim=list(lon_dim, lat_dim), missval=0)
        outnc <- ncdf4::nc_create(filename=fmask, force_v4=T, vars=mask_var)
        ncdf4::ncvar_put(nc=outnc, varid=mask_var, vals=inds)
        ncdf4::nc_close(outnc)

        # get area in m2
        if (outunit == "area" && tilei == 1 && typei == 1) {
            farea <- paste0(outpath, "/tmp_area_m2.nc")
            cmd <- paste0(cdo, " gridarea ", fmask, " ", farea)
            message("run `", cmd, "` ...")
            system(cmd)
        }
        
        # calc pft fraction with respect to grid cell area = eq 1.5 jsbach docu = 
        # grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
        fouti <- paste0(outpath, "/pft_fract_box_", pft_name, ".nc")
        cnt <- cnt + 1
        fouts[[cnt]] <- list(lev=cover_types[typei],
                             cover_type=cover_types[typei],
                             name=names(cover_types)[typei],
                             fout=fouti, 
                             ncells=ncells, ncells_rel=ncells_rel)
        cmd <- paste0(cdo, 
                      " -setname,cover_fract", # since multiplication with mask file would yield varname of mask file
                      " -setlevel,", cover_types[typei]) # PFT number
        if (outunit == "area") cmd <- paste0(cmd, " -mul ", farea)
        cmd <- paste0(cmd, " -mul ", fmask)
        if (names(cover_types)[typei] == "glacier") { 
            # special case glacier: does not need multiplication with `veg_ratio_max` as
            # "glaciers either cover a grid box fully or are completely absent." jsbach docu 1.3, p. 7
            # --> `veg_ratio_max` is zero where `cover_type` = 1, i.e. glacier
            cmd <- paste0(cmd, " -sellevel,", tilei, " -selname,cover_fract ", fcover_fract)
        } else {
            cmd <- paste0(cmd, " -mul -sellevel,", tilei, " -selname,cover_fract ", fcover_fract, " -selname,veg_ratio_max ", fveg_ratio_max)
        }
        cmd <- paste0(cmd, " ", fouts[[cnt]]$fout)
        message("run `", cmd, "` ...")
        system(cmd)

        # save non-glacier and ocean locations for bare land fraction calculation = `1 - veg_ratio_max`
        if (tilei == 1 && typei == 1) { # declare everywhere
            inds_non_glacier <- inds_ocean <- array(T, dim=dim(inds))
        }
        if (cover_types[typei] == 1) { # set glacier to false
            inds_non_glacier[inds] <- F
        }
        inds_ocean[inds] <- F # set land to false 

        # remove mask file
        if (remove_tmp_files) {
            message("`remove_tmp_files`=T --> rm mask file ", fmask, " ...")
            file.remove(fmask)
        }

    } # for typei in cover_types of current tile

} # for tilei ntiles

## calc bare land fraction
message("\n*********************************************\n",
        "calc bare land = `1 - veg_ratio_max` at non-glacier locations ...")

inds_non_glacier[inds_ocean] <- F # set ocean to NA
ncells <- length(which(inds_non_glacier))
ncells_rel <- ncells/nland*100
message("--> ", ncells, " grid cells = ", round(ncells_rel, 2), " % of ", nland, " land cells") 

# mask file for bare land
fmask <- paste0(outpath, "/tmp_mask_cover_type_non_glacier.nc")
message("create mask file ", fmask, " ...")
mask_var <- ncdf4::ncvar_def(name="mask_non_glacier", units="", 
                             dim=list(lon_dim, lat_dim), missval=0)
outnc <- ncdf4::nc_create(filename=fmask, force_v4=T, vars=mask_var)
ncdf4::ncvar_put(nc=outnc, varid=mask_var, vals=inds_non_glacier)
ncdf4::nc_close(outnc)
        
# calc bare land
fouti <- paste0(outpath, "/pft_fract_box_bare_land.nc")
cnt <- cnt + 1
fouts[[cnt]] <- list(lev=0,
                     cover_type=NA,
                     name=paste0("bare land (1-", veg_ratio_max_varname, ")"),
                     fout=fouti, 
                     ncells=ncells, ncells_rel=ncells_rel)
cmd <- cdo
if (outunit == "area") {
    cmd <- paste0(cmd, " -mul ", farea)
}
cmd <- paste0(cmd,
              " -setname,cover_fract", # since multiplication with mask file would yield varname of mask file
              " -mul ", fmask, 
              " -expr,'cover_fract=1-", veg_ratio_max_varname, "'", # same name as other pfts to get merged
              " -selname,", veg_ratio_max_varname, " ", fveg_ratio_max, 
              " ", fouti)
message("run `", cmd, "` ...")
system(cmd)

# set bare land zaxis to get merged with other pfts along lev dimension
cmd <- paste0(cdo, " zaxisdes ", fouts[[cnt-1]]$fout, " > ", outpath, "/tmp_zaxisdes.txt")
message("run `", cmd, "` ...")
system(cmd)
cmd <- paste0(cdo, 
              " -setlevel,", fouts[[cnt]]$lev,
              " -setzaxis,", outpath, "/tmp_zaxisdes.txt ", fouti, " ", 
              outpath, "/tmp && mv ", outpath, "/tmp ", fouti)
message("run `", cmd, "` ...")
system(cmd)
if (remove_tmp_files) file.remove(paste0(outpath, "/tmp_zaxisdes.txt"))

if (remove_tmp_files) {
    message("`remove_tmp_files`=T --> rm mask file ", fmask, " ...")
    file.remove(fmask)
}

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
              " -setattribute,", varout, "@long_name=\"", long_name, "\"",
              " -setunit,\"", varunit, "\"",
              " -setname,\"", varout, "\"", 
              " -merge ", paste(sapply(fouts, "[[", "fout"), collapse=" "), " ", fout)
message("run `", cmd, "` ...")
system(cmd)
                                                  
# remove old `code`
cmd <- paste0(ncatted, " -O -a code,", varout, ",d,, ", fout)
message("\nrun `", cmd, "` ...")
system(cmd)

# remove tmp files
if (remove_tmp_files) {
    message("\n`remove_tmp_files`=T --> rm merge files ...")
    invisible(file.remove(sapply(fouts, "[[", "fout")))
    if (outunit == "area") file.remove(farea)
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

