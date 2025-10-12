#!/usr/bin/env Rscript
    
# get jsbach PFT fraction or area with respect to grid area and not vegetated area (=`cover_fract`)
# --> calc eq 1.5 from jsbach docu (Reick et al. 2021):
#       c_i = v_i / V_veg = f_i / veg_max
#   <=> f_i = c_i * veg_max --> this is calculated here (times area if wanted)
# with c_i = cover fraction relative to the vegetated part of grid box
#      f_i = cover fraction relative to the full grid box
#      v_i = area covered by PFT associated with tile i in a grid box (m2)
#      V_veg = area of grid box accessible to vegetation = A veg_max
#      veg_max = fraction of grid box hospitable to vegetation
#      A = area of grid box

# - several `cover_type`s are saved on one tile in cover_fract(location,tile,time)
# - see reference at the end of script
# todo: what represents fldsum of outunit = fraction? 
# todo: jsbach's `cover_fract` uses binary jsbach:slm instead of fractional jsbach:slf
#       --> in T63, `cover_fract` at panama strait (`cdo remapnn,lon=279.375/lat=8.393669`) has no values because slm = 0 but slf != 0
#       --> is this a bug or a feature?

##########################################################################################

warn <- getOption("warn")
#options(warn=2) # stop on warnings    
options(warn=0)

if (interactive()) { # test
    if (T) {
        args <- c("--jsbach=/work/ab0995/from_Mistral/ab0995/a270046/meshes_default/core/tarfilesT63/input/jsbach/jsbach_T63CORE2_11tiles_5layers_1850.nc",
                  "--cover_fract=/work/ba1103/a270073/post/jsbach/select/cover_fract/awi-esm-1-1-lr_kh800_historical2_jsbach_select_cover_fract_global_Jan-Dec_1850-2014.nc",
                  "--veg_ratio_max=/work/ba1103/a270073/post/jsbach/select/veg_ratio_max/awi-esm-1-1-lr_kh800_historical2_jsbach_select_veg_ratio_max_global_Jan-Dec_1850-2014.nc",
                  "--outunit=area",
                  "--fout=/work/ba1103/a270073/post/jsbach/select/pft_area_box/awi-esm-1-1-lr_kh800_historical2_jsbach_select_pft_area_box_global_Jan-Dec_1850-2014.nc",
                  "--remove_tmp_files=T")
    }

} else {

    # get args
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    help <- paste0("\nGet jsbach PFT fraction or area with respect to grid area and not vegetated area (=`cover_fract`), i.e. calc eq. 1.5 from Reick et al. 2021\n\n",
                   "If `outunit` = \"fract\":\n",
                   "  if glacier (`cover_type`=1)\n",
                   "    pft_fract_box(lon,lat,pftlev,time) = pft_mask(lon,lat,pftlev) * cover_fract(lon,lat,tilelev,time)\n",
                   "  else\n",
                   "    pft_fract_box(lon,lat,pftlev,time) = pft_mask(lon,lat,pftlev) * cover_fract(lon,lat,tilelev,time) * veg_ratio_max(lon,lat,time)\n",
                   "If `outunit` = \"area\":\n",
                   "  pft_area_box(lon,lat,pftlev,time) = same as in `outunit` = \"fract\" case but RHS multiplied with `area_m2(lon,lat)`\n",
                   "\n",
                   "`cdo fldsum` of the result of the `outunit=\"{fract,area}\"`-case divided by the {number of land cells,total land area} and muliplied by 100 gives the percentage of pft wrt global land.\n",
                   "\n", 
                   "Usage:\n",
                   " $ ", me, " ",
                   "--jsbach=/path/to/input/jsbach.nc ",
                   "--cover_fract=/path/to/cover_fract.nc ",
                   "[--cover_fract_varname=cover_fract] ",
                   "--veg_ratio_max=/path/to/veg_ratio_max.nc ",
                   "[--veg_ratio_max_varname=veg_ratio_max] ",
                   "--fout=/path/to/outputfile ",
                   "[--outunit=area] ",
                   "[--remove_tmp_files=T]\n",
                   "\n",
                   "e.g. --jsbach=/work/ab0995/from_Mistral/ab0995/a270046/meshes_default/core/tarfilesT63/input/jsbach/jsbach_T63CORE2_11tiles_5layers_1850.nc\n")

    # check args 
    args <- commandArgs(trailingOnly=T)
    if (length(args) < 4) {
        message(help)
        quit()
    }
} # interactive or not

# check
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
    outunit <- "area" # default; "fract" or "area"
}
if (any(grepl("--remove_tmp_files", args))) {
    remove_tmp_files <- sub("--remove_tmp_files=", "", args[grep("--remove_tmp_files=", args)])
} else {
    remove_tmp_files <- T # default
}

host <- Sys.info()["nodename"]
if (grepl("mlogin", host)) stop("switch to mistralpp")

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

if (outunit == "fract") {
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

message("`which ncrename` = ", appendLF=F)
ncrename <- Sys.which("ncrename")
if (ncatted == "") stop("could not find ncrename")
message(ncrename)

message("`which ncap2` = ", appendLF=F)
ncap2 <- Sys.which("ncap2")
if (ncap2 == "") stop("could not find ncap2")
message(ncap2)

message("load ncdf4 package ...")
library(ncdf4)

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
    message("--> found cover_fract varname = \"", cover_fract_varname, "\"")
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
    message("--> found veg_ratio_max varname = \"", veg_ratio_max_varname, "\"")
}

# open jsbach input file
message("\nopen input file ", fjsbach, " ...")
jsbach_nc <- ncdf4::nc_open(fjsbach)

# check jsbach input file variables
jsbach_nc_vars <- names(jsbach_nc$var)
if (!any(!is.na(match(c("slm", "SLM"), jsbach_nc_vars)))) {
    stop("jsbach input file does not have \"slm\" or \"SLM\" variable")
}
if (!any(!is.na(match(c("cover_type"), jsbach_nc_vars)))) {
    stop("jsbach input file does not have \"cover_type\" variable")
}

## start
random_number <- paste0(Sys.getpid(), "_", format(Sys.time(), "%s"))

# get number of land points from binary land sea mask `slm`
# --> slm is either 1 (land) or 0 (ocean)
# --> todo: should slf be used instead?
slm_ind <- which(!is.na(match(names(jsbach_nc$var), c("slm", "SLM"))))
if (length(slm_ind) != 1) {
    stop("this input file has ", length(slm_ind), " vars named `slm` or `SLM`. must have 1")
}
slm_varname <- names(jsbach_nc$var)[slm_ind]
slm <- ncdf4::ncvar_get(jsbach_nc, slm_varname)
ntotal <- prod(dim(slm)) # e.g. 18432
nland <- length(which(slm == 1)) # e.g. 6126 (33.24%)
nocean <- ntotal - nland # e.g. 12306 (66.76%)
message("--> total grid cells = ", ntotal, "\n",
        "--> land grid cells = ", nland, " (", round(nland/ntotal*100, 2), "%)\n",
        "--> ocean grid cells = ", nocean, " (", round(nocean/ntotal*100, 2), "%)")
        
# get area in m2
if (outunit == "area") {
    message("\n`outunit` = area")

    farea <- paste0(outpath, "/tmp_area_m2_", random_number, ".nc")
    cmd <- paste0(cdo, " -gridarea -select,name=", slm_varname, " ", fjsbach, " ", farea)
    message("run `", cmd, "` ...")
    system(cmd)

    fslm <- paste0(outpath, "/tmp_slm_", random_number, ".nc")
    cmd <- paste0(cdo, " -select,name=", slm_varname, " ", fjsbach, " ", fslm)
    message("run `", cmd, "` ...")
    system(cmd)

    cmd <- paste0(cdo, " -s -output -fldsum ", farea)
    message("run `", cmd, "` ...")
    areatotal <- system(cmd, intern=T)
    areatotal <- as.numeric(trimws(areatotal)) # e.g. 5.10064e+14 m2
    cmd <- paste0(cdo, " -s -output -fldsum -mul ", farea, " ", fslm) #
    message("run `", cmd, "` ...")
    arealand <- system(cmd, intern=T)
    arealand <- as.numeric(trimws(arealand)) # e.g. 1.44686e+14 m2 (28.37%) 
    areaocean <- areatotal - arealand # e.g. 3.65378e+14 m2 (71.63%)

    message("--> total area = ", areatotal, "\n",
            "--> land area = ", arealand, " (", round(arealand/areatotal*100, 2), "%)\n",
            "--> ocean area = ", areaocean, " (", round(areaocean/areatotal*100, 2), "%)")
} # if outunit == "area"

# get number of tiles
if (!any(names(jsbach_nc$dim) == "ntiles")) {
    stop("this input file does not have a `ntiles` dim")
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
    stop("this input file does not have variable `cover_type`")
}
cover_type_atts <- ncdf4::ncatt_get(jsbach_nc, "cover_type")
lcts <- names(cover_type_atts) # e.g. "lct01" or "lct21"
if (!any(grepl("^lct", lcts))) {
    stop("not any attribute of variable `cover_type` starts with \"lct\"")
}
lcts <- lcts[which(grepl("^lct", lcts))]
lct_vals <- lct_types <- rep(NA, times=length(lcts))
for (lcti in seq_along(lcts)) {
    lct_vals[lcti] <- as.integer(sub("^lct", "", lcts[lcti])) # strip "lct" from "lct01"
    lct_types[lcti] <- cover_type_atts[[lcts[lcti]]]
}
lct <- data.frame(cover_type=lct_vals, 
                  name=lct_types, stringsAsFactors=F)
message("--> in this input file, there are ", length(lcts), 
        " _possible_ `cover_type`", ifelse(length(lcts) > 1, "s", ""), " defined:")
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
npfts <- length(unlist(tiles))
message("\n--> in this input file, ", npfts, " _actual_ `cover_type`",
        ifelse(length(unlist(tiles)) > 1, "s", ""), " are saved at `ntiles`=", 
        ntiles, " tiles:")
cat(capture.output(str(tiles)), sep="\n")
npfts <- npfts + 1 # +1 for bare land
message("--> +1 for bare land --> npfts = ", npfts)

# nc dims for output
lon_dim <- jsbach_nc$dim$lon
lat_dim <- jsbach_nc$dim$lat
pft_dim <- ncdf4::ncdim_def(name="pft", units="#", vals=seq_len(npfts))
                            
# calc pft fraction with respect to grid area for all pfts
fouts <- list()
pfti <- 0 # pft counter
for (tilei in seq_len(ntiles)) {
    
    cover_types <- tiles[[tilei]]
    message("\n*********************************************\n",
            "tile ", tilei, "/", ntiles, " has ", 
            length(cover_types), " `cover_type`s:")
    print(cover_types)

    for (typei in seq_along(cover_types)) {
        
        pfti <- pfti + 1

        # create mask file of current cover_type of current tile
        message("\ntype ", typei, "/", length(cover_types), ": get location inds where `cover_type` = ", cover_types[typei], 
                " (\"", names(cover_types)[typei], "\") ...")
        cmd <- rep(",", times=length(dim(cover_type)))
        cmd[ntiles_dim_ind] <- tilei
        cmd <- paste0("inds <- cover_type[", paste(cmd, collapse=""), ",drop=T] == ", cover_types[typei])
        message("run `", cmd, "` ...")
        eval(parse(text=cmd))
        if (!any(inds)) stop("this should not happen")
        ncells <- length(which(inds))
        ncells_rel <- ncells/nland*100
        message("--> ", ncells, " grid cells = ", round(ncells_rel, 2), " % of ", nland, " land cells") 
        pft_name <- gsub(" ", "_", names(cover_types)[typei])
        fmask <- paste0(outpath, "/tmp_mask_cover_type_", cover_types[typei], "_", pft_name, "_", random_number, ".nc")
        message("create mask file ", fmask, " ...")
        mask_var <- ncdf4::ncvar_def(name=paste0("mask_", pft_name), units="", 
                                     dim=list(lon_dim, lat_dim), missval=0)
        outnc <- ncdf4::nc_create(filename=fmask, force_v4=T, vars=mask_var)
        ncdf4::ncvar_put(nc=outnc, varid=mask_var, vals=inds)
        ncdf4::nc_close(outnc)

        # todo: issue mask(t=1) * mask(t=1) * data(t=n) = result(t=1)
        # but this works: mask(t=1) * data(t=n) = result(t=n)
        if (T && outunit == "area") {
            cmd <- paste0(cdo, " mul ", fmask, " ", farea, " tmp_", random_number, " && mv tmp_", random_number, " ", fmask)
            message("todo run `", cmd, "` ...")
            check <- system(cmd)
            if (check != 0) stop("error")
        }

        # calc pft fraction with respect to grid cell area = eq 1.5 jsbach docu = 
        # grid_area(j) * cover_fract(i,j,k) * veg_ratio_max(i,j)
        fouti <- paste0(outpath, "/tmp_pft_fract_box_", pft_name, "_", random_number, ".nc")
        fouts[[pfti]] <- list(#lev=cover_types[typei],
                             pft=pfti,
                             cover_type=cover_types[typei],
                             name=names(cover_types)[typei],
                             fout=fouti, 
                             ncells=ncells, ncells_rel=ncells_rel)
        cmd <- paste0(cdo, 
                      " -setname,cover_fract", # since multiplication with mask file would yield varname of mask file
                      #" -setlevel,", cover_types[typei]
                      " -setlevel,", pfti
                      ) 
        if (F && outunit == "area") cmd <- paste0(cmd, " -mul ", farea) # todo: can be re-enabled when fmask*fmask*fdata issue above is solved
        cmd <- paste0(cmd, " -mul ", fmask)
        if (names(cover_types)[typei] == "glacier") { 
            # special case glacier: does not need multiplication with `veg_ratio_max` as
            # "glaciers either cover a grid box fully or are completely absent." jsbach docu 1.3, p. 7
            # --> `veg_ratio_max` is zero where `cover_type` = 1, i.e. glacier
            cmd <- paste0(cmd, " -sellevel,", tilei, " -selname,cover_fract ", fcover_fract)
        } else {
            cmd <- paste0(cmd, " -mul -sellevel,", tilei, " -selname,cover_fract ", fcover_fract, " -selname,veg_ratio_max ", fveg_ratio_max)
        }
        cmd <- paste0(cmd, " ", fouts[[pfti]]$fout)
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
fmask <- paste0(outpath, "/tmp_mask_cover_type_non_glacier_", random_number, ".nc")
message("create mask file ", fmask, " ...")
mask_var <- ncdf4::ncvar_def(name="mask_non_glacier", units="", 
                             dim=list(lon_dim, lat_dim), missval=0)
outnc <- ncdf4::nc_create(filename=fmask, force_v4=T, vars=mask_var)
ncdf4::ncvar_put(nc=outnc, varid=mask_var, vals=inds_non_glacier)
ncdf4::nc_close(outnc)

# todo: issue mask(t=1) * mask(t=1) * data(t=n) = result(t=1)
# but this works: mask(t=1) * data(t=n) = result(t=n)
if (T && outunit == "area") {
    cmd <- paste0(cdo, " mul ", fmask, " ", farea, " tmp_", random_number, " && mv tmp_", random_number, " ", fmask)
    message("todo run `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("error")
}
        
# calc bare land
fouti <- paste0(outpath, "/tmp_pft_fract_box_bare_land_", random_number, ".nc")
pfti <- pfti + 1
fouts[[pfti]] <- list(#lev=0,
                      pft=pfti,
                      cover_type=NA,
                      name=paste0("bare land (=1-", veg_ratio_max_varname, ")"),
                      fout=fouti, 
                      ncells=ncells, ncells_rel=ncells_rel)
cmd <- cdo
if (F && outunit == "area") cmd <- paste0(cmd, " -mul ", farea) # todo: can be re-enabled when fmask*fmask*fdata issue above is solved
cmd <- paste0(cmd,
              " -setname,cover_fract", # since multiplication with mask file would yield varname of mask file
              " -mul ", fmask, 
              " -expr,'cover_fract=1-", veg_ratio_max_varname, "'", # same name as other pfts to get merged
              " -selname,", veg_ratio_max_varname, " ", fveg_ratio_max, 
              " ", fouti)
message("run `", cmd, "` ...")
system(cmd)

# set bare land zaxis to be able to be merged with other pfts along lev dimension
cmd <- paste0(cdo, " zaxisdes ", fouts[[pfti-1]]$fout, " > ", outpath, "/tmp_zaxisdes_", random_number, ".txt")
message("run `", cmd, "` ...")
system(cmd)
cmd <- paste0(cdo, 
              #" -setlevel,", fouts[[pfti]]$lev,
              " -setlevel,", fouts[[pfti]]$pft,
              " -setzaxis,", outpath, "/tmp_zaxisdes_", random_number, ".txt ", fouti, " ", 
              outpath, "/tmp_", random_number, " && mv ", outpath, "/tmp_", random_number, " ", fouti)
message("run `", cmd, "` ...")
system(cmd)
if (remove_tmp_files) file.remove(paste0(outpath, "/tmp_zaxisdes_", random_number, ".txt"))

if (remove_tmp_files) {
    message("`remove_tmp_files`=T --> rm mask file ", fmask, " ...")
    file.remove(fmask)
}

# merge pfts together
message("\n**********************************\n",
        "merge ", length(fouts), " files together ...")
atts <- paste0(#"@lev", sapply(fouts, "[[", "lev"), "=\"",
               "@pft", sapply(fouts, "[[", "pft"), "=\"",
               "name=", sapply(fouts, "[[", "name"), 
               "; cover_type=", sapply(fouts, "[[", "cover_type"), 
               "; ncells=", sapply(fouts, "[[", "ncells"), 
               "\"")
cmd <- paste0(cdo,
              " -setattribute,", varout, "@jsbach=", fjsbach,
              " -setattribute,", varout, "@slm_nocean=", nocean,
              " -setattribute,", varout, "@slm_nland=", nland,
              " -setattribute,", varout, "@slm_ntotal=", ntotal)
if (outunit == "area") {
    cmd <- paste0(cmd,
                  " -setattribute,", varout, "@slm_areaocean=", areaocean,
                  " -setattribute,", varout, "@slm_arealand=", arealand,
                  " -setattribute,", varout, "@slm_areatotal=", areatotal)
}
cmd <- paste0(cmd,
              " ", paste(rev(paste0("-setattribute,", varout, atts)), collapse=" "), 
              " -setattribute,", varout, "@long_name=\"", long_name, "\"",
              " -setunit,\"", varunit, "\"",
              " -setname,\"", varout, "\"", 
              " -merge ", paste(sapply(fouts, "[[", "fout"), collapse=" "), " ", fout)
message("run `", cmd, "` ...")
system(cmd)

# rename generic dim- amd varname sfc from `cdo merge` to pft
cmd <- paste0(ncrename, " -O -d sfc,pft -v sfc,pft ", fout)
message("\nrun `", cmd, "` ...")
system(cmd)

# set proper pft dim values
# --> todo: overwriting -O does not work; results in `ncap2: ERROR ncap_cst_mk(): Unrecognized dimension "pft" in LHS subscripts`
cmd <- paste0(ncap2, " -s 'pft[$pft]={", paste(seq_len(npfts), collapse=","), "}' ", fout, " ", fout, "_tmp && mv ", fout, "_tmp ", fout)
message("\nrun `", cmd, "` ...")
system(cmd)

# remove tmp files
if (remove_tmp_files) {
    message("\n`remove_tmp_files`=T --> rm merge files ...")
    invisible(file.remove(sapply(fouts, "[[", "fout")))
    if (outunit == "area") invisible(file.remove(c(farea, fslm)))
}

# finished
options(warn=warn) # back to default
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

