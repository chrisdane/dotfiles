#!/usr/bin/env Rscript
   
# get local winter and summer depending on hemisphere
# output has NH winter and summer dates

selmon_nh_winter <- "-selmonth,1,2,3" # jan,feb,mar
selmon_nh_summer <- "-selmonth,8,9,10" # aug,sep,oct
selmon_sh_winter <- selmon_nh_summer
selmon_sh_summer <- selmon_nh_winter

if (interactive()) {
    me <- "fesom1_get_meshinfo.r"
    args <- c("/work/ba1103/a270073/post/GFDL-ESM4/select/mlotst/GFDL-ESM4_historical_r1i1p1f1_GFDL-ESM4_select_mlotst_global_Jan-Dec_1970-2014.nc",
              "/work/ba1103/a270073/post/GFDL-ESM4/select/mlotst/GFDL-ESM4_historical_r1i1p1f1_GFDL-ESM4_select_<varname>_<NH/SH>_<winter/summer>_1970-2014.nc")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage:\n",
                " $ ", me, " fin \"fout_with_<varname>_and_<NH/SH>_and_<winter/summer>_strings\"\n",
                "\n")

# check
if (length(args) != 2) {
    message(usage)
    quit()
}
fin <- args[1]
fout <- args[2]
if (!file.exists(fin)) stop("input file ", fin, " does not exist")
if (!grepl("<varname>", fout)) stop("fout must have string \"<varname>\"")
if (!grepl("<NH/SH>", fout)) stop("fout must have string \"<NH/SH>\"")
if (!grepl("<winter/summer>", fout)) stop("fout must have string \"<winter/summer>\"")
cdo <- Sys.which("cdo")
if (cdo == "") stop("could not find cdo")

# get varnames of file
message("\nget varnames of input file with `cdo showname` ...")
varnames <- strsplit(trimws(system(paste0(cdo, " -s showname ", fin), intern=T)), " ")[[1]]
message("--> ", paste(varnames, collapse=", "))

# get local winter summer for all variables
for (vi in seq_along(varnames)) {
    message("\n******************************************************\n",
            "vari ", vi, "/", length(varnames), ": ", varnames[vi], " ...")

    # get nlon and nlat of variable
    cmd <- paste0(cdo, " -s -griddes -select,name=", varnames[vi], " ", fin)
    message("get nlon/nlat: `", cmd, "` ...")
    griddes <- system(cmd, intern=T)
    xsize <- griddes[grepl("^xsize", griddes)]
    xsize <- strsplit(xsize, "=")[[1]]
    xsize <- trimws(xsize[length(xsize)])
    ysize <- griddes[grepl("^ysize", griddes)]
    ysize <- strsplit(ysize, "=")[[1]]
    ysize <- trimws(ysize[length(ysize)])
    xfirst <- griddes[grepl("^xfirst", griddes)]
    xfirst <- strsplit(xfirst, "=")[[1]]
    xfirst <- trimws(xfirst[length(xfirst)])    
    xinc <- griddes[grepl("^xinc", griddes)]
    xinc <- strsplit(xinc, "=")[[1]]
    xinc <- trimws(xinc[length(xinc)])    
    message("--> xsize,ysize,xfirst,xinc = ", xsize, ",", ysize, ",", xfirst, ",", xinc)
    xsize <- as.integer(xsize)
    ysize <- as.integer(ysize)
    xfirst <- as.numeric(xfirst)
    xinc <- as.numeric(xinc)
    xend <- xfirst+xsize*xinc # more than actual max(lon) due to (from cdo -h sellonlatbox):
    # sellonlatbox  Select a longitude/latitude box
    #     Selects grid cells inside a lon/lat box. The user must specify the longitude and latitude of the edges of the box.
    #     Only those grid cells are considered whose grid center lies within the lon/lat box.
    sellonlatbox_nh <- paste0("-sellonlatbox,", xfirst, ",", xend, ",0,90")
    sellonlatbox_sh <- paste0("-sellonlatbox,", xfirst, ",", xend, ",-90,0")
    message("--> sellonlatbox_nh = ", sellonlatbox_nh)
    message("--> sellonlatbox_sh = ", sellonlatbox_sh)
    
    # get local nh winter
    nh_winter <- sub("<varname>", varnames[vi], fout)
    nh_winter <- sub("<NH/SH>", "NH", nh_winter)
    nh_winter <- sub("<winter/summer>", "winter", nh_winter)
    cmd <- paste0(cdo, " -s ", sellonlatbox_nh, " ", selmon_nh_winter, " ", fin, " ", nh_winter)
    message("get nh winter: `", cmd, "` ...")
    
    # check outpath once
    dir.create(dirname(nh_winter), recursive=T, showWarnings=F)
    if (!dir.exists(dirname(nh_winter))) stop("could not create outpath ", dirname(nh_winter))
    
    check <- system(cmd)
    if (check != 0) stop("cmd failed")

    # get local nh summer
    nh_summer <- sub("<varname>", varnames[vi], fout)
    nh_summer <- sub("<NH/SH>", "NH", nh_summer)
    nh_summer <- sub("<winter/summer>", "summer", nh_summer)
    cmd <- paste0(cdo, " -s ", sellonlatbox_nh, " ", selmon_nh_summer, " ", fin, " ", nh_summer)
    message("get nh summer: `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    
    # get local sh winter
    sh_winter <- sub("<varname>", varnames[vi], fout)
    sh_winter <- sub("<NH/SH>", "SH", sh_winter)
    sh_winter <- sub("<winter/summer>", "winter", sh_winter)
    cmd <- paste0(cdo, " -s ", sellonlatbox_sh, " ", selmon_sh_winter, " ", fin, " ", sh_winter)
    message("get sh winter: `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    
    # get local sh summer
    sh_summer <- sub("<varname>", varnames[vi], fout)
    sh_summer <- sub("<NH/SH>", "SH", sh_summer)
    sh_summer <- sub("<winter/summer>", "summer", sh_summer)
    cmd <- paste0(cdo, " -s ", sellonlatbox_sh, " ", selmon_sh_summer, " ", fin, " ", sh_summer)
    message("get sh summer: `", cmd, "` ...")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")

    # collgrid winter and summer of both hemispheres
    winter <- sub("<varname>", varnames[vi], fout)
    winter <- sub("<NH/SH>", "global", winter)
    winter <- sub("<winter/summer>", "winter", winter)
    cmd <- paste0(cdo, " -s collgrid ", nh_winter, " ", sh_winter, " ", winter)
    message("get winter: `", cmd, "` ...")
    if (file.exists(winter)) stop("winter fout ", winter, " already exists")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
    
    summer <- sub("<varname>", varnames[vi], fout)
    summer <- sub("<NH/SH>", "global", summer)
    summer <- sub("<winter/summer>", "summer", summer)
    cmd <- paste0(cdo, " -s collgrid ", nh_summer, " ", sh_summer, " ", summer)
    message("get summer: `", cmd, "` ...")
    if (file.exists(summer)) stop("summer fout ", summer, " already exists")
    check <- system(cmd)
    if (check != 0) stop("cmd failed")
        
    invisible(file.remove(c(nh_winter, sh_winter, nh_summer, sh_summer)))

} # for vi




