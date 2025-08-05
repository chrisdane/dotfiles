#!/usr/bin/env Rscript

if (interactive()) { # test
    rm(list=ls()); graphics.off()
    me <- "jsbach_tile2pft.r"
    args <- "/work/ab1095/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/run_20140101-20141231/work/lctlib.def"
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
} # if interactive or not

options(warn=2) # stop on warnings    

helpstring <- paste(rep(" ", t=nchar(me)), collapse="")
help <- paste0("\nUsage:\n",
               " $ ", me, " <lctlib.def>\n",
               "\n")

# checks
if (length(args) != 1) {
    message(help)
    quit()
}

flctlib <- args[1]
if (!file.exists(flctlib)) stop("lctlib file ", flctlib, " does not exist")

fout <- paste0("~/", basename(flctlib), ".csv")
if (file.exists(fout)) stop("fout ", fout, " already exists")

message("read lctlib file ", flctlib, " ...")
lctlib <- readLines(flctlib)

land_cover_types_all <- lctlib[grep("^#\\s+[0-9]+:", lctlib)] # e.g. "#  1: Glacier" or "# 10: Raingreen shrubs"
tmp <- strsplit(trimws(land_cover_types_all), ": ")
tmp1 <- sapply(tmp, "[[", 1) # "#  1" or "# 10"
tmp1 <- as.integer(gsub("^#\\s+", "", tmp1))
tmp2 <- sapply(tmp, "[[", 2)
land_cover_types_no <- tmp1
land_cover_types_name <- tmp2

df <- data.frame(lct_no=land_cover_types_no, lct=land_cover_types_name)
message("\nget info about ", length(land_cover_types_no), " possible land cover types ...")
print(df, width=200)

single_lines <- c("LctNumber", # Landcover type index numbers (not used in the model) 
                  "LandcoverClass", # LandcoverClass (bare soil: 0; glacier: 1; lake: 2; natural forest: 3; natural grassland: 4; other natural vegetation: 5; crops: 6; pastures: 7) 
                  "PhenologyType", # Phenology type (none: 0; evergreen: 1; summergreen: 2; raingreen: 3; grass: 4; crop: 5)
                  "NitrogenScalingFlag", # Is nitrogen scaling needed? (no: 0; yes: 1)
                  "C4flag", 
                  "DYNAMIC_PFT", # Flag to indicate those PFTs that are subject to dynamics
                  "WOODY_PFT", 
                  "PASTURE_PFT") 
for (li in seq_along(single_lines)) {
    tmp <- lctlib[grep(paste0("^", single_lines[li]), lctlib)]
    tmp <- trimws(sub(paste0("^", single_lines[li]), "", tmp))
    tmp <- strsplit(tmp, " ")[[1]]
    tmp <- as.numeric(tmp)
    df[,single_lines[li]] <- tmp
}
print(df, width=500)

message("\nsave fout ", fout, " ...")
write.table(df, fout, row.names=F, quote=F, sep=",")

