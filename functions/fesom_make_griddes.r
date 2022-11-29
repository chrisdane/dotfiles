# r

rm(list=ls()); graphics.off()

#remotes::install_github("FESOM/spheRlab")
#library(spheRlab, lib="/global/AWIsoft/R/4.1.0/lib/R/library") # 1.1.5
library(spheRlab, lib="/home/ollie/cdanek/scripts/r/packages/bin/r_4.1") # 1.1.5
message("\nloaded sheRlab ", packageVersion("spheRlab"), " from ", attr(packageDescription("spheRlab"), "file"))

if (F) { # my CbSCL mesh
    griddir <- "/work/ollie/cdanek/mesh/fesom/CbSCL"
    #griddir <- "/work/ab0246/a270073/mesh/fesom/CbSCL"
    ofile <- paste0(griddir, "/griddes_CbSCL.nc")
    fesom2 <- F # false for fesom1, true for fesom2
    rot <- T
    rot.invert <- T
    rot.abg <- c(50, 15, -90)
    remove.emptylev <- T
    fesom2velocities <- fesom2
} else if (T) { # my LSea2 mesh
    griddir <- "/work/ollie/cdanek/mesh/fesom/LSea2"
    ofile <- paste0(griddir, "/griddes_LSea2.nc")
    fesom2 <- F # false for fesom1, true for fesom2
    rot <- T
    rot.invert <- T
    rot.abg <- c(50, 15, -90)
    remove.emptylev <- T
    fesom2velocities <- fesom2
}

#######################################################

if (file.exists(ofile)) {
    message("ofile \"", ofile, "\" already exists, skip")
} else {
    dir.create(dirname(ofile), recursive=T, showWarnings=F)
    if (!dir.exists(dirname(ofile))) stop("could not create outdir = ", dirname(ofile))

    message("\nrun spheRlab::sl.grid.readFESOM() ...")
    grid <- spheRlab::sl.grid.readFESOM(griddir=griddir, rot=rot, rot.invert=rot.invert, rot.abg=rot.abg, remove.emptylev=remove.emptylev, fesom2=fesom2)

    message("\nrun spheRlab::sl.grid.writeCDO() ...")
    spheRlab::sl.grid.writeCDO(grid, ofile=ofile, fesom2velocities=fesom2velocities)
}

message("\nfinished\n")

