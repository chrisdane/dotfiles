# r

# call spheRlab::sl.grid.writeCDO() from sl.grid.writeCDO.R from https://github.com/FESOM/spheRlab.git

rm(list=ls()); graphics.off()

#remotes::install_github("FESOM/spheRlab")
message("load spheRlab package ...")
if (F) {
    library(spheRlab)
} else if (F) { # ollie
    #library(spheRlab, lib="/global/AWIsoft/R/4.1.0/lib/R/library") # 1.1.5
    library(spheRlab, lib="/home/ollie/cdanek/scripts/r/packages/bin/r_4.1") # 1.1.5
}
message("\nloaded spheRlab version ", packageVersion("spheRlab"), 
        " from path ", attr(packageDescription("spheRlab"), "file"))

#fesom2velocities <- T # elem-space
fesom2velocities <- F # node-space
remove.emptylev <- T
if (F) { # fesom1 core
    fesom2 <- F
    gridname <- "core"
    griddir <- "/pool/data/AWICM/FESOM1/MESHES/core"
    outdir <- "/work/ba1103/a270073/mesh/fesom/core"
    rot <- F
    rot.invert <- T; rot.abg <- c(50, 15, -90) # will be ignored if rot=F
} else if (F) { # fesom1 bold
    fesom2 <- F
    gridname <- "bold"
    griddir <- "/pool/data/AWICM/FESOM1/MESHES/bold"
    outdir <- "/work/ba1103/a270073/mesh/fesom/bold"
    rot <- F
    rot.invert <- T; rot.abg <- c(50, 15, -90) # will be ignored if rot=F
} else if (F) { # my CbSCL
    fesom2 <- F
    gridname <- "CbSCL"
    griddir <- "/work/ollie/cdanek/mesh/fesom/CbSCL"
    #griddir <- "/work/ab0246/a270073/mesh/fesom/CbSCL"
    outdir <- griddir
    rot <- T
    rot.invert <- T; rot.abg <- c(50, 15, -90) # will be ignored if rot=F
} else if (F) { # my LSea2
    fesom2 <- F
    gridname <- "LSea2"
    #griddir <- "/work/ollie/cdanek/mesh/fesom/LSea2"
    griddir <- "/work/ab0246/a270073/mesh/fesom/LSea2"
    outdir <- griddir
    rot <- T
    rot.invert <- T; rot.abg <- c(50, 15, -90) # will be ignored if rot=F
} else if (F) { # fesom2 oceanpeak mesh_coast_500km_200m_10km
    fesom2 <- T
    gridname <- "mesh_coast_500km_200m_10km"
    griddir <- "/albedo/work/user/ogurses/MESHES_F2/mesh_coast_500km_200m_10km"
    outdir <- "/albedo/work/user/cdanek/mesh/fesom2/mesh_coast_500km_200m_10km"
    rot <- F
    rot.invert <- T; rot.abg <- c(50, 15, -90) # will be ignored if rot=F
} else if (T) { # core3 cav
    fesom2 <- T
    gridname <- "core3_cav"
    #griddir <- "/albedo/work/user/sharig/mg/MT_v0.1/mesh_CORE3_cav"
    #outdir <- "/albedo/work/user/cdanek/mesh/fesom2/mesh_core3_cav"
    griddir <- "/albedo/work/user/sharig/mg/MT_v0.1/mesh_CORE3_cav/mesh_20251110"
    outdir <- "/albedo/work/user/cdanek/mesh/fesom2/mesh_core3_cav/mesh_20251110"
    rot <- F
    rot.invert <- T; rot.abg <- c(50, 15, -90) # will be ignored if rot=F
}

#######################################################

ofile <- paste0(outdir, "/griddes_", gridname, "_", 
                ifelse(fesom2velocities, "elem", "node"), ".nc")
message("\ncreate griddes for mesh files in\n   ", griddir, 
        "\nand save result to\n   ", ofile, "\n...")

if (file.exists(ofile)) {
    message("ofile \"", ofile, "\" already exists, skip")
} else {
    dir.create(outdir, recursive=T, showWarnings=F)
    if (!dir.exists(outdir)) stop("could not create outdir = ", outdir)

    message("\nstep 1/2: run spheRlab::sl.grid.readFESOM() ...")
    # spheRlab::sl.grid.readFESOM uses Rearth=6371000 m
    grid <- spheRlab::sl.grid.readFESOM(griddir=griddir, 
                                        rot=rot, 
                                        rot.invert=rot.invert, 
                                        rot.abg=rot.abg, 
                                        remove.emptylev=remove.emptylev, 
                                        fesom2=fesom2)

    message("\nstep 2/2: run spheRlab::sl.grid.writeCDO() ...")
    spheRlab::sl.grid.writeCDO(grid, 
                               ofile=ofile, 
                               fesom2velocities=fesom2velocities)
}

message("\nfinished\n")

