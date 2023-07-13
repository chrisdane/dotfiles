#!/usr/bin/env Rscript

rm(list=ls()); graphics.off()

if (interactive()) {
    me <- "fesom1_landice2nodes.r"
    args <- c("nod2d.out=/work/ba1103/a270073/mesh/fesom/core/nod2d.out",
              "landice_nodes=/work/ba1103/a270073/mesh/fesom/core/landice_nodes_in_region_1.out")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage:\n $ ", me, 
                " nod2d.out=/path/to/nod2d.out landice_nodes=/path/to/landice_nodes_in_region_1.out\n",
                "\n",
                " with e.g. nod2d.out=/pool/data/AWICM/FESOM1/MESHES/core/nod2d.out\n",
                "           landice_nodes=/work/ba1103/a270073/forcing/FESOM1/lgmf_0.2/landice_nodes_in_region_1.out\n")

# check
if (length(args) != 2) { # nod2d.out, landice_nodes
    message(usage)
    quit()
}
message()

if (any(grepl("^nod2d.out=", args))) {
    ind <- which(grepl("^nod2d.out=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"nod2d.out=\". must be 1")
    nod2d.out <- sub("nod2d.out=", "", args[ind])
    args <- args[-ind]
} else {
    stop("provide `nod2d.out=/path/to/nod2d.out")
}
if (file.access(nod2d.out, mode=0) == -1) { # not existing
    stop("provided nod2d.out = \"", nod2d.out, "\" does not exist")
}
if (file.access(nod2d.out, mode=4) == -1) { # not readable
    stop("provided nod2d.out = \"", nod2d.out, "\" not readable.")
}
nod2d.out <- normalizePath(nod2d.out)
message("nod2d.out = ", nod2d.out)

if (any(grepl("^landice_nodes=", args))) {
    ind <- which(grepl("^landice_nodes=", args))
    if (length(ind) != 1) stop("found ", length(ind), " args that start with \"landice_nodes=\". must be 1")
    landice_nodes <- sub("landice_nodes=", "", args[ind])
    args <- args[-ind]
} else {
    stop("provide `landice_nodes=/path/to/landice_nodes_in_region_1.out")
}
if (file.access(landice_nodes, mode=0) == -1) { # not existing
    stop("provided landice_nodes = \"", landice_nodes, "\" does not exist")
}
if (file.access(landice_nodes, mode=4) == -1) { # not readable
    stop("provided landice_nodes = \"", landice_nodes, "\" not readable.")
}
landice_nodes <- normalizePath(landice_nodes)
message("landice_nodes = ", landice_nodes)

################

message("read ", nod2d.out, " ...")
n2 <- base::scan(nod2d.out, what=integer(), n=1, quiet=T)
nod2d <- base::scan(nod2d.out, skip=1, quiet=T)
nod2d <- matrix(nod2d, nrow=n2, byrow=T)
colnames(nod2d) <- c("no", "lon", "lat", "coast")

message("read ", landice_nodes, " ...")
nfw <- base::scan(landice_nodes, what=integer(), n=1, quiet=T)
inds <- base::scan(landice_nodes, skip=1, quiet=T)

plotname <- paste0("~/dotfiles/functions/landice2nodes_", basename(dirname(landice_nodes)), ".png")
message("plot ", nfw, " freshwater locations on ", n2, " surface nodes: ", plotname, " ...")
if (!interactive()) png(plotname, width=1500, height=1500, res=200)
plot(nod2d[,"lon"], nod2d[,"lat"], pch=".", 
     xlab="lon", ylab="lat", yaxt="n",
     main=paste0("landice_nodes ", basename(dirname(landice_nodes))))
axis(2, las=2)
points(nod2d[inds,"lon"], nod2d[inds,"lat"], col=2)
if (!interactive()) dev.off()

if (F) {
    inds <- which(nod2d[,"coast"] == 1 &
                  nod2d[,"lat"] < -60)
    points(nod2d[inds,"lon"], nod2d[inds,"lat"], col=3)
    fout <- paste0("landice_", c("nodes", "yearly"), "_sofia_antwater_0.1_Sv_3155.76_Gt_yr-1.out")
    write(length(inds), file=fout[1])
    write(inds, file=fout[1], ncolumns=1, append=T)
    write(1, file=fout[2])
    write(3155.76, file=fout[2], append=T) # 0.1 Sv = 3155.76 Gt freshwater year-1 with 365.25 days per year
}

message("\nfinished\n")

