#!/usr/bin/env Rscript

if (interactive()) {
    me <- "fesom1_get_res_from_griddes.r"
    args <- c("--node=/work/ba1103/a270073/mesh/fesom/core/griddes_core_node.nc",
              "--elem=/work/ba1103/a270073/mesh/fesom/core/griddes_core_elem.nc",
              "--pathout=/work/ba1103/a270073/mesh/fesom/core")
} else {
    args <- commandArgs(trailingOnly=F)
    me <- basename(sub("--file=", "", args[grep("--file=", args)]))
    args <- commandArgs(trailingOnly=T)
}

usage <- paste0("\nUsage:\n",
                " $ ", me, " --node=/path/to/griddes_node --elem=/path/to/griddes_elem --pathout=/path/where/dmax_{node,elem}.nc/will/be/saved\n",
                "\n",
                " either --node or --elem or both\n",
                "\n")

# check
if (length(args) == 0 || length(args) > 3) {
    message(usage)
    quit()
}

griddes_node <- griddes_elem <- NULL
if (any(grepl("--node", args))) {
    griddes_node <- sub("--node=", "", args[grep("--node=", args)])
}
if (any(grepl("--elem", args))) {
    griddes_elem <- sub("--elem=", "", args[grep("--elem=", args)])
}
if (is.null(griddes_node) && is.null(griddes_elem)) stop(usage)
if (!is.null(griddes_node)) {
    if (!file.exists(griddes_node)) stop("griddes_node ", griddes_node, " does not exist")
}
if (!is.null(griddes_elem)) {
    if (!file.exists(griddes_elem)) stop("griddes_elem ", griddes_elem, " does not exist")
}
if (any(grepl("--pathout", args))) {
    pathout <- sub("--pathout=", "", args[grep("--pathout=", args)])
    dir.create(pathout, recursive=T, showWarnings=F)
    if (!dir.exists(pathout)) stop("could not create pathout")
} else {
    stop("provide pathout")
}

# calc resolution from Danilov 2022: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2022MS003177
# --> dmax = sqrt(2*area_elem) ~ sqrt(area_node)
if (!is.null(griddes_node)) {
    fout_node <- paste0(pathout, "/dmax_node.nc")
    if (file.exists(fout_node)) {
        message("fout on nodes ", fout_node, " does already exist. skip")
    } else {
        cmd <- paste0("cdo -setunit,km -expr,'dmax=sqrt(cell_area)/1e3' ", griddes_node, " ", fout_node)
        message("\nrun `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error: ", check)
    }
}

if (!is.null(griddes_elem)) {
    fout_elem <- paste0(pathout, "/dmax_elem.nc")
    if (file.exists(fout_elem)) {
        message("fout on elems ", fout_elems, " does already exist. skip")
    } else {
        cmd <- paste0("cdo -setunit,km -expr,'dmax=sqrt(2*cell_area)/1e3' ", griddes_elem, " ", fout_elem)
        message("run `", cmd, "` ...")
        check <- system(cmd)
        if (check != 0) stop("error: ", check)
    }
}

message("\nfinished")

