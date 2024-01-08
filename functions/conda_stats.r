# r

rm(list=ls())
warn <- options()$warn

# conda paths (result of `conda env list`)
envs <- list(pyfesom2=list(path="~/.conda/envs/pyfesom2"),
             argopy=list(path="~/.conda/envs/argopy"))


for (ei in seq_along(envs)) {
    fjson <- list.files(paste0(envs[[ei]]$path, "/conda-meta"), pattern="*.json", full.names=T)
    if (length(fjson) > 1) {
        message("********************************************************\n",
                "env ", ei, "/", length(envs), " ", names(envs)[ei], 
                ": get info of ", length(fjson), " installed packages from ", envs[[ei]]$path, " ...")
        pkgs <- versions <- sizes_byte <- sizes_pretty <- rep(NA, t=length(fjson))
        for (fi in seq_along(fjson)) {
            pkg <- system(paste0("grep '\"name\":' ", fjson[fi]), intern=T) # e.g. "  \"name\": \"zstd\","
            if (length(pkg) != 1) stop("this should not happen")
            pkg <- strsplit(pkg, "\\s+")[[1]][3] # e.g. "\"zstd\","
            pkg <- gsub("\"", "", pkg) # e.g. "zstd,"
            pkg <- gsub(",", "", pkg) # e.g. "zstd"
            version <- system(paste0("grep '\"version\":' ", fjson[fi]), intern=T) # e.g. "  \"version\": \"1.5.5\""
            if (length(version) != 1) stop("this should not happen")
            version <- strsplit(version, "\\s+")[[1]][3] # e.g. "\"1.5.5\""
            version <- gsub("\"", "", version) # e.g. "1.5.5"
            size_byte <- system(paste0("grep '\"size\":' ", fjson[fi]), intern=T) # e.g. "  \"size\": 2562,"
            if (length(size_byte) != 1) stop("this should not happen")
            size_byte <- strsplit(size_byte, "\\s+")[[1]][3] # e.g. "2562,"
            size_byte <- gsub("[[:punct:]]", "", size_byte) # e.g. 2562
            options(warn=2); size_byte <- as.integer(size_byte); options(warn=warn)
            size_pretty <- utils:::format.object_size(size_byte, units="auto")
            pkgs[fi] <- pkg
            versions[fi] <- version
            sizes_byte[fi] <- size_byte
            sizes_pretty[fi] <- size_pretty
        } # for fi
        inds <- order(sizes_byte, decreasing=T)
        envs[[ei]]$df <- data.frame(pkg=pkgs[inds], version=versions[inds],
                                    byte=sizes_byte[inds], size=sizes_pretty[inds])
    } # if any json file found
} # for ei

# check two envs for duplicated
inds <- match(c("pyfesom2", "argopy"), names(envs)) 
if (anyNA(inds)) stop("did not find conda envs to check for duplicated")
inds_dupl <- which(duplicated(c(envs[[inds[1]]]$df$pkg, envs[[inds[2]]]$df$pkg)))
if (length(inds_dupl)) {
    pkgs_dupl <- c(envs[[inds[1]]]$df$pkg, envs[[inds[2]]]$df$pkg)[inds_dupl]
    message("\nthere are ", length(inds_dupl), " duplicated pkgs in the two envs ",
            paste(names(envs)[inds], collapse=" and "), ":")
    print(sort(pkgs_dupl))
    inds_dupl1 <- inds_dupl2 <- rep(NA, t=length(pkgs_dupl))
    inds_dupl_vers1 <- inds_dupl_vers2 <- c()
    for (dupli in seq_along(pkgs_dupl)) {
        ind1 <- which(envs[[inds[1]]]$df$pkg == pkgs_dupl[dupli])
        if (length(ind1) != 1) stop("package is duplicated within env 1: ", names(envs)[inds[1]])
        ind2 <- which(envs[[inds[2]]]$df$pkg == pkgs_dupl[dupli])
        if (length(ind2) != 1) stop("package is duplicated within env 2: ", names(envs)[inds[2]])
        if (F) print(rbind(envs[[inds[1]]]$df[ind1,], 
                           envs[[inds[2]]]$df[ind2,]), row.names=F)
        inds_dupl1[dupli] <- ind1
        inds_dupl2[dupli] <- ind2
        if (envs[[inds[1]]]$df[ind1,"version"] == envs[[inds[2]]]$df[ind2,"version"]) {
            inds_dupl_vers1 <- c(inds_dupl_vers1, ind1)
            inds_dupl_vers2 <- c(inds_dupl_vers2, ind2)
        }
    } # for dupli
    if (length(inds_dupl_vers1) > 0) {
        message("\nthere are ", length(inds_dupl_vers1), "/", length(pkgs_dupl), 
                " duplicated packages with same version:")
        inds2 <- order(envs[[inds[1]]]$df$pkg[inds_dupl_vers1])
        print(envs[[inds[1]]]$df[inds_dupl_vers1[inds2],c("pkg", "version")], row.names=F)
        message("\nin sum, those duplicated packages with the same version need:")
        sum_byte <- sum(envs[[inds[1]]]$df$byte[inds_dupl_vers1])
        sum_pretty <- utils:::format.object_size(sum_byte, units="auto")
        message(names(envs)[inds[1]], ": ", sum_pretty, " (", sum_byte, " byte)")
        sum_byte <- sum(envs[[inds[2]]]$df$byte[inds_dupl_vers2])
        sum_pretty <- utils:::format.object_size(sum_byte, units="auto")
        message(names(envs)[inds[2]], ": ", sum_pretty, " (", sum_byte, " byte)")
    } # if any duplicated pkgs with same version
} # if any dupliacated


