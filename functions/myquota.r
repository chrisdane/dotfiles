#!/usr/bin/env Rscript

# dependencies: myfunctions.r:size2byte()

if (!interactive()) source("~/scripts/r/functions/myfunctions.r")

known_quotas <- c("levante"="/sw/bin/lfsquota.sh",
                  "albedo"="/albedo/soft/bin/info.sh")

##########################################################################

for (qi in seq_along(known_quotas)) {

    message("check known quota script ", qi, "/", length(known_quotas), ": ", 
            names(known_quotas)[qi], ": ", known_quotas[qi], " ...")

    args <- ""
    if (file.exists(known_quotas[qi])) {
        if (known_quotas[qi] == "/sw/bin/lfsquota.sh") { # levante
            # /sw/bin/lfsquota.sh -u <username> | -p projectname"
            # case -u:
            #   basescr=20000000
            #   uid=$(id -u)
            #   puid=$(id -u $2 2>/dev/null)
            #   let scrid=${basescr}+${puid}
            #   quota -A -s -u $2
            #   lfs quota -h -p ${scrid} /scratch
            # case -p:
            #   basework=30000000
            #   gid=$(getent group $2 | cut -f3 -d":")
            #   member=$(id -G | grep -c ${gid})
            #   let workid=${basework}+${gid}
            #   lfs quota -h -p ${workid} /work
            mylfsquota <- function(arg="-u a270073") {
                val <- substr(arg, 4, nchar(arg)) # a270073 or ba1103
                if (substr(arg, 1, 2) == "-u") {
                    basescr <- 20000000L
                    uid <- as.integer(system("id -u", intern=T)) # 23263
                    puid <- as.integer(system(paste0("id -u ", val, " 2>/dev/null"), intern=T)) # 23263
                    scrid <- basescr + puid # 20023263
                    cmd <- paste0("quota -A -s -u ", val) # quota -A -s -u a270073
                    message("run `", cmd, "` ...")
                    res <- system(cmd, intern=T)
                    print(res)
                    # [1] "Disk quotas for user a270073 (uid 23263): "                                     
                    # [2] "     Filesystem   space   quota   limit   grace   files   quota   limit   grace"
                    # [3] "10.128.13.67:/home"                                                             
                    # [4] "                 13564M  30720M  30720M            126k       0       0        "
                    cmd <- paste0("lfs quota -h -p ", scrid, " /scratch") # lfs quota -h -p 20023263 /scratch
                    message("run `", cmd, "` ...")
                    res <- system(cmd, intern=T)
                    print(res)
                    # [1] "Disk quotas for prj 20023263 (pid 20023263):"                                      
                    # [2] "      Filesystem    used   bquota  blimit  bgrace   files   iquota  ilimit  igrace"
                    # [3] "        /scratch     20k      15T     15T       -       5        0       0       -"
                } else if (substr(arg, 1, 2) == "-p") {
                    basework <- 30000000L
                    gid <- as.integer(system(paste0("getent group ", val, " | cut -f3 -d\":\""), intern=T)) # 1588
                    member <- as.integer(system(paste0("id -G | grep -c ", gid), intern=T)) # 1 if in project
                    workid <- basework + gid # 30001588
                    cmd <- paste0("lfs quota -h -p ", workid, " /work") # lfs quota -h -p 30001588 /work
                    message("run `", cmd, "` ...")
                    res <- system(cmd, intern=T)
                    print(res)
                    # [1] "Disk quotas for prj 30001588 (pid 30001588):"                                      
                    # [2] "      Filesystem    used   bquota  blimit  bgrace   files   iquota  ilimit  igrace"
                    # [3] "           /work  191.3T     255T    255T       - 8120554        0       0       -"
                } else {
                    stop("no")
                }
            } # mylfsquota
            args <- c("-u a270073" "-p ab1095", "-p ba1103")
            cmds <- known_quotas[qi]
            if (any(args != "")) cmds <- paste0(cmds, " ", args)
            for (ci in seq_along(cmds)) {
                message("***************************************************\n",
                        "run cmd ", ci, "/", length(cmds), ": `", cmds[ci], "` ...")
                if (F) {
                    res <- system(cmds[ci], intern=T)
                    # [1] "WORK Quota - ab1095"                                                                
                    # [2] "Disk quotas for prj 30001560 (pid 30001560):"                                       
                    # [3] "      Filesystem    used   bquota  blimit  bgrace   files   iquota  ilimit  igrace" 
                    # [4] "           /work  272.7T     277T    277T       - 10230845        0       0       -"
                } else if (T) {
                    res <- base::pipe(cmds[ci])
                    df <- utils::read.fwf(res, widths=c(16, 8, 9, 8, 8, 8, 9, 8, 8), header=F, skip=2, n=2, stringsAsFactors=F)
                    colnames(df) <- trimws(df[1,])
                    df <- df[2,]; df[1,] <- trimws(df)
                }
                used <- df$used                                                           # 139.9T 
                used_val <- as.numeric(gsub('[[:alpha:]]', "", used))                     # 139.9
                used_unit <- base::Reduce(setdiff, strsplit(c(used, used_val), split="")) # T
                used_byte <- size2byte(val=used_val, unit=used_unit)
                limit <- df$limit
                limit_val <- as.numeric(gsub('[[:alpha:]]', "", limit))
                limit_unit <- Reduce(setdiff, strsplit(c(limit, limit_val), split = ""))
                limit_byte <- size2byte(val=limit_val, unit=limit_unit)
                df$used_pcnt <- round(used_byte/limit_byte*100, 2)
                df$free_pcnt <- round(100 - df$used_pcnt, 2)
                free_byte <- limit_byte - used_byte
                df$free_G <- free_byte/1024^3
                df$free_T <- free_byte/1024^4
                print(df, width=200, row.names=F)
            } # for ci
        } else if (known_quotas[qi] == "/albedo/soft/bin/info.sh") { 
            args <- c("-q")
            cmd <- known_quotas[qi]
            if (any(args != "")) cmd <- paste0(cmd, " ", args)
            for (ci in seq_along(cmd)) {
                message("***************************************************\n",
                        "run `", cmd[ci], "` ...")
                res <- system(cmd[ci], ignore.stderr=T, intern=T)
                inds <- which(grepl("^/", res))
                if (length(inds) == 0) next # cmd
                res <- res[inds]
                res <- strsplit(res, "\\s+")
                paths <- sapply(res, "[", 1) # e.g. "/albedo/home/cdanek"
                blocks <- sapply(res, "[", 3) # e.g. "6.546G"
                quotas <- sapply(res, "[", 4) # e.g. "100G"
                blocks_byte <- size2byte(string=blocks) # e.g. 7.028714e+09
                quotas_byte <- size2byte(string=quotas) # e.g. 1.073742e+11
                blocks_rel <- blocks_byte/quotas_byte*100
                nfiles <- sapply(res, "[", 7)
                file_quotas <- lapply(res, "[", 8:10)
                file_quotas <- sapply(file_quotas, function(x) paste(x, collapse=" "))
                df <- data.frame(path=paths,
                                 block=blocks,
                                 quota=quotas,
                                 block_rel=paste0(round(blocks_rel, 2), "%"),
                                 nfiles=nfiles,
                                 file_quota=file_quotas)
                print(df, width=200, row.names=F)
            } # for ci
        } # which machine
    }

} # for qi

