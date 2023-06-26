#!/usr/bin/env Rscript

# dependencies: myfunctions.r:size2byte()
if (!interactive()) source("~/scripts/r/functions/myfunctions.r")

known_quotas <- c("levante"="/sw/bin/lfsquota.sh")

##########################################################################

for (qi in seq_along(known_quotas)) {

    message("check known quota script ", qi, "/", length(known_quotas), ": ", 
            names(known_quotas)[qi], ": ", known_quotas[qi], " ...")

    args <- ""
    if (file.exists(known_quotas[qi])) {
        if (known_quotas[qi] == "/sw/bin/lfsquota.sh") { # levante
            args <- c("-p ab1095", "-p ba1103")
            cmd <- known_quotas[qi]
            if (any(args != "")) cmd <- paste0(cmd, " ", args)
            for (ci in seq_along(cmd)) {
                message("***************************************************\n",
                        "run `", cmd[ci], "` ...")
                #res <- system(cmd[ci], intern=T)
                res <- base::pipe(cmd[ci])
                df <- utils::read.fwf(res, widths=c(15, rep(8, t=7)), header=F, skip=2, n=2, stringsAsFactors=F)
                colnames(df) <- trimws(df[1,])
                df <- df[2,]; df[1,] <- trimws(df)
                used <- df$used                                                       # 139.9T 
                used_val <- as.numeric(gsub('[[:alpha:]]', "", used))                 # 139.9
                used_unit <- Reduce(setdiff, strsplit(c(used, used_val), split = "")) # T
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
        } # which machine
    }

} # for qi

