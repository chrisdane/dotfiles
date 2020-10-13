#!/usr/bin/env Rscript

if (interactive()) {
    user <- "a270073"
} else {
    user <- commandArgs(trailingOnly=T) 
}

if (length(user) != 1) stop("aasd")

hostnames <- c(paste0("mlogin10", c(0, 1, 2, 3, 4, 5, 8)), 
               paste0("mistralpp", 1:5))
domains <- rep("hpc.dkrz.de", t=length(hostnames))

# if any line starts with "On since "
onpattern <- "On since"

for (i in 1:length(hostnames)) {

    message("Check server ", i, "/", length(hostnames), ": ", hostnames[i], ".", domains[i], " ...")
    cmd <- paste0("ssh a270073@", hostnames[i], ".", domains[i], " finger ", user)
    finger <- system(cmd, intern=T)
    
    # if online
    if (any(regexpr(onpattern, finger) != -1)) {
   
        message("==========================")
        if (F) {
            message("Run \"", cmd, "\" ...")
        }

        print(finger)
        onlines <- which(regexpr(onpattern, finger) != -1)

        # what is the user doing on that server
        if (F) {
            cmd <- paste0("ssh a270073@", hostnames[i], ".", domains[i], " TERM=vt100; top -cu ", user)
        } else if (T) {
            cmd <- paste0("ssh a270073@", hostnames[i], ".", domains[i], " w ", user)
        }
        message("Run \"", cmd, "\" ...")
        w <- system(cmd, intern=T)
        print(w)

        message("==========================")
        
    # not online 
    } else {
        if (F) message("-> Not online")
    }
}
    

