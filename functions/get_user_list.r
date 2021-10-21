# r

# get user list with finger

if (!file.exists("~/bin/myfinger.r")) {
    stop("could not find ~/myfinger.r")
}

host_in <- Sys.info()["nodename"]

# mistral
if (grepl("mlogin", host_in) ||
    grepl("mistral", host_in)) {
    host_out <- "mistral"
    patterns <- list(#list(from="a000000", to="a999999"),
                     list(from="b000000", to="b999999"),
                     list(from="g000000", to="g999999"),
                     list(from="k000000", to="k999999"),
                     list(from="m000000", to="m999999"),
                     list(from="u000000", to="u999999"))
    for (pati in seq_along(patterns)) {
        from <- patterns[[pati]]$from
        to <- patterns[[pati]]$to
        cmd <- paste0("~/bin/myfinger.r ", from, " ", to, " > ", 
                      host_out, "_users_", from, "_to_", to, ".txt")
        message("run `", cmd, "` ...")
        tic <- Sys.time()
        system(cmd)
        toc <- Sys.time()
        elapsed <- toc - tic
        message("--> took ", elapsed, " ", attributes(elapsed)$units)
    } # for pati
} # which host

