#!/sw/rhel6-x64/r/r-3.3.3-gcc48/bin/Rscript --vanilla

args <- commandArgs(trailingOnly=T)

if (length(args) == 0 ||
    (length(args) != 0 && length(args) < 3)) {

    cat("mycat usage: mycat outname files_to_cat_seperated_by_space\n")
    quit()
    
} else if (length(args) != 0 && length(args) >= 3) {

    outname <- args[1]
    fs <- args[2:length(args)] # files to cat
    
    if (F) {
        cat("******************\n")
        cat(paste0("outname=", outname), "\n")
        for (i in 1:length(fs)) {
            cat(paste0("   ", i, ": ", fs[i]), "\n")
        }
        print(args)
        stop("asd")
    }

    cat("******************\n")
    
    for (i in 1:length(fs)) {
        cmd <- paste0("ncks -O -4 --mk_rec_dmn time ", 
                      dirname(fs[i]), "/", basename(fs[i]), " ", 
                      dirname(fs[i]), "/r_", basename(fs[i]))
        cat(paste0(i, ": ", cmd, " ..."), "\n")
        system(cmd)
    }

    cmd <- paste0("ncrcat ", 
                  paste0(dirname(fs), "/r_", basename(fs), collapse=" "), " ",
                  dirname(outname), "/r_", basename(outname))
    cat(paste0(cmd, " ..."), "\n")
    system(cmd)

    cmd <- paste0("ncks --fix_rec_dmn time ", 
                  dirname(outname), "/r_", basename(outname), " ", 
                  dirname(outname), "/", basename(outname))
    cat(paste0(cmd, " ..."), "\n")
    system(cmd)

    cat("******************\n")

}

