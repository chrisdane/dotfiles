# r

rm(list=ls()); graphics.off()

submit_via_nohup <- F
submit_via_sbatch <- T # uses account resources
dry <- F # do not submit jobs

if (F) { # awi-esm-1-1-lr_kh800 piControl
    prefix <- "awi-esm-1-1-lr_kh800_piControl"
    meshdir <- "/pool/data/AWICM/FESOM1/MESHES/core"
    outdir <- "/mnt/lustre02/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/levelwise"
    shifttime <- "-1day"
    files <- list.files("/mnt/lustre02/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/piControl/outdata/fesom", 
                        pattern="^thetao_fesom*", full.names=T)
    years <- as.integer(substr(basename(files), 14, 17))
    inds <- which(years >= 2686 & years <= 2936)
    files <- files[inds]
    start <- 1; end <- length(files)
    njobs_wanted <- 40
} else if (F) { # awi-esm-1-1-lr_kh800 historical
    prefix <- "awi-esm-1-1-lr_kh800_historcal2"
    meshdir <- "/pool/data/AWICM/FESOM1/MESHES/core"
    outdir <- "/mnt/lustre02/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/levelwise"
    shifttime <- "-1day"
    files <- list.files("/mnt/lustre02/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/historical2/outdata/fesom", 
                        pattern="^thetao_fesom*", full.names=T)
    years <- as.integer(substr(basename(files), 14, 17))
    start <- 1; end <- length(files)
    njobs_wanted <- 20
} else if (T) { # awi-esm-1-1-lr_kh800 ssp585
    prefix <- "awi-esm-1-1-lr_kh800_ssp585"
    meshdir <- "/pool/data/AWICM/FESOM1/MESHES/core"
    outdir <- "/mnt/lustre02/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585/outdata/levelwise"
    shifttime <- "-1day"
    files <- list.files("/mnt/lustre02/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585/outdata/fesom", 
                        pattern="^thetao_fesom*", full.names=T)
    years <- as.integer(substr(basename(files), 14, 17))
    njobs_wanted <- 10
    if (T) {
        inds <- match(c(2021, 2022, 2030, 2031, 2046, 2047, 2048), years)
        files <- files[inds]
        njobs_wanted <- length(inds)
    }
    start <- 1; end <- length(files)
}

ntot <- end - start + 1
df <- replicate(floor(seq(start, end, b=ntot/njobs_wanted)), n=2)
df[,2] <- c(df[2:njobs_wanted,2]-1, end)
df <- data.frame(from=df[,1], to=df[,2], len=df[,2]-df[,1]+1)
njobs <- dim(df)[1]
message("construct ", njobs, " jobs:")
print(df)

###################################################################

logpath <- "fesom1_nod3d_levelwise_loop_logs"
dir.create(logpath, recursive=T, showWarnings=F)
if (file.access(logpath, 2) == -1) stop("no write permission to `logpath` = \"", logpath, "\"")

message("submit ", njobs, " jobs ...")
for (jobi in seq_len(njobs)) {

    message("*** job ", jobi, "/", njobs, " ***")
    
    # inds of job
    inds <- seq(df$from[jobi], df$to[jobi], b=1)
    fname <- paste0(logpath, "/fesom1_nod3d_levelwise_", prefix, "_job_", 
                    sprintf(paste0("%0", nchar(njobs), "i"), jobi), 
                    "_of_", njobs, "_inds_", 
                    sprintf(paste0("%0", nchar(max(df$from)), "i"), df$from[jobi]), "_to_", 
                    sprintf(paste0("%0", nchar(max(df$to)), "i"), df$to[jobi]), 
                    "_len_", df$len[jobi])

    # create jobs command
    if (submit_via_nohup) {
        logfile <- paste0(fname, ".log")
        if (file.exists(logfile)) stop("logfile = ", logfile, " already exists")
        cmd <- paste0("fesom1_nod3d_levelwise.r meshdir=", meshdir, " outdir=", outdir, " shifttime=", shifttime, " ", 
                      paste(files[inds], collaps=" "), " > ", logfile, " 2>&1 &")
    
    } else if (submit_via_sbatch) {
        slurmfile <- paste0(fname, ".run")
        if (file.exists(slurmfile)) stop("slurmfile = ", slurmfile, " already exists")

        # mistral example scripts: https://www.dkrz.de/up/systems/mistral/running-jobs/example-batch-scripts
        # mistral partition limits: https://www.dkrz.de/up/systems/mistral/running-jobs/partitions-and-limits 
        # ollie example scripts: https://swrepo1.awi.de/plugins/mediawiki/wiki/hpc/index.php/Slurm_Example_Scripts
        # ollie partition limits: https://swrepo1.awi.de/plugins/mediawiki/wiki/hpc/index.php/SLURM#Partitions
        cmd <- c("#!/bin/bash",
                 paste0("#SBATCH --job-name=loop_", basename(fname), "      # Specify job name"),
                 "#SBATCH --partition=shared     # Specify partition name",
                 #"#SBATCH --partition=prepost     # Specify partition name",
                 #"#SBATCH --ntasks=1             # Specify max. number of tasks to be invoked",
                 "#SBATCH --time=08:00:00        # Set a limit on the total run time",
                 #"#SBATCH --mail-type=FAIL       # Notify user by email in case of job failure",
                 #"#SBATCH --account=ab0246       # Charge resources on this project account",
                 #"#SBATCH --account=ba0989       # Charge resources on this project account",
                 "#SBATCH --account=ba1103       # Charge resources on this project account",
                 # memory:
                 #"#SBATCH --mem=0                    # 0 = use all mem",
                 "#SBATCH --mem=15000M                    # 0 = use all mem",
                 # logs and errors in different files:
                 #paste0("#SBATCH --output=", fname, "_o%j.log    # File name for standard output"),
                 #paste0("#SBATCH --error=", fname, "_e%j.log     # File name for standard error output"),
                 # logs and errors in same file:
                 paste0("#SBATCH --output=", fname, "_%j.log    # File name for standard output"),
                 paste0("#SBATCH --error=", fname, "_%j.log     # File name for standard error output"),
                 "",
                 paste0("fesom1_nod3d_levelwise.r meshdir=", meshdir, " outdir=", outdir, " shifttime=", shifttime, " ", 
                        paste(files[inds], collapse=" ")))
        write(cmd, slurmfile)
        cmd <- paste0("sbatch ", slurmfile)
    } # which job submission method
   
    message("run `", cmd, "` ...")

    # submit job
    if (!dry) {
        system(cmd)
    } else {
        message("`dry` = T --> do not run this command")
    }

} # for jobi njobs

