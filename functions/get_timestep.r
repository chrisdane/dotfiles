#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=T)

nstep_per_day <- 1500:2
inds <- which(86400 %% nstep_per_day == 0)
nstep_per_day <- nstep_per_day[inds]
dt_sec <- 86400/nstep_per_day
dt_min <- dt_sec/60
day_minus_dt_sec <- 86400 - dt_sec
df <- data.frame(dt_sec=dt_sec, dt_min=dt_min, nstep_per_day=nstep_per_day, day_minus_dt_sec=day_minus_dt_sec)

# check even devide
devides_sec <- c(3600, 7200) # e.g. oasis coupling time step
for (di in seq_along(devides_sec)) {
    tmp <- devides_sec[di]/dt_sec
    inds <- tmp %% 1 == 0
    tmp[inds] <- T
    tmp[!inds] <- F
    df[[length(df)+1]] <- as.logical(tmp)
    names(df)[length(df)] <- paste0(devides_sec[di], "/dt_even")
} # for di

# print
inds <- which(dt_min %% 1 == 0) # full minutes, no decimals
print(df[inds,])

