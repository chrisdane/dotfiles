#!/usr/bin/env Rscript

dt_sec_oasis <- 7200
args <- commandArgs(trailingOnly=T)
if (length(args) == 1) dt_sec_oasis <- as.numeric(args[1])

nstep <- 1500:2
inds <- which(86400 %% nstep == 0)
nstep <- nstep[inds]
dt_sec <- 86400/nstep
dt_min <- dt_sec/60
day_minus_dt_sec <- 86400 - dt_sec
dt_sec_over_dt_sec_oasis <- dt_sec/dt_sec_oasis
df <- data.frame(nstep_per_day=nstep, dt_sec=dt_sec, dt_min=dt_min, day_minus_dt_sec=day_minus_dt_sec)
df[[paste0("dt_sec_divc_", dt_sec_oasis)]] <- dt_sec_over_dt_sec_oasis
inds <- which(dt_min %% 1 == 0) # full minutes, no decimals
print(df[inds,])

