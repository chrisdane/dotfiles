#!/usr/bin/env Rscript

nstep <- 200:10
inds <- which(86400 %% nstep == 0)
nstep <- nstep[inds]
dt_sec <- 86400/nstep
print(data.frame(nstep_per_day=nstep, dt_sec=dt_sec, dt_min=dt_sec/60))

