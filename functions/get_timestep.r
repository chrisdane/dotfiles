#!/usr/bin/env Rscript

nstep <- 500:5
inds <- which(86400 %% nstep == 0)
nstep <- nstep[inds]
dt_sec <- 86400/nstep
day_minus_dt_sec <- 86400 - dt_sec
print(data.frame(nstep_per_day=nstep, dt_sec=dt_sec, dt_min=dt_sec/60, day_minus_dt_sec=day_minus_dt_sec))

