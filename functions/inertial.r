#!/usr/bin/env Rscript
phi <- c(0.1, 1, 2, 3, 4, 5, 7.5, seq(10, 90, b=5))
omega <- 2*pi/86400
f <- 2*omega*sin(phi*pi/180)
inertial_period_sec <- 1/f
inertial_period_min <- inertial_period_sec/60
inertial_period_hour <- inertial_period_sec/(60*60)
inertial_period_day <- inertial_period_sec/(24*60*60)
inertial_period_month <- inertial_period_sec/(30*24*60*60)
inertial_period_year <- inertial_period_sec/(12*30*24*60*60)

df <- data.frame(phi=phi, f=f,
                 sec=inertial_period_sec,
                 min=inertial_period_min,
                 hour=inertial_period_hour,
                 day=inertial_period_day,
                 month=inertial_period_month,
                 year=inertial_period_year
                 )
print(df, digits=3)

message("diurnal:     0.87-1.05 cpd (K1)\n",
        "semidiurnal: 1.86-2.05 cpd (M2)\n",
        "supertidal:  >2.06 cpd (savage et al. 2017)")
