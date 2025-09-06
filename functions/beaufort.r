#!/usr/bin/env Rscript

ms <- c(seq(0.1, 0.9, b=0.1), 1:33)
bft_waves <- c("0", "0-0.2", "0.2-0.5", "0.5-1", "1-2", "2-3", "3-4",  # m
               "4-5.5", "5.5-7.5", "7-10", "9-12.5", "11.5-16", ">=14")
bft_names <- c("calm", "light air", "light breeze", "gentle breeze",
               "moderate breeze", "fresh breeze", "strong breeze",
               #"high wind, moderate gale, near gale", 
               "moderate/near gale",
               "gale/fresh gale",
               "strong/severe gale", "storm, whole gale", "violent storm",
               "hurricane force")
bft <- ms
bft_waves_all <- ms
bft_names_all <- ms
for (i in 1:13) {
    if (i == 1) lims <- 0.3
    if (i == 2) lims <- c(0.3, 1.5)
    if (i == 3) lims <- c(1.6, 3.3)
    if (i == 4) lims <- c(3.4, 5.5)
    if (i == 5) lims <- c(5.5, 7.9)
    if (i == 6) lims <- c(8.0, 10.7)
    if (i == 7) lims <- c(10.8, 13.8)
    if (i == 8) lims <- c(13.9, 17.1)
    if (i == 9) lims <- c(17.2, 20.7)
    if (i == 10) lims <- c(20.8, 24.4)
    if (i == 11) lims <- c(24.5, 28.4)
    if (i == 12) lims <- c(28.5, 32.6)
    if (i == 13) lims <- 32.7
    
    if (i == 1) {
        inds <- ms < lims
    } else if (i == 13) {
        inds <- ms >= lims
    } else {
        inds <- ms >= lims[1] & ms <= lims[2]
    }
    bft[inds] <- as.integer(i-1)
    bft_waves_all[inds] <- bft_waves[i]
    bft_names_all[inds] <- bft_names[i]
} # for all bfts
df <- data.frame("m/s"=ms,
                 "km/h"=ms*3.6,
                 "cm/s"=ms*100,
                 "mph"=ms*2.2369362920544,
                 "knots"=ms*1.9438444924574,
                 "bft"=bft,
                 "waves m"=bft_waves_all,
                 "bft name"=bft_names_all,
				 check.names=F)
print(df, digits=3)
