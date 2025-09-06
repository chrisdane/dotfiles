#!/usr/bin/env Rscript

vels <- c(50/4, 0.033, 50/300, 0.167, 16.4)
vels_text <- c("50/4", "0.033", "50/300", "0.167", "16.4")
names(vels) <- names(vels_text) <- c("m/4a", "m/d", "m/300d", "m/d", "mm/d")

# sort
inds <- sort(vels, index.return=T)$ix
vels <- vels[inds]
vels_text <- vels_text[inds]

vels_m_d <- rep(NA, t=length(vels_text))
for (vi in seq_along(vels)) {
    if (names(vels)[vi] == "m/d") vels_m_d[vi] <- vels[vi]
    if (names(vels)[vi] == "m/4a") vels_m_d[vi] <- vels[vi]/365.25
    if (names(vels)[vi] == "m/300d") vels_m_d[vi] <- vels[vi]*300
    if (names(vels)[vi] == "mm/d") vels_m_d[vi] <- vels[vi]*1e3
}
if (any(is.na(vels_m_d))) stop("some cases are not defined")
df <- data.frame(vel=vels_text, unit=names(vels), vel_m_per_day=vels_m_d)
scipen <- options()$scipen
options(scipen=10)
print(df)
options(scipen=scipen)

