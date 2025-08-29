# r

# calc OAE efficiency eta = dDIC/dAlk as eta = eta(S,T,p) after Renforth and Henderson 2017 (eq. 11)

if (T) { # glodap levante
    fsalt <- "/work/ab0246/a270073/data/glodap/data/GLODAPv2.2016b/GLODAPv2.2016b.salinity.nc"
    ftemp <- "/work/ab0246/a270073/data/glodap/data/GLODAPv2.2016b/GLODAPv2.2016b.temperature.nc"
    fpco2 <- "/work/ab0246/a270073/data/glodap/post/GLODAPv2.2016b/GLODAPv2.2016b.pCO2_with_PO4_with_silicate_b_l10.nc"
    ncsalt <- ncdf4::nc_open(fsalt)
    nctemp <- ncdf4::nc_open(ftemp)
    ncpco2 <- ncdf4::nc_open(fpco2)
    salt <- ncdf4::ncvar_get(ncsalt, "salinity")[,,1]
    temp <- ncdf4::ncvar_get(nctemp, "temperature")[,,1]
    pco2 <- ncdf4::ncvar_get(ncpco2, "pCO2")[,,1]
    lon <- ncsalt$dim$lon$vals
    lat <- ncsalt$dim$lat$vals
    fout <- "/work/ab0246/a270073/data/glodap/post/GLODAPv2.2016b/GLODAPv2.2016b.OAE_efficiency_eta_RH17_eq11.nc"
} else {
    stop("asd")
}

dir.create(dirname(fout), recursive=T, showWarnings=F)
if (!dir.exists(dirname(fout))) stop("could not create outdir ", dirname(fout))

eta <- array(NA, dim=c(length(lon), length(lat)))
for (i in seq_len(dim(eta)[1])) {
    for (j in seq_len(dim(eta)[2])) {
        eta[i,j] <-  (salt[i,j]*10^(-3.009) + 10^(-1.519))*log(pco2[i,j]) +  
                    -salt[i,j]*10^(-2.1) + 
                    -(temp[i,j]*pco2[i,j])*(salt[i,j]*10^(-7.501) - 10^(-5.598)) + 
                    -temp[i,j]*10^(-2.337) + 
                     10^(-0.102)
    } # for j
} # for i 

ncvar <- ncdf4::ncvar_def(name="eta_rh17", units="", dim=list(ncsalt$dim$lon, ncsalt$dim$lat), missval=NaN)
outnc <- ncdf4::nc_create(fout, vars=ncvar, force_v4=T)
ncdf4::ncvar_put(outnc, ncvar, eta)
ncdf4::ncatt_put(outnc, 0, "ref", "Renforth and Henderson 2017: doi:10.1002/2016RG000533")
ncdf4::ncatt_put(outnc, 0, "script", "dotfiles/functions/renforth_and_henderson_2017.r")
ncdf4::nc_close(outnc)

