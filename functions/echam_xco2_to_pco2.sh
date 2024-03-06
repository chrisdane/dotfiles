# 

# convert echam co2:xCO2 mixing ratio to pCO2 via water vapor correction
# --> pCO2 = xCO2 * (p_0 - pH2O); eq. C.2.4 in Zeebe and Wolf-Gladrow 2001 (p. 282)
# --> calc pH2O with Clausius-Clapeyron equation; section 18.1 in Vallis 2017 

if true; then
    fechammon="/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/echam/esm-hist_185001.01_echammon"
    ftracer="/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/echam/esm-hist_185001.01_tracer"
else
    fechammon="/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/echam/esm-hist_1850*.01_echammon"
    ftracer="/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/echam/esm-hist_1850*.01_tracer"
fi
fechammon_code="/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/echam/esm-hist_185001.01_echammon.codes"
ftracer_code="/work/ba1103/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-hist/outdata/echam/esm-hist_185001.01_tracer.codes"

pathout="${HOME}/test"
mkdir -p $pathout

# 1: get saturation water vapor pressure via Clausius-Clapeyron:
# --> saturation water vapor pressure in hPa = e_s = e_s_0 * exp(L/R_w * (1/T_0 - 1/T))
# e_s_0 = 6.11 hPa = saturation water vapor pressure at reference temperature T_0
# L = latent heat of evaporation of water = 2.5e6 J kg-1
# R_w = 461.52 J kg-1 K-1 = specific gas constant of water vapor 
# T = air temperature in K
# T_0 = 273.15 K = reference temperature
cdo --pedantic -P $(nproc) -f nc -setunit,hPa -expr,'p_vap_sat=6.11*exp(2.5e6/461.52*(1/273.15 - 1/var130))' \
    -sp2gp -select,code=130 ${fechammon} \
    ${pathout}/p_vap_sat.nc

# 2: get water vapor pressure
# --> water vapor pressure in hPa = e = e_s * relative humidity
# relative humidity in [0,1], not [0,100]
cdo --pedantic -f nc -setname,p_vap -mul \
    ${pathout}/p_vap_sat.nc \
    [ -select,code=157 ${fechammon} ] \
    ${pathout}/p_vap.nc

# 3: get water vapor correction and apply to xCO2
# --> pCO2 = xCO2 * (p_0 - e)
# p_0 = 1013.25 hPa = 1 atm = reference air pressure
# !!! must use pressure unit atm, not pascal !!!
# convert CO2 mass mixing ratio to volume mixing ratio
# --> kgCO2 kg-1 = molCO2 mol-1 * molar_mass_dry_air_g_mol / molar_mass_co2_g_mol
molar_mass_co2_g_mol=44.0095 # g mol-1
molar_mass_dry_air_g_mol=28.9652 # g mol-1
cdo --pedantic -setunit,atm -expr,"dp_vap=1-p_vap/1013.25" ${pathout}/p_vap.nc ${pathout}/dp_vap.nc
cdo --pedantic -setunit,ppm -setname,pCO2 -mulc,1e6 -mul \
    [ -mulc,${molar_mass_dry_air_g_mol} -divc,${molar_mass_co2_g_mol} -select,code=1 ${ftracer} ] \
    ${pathout}/dp_vap.nc \
    ${pathout}/pco2.nc

