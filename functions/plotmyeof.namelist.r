# r

# which settings
if (F) {
    settings <- list("Hol-Tx10_temp2_svd"=
                     list(files=c(eof="cosmos-aso-wiso_Hol-Tx10_wiso_mm_echam5_select_temp2_minus_timmean_global_annual_0001-7001.nc_temp2_eof_3_base_svd.nc")),
                     "Hol-Tx10_temp2_cdo"=
                     list(files=c(eigval="cosmos-aso-wiso_Hol-Tx10_wiso_mm_echam5_select_temp2_minus_timmean_global_annual_0001-7001.nc_temp2_eof_3_cdoeof_eigval.nc",
                                  eigvec="cosmos-aso-wiso_Hol-Tx10_wiso_mm_echam5_select_temp2_minus_timmean_global_annual_0001-7001.nc_temp2_eof_3_cdoeof_eigvec.nc",
                                  pc="cosmos-aso-wiso_Hol-Tx10_wiso_mm_echam5_select_temp2_minus_timmean_global_annual_0001-7001.nc_temp2_eof_3_cdoeof_pc.nc")))
} else if (F) {
    settings <- list("Hol-Tx10_temp2_svd"=
                     list(files=c(eof="cosmos-aso-wiso_Hol-Tx10_wiso_mm_echam5_select_temp2_minus_timmean_global_annual_0001-7001.nc_temp2_eof_3_base_svd.nc")),
                     "Hol-Tx10_temp2_cdo"=
                     list(files=c(eigval="cosmos-aso-wiso_Hol-Tx10_wiso_mm_echam5_select_temp2_minus_timmean_global_annual_0001-7001.nc_temp2_eof_3_cdoeof_eigval.nc",
                                  eigvec="cosmos-aso-wiso_Hol-Tx10_wiso_mm_echam5_select_temp2_minus_timmean_global_annual_0001-7001.nc_temp2_eof_3_cdoeof_eigvec.nc",
                                  pc="cosmos-aso-wiso_Hol-Tx10_wiso_mm_echam5_select_temp2_minus_timmean_global_annual_0001-7001.nc_temp2_eof_3_cdoeof_pc.nc")))
} else if (T) {
    settings <- list("Hol-T_temp2"=
                     list(files=c(eof="cosmos-aso-wiso_Hol-T_main_mm_echam5_select_temp2_minus_timmean_global_annual_0004-7000.nc_temp2_eof_3_base_svd.nc")),
                     "Hol-Tx10_temp2"=
                     list(files=c(eof="cosmos-aso-wiso_Hol-Tx10_wiso_mm_echam5_select_temp2_minus_timmean_global_annual_0001-7001.nc_temp2_eof_3_base_svd.nc")))
}

# run
source("plotmyeof.r")
plotmyeof(settings=settings)


