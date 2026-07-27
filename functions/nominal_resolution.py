# py

griddes = "/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc" # core mesh: 99.3384341924696 100 km
#griddes = "/work/ab0246/a270092/input/fesom2/dars2/mesh.nc" # dars2 mesh: 20.86033708177036 25 km

import sys, netCDF4, numpy
sys.path.insert(0, "/home/a/a270073/proj/cmip/cmor/nominal_resolution") # wrapper for https://github.com/PCMDI/nominal_resolution.git
from lib import mean_resolution, nominal_resolution

ds = netCDF4.Dataset(griddes)
cell_area = ds.variables["cell_area"][:]   # m2, (ncells,)
lat_bnds  = ds.variables["lat_bnds"][:]    # deg, (ncells, vertices)
lon_bnds  = ds.variables["lon_bnds"][:]
ds.close()

mean_res = mean_resolution(cell_area, lat_bnds, lon_bnds, convertdeg2rad=True)
nom_res = nominal_resolution(mean_res)
print(mean_res, nom_res)

