#!/bin/bash

# split areas,grids,masks from Etienne

set -e
#grids="A009 B009 L009 R009 A079..."
grids=$(cdo -s showname masks.nc) # A009.msk L009.msk R009.msk RnfA.msk RnfO.msk
grids=$(echo $grids | sed -e 's/.msk//g') # A009 L009 R009 RnfA RnfO
for grid in $grids; do
    cdo -O selvar,${grid}.lat,${grid}.lon,${grid}.cla,${grid}.clo grids.nc grids-${grid}.nc
    cdo -O selvar,${grid}.srf areas.nc areas-${grid}.nc
    cdo -O selvar,${grid}.msk masks.nc masks-${grid}.nc
done

