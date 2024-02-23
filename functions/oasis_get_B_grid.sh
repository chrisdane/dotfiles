#!/bin/bash

# create B grid with oasis mask = 0 everywhere

set -e

if [ "$#" -ne 2 ]; then
    echo "provide 'gridin' and 'gridout', e.g. 'oasis_get_B_grid.sh A009 B009'"
    exit
fi
gridin=$1; gridout=$2
#gridin="A009"; gridout="B009"

# do not use `cdo select` but since this may lead to automatic dimension renaming based on cdo version
# --> cdo 2.2.0 does automatic dim renaming, e.g. `x_$gridin` --> `x`
# --> cdo 2.2.2 does not rename dim
# --> use `ncks -{d,v}` instead

# areas.nc
if [ ! -f areas-$gridout.nc ]; then
    ncks -v $gridin.srf areas.nc areas-$gridout.nc
    ncrename -d x_$gridin,x_$gridout -d y_$gridin,y_$gridout -v $gridin.srf,$gridout.srf -v $gridin.lon,$gridout.lon -v $gridin.lat,$gridout.lat areas-$gridout.nc
    ncatted -a coordinates,$gridout.srf,o,c,"$gridout.lat $gridout.lon" areas-$gridout.nc
else 
    echo "output file areas-$gridout.nc aleady exists. skip"
fi

# grids.nc
if [ ! -f grids-$gridout.nc ]; then
    ncks -v $gridin.lon,$gridin.lat,$gridin.clo,$gridin.cla grids.nc grids-$gridout.nc
    ncrename -d x_$gridin,x_$gridout -d y_$gridin,y_$gridout -d crn_$gridin,crn_$gridout -v $gridin.lon,$gridout.lon -v $gridin.lat,$gridout.lat -v $gridin.clo,$gridout.clo -v $gridin.cla,$gridout.cla grids-$gridout.nc
else 
    echo "output file grids-$gridout.nc aleady exists. skip"
fi

# masks.nc
if [ ! -f masks-$gridout.nc ]; then
    ncks -v $gridin.msk masks.nc masks-$gridout.nc
    ncrename -d x_$gridin,x_$gridout -d y_$gridin,y_$gridout -v $gridin.msk,$gridout.msk -v $gridin.lon,$gridout.lon -v $gridin.lat,$gridout.lat masks-$gridout.nc
    ncatted -a coordinates,$gridout.msk,o,c,"$gridout.lat $gridout.lon" masks-$gridout.nc
    cdo -s -setrtoc,0,1,0 masks-$gridout.nc masks-$gridout.nc_tmp && mv masks-$gridout.nc_tmp masks-$gridout.nc # todo: dim renaming?
else 
    echo "output file masks-$gridout.nc aleady exists. skip"
fi

echo "done. if you want, append $gridout grid with"
echo "ncks -A areas-$gridout.nc areas.nc"
echo "ncks -A grids-$gridout.nc grids.nc"
echo "ncks -A masks-$gridout.nc masks.nc"

