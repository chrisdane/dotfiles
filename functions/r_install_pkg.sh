#!/bin/bash

# r_install_pkg.sh: part 1/2 of complicated r package installation
# r_install_pkg.r:  part 2/2

# part 1: load wanted R+compiler versions and set paths; call part 2

set -e

echo
echo "############## part 1/2: $(readlink -f $0) ################"
echo

# which packages to install
#pkgs=(rgeos proj4 sf)
#pkgs=sf
#pkgs=terra
#pkgs=raster
pkgs=units
#pkgs=oce

# choose r-and-compiler-combination:

if true; then # case 1: mistral r-3.6.1 gcc-9.1.0
#if false; then
    echo "module purge"; module purge
    echo "module load r/3.6.1-gcc-9.1.0 gcc/9.1.0-gcc-7.1.0"
    module load r/3.6.1-gcc-9.1.0 gcc/9.1.0-gcc-7.1.0
    
    lib=~/scripts/r/packages/bin/r_3.6
    
    #gdal_path=/sw/spack-rhel6/gdal-3.0.1-gs7y3t
    #geos_path=/sw/spack-rhel6/geos-3.7.2-gscs6a
    #proj_path=/sw/spack-rhel6/proj-6.2.0-psms4p
    #sqlite_path=/sw/spack-rhel6/sqlite-3.30.1-bekvvp
    #udunits_path=/sw/spack-rhel6/udunits-2.2.24-fpumit
    gdal_path=/sw/spack-rhel6/gdal-3.1.3-f7koyc
    geos_path=/sw/spack-rhel6/geos-3.8.1-ru2zkr
    proj_path=/sw/spack-rhel6/proj-7.1.0-w57onb
    sqlite_path=/sw/spack-rhel6/sqlite-3.34.0-gnklvh
    udunits_path=/sw/spack-rhel6/udunits-2.2.28-h6kiqa
fi

#if true; then # case 2: mistral r-3.5.3 gcc-4.8.2
if false; then
    echo "module purge"
    module purge
    echo "module load r/3.5.3 gcc/4.8.2"
    module load r/3.5.3 gcc/4.8.2
    
    lib=~/scripts/r/packages/bin/r_3.5
    
    gdal_path=/sw/rhel6-x64/gdal-2.1.3-gcc48 # terra needs gdal >= 2.2.3
    geos_path=/sw/rhel6-x64/geos-3.6.1-gcc48
    proj_path=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48
    sqlite_path=/sw/spack-rhel6/sqlite-3.30.1-bekvvp
    udunits_path=/sw/rhel6-x64/util/udunits-2.2.17-gcc48
fi

#if true; then # case 3: ollie r-4.1.0 gcc-6.1.0
if false; then
    echo "module purge"
    module purge
    echo "module load r/3.5.3 gcc/4.8.2"
    module load r/3.5.3 gcc/4.8.2
    
    lib=~/scripts/r/packages/bin/r_3.5

fi

# add paths to Rscript call at the bottom

module list

unset LD_LIBRARY_PATH # should be clean

export PKG_CONFIG_PATH=${proj_path}/lib/pkgconfig:${gdal_path}/lib/pkgconfig

ldflags='-Wl,-rpath,'${gdal_path}'/lib:'${proj_path}'/lib:'${geos_path}'/lib'

Rscript --vanilla r_install_pkg.r \
    --root_pid=$$ \
    --lib=$lib \
    --gdal_path=$gdal_path \
    --geos_path=$geos_path \
    --proj_path=$proj_path \
    --sqlite_path=$sqlite_path \
    --udunits_path=$udunits_path \
    --ldflags=$ldflags \
    ${pkgs[@]} # last argument without name: packages to install

