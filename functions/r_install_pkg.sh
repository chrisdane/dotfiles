#!/bin/bash

set -e

#################### start user input ####################

# choose machine-and-r-and-compiler-combination

# case 1: mistral r-3.6.1 gcc-9.1.0
#if true; then
if false; then
    module purge
    module load r/3.6.1-gcc-9.1.0 gcc/9.1.0-gcc-7.1.0
    install_script=r_install_pkg_mistral_r-3.6.1-gcc-9.1.0.r
    gdal_path=/sw/spack-rhel6/gdal-3.0.1-gs7y3t
    geos_path=/sw/spack-rhel6/geos-3.7.2-gscs6a
    proj_path=/sw/spack-rhel6/proj-6.2.0-psms4p
    sqlite_path=/sw/spack-rhel6/sqlite-3.30.1-bekvvp
fi

# case 2: mistral r-3.5.3 gcc-4.8.2
if true; then
#if false; then
    module purge
    module load r/3.5.3 gcc/4.8.2
    install_script=r_install_pkg_mistral_r-3.5.3-gcc-4.8.2.r
    gdal_path=/sw/rhel6-x64/gdal-2.1.3-gcc48
    geos_path=/sw/rhel6-x64/geos-3.6.1-gcc48/
    proj_path=/sw/rhel6-x64/graphics/proj4-4.9.3-gcc48
    sqlite_path=/sw/spack-rhel6/sqlite-3.30.1-bekvvp
fi

#################### user input end ########################

echo "start r_install_pkg.sh ..."
module list

if [ -z ${install_script+x} ]; then echo "error: \${install_script} not defined"; exit; fi
if [ -z ${gdal_path+x} ]; then echo "error: \${gdal_path} not defined"; exit; fi
if [ -z ${geos_path+x} ]; then echo "error: \${geos_path} not defined"; exit; fi
if [ -z ${proj_path+x} ]; then echo "error: \${proj_path} not defined"; exit; fi
if [ -z ${sqlite_path+x} ]; then echo "error: \${sqlite_path} not defined"; exit; fi

export PKG_CONFIG_PATH=${proj_path}/lib/pkgconfig:${gdal_path}/lib/pkgconfig
PATH=${gdal_path}/bin:${geos_path}/bin:$PATH
ldflags='-Wl,-rpath,'${gdal_path}'/lib:'${proj_path}'/lib:'${geos_path}'/lib'

cat > ${install_script} <<EOF

pkgs <- c("rgeos", "proj4", "sf")
lib <- .libPaths()[1] # change if needed
repos <- "https://cloud.r-project.org"

for (pkgi in seq_along(pkgs)) {
    
    if (pkgs[pkgi] == "rgeos") {
        configure.args <- paste0("--with-geos-config=${geos_path}/bin/geos-config ",
                                 "PKG_LIBS=$ldflags")
    
    } else if (pkgs[pkgi] == "proj4") {
        configure.args <- paste0("PKG_CPPFLAGS=-I${proj_path}/include ",
                                 "PKG_LIBS='-L${proj_path}/lib ",
                                           "-L${sqlite_path}/lib ",
                                           "$ldflags'")
    
    } else if (pkgs[pkgi] == "sf") {
        configure.args <- paste0("--with-gdal-config=${gdal_path}/bin/gdal-config ",
                                 "--with-geos-config=${geos_path}/bin/geos-config ",
                                 "PKG_LIBS='-L${sqlite_path}/lib ",
                                           "$ldflags'")
    
    } else {
        stop("package \"", pkgs[pkgi], "\" not defined")
    
    }
    
    cmd <- paste0("install.packages(\"", pkgs[pkgi], "\", lib=\"", lib, "\", repos=\"", 
                  repos, "\", configure.args=\"", configure.args, "\")")
    msg <- paste0("\n**************************************************************\n",
                  "pkg ", pkgi, "/", length(pkgs), ": run \"", cmd, "\" ...")
    message(msg)
    cat(paste0("# ", msg), file="${install_script}", append=T)
    eval(parse(text=cmd))

} # for pkgi

#file.remove("${install_script}") # if wanted

EOF

R --no-save < ${install_script}

