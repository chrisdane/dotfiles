#!/bin/bash

esm_tools_info() {
    if command -v esm_tools > /dev/null 2>&1; then
        #echo "run 'esm_tools --version' ..."
        esm_tools_version=$(esm_tools --version) # e.g. "esm_tools, 6.1.3"
        esm_master_bin=$(which esm_master) # e.g. ~/.local/bin/esm_master
        esm_master_py_bin=$(head -1 $esm_master_bin) # "#!/path"
        esm_master_py_bin=${esm_master_py_bin:2} # "/path"
        esm_tools_src_path=$($esm_master_py_bin -c "import site; print(site.getusersitepackages())") # e.g. ~/.local/lib/python3.8/site-packages
        esm_tools_src_path=$(head -1 $esm_tools_src_path/esm-tools.egg-link) # /path/to/esm_tools/src/
        esm_tools_src_path=$(dirname $esm_tools_src_path) # without /src
        owd=$(pwd); cd $esm_tools_src_path
        esm_tools_src_path=$(pwd) # normalize path; better than readlink -f
        esm_tools_branch=$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')
        esm_tools_hash=$(git rev-parse --short HEAD)
        cd $owd
        echo "version = $esm_tools_version"
        echo "branch  = $esm_tools_branch"
        echo "hash    = $esm_tools_hash"
        echo "src     = $esm_tools_src_path ($(hostname))"
    else
        echo "could not find esm_tools"
        return 1
    fi
} # esm_tools_info
   
recomp_fesom() {
    if command -v esm_tools > /dev/null 2>&1; then
        if [[ ! -z "$@" && -d $"$@" ]]; then # provided dir is not empty and found
            setup_path=$(cd "$@"; pwd) # better than readlink -f
            printf "setup_path = $setup_path"
            setup_name=$(basename $setup_path) # e.g. "awicm-1.0-recom"
            echo " --> setup name = $setup_name"
            fesom_path="${setup_path}/fesom-1.4"
            echo "fesom_path = $fesom_path"
            if [[ ! -d "${fesom_path}" ]]; then 
                echo "provided setup dir \"$setup_path\" does not have a \"fesom-1.4\" subdir"
                return 1
            fi
            fesom_branch=$(git --git-dir=${fesom_path}/.git --work-tree=${fesom_path} branch| sed -n -e 's/^\* \(.*\)/\1/p')
            fesom_hash=$(git --git-dir=${fesom_path}/.git --work-tree=${fesom_path} rev-parse --short HEAD)
            echo; echo "compile fesom on branch ${fesom_branch} (${fesom_hash}) with esm_tools"
            esm_tools_info
            if command -v host &> /dev/null; then
                hostname=$(host $(hostname))
                hostname=$(echo $hostname | cut -d' ' -f1)
            else
                hostname=$(hostname)
            fi
            # must be in parent path of setup
            owd=$(pwd)
            echo; echo "cd $(dirname $setup_path)"
            cd $(dirname $setup_path)
            echo; echo "esm_master recomp-$setup_name/fesom"; echo
            tic_fesom=$(date +%s)
            esm_master recomp-$setup_name/fesom
            toc_fesom=$(date +%s)
            msg="finished fesom ($((($toc_fesom - $tic_fesom)/60)) min) compile on $hostname"
            if command -v zenity &> /dev/null; then # inform via small zenity GUI alert
                zenity --info --text="$msg"
            else # inform just in terminal
                echo "program zenity not found"
                echo $msg
            fi
            echo; echo "cd back to old dir $owd"
            cd $owd
            echo; echo "finished"
            echo; echo "DONT FORGET TO COPY THE NEW FESOM BINARY TO EXPID!!!"
            echo
        else # provided dir is empty or not found
            if [[ ! -z "$@" ]]; then
                echo "provided directory \"$@\" not found"
            else
                echo "provided directory \"$@\" is not a directory"
            fi
            echo "provide esm_tools setup-directory with fesom-1.4 subdir"
            return 1
        fi
    else
        echo "could not find esm_tools"
        return 1
    fi # esm_tools exist
} # recomp_fesom

recomp_recom() {
    if command -v esm_tools > /dev/null 2>&1; then
        if [[ ! -z "$@" && -d $"$@" ]]; then # provided dir is not empty and found
            setup_path=$(cd "$@"; pwd) # better than readlink -f
            printf "setup_path = $setup_path"
            setup_name=$(basename $setup_path) # e.g. "awicm-1.0-recom"
            echo " --> setup name = $setup_name"
            recom_path="${setup_path}/recom"
            echo "recom_path = $recom_path"
            if [[ ! -d "${recom_path}" ]]; then 
                echo "provided setup dir \"$setup_path\" does not have a \"recom\" subdir"
                return 1
            fi
            fesom_path="${setup_path}/fesom-1.4"
            echo "fesom_path = $fesom_path"
            if [[ ! -d "${fesom_path}" ]]; then 
                echo "provided setup dir \"$setup_path\" does not have a \"fesom-1.4\" subdir"
                return 1
            fi
            recom_branch=$(git --git-dir=${recom_path}/.git --work-tree=${recom_path} branch| sed -n -e 's/^\* \(.*\)/\1/p')
            recom_hash=$(git --git-dir=${recom_path}/.git --work-tree=${recom_path} rev-parse --short HEAD)
            fesom_branch=$(git --git-dir=${fesom_path}/.git --work-tree=${fesom_path} branch| sed -n -e 's/^\* \(.*\)/\1/p')
            fesom_hash=$(git --git-dir=${fesom_path}/.git --work-tree=${fesom_path} rev-parse --short HEAD)
            echo; echo "compile 1) recom on branch ${recom_branch} (${recom_hash}) and then 2) fesom on branch ${fesom_branch} (${fesom_hash}) with esm_tools"
            esm_tools_info
            if command -v host &> /dev/null; then
                hostname=$(host $(hostname))
                hostname=$(echo $hostname | cut -d' ' -f1)
            else
                hostname=$(hostname)
            fi
            # must be in parent path of setup
            owd=$(pwd)
            echo; echo "cd $(dirname $setup_path)"
            cd $(dirname $setup_path)
            echo; echo "esm_master recomp-$setup_name/recom"; echo
            tic_recom=$(date +%s)
            esm_master recomp-$setup_name/recom # in this 
            toc_recom=$(date +%s)
            echo; echo "esm_master recomp-$setup_name/fesom"; echo
            tic_fesom=$(date +%s)
            esm_master recomp-$setup_name/fesom # order!
            toc_fesom=$(date +%s)
            msg="finished recom ($((($toc_recom - $tic_recom))) sec) and fesom ($((($toc_fesom - $tic_fesom)/60)) min) compile on $hostname"
            if command -v zenity &> /dev/null; then # inform via small zenity GUI alert
                zenity --info --text="$msg"
            else # inform just in terminal
                echo "program zenity not found"
                echo $msg
            fi
            echo; echo "cd back to old dir $owd"
            cd $owd
            echo; echo "finished"
            echo; echo "DONT FORGET TO COPY THE NEW FESOM BINARY TO EXPID!!!"
            echo
        else # provided dir is empty or not found
            if [[ ! -z "$@" ]]; then
                echo "provided directory \"$@\" not found"
            else
                echo "provided directory \"$@\" is not a directory"
            fi
            echo "provide esm_tools setup-directory with recom and fesom-1.4 subdirs"
            return 1
        fi
    else
        echo "could not find esm_tools"
        return 1
    fi # esm_tools exist
} # recomp_recom

