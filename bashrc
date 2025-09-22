#
# ~/.bashrc
#

## .bashrc vs .bash_profile: nothing but annoying
# if non-interactive shell:
#   echo $- = hB (e.g.); "i" is not included
# if interactive shell:
#   echo $- = himBHs (e.g.); "i" is included 
#   run 1) /etc/bash.bashrc, 2) ~/.bashrc
#   if login shell:
#       echo $0 = "-bash" or `shopt login_shell` = on
#       on login:  run 1) /etc/profile, 2) ~/.bash_profile, 3) ~/.bash_login, 4) ~/.profile
#       on logout: run 1) ~/.bash_logout, 2) /etc/bash.bash_logout
#   if non-login shell:
#       echo $0 = "bash" or `shopt login_shell` = off
#       on login: run ~/.bashrc
# a) start a new shell with `bash`           -->     interactive & non-login shell --> bashrc
# b) connect via ssh                         -->     interactive &     login shell --> bash_profile 
# c) submit a command via ssh or scp a file  --> non-interactive & non-login shell --> bashrc
# d) from within a bash script (#!/bin/bash) --> non-interactive & non-login shell --> none
# --> conclusion: you always need both: .bashrc and .bash_profile, i.e. the most complicated way was chosen -_-
# https://wiki.archlinux.org/index.php/bash#Invocation
# https://linux.die.net/man/1/bash (incovation chapter)
# https://www.gnu.org/software/bash/manual/bash.html#Bash-Startup-Files

# If not running interactively, don't do anything
#[[ $- != *i* ]] && return
if [[ $- != *i* ]]; then
    
    if false; then 
        echo "*** .bashrc non-interactive session ***" 
        printf "\$- = \"$-\""
        printf " --> no \"i\" for interactive shell"
        if shopt -q login_shell; then
            echo "\$0 = -$(basename $SHELL) or \`shopt login_shell\` = on -> login shell"
        else
            echo "\$0 = $(basename $SHELL) or \`shopt login_shell\` = off -> not login shell"
            fi
        echo "***************************************" 
    fi # if true/false

## From here, everyhing happens only if running interactively
else

    nch=30 # columns to print
    ncol=$(($(tput cols)/2))
    ncol=$(($ncol<$nch?$ncol:$nch)) # = min(ncol,nch)
    printf '%*s' "$ncol" | tr ' ' "*"
    printf " ~/.bashrc start "
    printf '%*s' "$ncol" | tr ' ' "*"
    echo ""

    # Source global definitions
    if [ -f /etc/bashrc ]; then
        source /etc/bashrc # todo: difference `.` and `source`
    fi
    
    export HISTCONTROL=ignoreboth # ignore commands with leading space and duplicates

    # use bash completion, if installed
    if [ -f /etc/bash_completion ]; then
        source /etc/bash_completion
    fi

    # git automplete
    # from https://apple.stackexchange.com/questions/55875/git-auto-complete-for-branches-at-the-command-line
    # does not work
    #if [ -f ~/.git-completion.bash ]; then
    #    source ~/.git-completion.bash
    #fi
    
    # enable git colors
    #git config --global color.ui auto
    
    # my bins (doing this recursively is not recommended; security)
    if [ -d ~/bin ]; then
        export PATH=~/bin:$PATH
    fi
    if [ -d ~/.local/bin ]; then
        export PATH=~/.local/bin:$PATH
    fi
    
    # get battery capacity percentage if available
    get_battery_capacity(){ 
        if [ -f /sys/class/power_supply/BAT0/capacity ]; then
            capacity=$(cat /sys/class/power_supply/BAT0/capacity)
            echo "$capacity%"
        else
            echo ""
        fi
    }
    
    # get battery status if available
    get_battery_status(){ 
        if [ -f /sys/class/power_supply/BAT0/status ]; then
            stat=$(cat /sys/class/power_supply/BAT0/status)
            if [ "${stat}" == "Charging" ]; then
                echo "↑"
            elif [ "${stat}" == "Discharging" ]; then
                echo "↓"
            elif [ "${stat}" == "Full" ]; then
                echo "F"
            elif [ "${stat}" == "Not charging" ]; then # shortly after plugged in
                echo ${stat}
            else
                echo ${stat}
            fi
        else
            echo ""
        fi
    }

    # get cpu temperature if available
    get_current_temp(){
        if [ -x "$(command -v sensors)" ]; then
            temp=$(sensors 2> /dev/null | \grep -oP 'Package id 0.*?\+\K[0-9.]+') # \grep and not grep to remove possible color
            temp=$(echo $temp | xargs printf "%.0f\n") # round to full decimal
            echo "$temp°"
        else
            echo ""
        fi
    }

    # make broken symlinks blinking
    if true; then
    #if false; do
        if [[ -z "$LS_COLORS" ]]; then # if not set
            eval "$(dircolors -b)" # defines env variable LS_COLORS based on /etc/DIR_COLORS*
        fi
        if grep -q 'or=' <<<"$LS_COLORS"; then # if or= is set
            current_or=$(echo "$LS_COLORS" | grep -o 'or=[^:]*' | cut -d= -f2)
            echo "current_or $current_or"
            if [[ $current_or != *";5"* ]]; then # append ";5" if not there
                new_or="${current_or};5"
                echo "new_or $new_or"
                export LS_COLORS="${LS_COLORS/or=$current_or/or=$new_or}"
            fi
        else # if or= is not set
            export LS_COLORS="${LS_COLORS}:or=48;5;232;38;5;9" # my: 40;31;01; levante: 48;5;232;38;5;9
        fi
    fi

    # prompt
    PS1='[\u@\h \W]\$ ' # default
    if true; then # my prompt; use `\$` to evaulate on every new line (i.e. when pressing enter)
        #PS1='\[\033[0;34m\]\h:$(pwd)/>\[\033[0m\] ' 
        PS1='\[\033[0;34m\]\h:$(get_battery_capacity)$(get_battery_status)$(get_current_temp):$(pwd)/>\[\033[0m\] '
    fi

    # enable make autocomplete:
    # https://stackoverflow.com/questions/4188324/bash-completion-of-makefile-target
    complete -W "\`grep -oE '^[a-zA-Z0-9_.-]+:([^=]|$)' Makefile | sed 's/[^a-zA-Z0-9_.-]*$//'\`" make

    # helper functions
    # check if program exists also if its masked by alias
    # if [ -x "$(command -v vi)" ]; then will not work if vi is aliased
    # https://unix.stackexchange.com/questions/85249/why-not-use-which-what-to-use-then/85250#85250
    mygroups(){
        groups | tr " " "\n" | sort
    }
    getmod () {
        stat --format '%a' $1
    }
    check_existance(){
        if command -v $1 > /dev/null 2>&1; then
            return 0
        else
            return 1
        fi
    }
    tl(){
        file=$(\ls -t *.log | grep -v _observe_ | head -n1)
        echo `\ls --color=auto -lFh $(pwd)/$file`
        tail -f $file
    }
    ml(){
        file=$(\ls -t *.log | grep -v _observe_ | head -n1)
        echo `\ls --color=auto -lFh $(pwd)/$file`
        less -i $file
    }
    pwd2(){
        #printf "\$(readlink -f .) = "
        readlink -f . # or pwd -P
    }
    pwd3(){
        printf "lfs getstripe --mdt-index $(readlink -f .): "
        lfs getstripe --mdt-index .
    }
    diffc(){ # colored diff
        diff --color=always -u $1 $2 | less -i -R # `always` when piped
    }
    diffcc(){ # colored character-wise diff with git
        git diff --no-index --word-diff-regex=. $1 $2
    }
    diffc2(){ # colored diff with vi
        diff $1 $2 | vim -R -
        #diff $1 $2 | colordiff # needs colordiff
    }
    diffcg(){ # colored diff of non-git files with git
        git diff --color=always --no-index $1 $2
    }
    diffcgw(){ # colored diff of non-git files with git
        # word-diff=porcelain
        git diff --color=always --word-diff=color --no-index $1 $2
    }
    myfind() { find -name "*$1*" 2>/dev/null ; }
    myfindp() { find -path "*$1*" 2>/dev/null ; } # -path and not -name to support '/'
    myfinds(){ find -name "*$1*" 2>/dev/null | sort ; }
    myfindps(){ find -path "*$1*" 2>/dev/null | sort ; }
    myfindi(){ find -iname "*$1*" 2>/dev/null ; }
    myfindpi(){ find -ipath "*$1*" 2>/dev/null ; }
    myfindis(){ find -iname "*$1*" 2>/dev/null | sort ; }
    myfindpis(){ find -ipath "*$1*" 2>/dev/null | sort ; }
    myfindb(){ find -ipath "*$1*" -xtype l 2>/dev/null ; } # broken links
    myfindbs(){ find -ipath "*$1*" -xtype l 2>/dev/null | sort ; }
    psme(){
        ps -u $(whoami) # todo: ps -u $whoami -F > ps_out
    }
    topme(){
        top -u $(whoami)
        #top -u $(whoami) -b -n 1 -c -w 512
        #top -u $(whoami) -b -n 1 -c -o %CPU -w 512 | awk 'NR>6 { printf "%6s %-4s %-4s %-4s %s\n",$1,$2,$9,$10,$12}'
    }
    pgrepme(){
        pgrep -u $(whoami) -ai $1
    }
    mount_check(){
        echo "mount -l -t fuse.sshfs"
        mount -l -t fuse.sshfs
    }
    update_chmod(){ # give group and others reading and accessing dirs
        # old:
        #echo "run 'find ./ -type f -exec chmod go+r -- {} +' ..."
        #find ./ -type f -exec chmod go+r -- {} + # group & others get +r on all files
        #echo "run 'find ./ -type d -exec chmod go+rx -- {} +' ..."
        #find ./ -type d -exec chmod go+rx -- {} + # group & others get +rx on all dirs (+x necessary to `cd` into dir) 
        # new:
        echo "chmod -R go+rX ."
        chmod -R go+rX .
        # before:
        # -rw-r----- 1 f1
        # drwxr-x--- 2 dir1/
        # -rwxr-x--- 1 script1*
        # after:
        # -rw-r--r-- 1 f1
        # drwxr-xr-x 2 dir1/
        # -rwxr-xr-- 1 script1* 
    }
    #mycbind(){
    #    paste <all> | column -s $'\t' -t 
    #}
    mycrop(){
        if [ $# -ne 6 ]; then
            echo "Usage: mycrop left bottom right up fin fout"
            return 1
        fi
        left=$1; bottom=$2; right=$3; top=$4; fin=$5; fout=$6
        convert $fin -gravity West -chop ${left}x0 $fout
        convert $fout -gravity South -chop 0x$bottom $fout
        convert $fout -gravity East -chop ${right}x0 $fout
        convert $fout -gravity North -chop 0x$top $fout
    }
    linuxhelp(){
        echo "file permissions"
        echo "r: 4, w: 2, x: 1"
        echo wifi
        echo "  sudo systemctl start NetworkManager.service # systemctl defaults to --system, hence sudo"
        echo "  nmcli device wifi list"
        echo "  nmcli device wifi connect SSID_or_BSSID password password"
        echo "  nmcli device wifi show-password"
        echo "  sudo cat /etc/NetworkManager/system-connections/*"
        echo "  sudo grep -r '^psk=' /etc/NetworkManager/system-connections/"
        echo cat
        echo "  'gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress -sOutputFile=out.pdf in1.pdf in2.pdf'"
        echo "  'pdftk in.pdf cat 1-12 14-end output out.pdf'"
        echo "  'pdftk in1.pdf in2.pdf output out.pdf'"
        echo "  'magick Screenshot* slides.pdf'"
        echo "  'magick *.png -auto-orient slides.pdf'"
        echo convert
        echo "   img2pdf -o f.pdf f.png # better quality than 'magick f.png f.pdf'"
        echo compress
        echo "  'gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/default -dNOPAUSE -dQUIET -dBATCH -dDetectDuplicateImages -dCompressFonts=true -r150 -sOutputFile=output.pdf input.pdf'"
        echo cut
        echo "  'gs -dBATCH -sOutputFile= -dFirstPage= -dLastPage= -sDEVICE=pdfwrite infile'"
        echo crop
        echo "  'pdfcrop --xetex --resolution 72 diffusion_vs_res.pdf diffusion_vs_res.pdf'"
        echo "  'convert -trim in.png out.png'"
        echo diff
        echo "  git diff --no-index f1 f2"
        echo "  xxdiff"
        echo grep
        echo "  git grep --no-index -e pattern1 --and -e pattern2 --and -e pattern3"
        echo sudo
        echo "sudoedit # not 'sudo vi'"
        echo watch
        echo "  watch -n 0.1 ls"
        echo "get laptop size"
        echo "  unplug external monitor; 'xrandr --query' returns sth like 'eDP or LVDS ... 309mm x 174mm'"
        echo "  echo \"scale=2; sqrt((309/25.4)^2 + (174/25.4)^2)\" | bc -l # = 13.96151 ~ 14 inch"
    }
    archhelp(){
        echo "debug"
        echo "journalctl --follow # or -f"
        echo "journalctl --since=today"
        echo "journalctl -b -1 # since last boot"
        echo "journalctl --since=today | grep -B 15 '(EE) Backtrace:'"
        echo "loginctl session-status # get session details"
        echo "dmesg -T"
        echo "tail -f /var/log/Xorg.0.log"
        echo "/var/log/pacman.log"
        echo "systemctl"
        echo "systemctl restart NetworkManager.service"
        echo "systemctl status/enable/start cups.service"
        echo "list stuff"
        echo "ip addr / ip link / ip r / ip tuntap show / ifconfig -a"
        echo "nmcli; nmcli device"
        echo "lspci -k | grep -iEA5 'vga|3d|display # get hardware info"
        echo "xrandr; hwinfo --monitor; glxinfo"
        echo "ntpq -p # update wrong time after reboot"
        echo "display manager"
        echo "grep '/usr/s\?bin' /etc/systemd/system/display-manager.service # which login manager"
        echo "window manager"
        echo "mate-session-properties"
    }
    bashhelp(){
        echo "find . -name "*.bak" -type f -delete"
        echo "./script > script.log 2>&1 &"
        echo "nohup sh -c './script arg1 arg2' > script.log 2>&1 &"
        echo "ln -sfn path/to/file-name link-name"
        echo "find /usr/local/bin -lname '/usr/local/texlive/*'" -delete # delete links
        echo "find / -iname openssl.pc 2>/dev/null \# locate alternative"
        echo "for f in *1954*; do echo \$f; ln -s \$(pwd)/\$f /aim/\$f; done"
        echo "rename 's/\.DAT/\.dat/' * \# -n for dry"
        echo "while read -r f; do mv '\$f' '\${f//:/_}'; done <files.txt"
        echo "arr=(\$(ls -F historical2_185012* | grep -v codes))"
        echo "printf '%s\\n' '\${arr[@]}'"
        echo "for f in \${arr[@]}; do echo \$f; cdo ntime \$f; done"
        echo "remove carets: sed -i -e 's/\\r$//'"
        echo "sed -i \"s|ResultPath.*|ResultPath='${out_dir}/'|g\" \"${out_dir}/namelist.config\""
        echo "lsof +D /path \# list open files"
        echo "p_pa=$(seq -s, 100000 -5000 20000)"
        echo "while read p; do echo \$p; done < file.txt"
        echo "Glob Wildcard     Regular Expression  Meaning"
        echo "?                 .                   Any single character"
        echo "*                 .*                  Zero or more characters"
        echo "[a-z]             [a-z]               Any character from the range"
        echo "[!a-m]            [^a-m]              A character not in the range"
        echo "[a,b,c]           [abc]               One of the given characters"
        echo "{cat,dog,bat}     (cat|dog|bat)       One of the given options"
        echo "{*.tar,*.gz}      (.*\.tar|.*\.gz)    One of the given options, considering nested wildcards}"
    }
    scphelp(){
        echo "scp -O -r dir/ user@host:/path # -O legacy mode to prevent 'path canonicalization failed' error"
    }
    ftphelp(){
        echo "lftp url"
        echo "!ls"
        echo "get fremote [-o flocal]"
        echo "put flocal [-o fremote]"
        echo "mirror [-P nfiles_in_parallel] dirremote dirlocal"
        echo "mirror -R [-P nfiles_in_parallel] dirlocal dirremote # -R for reverse"
        echo "lftp -c 'open ftp://ouruser:ourpassword@ftp.remotehost.com; mirror ...' # non-interactive mode"
    }
    vpnhelp(){
        echo "sudo openconnect -v --background --certificate=cert --csd-wrapper=script --timestamp --printcookie -u <name> <server>"
        echo "certificate: deutsche-telekom-root-ca-2.pem"
        echo "csd-wrapper script: /usr/lib/openconnect/csd-wrapper.sh"
    }
    vimhelp(){
        echo ":w !sudo tee %"
        echo "find missing bracket: 1) cursor on open or close bracket 2) %"
        echo ". # repeats last operation"
        echo "operations ~ verbs:"
        echo ""
        echo "d # delete"
        echo "c # change (= delete + enter insert mode)"
        echo "> # indent"
        echo "v # visually select (V for whole line)"
        echo "y # yank = copy"
        echo "f, F # find (F go backwards)"
        echo "t, T # find (T go backwards)"
        echo "/ # find"
        echo ""
        echo "repeat operation for multiple lines:"
        echo "ctrl + v -> arrow up/down to select lines -> shift + i -> <make edit> -> esc"
        echo ""
        echo "motions ~ nouns:"
        echo "w # forward by word "
        echo "b # back by word"
        echo "2j # down 2 lines"
        echo "iw # inner word"
        echo "it # inner tag"
        echo "i # inner quotes"
        echo "ip # inner paragraph"
        echo "as # as sentence"
        echo ""
        echo "combine verb with noun:"
        echo "dw # delete word"
        echo "fac # find next a and change"
        echo "c6j # change following 6 lines"
        echo "ds\" # delete surrounding quotes"
        echo "cs\"' # change surroundnig \" to '"
        echo "ys\" # add surrounding \""
    }
    fortranhelp() {
        echo "line length!!!!!!!!!!!!!!"
    }
    tarhelp(){
        echo "https://www.gnu.org/software/tar/manual/html_section/"
        echo "also use tar to process 'gzip compressed data' files, i.e. file.tar.gz"
        echo "--> add -z if file.tar.gz and not file.tar; -z: filter through gzip"
        echo "always: -f archive.tar"
        echo "list: -t"
        echo "tar -tvf archive.tar"
        echo "create -c"
        echo "tar -cvf archive.tar f1 f2"
        echo "extract: -x"
        echo "tar -xvf archive.tar"
        echo "tar --wildcards \"*.nc\" -xvf archive.tar                # extract specific files"
        echo "tar --wildcards \"*{pat1,pat2}*nc\" -xvf archive.tar"
        echo "tar --transform='s/^.*\///' -xvf archive.tar             # strip all leading dirs"
        echo "tar --stip=3 -xvf archive.tar                            # strip n leading dirs"
        echo "tar --transform='s:.*/\([^/]*\)/:\1/:' -xvf archive.tar  # strip n-1 leading dirs"
        echo "tar --transform='s/^.*\///' --wildcards \"*pat*\" -xvf archive.tar"
        echo "tar --transform='s:.*/\([^/]*\)/:\1/:' --wildcards \"*pat*\" -xvf archive.tar"
    }
    untar() {
        tar -xvf $1
    }
    ziphelp(){
        echo "zip archive.zip file1 file2 # create"
        echo "unzip file"
    }
    markdownhelp(){
        echo "url: [Duck Duck Go](https://duckduckgo.com)"
        echo "<details>"
        echo "<summary>Click to expand</summary>"
        echo "# blank line"
        echo "..."
        echo "</details>"
    }
    githelp(){
        echo "get hash: git rev-parse --short HEAD"
        echo "git lol = git log --graph --decorate --pretty=oneline --abbrev-commit"
        echo "git lola = git log --graph --decorate --pretty=oneline --abbrev-commit --all"
        echo "git config core.fileMode false \# ignore file permission changes" 
        echo "# branch/tag"
        echo "show list:        git {branch,tag} [-av]"
        echo "default branch:   cat .git/refs/remotes/origin/HEAD"
        echo "create local:     git branch -b test"
        echo "delete local:     git branch -d test"
        echo "find and checkout to remote branch:"
        echo "git -r | grep <name>"
        echo "git checkout -b feat/awiesm3-v3.4-co2 origin/feat/awiesm3-v3.4-co2"
        echo "create remote:    git push origin test"
        echo "delete remote:    "
        echo "git push origin --delete bname # delete remote"
        echo "git < 2.23:"
        echo "  if one remote:       git checkout test"
        echo "  if multiple remotes: git checkout -b test {origin[/remote],tags}/test"
        echo "git >= 2.23:"
        echo "  if one remote:       git switch test"
        echo "  if multiple remotes: git switch -c test {origin[/remote],tags}/test"
        echo "check if a branch exists on server: git ls-remote --heads origin refs/heads/test"
        echo "get latest tag: tag=\$(git describe --tags \$(git rev-list --tags --max-count=1))"
        echo "compare: https://github.com/user/repo/compare"
        echo " # commit"
        echo "git reset --hard HEAD^ # remove last commit; also deletes unstaged changes!"
        echo "git reset --hard HEAD~2 # remove last 2 commits; also deletes unstaged changes!"
        echo "git push origin -f # update remote"
        echo "# clone"
        echo "git clone --branch=test url"
        echo "# diff
        echo "git diff [from] to"
        echo "git diff --name-only"
        echo "git diff 6843db8 -- '*.functions'"
        echo "git -c core.fileMode=false diff # temporarily exclude file mode changes"
        echo "git diff --word-diff-regex=."
        echo "git diff --color-words=."
        echo "# merge"
        echo "git merge -s ours # --strategy"
        echo "# stash"
        echo "git stash list; stash show -p stash@{1}; stash apply stash@{n}; stash drop stash@{2}"
        echo "# cherry-pick"
        echo "git checkout commitx"
        echo "git cherry-pick commity [commitz1 commitz2]"
        echo "git cherry-pick --strategy=recursive -X theirs 7501f4d"
        echo "git cherry-pick -n commit # dont make a commit"
        echo "# squash"
        echo "git checkout branchname"
        echo "git rebase -i HEAD~n # combine latest n commits"
        echo "@editor: first line let \"pick\"; all other lines from \"pick\" --> \"squash\""
        echo "git push -u origin +branchname # \"+\" similar as --force but different"
        echo "@other pc: git reset --hard origin/branchname # caution: overwrites potential local changes irreversible"
        echo "rebase branch"
        echo "git checkout commit_from_where_my_branch_should_start"
        echo "git checkout -b new_branch # create and checkout new branch"
        echo "git cherry-pick commit_i_want_to_include"
        echo "git branch -d old_branch # delete old branch locally"
        echo "git push origin --delete old_branch # delete old branch remote"
        echo "@other pc: git remote prune origin # to delete the old branch in the 'git branch -av' list"
    } # githelp
    llg(){
        repofiles="$(git ls-tree --full-tree --name-only -r HEAD)" # omits untracked, ignored, deleted files., e.g. potentially large dir ./git
        if [ $? -ne 0 ]; then
            return 1
        fi
        #repofiles_vec=("${repofiles}"); # does not work if spaces in fname
        #repofiles_vec=$(printf '%s\n' "${repofiles}") # does not work if spaces in fname
        #declare -a repofiles_vec=("${repofiles}") # does not work if spaces in fname
        IFS=$'\n' read -r -d '' -a repofiles_vec < <( printf '%s\n' "${repofiles}" && printf '\0' ) # works
        #printf ' %s' "${repofiles_vec[@]}"
        nrepofiles=${#repofiles_vec[@]} # n
        repopath=$(git rev-parse --show-toplevel) # full path as of `readlink -f`
        declare -a vec=()
        for i in $(seq 0 $(( $nrepofiles - 1))); do # concatenate path and files
            #echo "$i: ${repofiles_vec[$i]}"
            vec[$i]=${repopath}/"${repofiles_vec[$i]}"
        done # todo: without loop
        #printf ' %s' "${vec[@]}"
        #printf -v repofiles ' %s' "${vec[@]}" # convert array back to string
        dus=$(du -hc "${vec[@]}" | sort -h)
        homeprefix=$(readlink -f ~/)
        # replace home in du result
        repl_pat_home='~' # need this extra variable for line below
        #dus="${dus//$homeprefix/$repl_pat_home}" # replace "[/optional/prefix]/home/user" with pattern
        # replace absolute path in du result
        repl_pat_abs='.'
        dus="${dus//$repopath/$repl_pat_abs}" # replace "[/optional/prefix]/home/user" with pattern
        printf '%s\n' "${dus[@]}"
        printf '%s\n' "--> $nrepofiles _tracked_ repo files in repo"
        printf '%s' "--> _total_ size (including untracked, ignored, deleted) of ${repopath//$homeprefix/$repl_pat_home}: "
        du -hcs $repopath | tail -1
    } # llg
    llg2() {
        if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then # if in git repo
            tracked=$(git ls-files -- "$PWD") # repo files of current dir
            if [ -n "$tracked" ]; then # if any repo file in current dir
                all=$(find . -maxdepth 1 -type f -printf "%f\n") # all files of current dir
                comm -23 <(printf "%s\n" $all | sort) <(printf "%s\n" $tracked | xargs -n1 basename | sort) # files in current dir not part of git repo
            fi
        else
            echo "current path is not part of a git repo"
        fi
    }    
    gp(){ # workaround for git token
        if git rev-parse --git-dir > /dev/null 2>&1; then
            if [ -f ~/.myprofile ]; then
                if [ -z "$1" ]; then
                    echo "must provide one argument; one of: github.com, git.smhi.se"
                    return 1
                fi
                token=$(grep "pat "${1} ~/.myprofile | cut -d ' ' -f1 | cut -c2-) # remove everything after space and first char
                if [ "$token" == "" ]; then
                    echo "did not found pat with domain pattern $1"
                    return 1
                fi
                url=$(git config --get remote.origin.url)         # https://[<user>]@github.com/<user_or_group>/<repo>.git
                repo=$(basename $(git rev-parse --show-toplevel))
                usergroup=$(basename $(dirname $url))
                domain=$(basename $(dirname $(dirname $url)))     # usergroup@github.com or github.com
                domain=${domain/${usergroup}@/""}                 # github.com    
                cmd="git push https://<token>@${domain}/${usergroup}/${repo}.git"
                echo "run '$cmd' ..."
                cmd="git push https://${token}@${domain}/${usergroup}/${repo}.git"
                #cmd="git push https://oauth2:${token}@${domain}/${usergroup}/${repo}.git"
                #echo "run '$cmd'"
                eval $cmd
                history -d $((HISTCMD)) # remove last command from bash history
                unset $token
                git fetch # change `ahead by 1 commit` to `up-to-date`; not needed for default `git push`
            else
                echo "could not find ~/.myprofile"
                return 1
            fi
        else
            echo "current dir $(pwd) is not a repo dir"
            return 1
        fi
    } # gp
    if [[ -f ~/sw/git-diff-blame/git-diff-blame ]]; then # git diff blame git-diff-blame
        #alias git-diff-blame="~/sw/git-diff-blame/git-diff-blame"
        alias gd="~/sw/git-diff-blame/git-diff-blame"
    fi
    svnhelp(){
        echo "colored diff: svn diff | vi -R - # or vim"
    }
    cdohgrep(){
        cdo -h 2>&1 | grep --color=auto -i $1
    }
    cdohelp(){
        echo "man cdo does not exist: cdo manual -> Intro -> Usage -> Options"
        echo "cdo --operators"
        echo "cdo --argument_groups"
        echo "cdo -h [operator] # e.g. cdo -h after"
        echo "-b F32: to float; F64: to double"
        echo "cdo -b f32 copy  infile ofile \# convert short to float with add_offset and scale_factor" 
        echo "cdo [-t echam6] -f nc copy file.grb file.nc"
        echo "cdo -f nc -t ecmwf -setgridtype,regular"
        echo "for f in *01.grb; do echo \$f; cdo -t echam6 -f nc copy \$f \$f.nc; done"
        echo "cdo -select,name=temp2 *echam6_echam_* tmp1 && cdo fldmean tmp1 tmp2 && ncview tmp2" 
        echo "cdo -select,name=var167 *echam6_echam_* tmp1 && cdo fldmean tmp1 tmp2 && ncview tmp2" 
        echo "merge different variables: cdo merge"
        echo "cdo chname,var1,var2 in.nc out.nc"
        echo "for f in *.nc; do echo \$f; ncrename -v XXX,YYY \$f; done"
        echo "for f in *.nc; do echo \$f; ncdump -h \$f | grep var167; done"
        echo "nohup sh -c 'for y in {2081..2100}; do ...; done' > test.log 2>&1 &"
        echo "setgrid,global_1 in out --> lon from 0"
        echo "setgrid,r360x180 in out --> lon from -180"
        echo "cdo setmissval,nan"
        echo "cdo daymean: 1,2,miss,3 --> (1+2+3)/3      = 6/3    = 2"
        echo "cdo dayavg:  1,2,miss,3 --> (1+2+miss+3)/4 = miss/4 = miss"
        echo "cdo -r copy in out"
        echo "cdo -seltimestep,\$(cat steps | tr ' ' ',') in out"
        echo "cdo expr,'hvel=sqrt(u*u + v*v)' fin fout"
        echo "cdo trend in intercepts slopes"
        echo "export CDO_WEIGHT_MODE=off; export MAX_JACOBI_ITER=100"
        echo "cdo sub fin -timmean fin anom_fin # calc anomaly"
        echo "cdo eof,40 anom_fin eigen_val eigen_vec" 
        echo "cdo eofcoeff eigen_vec anom_fin obase"
        echo "Interpolate remapcon        First order conservative remapping"
        echo "Interpolate remapcon2       Second order conservative remapping"
        echo "Interpolate remapbil        Bilinear interpolation"
        echo "Interpolate remapbic        Bicubic interpolation"
        echo "Interpolate remapdis        Distance-weighted averaging"
        echo "Interpolate remapnn         Nearest neighbor remapping; remapnn,lon=108.12673/lat=52.787004"
        echo "Interpolate remaplaf        Largest area fraction remapping"
        echo "Remap       remap           SCRIP grid remapping"
        echo "-setattribute,precip@long_name='Precipitation' in out"
    } # cdohelp
    # argument list too long
    #/bin/echo "$(printf "%*s" 131071 ".")" > /dev/null
    #/bin/echo "$(printf "%*s" 131072 ".")" > /dev/null --> too long
    # --> $(getconf PAGE_SIZE)*32 = 4096*32 = 131072
    # --> this is hardcoded in binfmts.h
    # getconf ARG_MAX = 2097152
    # too long: 2612711 characters wout multiple spaces
    # ok:       2612710 characters wout multiple spaces
    # start counting from: `cdo a b`
    #                          1234 -> nchar = 4 in this example
    # --> it seems multiple spaces count as single spaces
    # --> it seems to be independet of the number of input files provided (29355 in this example)
    ncohelp(){
        echo "ncap2 -s 'time=time-1999;' in.nc foo.nc"
        echo "ncap2 -O -s 'TEMP=double(TEMP)' in.nc out.nc"
        echo "ncap2 -O -s 'defdim(\"bnds\",2); time_bnds=make_bounds(time,\$bnds,\"time_bnds\");' in.nc out.nc"
        echo "ncatted -O -h -a history,global,d,, mesh_core_deriv_3d_geo.nc # remove history"
        echo "nccopy -u -w -c time/12,nodes_2d/126859 fin fout"
        echo "ncks --fix_rec_dmn <dimname> <ifile> <ofile> # unlimied --> fixed dim (e.g. time)"
        echo "ncks -d nod3d,from-1,to-1 # or -F for 1-based indexing"
        echo "ncrename -d record,time out.nc # rename dim"
        echo "ncpdq -a time,depth in out # switch dims"
        echo "ncwa -a lev in.nc out.nc # remove dim"
    }
    ncdumphelp() {
        echo "ncdump -hs fin # show chunks"
    }
    ncviewhelp(){
        echo "ncview -minmax all Sample.nc"
    }
    alias ncviewa="ncview -minmax all"
    gribhelp(){
        echo "grib_ls fin"
        echo "grib_copy -w shortName=myname fin fout"
        echo "grib_copy fin 'fout_[shortName]' # split; must use quotes for fout"
        echo "codes_split_file [-v] nchunks input # faster split than grib_copy; nchunks=-1 for individual messages"
    }
    pyhelp(){
        echo "python -c 'import sys; print(sys.path)'" 
        echo "exec(open('script.py').read())"
        echo "exec(open(os.path.expanduser('~/foo.py')).read())"
        echo "%run scriptname # only works in ipython"
        echo "ipynb2py: jupyter nbconvert --to script 'file.ipynb'"
        echo "ipynb2py: jupyter nbconvert --output-dir='~/' --to script 'file.ipynb'"
        echo "python -mpdb ~/.local/bin/esm_master # then: c for continue (h for help)"
        echo "pip install [-e .] # --editable runs 'python setup.py develop' and not the default 'python setup.py install'"
    }
    condahelp(){
        echo "conda info"
        echo "conda config --show-sources"
        echo "conda config --describe"
        echo "conda shell.bash activate"
        echo "conda shell.bash deactivate"
        echo "conda env list"
        echo "conda create -n myname"
        echo "conda create -p /path <env>"
        echo "conda activate <env>"
        echo "conda deactivate"
        echo "conda env list"
        echo "conda list"
        echo "conda install -c <chan> <pkg>"
        echo "conda clean --all"
        echo "conda env remove -n <env> # this removes all packages installed in <env>"
    }
    jupyterhelp(){
        echo "ssh -L localhost:9999:localhost:9999 user@host"
        echo "jupyter lab --no-browser --port=9999"
    }
    texhelp(){
        echo "latexdiff old.tex new.tex > changes.tex # -t UNDERLINE; -t CFONT; set link colors to black"
        echo "trace back missing bracket: add '\\endinput' and recompile to come closer to the error line"
        # minimal example:
        echo "\documentclass{article}"
        echo "\usepackage[margin=0.7in]{geometry}"
        echo "\usepackage[parfill]{parskip}"
        echo "\usepackage[utf8]{inputenc}"
        echo "\usepackage{amsmath,amssymb,amsfonts,amsthm}"
        echo "\begin{document}"
        echo "\end{document}"
    }
    texclear(){( # `(` necessary for -e
        set -e
        if [ "$#" -ne 1 ]; then
            echo "provide basename of file to clean"
            exit
        fi
        files=($1.{aux,bbl,blg,log,out,pdf})
        echo "rm ${files[*]} ..."
        rm $1.{aux,bbl,blg,log,out,pdf}
    )}
    inkscapehelp(){
        echo "clip/mask: draw rectangle over area you want to clip. select both. objects -> clip -> set"
        echo "crop white space: select -> edit -> resize page to selection"
    }
    slurmhelp(){
        echo "scontrol show jobid -dd jobid"
        echo "scancel {1000..1050}"
        echo "scontrol show partition <name>"
        echo "sacctmgr -s show user name=\$USER format=Account%30"
    }
    officehelp(){
        echo "enter line before TOC:"
        echo "1: rightclick toc -> edit -> do not protect index against manual changes"
        echo "2: click in upper left corner of toc"
        echo "3: [alt+enter] twice"
        echo "enter field number = page count +1:"
        echo "1: insert -> field -> more fields"
        echo "2: insert formula -> formula = page+1"
        echo "libreoffice --convert-to \"pdf\" file.txt"
        echo "copy pic from googledoc: 1: double-click pic, 2: shift+right-click on pic"
    }
    octavehelp(){
        echo "yay -S octave-netcdf"
        echo "pkg install -forge -verbose netcdf"
        echo "pkg load netcdf"
        echo "test_netcdf"
        echo "pkg install -forge -verbose io"
        echo "pkg install -forge -verbose statistics"
        echo "pkg install -forge -verbose octproj"
    }
    qgishelp(){
        echo "get hull"
        echo "1 save lon,lat coords as two columns ascii"
        echo "2 qgis -> layer -> add layer -> add delimited text layer"
        echo "3 processing toolbox -> vector geometry -> concave hull k-nearest neighbour"
        echo "4 processing toolbox -> vector geometry -> extract vertices"
        echo "5 processing toolbox -> vector geometry -> add geometry attributes"
        echo "6 export -> csv -> select coords"
    }
    wgethelp(){
        echo "user='cdanek1'"; user='cdanek1'
        echo "pw='mypw'"; pw='mypw'
        echo "url='ftp://my.cmems-du.eu/Core/GLOBAL_MULTIYEAR_PHY_001_030/cmems_mod_glo_phy_my_0.083_P1D-m/1993/01/mercatorglorys12v1_gl12_mean_19930101_R19930106.nc'"
        echo "url='ftp://my.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047/dataset-duacs-rep-global-merged-allsat-phy-l4/2016/*'"
        echo "url='ftp://my.cmems-du.eu/Core/OCEANCOLOUR_GLO_BGC_L4_MY_009_104/cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D/'"
        echo "url='ftp://my.cmems-du.eu/Core/OCEANCOLOUR_GLO_BGC_L4_MY_009_104/cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M /'"
        echo "url='ftp://my.cmems-du.eu/Core/OCEANCOLOUR_GLO_BGC_L4_MY_009_104/cmems_obs-oc_glo_bgc-pp_my_l4-multi-4km_P1M /'"
        echo "url='ftp://my.cmems-du.eu/Core/GLOBAL_MULTIYEAR_BGC_001_029/cmems_mod_glo_bgc_my_0.25_P1M-m/'"
        url='ftp://my.cmems-du.eu/Core/GLOBAL_MULTIYEAR_BGC_001_029/cmems_mod_glo_bgc_my_0.25_P1M-m/'
        echo "# password issue"
        echo "cat ~/.wgetrc"
        echo "user=$user"
        echo "password=$pw"
        echo "# get file names wget; does not work"
        echo "wget -r -np --spider -R "index.html*" -e robots=off <url> \# ignore all index.html; does not work, also not with --no-remove-listing"
        echo "# get file names curl; does not work recursively"
        echo "curl -l $url --user '$user':'$pw' # enter user and pw directly"
        echo "curl --netrc-file ~/.netrc -l $url # use user and pw from .netrc"
        echo "# get file names lftp"
        echo "lftp <url> # and"
        echo "du -a > fnames.txt # 'lftp -e \"du -a\" <url> > fnames.txt' does not work"
        echo "lftp -e 'find;quit' ftp://ftp.example.com/ > listing.txt # seems to work"
        echo "# download in background (-bqc):"
        echo "nohup wget -r -bqc --no-directories --user='$user' --password='$pw' $url > dl.log 2>&1 & # this will show pw in top"
        echo "nohup wget -r -bqc --no-directories $url > dl.log 2>&1 & # will look for ~/.wgetrc; -P /path/to/destination"
        echo "if [[ \$(wget -S --spider \$url 2>&1 | grep 'HTTP/1.1 200 OK') ]]; then echo \"true\"; fi"
        echo "# fnames from file, ignore already existing and set destination"
        echo "wget -N -i fnames.txt -P /path/to/out"
    } # wgethelp
    cmakehelp(){
        echo "cmake -LAH"
        echo "add to cmakelists.txt:"
        echo "execute_process(COMMAND \"\${CMAKE_COMMAND}\" \"-E\" \"environment\")"
        echo "add to cmakelists.txt:"
        echo "get_cmake_property(_variableNames VARIABLES)"
        echo "list (SORT _variableNames)"
        echo "foreach (_variableName \${_variableNames})"
        echo "    message(STATUS \"\${_variableName}=\${\${_variableName}}\")"
        echo "endforeach()"
        echo "add to cmakelists.txt:"
        echo "set(foo \$ENV{NETCDF_CXX_INCLUDE_DIRECTORIES})"
        echo "message(STATUS \"bar \${too}\")"
    }
    configurehelp(){
        echo "./configure CFLAGS=\"-I/usr/local/include\" LDFLAGS=\"-L/usr/local/lib\""
    }
    mypath(){
        Rscript -e "strsplit(system('echo $PATH', intern=T), ':')[[1]]"
    }

    # aliase (check with 'type alias')
    alias ls='ls --color=auto -F' # default from /etc/skel/.bashrc: ls='ls --color=auto'
    alias ll='ls --color=auto -lFh'
    alias la='ls --color=auto -alFh'
    alias llxl='ll -I "xios*" -I "lucia*"'
    # ls only files excluding dotfiles
    alias lsf='find . -maxdepth 1 -type f -a ! -iname '\''.*'\'' -print0 | xargs -0r ls'
    # ls only files including dotfiles
    alias lsf2='find . -maxdepth 1 -type f -print0 | xargs -0r ls'
    #alias grep="grep --color=auto"
    alias grep="grep --color=always" # keeps color when piped to less 
    alias R='R --quiet'
    alias R0='unalias R 2>/dev/null; R --no-init-file'
    alias vi='vim'
    if check_existance vimx; then
        alias vi='vimx' # for +clipboard
        alias vim='vimx'
    fi
    alias less="less -i -R" # ignore case; escape colors

    # own variables
    export VISUAL=vim
    export EDITOR="$VISUAL" # also applies to git
    
    # hostname
    printf "\$(hostname)@\$(hostname -d): "
    printf "$(hostname)@"
    domain=$(timeout 2 hostname -d) # todo: can be slow if domain is "(none)"; `dnsdomainname`?
    if [ $? -eq 124 ]; then # timeout
        echo "<'hostname -d' forced timeout after 2 sec>"
    else
        echo "$domain"
    fi

    # external ip address
    # todo: dig +short myip.opendns.com @resolver4.opendns.com
    if false; then
        #if [ ! "$domain" == "(none)" ]; then
            printf "  get public ip: 'wget -qO- ifconfig.me' ... "
            wget -q --spider ifconfig.me
            if [ $? -eq 0 ]; then # online
                ip=$(wget -qO- ifconfig.me)
                #ip=$(curl ifconfig.me) # needs awk/cut
                #ip=$(dig +short ANY whoami.akamai.net @ns1-1.akamaitech.net) # faster than wget/curl but does not work on every HPC
                echo "$ip"
                printf '%s' "  --> 'nslookup $ip': "
                echo "'$(nslookup $ip | head -1)'"
            else
                echo "  no internet connection or ifconfig.me is offline"
            fi 
        #fi
    fi

    # check cpus
    if check_existance lscpu; then
        printf "/proc/cpuinfo: "
        cat /proc/cpuinfo | grep --color=never "model name" | head -1
        #printf "lscpu | grep Model name::"
        #lscpu | grep --color=never "Model name:"
        printf "\$(nproc): "; nproc
        #lscpu | grep --color=never "^CPU(s):"
    fi

    # uptime
    printf "\$(uptime): "
    uptime | awk -F'( |,|:)+' '{print $6,$7",",$8,"hours,",$9,"minutes"}'

    # which OS/distro
    if [ -f /etc/os-release ]; then
        printf "cat /etc/os-release | head -1: "
        cat /etc/os-release | head -1
    else
        echo "/etc/os-release does not exist. what crazy OS/distro is this!?"
    fi
    if [ -f /proc/version ]; then
        printf "cat /proc/version: "
        cat /proc/version # gives distro name; `uname` does not; `lsb_release -a` not always available
    else
        echo "/proc/version does not exist. what crazy OS/distro is this!?"
    fi
    
    # x11 or wayland or not
    echo "\$XDG_SESSION_TYPE = $XDG_SESSION_TYPE"
    
    # display server, display/login manager
    if [ -f /etc/systemd/system/display-manager.service ]; then
        printf "readlink -f /etc/systemd/system/display-manager.service = "
        readlink -f /etc/systemd/system/display-manager.service
    else
        echo "/etc/systemd/system/display-manager.service does not exist. which display/login manager?"
    fi
    printf "ps auxf | awk '{print \$11}' | \\grep -e dm\$ -e slim\$ = "
    ps auxf | awk '{print $11}' | \grep -e dm -e slim$
    
    # window manager
    echo "\$DESKTOP_SESSION = $DESKTOP_SESSION"
    echo "\$XDG_CURRENT_DESKTOP = $XDG_CURRENT_DESKTOP"
    echo "\$GDMSESSION = $GDMSESSION"

    # package manager 
    declare -A osInfo;
    osInfo[/etc/redhat-release]=yum
    osInfo[/etc/arch-release]=pacman
    osInfo[/etc/gentoo-release]=emerge
    osInfo[/etc/SuSE-release]=zypp
    osInfo[/etc/debian_version]=apt-get
    for f in ${!osInfo[@]}; do
        if [[ -f $f ]]; then
            printf "%s" "\"$f\" exists --> package manager is ${osInfo[$f]}"
            if [[ ${osInfo[$f]} == "pacman" ]]; then
                printf "%s" " --> /var/cache/pacman/pkg needs "
                printf "%s " $(du -hc /var/cache/pacman/pkg | tail -1)
            fi
            echo
        fi
    done
    if check_existance yay; then
        printf "%s" " --> ~/.cache/yay needs "
        printf "%s " $(du -hc ~/.cache/yay | tail -1)
        echo
    fi

    # print free disk space on ~/ 
    printf "~/ "
    df -h ~/

    # which tty
    printf "\$(tty): "; tty
    #set -o vi # unfortunatley this breaks ctrl+a/+e
    # kill open remote sessions:
    #ssh cdanek@stan1.awi.de w
    #ssh cdanek@stan1.awi.de pkill -9 -t pts/3
    #ssh cdanek@stan1.awi.de pkill -u cdanek

    # which shell
    printf "\$SHELL: "; echo $SHELL

    # show what kind of shell (at this point it must be an interactive shell since)
    # h: Remember the location of commands as they are looked up for execution.  This is enabled by default.
    # i: interactive
    # m: Monitor mode.  Job control is enabled
    # B: The shell performs brace expansion (see Brace Expansion above).  This is on by default
    # H: Enable !  style history substitution.  This option is on by default when the shell is interactive.
    printf "\$- = \"$-\""
    if [[ $- == *i* ]]; then
        printf " --> \"i\" for interactive shell"
    fi
    echo

    # check if login shell (cannot check $0 from within this script)
    if check_existance shopt; then
        if shopt -q login_shell; then
            echo "\$0 = \"-$(basename $SHELL)\" or \`shopt login_shell\` = on -> login shell"
        else
            echo "\$0 = \"$(basename $SHELL)\" or \`shopt login_shell\` = off -> not login shell"
        fi
    else
        echo "cannot check if this is a login or non-login shell since \`shopt\` is not installed and"
        echo "\$0 cannot be evaluated from within this .bashrc"
    fi

    # check if module tun is available or not (it is not after system upgrade)
    if check_existance modprobe; then
        modprobe tun &> /dev/null # silent output
        if [ $? -ne 0 ]; then # if not successfull either due to missing permissions or file not found
            tun_file=$(find /lib/modules/`uname -r` -print | grep -i "tun.ko")
            if [ ${#tun_file} == 0 ]; then # if missing file 
                echo "'modprobe tun' raised some problem, consider restart:"
                modprobe tun
            fi
        fi
    fi

    # check if vim/vimx is installed and supports clipboard pasting
    if check_existance vim || check_existance vimx; then
        if check_existance vim; then
            echo $(which vim)
            tmp=$(vim --version) # | grep clipboard) # <-- with incl. grep the later check `"$tmp" == *"+clipboard"*` does not work anymore o_O
            #tmp="+clipboard +lambda -python +viminfo +cryptv +localmap +scrollbind +wayland_clipboard +ex_extra +mouse_sgr +tag_binary +xterm_clipboard"
            vim_clipboard=1 # default: no clipboard
            vim_xterm_clipboard=1
            if [[ "$tmp" == *"+clipboard"* ]]; then 
                vim_clipboard=0 # vim has +clipboard 
            fi
            if [[ "$tmp" == *"+xterm_clipboard"* ]]; then 
                vim_xterm_clipboard=0 # vim has +xterm_clipboard 
            fi
            if [[ $vim_clipboard == 1 ]] && [[ $vim_xterm_clipboard == 1 ]]; then
                vim_return=1 # -clipboard
            else
                vim_return=0 # +clipboard
            fi
        else
            vim_return=1 # no vim
        fi
        if check_existance vimx; then
            tmp=$(vimx --version | grep clipboard)
            vimx_clipboard=1 # default: no clipboard
            vimx_xterm_clipboard=1
            if [[ "$tmp" == *"+clipboard"* ]]; then 
                vimx_clipboard=0 # vimx has +clipboard 
            fi
            if [[ "$tmp" == *"+xterm_clipboard"* ]]; then 
                vimx_xterm_clipboard=0 # vimx has +xterm_clipboard 
            fi
            if [[ $vimx_clipboard == 1 ]] && [[ $vimx_xterm_clipboard == 1 ]]; then
                vimx_return=1 # -clipboard
            else
                vimx_return=0 # +clipboard
            fi
        else
            vimx_return=1 # no vimx
        fi
        if check_existance vim; then
            if [[ $vim_return == 1 ]]; then
                echo warning: vim exists but with -clipboard and -xterm_clipboard
            fi
        fi
        if check_existance vimx; then
            if [[ $vimx_return == 1 ]]; then
                echo warning: vimx exists but with -clipboard and -xterm_clipboard
            fi
        fi
    fi # if vim or vimx exist
    
    # check if go exists and change default go path
    if check_existance go; then
        go version &> /dev/null # silent output
        if [ $? -eq 0 ]; then # if successfull
            go env -w GOPATH="${HOME}/.local/share/go"
        fi
    fi

    # check if there are cronjobs running
    if check_existance crontab; then
        printf "crontab -l ..."
        ct=$(crontab -l 2>/dev/null)
        if [[ $ct ]]; then # is set and it is not empty
            #echo ""
            #echo "$ct"
            readarray -t arr <<<$ct # split vector by \n to array
            cnt=0
            for line in "${arr[@]}"; do
                #if [[ "$line" =~ ^#.* ]]; then # =~: regex; starts with "#"
                if [[ "$line" != \#* && ${#line} != 0 ]]; then # starts not with "#"
                    cnt=$((cnt+1))
                    if (( $cnt == 1 )); then
                        echo
                    fi
                    echo "  active cronjob $cnt: $line"
                fi
            done
            elif [[ ! $ct ]]; then # is not set or it is set to an empty string
            printf " no active cronjob running\n"
        fi
    fi # if crontab exists
    
    # check if there are systemctl timers running
    if check_existance systemctl; then
        printf "systemctl list-timers ... "
        timers=$(systemctl list-timers) #; echo "$timers"
        readarray -t arr <<<$timers # split vector by \n to array #; echo "$arr"
        from=1
        to=${#arr[@]} #; echo "$to lines"
        to=$((to-4)) #; echo "select lines $((from+1)) to $((to+1)) ..."
        arr2=("${arr[@]:$from:$to}") #; echo "--> ${#arr2[@]} lines"
        system_services=(systemd-tmpfiles-clean.service 
                         logrotate.service
                         man-db.service
                         shadow.service
                         updatedb.service
                         motd-news.service
                         apt-daily.service
                         apt-daily-upgrade.service
                         fstrim.service
                         snapd.snap-repair.service
                         ureadahead-stop.service)
        for line in "${arr2[@]}"; do
            #echo $line
            if [[ "$line" = *" ago "* ]]; then # check if valid line
                service=${line##* } # get active service --> last component of line
                # exclude default system services
                #if [[ ${system_services[*]} =~ (^|[[:space:]])"$service"($|[[:space:]]) ]]; then # =~: regex; if included
                #    printf "\n  service \"$service\" is included in system_services. skip."
                if ! [[ ${system_services[*]} =~ (^|[[:space:]])"$service"($|[[:space:]]) ]]; then # if not included
                    printf "\n  systemctl status $service:"
                    status=$(systemctl status $service) #; echo "$status"
                    readarray -t arr3 <<<$status # split vector by \n to array
                    #echo "$arr3"
                    for line in "${arr3[@]}"; do
                        if [[ "$line" =~ ^"    Process: ".* ]]; then # starts with "#"
                            #echo $line
                            cmd=${line##*"ExecStart="}
                            printf "\n    $cmd"
                        fi
                    done    
                fi
            fi
        done
        echo
    fi # if systemctl exists
    
    # find module binary
    # $?: last command return value
    # $*: list of all args
    # works: eval `/sw/rhel6-x64/tcl/modules-3.2.10/Modules/$MODULE_VERSION/bin/modulecmd bash list`
    # works: eval `/sw/rhel6-x64/tcl/modules-3.2.10/Modules/$MODULE_VERSION/bin/modulecmd bash purge`
    # works: eval `/sw/rhel6-x64/tcl/modules-3.2.10/Modules/$MODULE_VERSION/bin/modulecmd bash load gcc`
    # $ ldd binary (executes the binary!)
    # $ readelf -d | grep NEEDED (does not execute the binary)
    if check_existance module; then
        printf "type module: "
        echo $(type module)
        modulegrep(){ # e.g. "^r/", "cdo"
            if [ $# -eq 0 ]; then
                echo "Usage: 'modulegrep cdo' or 'modulegrep ^r/' will run 'module avail -t 2>&1 | grep -i \$1'"
            else
                #echo "run 'module avail -t 2>&1 | grep -i $1'"
                module avail -t 2>&1 | grep -i $1 | sort
            fi
        }
        echo "loaded startup modules:"; module list
    else
        echo "module command is not set"
    fi

    # run R stuff if available
    if check_existance Rscript; then
        if check_existance mytimes.r; then
            mytimes.r
        fi
    fi
   
    # python config
    if [ -f ~/.pyprofile ]; then
        echo "run 'export PYTHONSTARTUP=~/.pyprofile' ..."
        export PYTHONSTARTUP=~/.pyprofile
    fi
    
    # set links to dotfiles-repo functions to bin
    fs=(
        ntfs_fix_filenames.r
        diff_filelists.r diff_namelists.r
        cpu cpuall cpu_total mem scpd 
        rnohup mnohup nclnohup 
        checkall check_nc_integrity.r 
        myquota.r 
        myfinger myfinger.r finduser.r 
        get_timestep.r get_dir_sizes.sh 
        ping_wait
        slurm_wait slurm_check.r slurm_stats.r slurm_get_npes.r
        esm_check_yaml.r esm_check_err.r esm_check_paths.r esm_get_output.r
        esm_get_esm_version_exp esm_get_esm_version_home
        esm_tools_helpers.sh
        echam_get_mvstreams_from_atmout.r echam_set_time_weight.r
        jsbach_pft_wrt_box.r jsbach_tile2pft.r jsbach_plot_pft.r 
        fesom_setgrid_regrid.r 
        fesom1_get_meshinfo.r fesom1_shifttime_-1dt.r fesom1_nod3d_levelwise.r fesom1_nod3d_levelwise_fast.r
        fesom1_plot_2d.r fesom1_landice2nodes_plot.r
        recom_calc_pCO2a.r
        oasis_get_B_grid.sh oasis_split_grids.sh oasis_plot_mask.r
        oifs_check_input.r oifs_post_ifs.stat.r
        esgf_get_variables.r esgf_json_tree.sh
        slk2list.r
        select_winter_summer.r
        mycdotimstat_loop.r mycdoseasmean.r mycdoseassum.r 
        mycdosplitlevel.r
        mytrend.r mycdotrend.r mycdoeof.r
        mycat_areadepth mycat_time.r mycat_time_depth mycat_time_depth_lat.r mycat_time_depth.r
        myeof.r plotmyeof.r
        myncrcat.r
        mycat_moc.r
        my_gsw_O2sol_SP_pt.r calc_DIC_remin.r
        rechunk.r remap_quasi_conservative.r
        nominal_resolution.r convert_lon_360_to_180.r 
        takahashi_etal_2002.r
        precip_mm_day2month.r precip_mm_month2day.r
        when.r kelv feet dom beaufort.r inertial.r
        )
    mkdir -p ~/bin
    for f in "${fs[@]}"; do
        if [ -f ~/sw/dotfiles/functions/$f ]; then
            if [ ! -f ~/bin/$f ]; then
                echo "ln -s ~/sw/dotfiles/functions/$f ~/bin/$f ..."
                ln -s ~/sw/dotfiles/functions/$f ~/bin/$f
            fi
        fi
    done
   
    # slurm specific stuff
    if [ -f ~/sw/dotfiles/functions/slurm_jobid_autocomplete.sh ]; then
        source ~/sw/dotfiles/functions/slurm_jobid_autocomplete.sh
        if check_existance scontrol; then
            echo "activate slurm jobid autocomplete for scontrol and scancel"
            complete -F _cluster_jobs scontrol
            complete -F _cluster_jobs scancel
            if check_existance slurm_wait; then
                echo "activate slurm jobid autocomplete for slurm_wait"
                complete -F _cluster_jobs slurm_wait
            fi
        fi
    fi
    if check_existance squeue; then
        sme() {
            echo "squeue -u $(whoami) --sort=-i -o \"%.8i %.12P %.25j %.25a %.15u %.2t %.20V %.20S %.10M %.6D %R\" # jobid partition jobname account user status submit_time start_time time maxtime nodes nodelist"
            squeue -u $(whoami) --sort=-i -o "%.8i %.12P %.25j %.25a %.15u %.2t %.20V %.20S %.10M %.6D %R"
        } 
        smi() { squeue -u $(whoami) --sort=-i -i 1 -o "%.8i %.12P %.25j %.25a %.15u %.2t %.20V %.20S %.10M %.6D %R" ; }
    fi
    if check_existance sacctmgr; then
        smy() {
            echo "sacctmgr -s show user name=\$(whoami) format=\"Account%-30\""
            sacctmgr -s show user name=$(whoami) format="Account%-30"
        } 
    fi
    if check_existance sshare; then
        sshareme() {
            echo "sshare -U -u \$(whoami) --format=\"Account%-30,User%15,NormShares,RawUsage,EffectvUsage,FairShare\""
            sshare -U -u $(whoami) --format="Account%-30,User%15,NormShares,RawUsage,EffectvUsage,FairShare"
        }
    fi
    if check_existance scontrol; then
        smee() {
            if [ -z "$1" ]; then
                echo "> smee <jobid>"
            else 
                scontrol show jobid -dd $1
            fi
        }
    fi

    # recom stuff
    recom_normalize_nml() {
        fout=$(basename $1)
        cp $1 $fout
        sed -i 's/d0/0/' $fout # d0 --> 0
        sed -i 's/1\.d-/1e-/' $fout # 1.d- --> 1e-
        sed -i '/^[[:space:]]*$/d' $fout # rm white spaces
    }

    # esm_tools stuff
    if [ -f ~/bin/esm_tools_helpers.sh ]; then
        source ~/bin/esm_tools_helpers.sh
    fi
    if [ -f ~/esm/awicm3-v3.1.1/oasis/util/lucia/lucia ]; then
        alias lucia='~/esm/awicm3-v3.1.2/oasis/util/lucia/lucia'
    fi
    if [ -f ~/esm/awicm3-v3.1.2/oasis/util/lucia/lucia ]; then
        alias lucia='~/esm/awicm3-v3.1.2/oasis/util/lucia/lucia'
    fi

    # load private stuff at the end to overwrite defaults from above
    if [ -f ~/.myprofile ]; then
        printf '%*s' "$ncol" | tr ' ' "-"
        printf " ~/.myprofile start "
        printf '%*s' "$ncol" | tr ' ' "-"
        echo ""
        source ~/.myprofile
        printf '%*s' "$ncol" | tr ' ' "-"
        printf " ~/.myprofile finish "
        printf '%*s' "$ncol" | tr ' ' "-"
        echo ""
    fi
    
    # replace prompt with liquidprompt if git is available (thats why do it after .myprofile)
    if true; then # use liquidprompt prompt
        if [ -x "$(command -v liquidprompt)" ]; then 
            LP_PS1_PREFIX="\$(get_battery_capacity)\$(get_battery_status)\$(get_current_temp)"
            source liquidprompt # check ~/.liquidpromptrc
        else 
            echo "could not load liquidprompt --> run 'git clone https://github.com/nojhan/liquidprompt.git' and ln -s ~/sw/liquidprompt/liquidprompt ~/bin/liquidprompt"
        fi
    fi
    if false; then # use starship prompt
        if [ -x "$(command -v starship)" ]; then 
            eval "$(starship init bash)"
        else 
            echo "could not load starship --> run 'curl -O https://starship.rs/install.sh', 'chmod +x install.sh', './install.sh -b ~/.local/bin', 'ln -s ~/sw/starship/starship ~/bin/starship'"
        fi
    fi

    # run bash stuff if available
    if ! check_existance nc-config; then
        echo 'nc-config' is missing!
    fi

    # change default pip/conda paths
    # xarray:   2.7G
    # pyfesom2: 1.9G 4.6G
    # pyint:    1.8G 6.4G
    if [ ! -z ${mywork+x} ]; then # not empty
        echo "mywork = ${mywork} is set"
        # todo: how to do that correctly?
        #export PYTHONUSERBASE="${mywork}/sw/pip" # = pythons site.USER_SITE
        #echo "--> change default pip path from ~/.local to PYTHONUSERBASE=${PYTHONUSERBASE} ..."
        #export PATH="${PYTHONUSERBASE}/bin:$PATH"
        # without tilde expansion (hide name of home to be machine-agnostic):
        if false; then
            conda_prefix="${mywork}/sw/conda/envs"
            conda_envs_dirs="${conda_prefix}"
            conda_pkgs_dirs="${mywork}/sw/conda/pkgs"
            echo "--> change default conda path from ~/.conda:"
            echo "       CONDA_PREFIX=${conda_prefix}"
            echo "       CONDA_ENVS_DIRS=${conda_envs_dirs}"
            echo "       CONDA_PKGS_DIRS=${conda_pkgs_dirs}"
            # with tilde expansion: necessary for conda and mkdir:
            export CONDA_PREFIX="${conda_prefix/#\~/$HOME}"
            export CONDA_ENVS_DIRS="${conda_envs_dirs/#\~/$HOME}"
            export CONDA_PKGS_DIRS="${conda_pkgs_dirs/#\~/$HOME}"
            # todo: conda activate --stack
            # todo: when both CONDA_PREFIX and PIP_TARGET are set: ERROR: Can not combine '--user' and '--target'
            #mkdir -p ${CONDA_PREFIX} ${CONDA_ENVS_DIRS} ${CONDA_PKGS_DIRS} ${PIP_PREFIX}
            #export PYTHONPATH=${conda_envs_dirs/#\~/$HOME}:${conda_pkgs_dirs/#\~/$HOME}:${pip_prefix/#\~/$HOME}:$PYTHONPATH
        fi # true/false
        if false; then # pip_prefix cannot be used on system where home, work, tmp, ... are mounted on different file systems
                       # --> at the end of the build the linking between /tmp and local fails: OSError: [Errno 18] Invalid cross-device link
            pip_prefix="${mywork}/sw"
            echo "--> set PIP_PREFIX = ${pip_prefix}"
            export PIP_PREFIX="${pip_prefix/#\~/$HOME}"
        fi # true/false
        #if true; then
        if false; then
            pythonuserbase="${mywork}/sw/pip"
            pyversion=$(python -V 2>&1 | \grep -Po '(?<=Python )(.+)') # e.g. 3.10.10
            pyversion=${pyversion%.*} # e.g. 3.10
            pythonpath="${pythonuserbase}/lib/python${pyversion}/site-packages"
            echo "--> set PYTHONUSERBASE = $pythonuserbase"
            echo "            PYTHONPATH = $pythonpath"
            echo "-->         add to PATH: ${pythonuserbase}/bin"
            export PYTHONUSERBASE="${pythonuserbase/#\~/$HOME}"
            export PYTHONPATH="${pythonpath/#\~/$HOME}"
            export PATH=${PYTHONUSERBASE}/bin:$PATH
            echo "--> install with 'pip install [-v] --user [-e .]'"
        fi
        conda_deactivate(){
            echo "---------------- conda_deactivate() ----------------"
            echo "run 'conda deactivate' ..."
            conda deactivate
            echo "run 'export CONDA_PREFIX=${conda_prefix}' ..."
            export CONDA_PREFIX="${conda_prefix/#\~/$HOME}" # re-set CONDA_PREFIX since this is unset on default `conda deactivate`
            echo "---------------- conda_deactivate() ----------------"
        } # conda_deactivate
    else
        echo "'mywork' is not set --> do not change default pip/conda paths"
    fi # if mywork is set

    # finish
    printf '%*s' "$ncol" | tr ' ' "*"
    printf " ~/.bashrc finish "
    printf '%*s' "$ncol" | tr ' ' "*"
    echo ""

fi # interactive or not

