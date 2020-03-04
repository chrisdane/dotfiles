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
[[ $- != *i* ]] && return
#if [[ $- != *i* ]]; then
    #echo "*** .bashrc non-interactive session ***" 
    #echo "\$- = $-"
    #if shopt -q login_shell; then
    #    echo "\$0 = -$(basename $SHELL) or \`shopt login_shell\` = on -> login shell"
    #else
    #    echo "\$0 = $(basename $SHELL) or \`shopt login_shell\` = off -> not login shell"
    #    fi
    #echo "***************************************" 
    
## From here, everyhing happens only if running interactively
#else

    nch=30 # columns to print
    ncol=$(($(tput cols)/2))
    ncol=$(($ncol<$nch?$ncol:$nch)) # = min(ncol,nch)
    printf '%*s' "$ncol" | tr ' ' "*"
    printf " ~/.bashrc start "
    printf '%*s' "$ncol" | tr ' ' "*"
    echo ""

    ## Source global definitions
    if [ -f /etc/bashrc ]; then
        source /etc/bashrc
    fi

    ## use bash completion, if installed
    if [ -f /etc/bash_completion ]; then
        source /etc/bash_completion
    fi

    ## git automplete
    # from https://apple.stackexchange.com/questions/55875/git-auto-complete-for-branches-at-the-command-line
    ## does not work
    #if [ -f ~/.git-completion.bash ]; then
    #    source ~/.git-completion.bash
    #fi
    
    ## enable git colors
    #git config --global color.ui auto
    
    ## my bins (doing this recursively is not recommended; security)
    if [ -d ~/bin ]; then
        export PATH=~/bin/:$PATH
    fi
    if [ -d ~/.local/bin ]; then
        export PATH=~/.local/bin/:$PATH
    fi

    ## default prompt
    PS1='[\u@\h \W]\$ '

    ## my prompt
    PS1='\[\033[0;34m\]\h:$(pwd)/>\[\033[0m\] '

    # attach cpu temp to prompt if available
    if [ -x "$(command -v sensors)" ]; then
        show_temp(){
            sensors | grep -oP 'Package id 0.*?\+\K[0-9.]+'
        }
        PS1='\[\033[0;34m\]\h:$(show_temp)°C:$(pwd)/>\[\033[0m\] '
    fi

    # use liquidprompt if available https://github.com/nojhan/liquidprompt
    if [ -x "$(command -v liquidprompt)" ]; then
        # add cpu temp to liquidprompt
        # this is not perfect way yet
        # check ~/.liquidpromptrc
        #PROMPT_COMMAND=show_temp
        if [ -n "$(LC_ALL=C type -t show_temp)" ] && [ "$(LC_ALL=C type -t show_temp)" = function ]; then
               LP_PS1_PREFIX="$(show_temp)°C "
        fi
        source liquidprompt # need to source here because its not bash?!

    else 
        echo could not load liquidprompt
    fi

    # enable make autocomplete:
    # https://stackoverflow.com/questions/4188324/bash-completion-of-makefile-target
    complete -W "\`grep -oE '^[a-zA-Z0-9_.-]+:([^=]|$)' Makefile | sed 's/[^a-zA-Z0-9_.-]*$//'\`" make

    ## helper functions
    # check if program exists also if its masked by alias
    # if [ -x "$(command -v vi)" ]; then will not work if vi is aliased
    # https://unix.stackexchange.com/questions/85249/why-not-use-which-what-to-use-then/85250#85250
    check_existance(){
        if command -v $1 > /dev/null 2>&1; then
            return 0
        else
            return 1
        fi
    }
    tl(){
        file=$(ls -t | head -n1)
        echo `ls --color=auto -lFh $(pwd)/$file`
        tail -f $file
    }
    ml(){
        file=$(ls -t | head -n1)
        echo `ls --color=auto -lFh $(pwd)/$file`
        less -i $file
    }
    bashhelp(){
        echo "./script > script.log 2>&1 &"
        echo "ln -sfn path/to/file-name link-name"
        echo "find /usr/local/bin -lname '/usr/local/texlive/*'" -delete # delete links
        echo "for f in *1954*; do echo \$f; ln -s \$(pwd)/\$f /aim/\$f; done"
    }
    cdohelp(){
        echo "man cdo does not exist: cdo manual -> Intro -> Usage -> Options"
        echo "cdo --operators"
        echo "cdo -h [operator] # e.g. cdo -h after"
        echo "cdo [-t echam6] -f nc copy file.grb file.nc"
        echo "for f in *01.grb; do echo \$f; cdo -t echam6 -f nc copy \$f \$f.nc; done"
        echo "cdo -select,name=temp2 *echam6_echam_* tmp1 && cdo fldmean tmp1 tmp2 && ncview tmp2" 
        echo "cdo -select,name=var167 *echam6_echam_* tmp1 && cdo fldmean tmp1 tmp2 && ncview tmp2" 
        echo "cdo chname,var1,var2 in.nc out.nc"
        echo "for f in *.nc; do echo \$f; ncrename -v XXX,YYY \$f; done"
        echo "for f in *.nc; do echo \$f; ncdump -h \$f | grep var167; done"
        echo "cdo -r copy in out"
    }
    pwd2(){
        readlink -f .
    }
    ddiff(){
        diff $1 $2 | vim -R -
    } # or `diff old new | colordiff`
    
    ## argument list too long
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

    ## aliase
    # check aliase with 'type alias'
    alias ll='ls --color=auto -lFh'
    alias la='ls --color=auto -alFh'
    alias ls='ls --color=auto -F' # default: ls='ls --color=auto'
    # ls only files excluding .dotfiles
    alias lsf='find . -maxdepth 1 -type f -a ! -iname '\''.*'\'' -print0 | xargs -0r ls'
    # ls only files including .dotfiles
    alias lsf2='find . -maxdepth 1 -type f -print0 | xargs -0r ls'
    alias grep="grep --color=auto"
    alias R='R --quiet'
    alias R0='R --no-init-file'
    alias vi='vim'
    if check_existance vimx; then
        alias vi='vimx' # for +clipboard
        alias vim='vimx'
    fi
    alias less="less -i"
    alias more="less"

    ## own variables
    export VISUAL=vim
    export EDITOR="$VISUAL" # also applies to git
    # todo: need to convert these to functions:
    export compress='gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/default -dNOPAUSE -dQUIET -dBATCH -dDetectDuplicateImages -dCompressFonts=true -r150 -sOutputFile=output.pdf input.pdf'
    export cut='gs -dBATCH -sOutputFile= -dFirstPage= -dLastPage= -sDEVICE=pdfwrite infile'
    export cat1='gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress -sOutputFile=out.pdf in1.pdf in2.pdf'
    export cat2='pdftk in.pdf cat 1-12 14-end output out.pdf'
    export cat3='pdftk in1.pdf in2.pdf output out.pdf'
    export crop='pdfcrop --xetex --resolution 72 diffusion_vs_res.pdf diffusion_vs_res.pdf'
    # watch -n 0.1 ls

    ## hostname
    printf "[\$(hostname)][\$(hostname -d)]: "
    echo "[$(hostname)][$(hostname -d)]"
  
    ## check cpus
    if check_existance lscpu; then
        printf "/proc/cpuinfo: "
        cat /proc/cpuinfo | grep --color=never "model name" | head -1
        #printf "lscpu | grep Model name::"
        #lscpu | grep --color=never "Model name:"
        printf "\$(nproc): "; nproc
        #lscpu | grep --color=never "^CPU(s):"
    fi

    ## uptime
    printf "\$(uptime): "
    uptime | awk -F'( |,|:)+' '{print $6,$7",",$8,"hours,",$9,"minutes"}'

    ## which OS
    if [ -f /etc/os-release ]; then
        printf "\$(head -1 /etc/os-release): "
        head -1 /etc/os-release
    elif [ -f /etc/system-release ]; then
        printf "\$(head -1 /etc/system-release): "
        head -1 /etc/system-release
    else
        echo operating system unknown!
    fi

    ## which package manager 
    declare -A osInfo;
    osInfo[/etc/redhat-release]=yum
    osInfo[/etc/arch-release]=pacman
    osInfo[/etc/gentoo-release]=emerge
    osInfo[/etc/SuSE-release]=zypp
    osInfo[/etc/debian_version]=apt-get
    for f in ${!osInfo[@]}; do
        if [[ -f $f ]]; then
            echo "\"$f\" exists -> package manager is ${osInfo[$f]}"
        fi
    done

    ## which shell
    printf "\$SHELL: "; echo $SHELL

    ## which tty
    printf "\$(tty): "; tty
    #set -o vi # unfortunatley this breaks ctrl+a/+e
    # kill open remote sessions:
    #ssh cdanek@stan1.awi.de w
    #ssh cdanek@stan1.awi.de pkill -9 -t pts/3
    #ssh cdanek@stan1.awi.de pkill -u cdanek

    ## show what kind of shell (at this point it must be an interactive shell since)
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

    ## check if login shell (cannot check $0 from within this script)
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

    ## which display manager (dm) is used?
    echo "\$DESKTOP_SESSION = $DESKTOP_SESSION"
    printf "ps auxf | awk '{print \$11}' | \\grep -e dm\$ -e slim\$ = "
    tmp=$(ps auxf | awk '{print $11}' | \grep -e "^/.*dm$" -e "/.*slim$")
    printf "%s" $tmp
    echo
    unset tmp

    ## check if module tun is available or not (it is not after system upgrade)
    modprobe tun &> /dev/null # silent output
    if [ $? -ne 0 ]; then # if not successfull either due to missing permissions or file not found
        tun_file=$(find /lib/modules/`uname -r` -print | grep -i "tun.ko")
        if [ ${#tun_file} == 0 ]; then # if missing file 
            echo "'modprobe tun' raised some problem, consider restart:"
            modprobe tun
        fi
    fi

    ## check if vim/vimx is installed and supports clipboard pasting
    if check_existance vim || check_existance vimx; then
        if check_existance vim; then
            tmp=$(vim --version | grep clipboard)
            if [[ $tmp == *clipboard* ]]; then
                while read -r pos ; do
                    # test if +/-clipboard or +/-xterm_clipboard
                    clipboard="${tmp:(($pos-5)):15}" # xterm_clipboard
                    if [[ $clipboard == "xterm_clipboard" ]]; then
                        vim_xterm_clipboard="${tmp:(($pos-6)):16}"
                    else # or +/-clipboard
                        vim_clipboard="${tmp:(($pos-1)):10}"
                    fi
                done < <(echo $tmp | grep -b -o "clipboard" | awk 'BEGIN {FS=":"}{print $1}')                
            fi            
            if [[ ${vim_clipboard:0:1} == "-" ]] && [ ${vim_xterm_clipboard:0:1} == "-" ]; then
                vim_return=1
            else
                vim_return=0
            fi
        else
            vim_return=1
        fi
        if check_existance vimx; then
            tmp=$(vimx --version | grep clipboard)
            if [[ $tmp == *clipboard* ]]; then
                while read -r pos ; do
                    # test if +/-clipboard or +/-xterm_clipboard
                    clipboard="${tmp:(($pos-5)):15}" # xterm_clipboard
                    if [[ $clipboard == "xterm_clipboard" ]]; then
                        vimx_xterm_clipboard="${tmp:(($pos-6)):16}"
                    else # or +/-clipboard
                        vimx_clipboard="${tmp:(($pos-1)):10}"
                    fi
                done < <(echo $tmp | grep -b -o "clipboard" | awk 'BEGIN {FS=":"}{print $1}')                
            fi            
            if [[ ${vimx_clipboard:0:1} == "-" ]] && [ ${vimx_xterm_clipboard:0:1} == "-" ]; then
                vimx_return=1
            else
                vimx_return=0
            fi
        else 
            vimx_return=1
        fi
        if [[ $vim_return == 1 ]] && [[ $vimx_return == 1 ]]; then
            if check_existance vim; then
                echo warn: vim exists but with $vim_clipboard and $vim_xterm_clipboard
            fi
            if check_existance vimx; then
                echo warn: vimx exists but with $vimx_clipboard and $vimx_xterm_clipboard
            fi
        fi
    fi # if vim or vimx exist

    ## run bash stuff if available
    if ! check_existance nc-config; then
        echo nc-config is missing!!!
    fi
    
    ## run R stuff if available
    if check_existance Rscript; then
        if check_existance mytimes; then
            mytimes
        fi
    fi
    
    ## run bash stuff if available
    if check_existance bash; then
        if check_existance birthdays; then
            birthdays
        fi
    fi

    ## find module binary
    # $?: last command return value
    # $*: list of all args
    # works: eval `/sw/rhel6-x64/tcl/modules-3.2.10/Modules/$MODULE_VERSION/bin/modulecmd bash list`
    # works: eval `/sw/rhel6-x64/tcl/modules-3.2.10/Modules/$MODULE_VERSION/bin/modulecmd bash purge`
    # works: eval `/sw/rhel6-x64/tcl/modules-3.2.10/Modules/$MODULE_VERSION/bin/modulecmd bash load gcc`
    # $ ldd binary (executes the binary!)
    # $ readelf -d | grep NEEDED (does not execute the binary)
    if check_existance module; then
        echo "type module"
        echo $(type module)
        echo "loaded startup modules:"; module list
    fi

    ## load private stuff at the end to overwrite defaults (and conda) from above
    if [ -f ~/.myprofile ]; then
        echo "source ~/.myprofile ..."
        source ~/.myprofile
    fi

    ## Finish
    printf '%*s' "$ncol" | tr ' ' "*"
    printf " ~/.bashrc finish "
    printf '%*s' "$ncol" | tr ' ' "*"
    echo ""

#fi # interactive or not

