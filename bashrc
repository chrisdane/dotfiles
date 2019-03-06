#
# ~/.bashrc
#

## .bashrc vs .bash_profile
# if non-interactive shell:
#   echo $- = hB (e.g.); "i" is not included or
# if interactive shell:
#   echo $- = himBHs (e.g.); "i" is included or 
#   run 1) /etc/bash.bashrc, 2) ~/.bashrc
#   if login shell:
#       echo $0 = "-bash" or shopt login_shell = on
#       on login:  run 1) /etc/profile, 2) ~/.bash_profile, 3) ~/.bash_login, 4) ~/.profile
#       on logout: run 1) ~/.bash_logout, 2) /etc/bash.bash_logout
#   if non-login shell:
#       echo $0 = "bash" or shopt login_shell = off
#       on login: run ~/.bashrc
# a) on the local machine, a bash session starts as interactive (echo $- = himBHs) non-login (echo $0 = bash) shell
# b) when SSH/SCPing to a server, a bash session starts as interactive (echo $- = himBHs) login (echo $0 = -bash) shell
# c) from a script (e.g. #!/bin/bash) bash starts as non-interactive session (echo $- = hB)
# https://wiki.archlinux.org/index.php/bash#Invocation
# https://linux.die.net/man/1/bash (incovation chapter)
# https://www.gnu.org/software/bash/manual/bash.html#Bash-Startup-Files

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## From here, everyhing happens only if running interactively

# use bash completion, if installed
if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
fi

# git automplete
# from https://apple.stackexchange.com/questions/55875/git-auto-complete-for-branches-at-the-command-line
## does not work
#if [ -f ~/.git-completion.bash ]; then
#    source ~/.git-completion.bash
#fi

# my bins (doing this recursively is not recommended; security)
export PATH=~/bin/:$PATH

# default prompt
PS1='[\u@\h \W]\$ '

# my prompt
PS1='\[\033[0;34m\]\h:$(pwd)/>\[\033[0m\] '

# attach cpu temp to prompt if available
if [ -x "$(command -v sensors)" ]; then
    show_temp(){
        sensors | grep -oP 'Package id 0.*?\+\K[0-9.]+'
    }
    PS1='\[\033[0;34m\]\h:$(show_temp)°C:$(pwd)/>\[\033[0m\] '
fi

# use liquidprompt if available
# https://github.com/nojhan/liquidprompt
if [ -x "$(command -v liquidprompt)" ]; then

    # add cpu temp to liquidprompt
    # this is not perfect way yet
    # check ~/.liquidpromptrc
    #PROMPT_COMMAND=show_temp
    if [ -n "$(LC_ALL=C type -t show_temp)" ] && [ "$(LC_ALL=C type -t show_temp)" = function ]; then
           LP_PS1_PREFIX="$(show_temp)°C "
    fi
    source liquidprompt # need to source here because its not bash?!
fi

# enable make autocomplete:
# https://stackoverflow.com/questions/4188324/bash-completion-of-makefile-target
complete -W "\`grep -oE '^[a-zA-Z0-9_.-]+:([^=]|$)' Makefile | sed 's/[^a-zA-Z0-9_.-]*$//'\`" make

# check aliase with 'type alias'
alias ll='ls --color=auto -lFh'
alias la='ls --color=auto -alFh'
alias ls='ls --color=auto -F' # default: ls='ls --color=auto'
# ls only files excluding .dotfiles
alias lsf='find . -maxdepth 1 -type f -a ! -iname '\''.*'\'' -print0 | xargs -0r ls'
# ls only files including .dotfiles
alias lsf2='find . -maxdepth 1 -type f -print0 | xargs -0r ls'
alias vi='vim'
alias R='R --quiet'

# helper functions
tl(){
    file=$(ls -t | head -n1)
    echo `pwd`/$file
    tail -f $file
}
# need to convert these to functions:
export compress='gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/default -dNOPAUSE -dQUIET -dBATCH -        dDetectDuplicateImages -dCompressFonts=true -r150 -sOutputFile=output.pdf input.pdf'
export cut='gs -dBATCH -sOutputFile= -dFirstPage= -dLastPage= -sDEVICE=pdfwrite infile'
export cat='gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress -sOutputFile=out.pdf in1.pdf in2.pdf'

# run private stuff
if [ -f ~/.myprofile ]; then
    source ~/.myprofile
else 
    echo ".bashrc: could not find ~/.myprofile"
fi

# run R stuff if available
if [ -x "$(command -v Rscript)" ]; then
    if [ -x "$(command -v mytimes)" ]; then
        mytimes
    fi
fi

# run bash stuff if available
if [ -x "$(command -v bash)" ]; then
    if [ -x "$(command -v birthdays)" ]; then
        birthdays
    fi
fi
