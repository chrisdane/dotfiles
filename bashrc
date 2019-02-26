#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

## Use Bash completion, if installed
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

## my bins
export PATH=~/bin/:$PATH
export PATH=~/bin/lpacman/:$PATH
export PATH=~/bin/leap/:$PATH

## my prompt
#PS1='[\u@\h \W]\$ ' # default
#PS1='\[\033[0;34m\]\h:$(pwd)/>\[\033[0m\] '
show_temp(){
    sensors | grep -oP 'Package id 0.*?\+\K[0-9.]+'
}
#PS1='\[\033[0;34m\]\h:$(show_temp)°C:$(pwd)/>\[\033[0m\] '
# https://github.com/nojhan/liquidprompt
# check ~/.liquidpromptrc
LP_PS1_PREFIX="$(show_temp)°C "
#LP_PS1_PREFIX=""
#PROMPT_COMMAND=show_temp
source ~/bin/liquidprompt/liquidprompt

## enable make autocomplete:
# https://stackoverflow.com/questions/4188324/bash-completion-of-makefile-target
complete -W "\`grep -oE '^[a-zA-Z0-9_.-]+:([^=]|$)' Makefile | sed 's/[^a-zA-Z0-9_.-]*$//'\`" make

## my autostart
bash /home/mozi/prog/home/age_calender/own_calender.sh

## my exports and aliase
# for yaourt
export VISUAL="vim" 
# hint: sudoedit for editing system files

# aliase
alias vi='vim'
alias ll='ls --color=auto -lFh'
alias la='ls --color=auto -alFh'
#alias ls='ls --color=auto' # default
alias ls='ls --color=auto -F'

alias R='R --quiet'

alias awissh='ssh cdanek@ssh.awi.de'
alias rayo2='ssh -Y cdanek@rayo2.awi.de'
alias rayo3='ssh -Y cdanek@rayo3.awi.de'
alias rayl='ssh -Y cdanek@rayl4.awi.de'
alias ollie0='ssh -Y cdanek@ollie0.awi.de'
alias ollie1='ssh -Y cdanek@ollie1.awi.de'
alias bkli='ssh -Y cdanek@bkli06l001'
alias linsrv='ssh -Y cdanek@linsrv1.awi.de'
alias paleosrv1='ssh -Y cdanek@paleosrv1'
alias uv='ssh -Y cdanek@uv100.awi.de'
alias miub='ssh -Y MiuB-ssh@miubgate.meteo.uni-bonn.de'
alias mistral='ssh -Y a270073@mistral.dkrz.de'
alias mistralpp='ssh -Y a270073@mistralpp.dkrz.de'

#alias scp='rsync --partial --progress --rsh=ssh --archive' 
#sftp:
#  echo "get /hs/csys/SiGePAX/PROJECT_SOO/Cmhpi2SOO5/*oce.mean*" | sftp cdanek@hssrv2.awi.de
#  echo "get /hs/csys/SiGePAX/PROJECT_SOO/TSOO5/*oce.diag*" | sftp cdanek@hssrv2.awi.de
#  ctrl-z
#  bg
#  disown

# nohup lftp -e "cd /hs/csys/SiGePAX/PROJECT_SOO/Cmhpi2SOO5/; mget -c ${path}.tar.gz_* ${path}/${logfile}; bye" $archive_host &

# nohup rsync --partial --progress --rsh=ssh --archive /hs/csys/SiGePAX/PROJECT_SOO/TSOO5/*clock* . > bla.log 2>&1

alias paleo1='cd /csys/paleo1/cdanek'
alias cdw='cd /csys/nobackup1_PALEO/cdanek'
alias patrickh='cd /home/csys/pscholz/MODELs/FESOM_MATLAB_ROUTINES/'
alias patrickP='cd /csys/nobackup1_PALEO/pscholz'
alias patrickp='cd /csys/paleo1/pscholz/'
# patricks startup.m: /home/csys/pscholz/Documents/MATLAB/
alias claudih='cd /csys/CLIDYN2/cwekerle/MATLAB/'
alias moritz='cd /csys/paleo7/mkrieger/data'
alias christian='cd /home/csys/stepanek/bin/R_tcltk/R-3.1.2_linux/'

# compress pdf:
export compress='gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/default -dNOPAUSE -dQUIET -dBATCH -dDetectDuplicateImages -dCompressFonts=true -r150 -sOutputFile=output.pdf input.pdf'
export cut='gs -dBATCH -sOutputFile= -dFirstPage= -dLastPage= -sDEVICE=pdfwrite infile'
export cat='gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress -sOutputFile=out.pdf in1.pdf in2.pdf'

# run wine in 32 bit mode
export WINEPREFIX=$HOME/.config/wine/
export WINEARCH=win32

tl(){
    file=$(ls -t | head -n1)
    echo `pwd`/$file
    tail -f $file
}

