#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH=/home/oliwier/.config/emacs/bin:$PATH

alias clr='clear'
alias ll='ls -la --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$'

eval "$(starship init bash)"