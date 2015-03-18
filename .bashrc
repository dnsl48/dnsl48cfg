#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias ll='ls -lhF --color=auto'
alias lla='ls -lhaF --color=auto'

PATH="$PATH:~/.cabal/bin"
export PATH

PS1='[\u@\h \W]\$ '