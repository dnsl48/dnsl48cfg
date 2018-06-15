#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

HISTFILESIZE=10000
HISTSIZE=5000

export EDITOR="mg -n"
# export PATH="$PATH:~/.cabal/bin:~/.gem/ruby/2.2.0/bin"


PS1='[\u@\h \W]\$ '

alias ls='ls --color=auto'
alias ll='ls -lhF --color=auto'
alias lla='ls -lhaF --color=auto'
alias dc=docker-compose

shopt -s histappend
