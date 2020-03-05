#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

HISTFILESIZE=10000
HISTSIZE=5000
shopt -s histappend

export EDITOR="mg -n"
export PATH="~/.local/bin:$PATH:~/dnsl48cfg/shims"


PS1='[\u \W]\$ '

alias ls='ls --color=auto'
alias l='ls -lhF --color=auto'
alias la='ls -lhaF --color=auto'
alias dc=docker-compose
