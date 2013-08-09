# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _match _correct _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' format 'comp.. %d'
zstyle ':completion:*' glob 1
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' menu select=long
zstyle ':completion:*' prompt '>>'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle :compinstall filename '/home/allie/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=500000
SAVEHIST=500000
setopt appendhistory extendedglob nomatch notify
# End of lines configured by zsh-newuser-install
# Path to your oh-my-zsh configuration.
ZSH=~/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
DISABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(git python django archlinux colorize pip virtualenv virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

bindkey "^?" backward-delete-char
bindkey "^W" backward-kill-word 
bindkey "^H" backward-delete-char      # Control-h also deletes the previous char
bindkey "^U" kill-line            
export SCONSFLAGS="-j 5"

alias lh="ls -lh "
alias ll="ls -l"
alias la="ls -lah"
eval `dircolors ~/.dircolors`
