# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt beep extendedglob notify
# End of lines configured by zsh-newuser-install

#aliases
alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -lhA'
alias l='ls'
alias c='clear'
alias x='exit'
alias pacman='pacman-color'
alias sudo='sudo '

#key bindings
source /home/allie/.zkbd/xfce-terminal-:0.0
bindkey "${key[Delete]}" delete-char
bindkey "${key[Insert]}" overwrite-mode
bindkey -v

export PERL_LOCAL_LIB_ROOT="/home/allie/perl5";
export PERL_MB_OPT="--install_base /home/allie/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/allie/perl5";
export PERL5LIB="/home/allie/perl5/lib/perl5/x86_64-linux-thread-multi:/home/allie/perl5/lib/perl5";
export PATH="/home/allie/perl5/bin:$PATH";
