# Filename:      /etc/zsh/zshrc
# Purpose:       config file for zsh (z shell)
# Authors:       grml-team (grml.org), (c) Michael Prokop <mika@grml.org>
# Bug-Reports:   see http://grml.org/bugs/
# License:       This file is licensed under the GPL v2.
################################################################################
# This file is sourced only for interactive shells. It
# should contain commands to set up aliases, functions,
# options, key bindings, etc.
#
# Global Order: zshenv, zprofile, zshrc, zlogin
################################################################################

# USAGE
# If you are using this file as your ~/.zshrc file, please use ~/.zshrc.pre
# and ~/.zshrc.local for your own customisations. The former file is read
# before ~/.zshrc, the latter is read after it. Also, consider reading the
# refcard and the reference manual for this setup, both available from:
#     <http://grml.org/zsh/>

# Contributing:
# If you want to help to improve grml's zsh setup, clone the grml-etc-core
# repository from git.grml.org:
#   git clone git://git.grml.org/grml-etc-core.git
#
# Make your changes, commit them; use 'git format-patch' to create a series
# of patches and send those to the following address via 'git send-email':
#   grml-etc-core@grml.org
#
# Doing so makes sure the right people get your patches for review and
# possibly inclusion.

# zsh-refcard-tag documentation:
#   You may notice strange looking comments in this file.
#   These are there for a purpose. grml's zsh-refcard can now be
#   automatically generated from the contents of the actual configuration
#   file. However, we need a little extra information on which comments
#   and what lines of code to take into account (and for what purpose).
#
# Here is what they mean:
#
# List of tags (comment types) used:
#   #a#     Next line contains an important alias, that should
#           be included in the grml-zsh-refcard.
#           (placement tag: @@INSERT-aliases@@)
#   #f#     Next line contains the beginning of an important function.
#           (placement tag: @@INSERT-functions@@)
#   #v#     Next line contains an important variable.
#           (placement tag: @@INSERT-variables@@)
#   #k#     Next line contains an important keybinding.
#           (placement tag: @@INSERT-keybindings@@)
#   #d#     Hashed directories list generation:
#               start   denotes the start of a list of 'hash -d'
#                       definitions.
#               end     denotes its end.
#           (placement tag: @@INSERT-hasheddirs@@)
#   #A#     Abbreviation expansion list generation:
#               start   denotes the beginning of abbreviations.
#               end     denotes their end.
#           Lines within this section that end in '#d .*' provide
#           extra documentation to be included in the refcard.
#           (placement tag: @@INSERT-abbrev@@)
#   #m#     This tag allows you to manually generate refcard entries
#           for code lines that are hard/impossible to parse.
#               Example:
#                   #m# k ESC-h Call the run-help function
#               That would add a refcard entry in the keybindings table
#               for 'ESC-h' with the given comment.
#           So the syntax is: #m# <section> <argument> <comment>
#   #o#     This tag lets you insert entries to the 'other' hash.
#           Generally, this should not be used. It is there for
#           things that cannot be done easily in another way.
#           (placement tag: @@INSERT-other-foobar@@)
#
#   All of these tags (except for m and o) take two arguments, the first
#   within the tag, the other after the tag:
#
#   #<tag><section># <comment>
#
#   Where <section> is really just a number, which are defined by the
#   @secmap array on top of 'genrefcard.pl'. The reason for numbers
#   instead of names is, that for the reader, the tag should not differ
#   much from a regular comment. For zsh, it is a regular comment indeed.
#   The numbers have got the following meanings:
#         0 -> "default"
#         1 -> "system"
#         2 -> "user"
#         3 -> "debian"
#         4 -> "search"
#         5 -> "shortcuts"
#         6 -> "services"
#
#   So, the following will add an entry to the 'functions' table in the
#   'system' section, with a (hopefully) descriptive comment:
#       #f1# Edit an alias via zle
#       edalias() {
#
#   It will then show up in the @@INSERT-aliases-system@@ replacement tag
#   that can be found in 'grml-zsh-refcard.tex.in'.
#   If the section number is omitted, the 'default' section is assumed.
#   Furthermore, in 'grml-zsh-refcard.tex.in' @@INSERT-aliases@@ is
#   exactly the same as @@INSERT-aliases-default@@. If you want a list of
#   *all* aliases, for example, use @@INSERT-aliases-all@@.

# zsh profiling
# just execute 'ZSH_PROFILE_RC=1 zsh' and run 'zprof' to get the details
if [[ $ZSH_PROFILE_RC -gt 0 ]] ; then
    zmodload zsh/zprof
fi

# load .zshrc.pre to give the user the chance to overwrite the defaults
[[ -r ${HOME}/.zshrc.pre ]] && source ${HOME}/.zshrc.pre

# check for version/system
# check for versions (compatibility reasons)
is4(){
    [[ $ZSH_VERSION == <4->* ]] && return 0
    return 1
}

is41(){
    [[ $ZSH_VERSION == 4.<1->* || $ZSH_VERSION == <5->* ]] && return 0
    return 1
}

is42(){
    [[ $ZSH_VERSION == 4.<2->* || $ZSH_VERSION == <5->* ]] && return 0
    return 1
}

is425(){
    [[ $ZSH_VERSION == 4.2.<5->* || $ZSH_VERSION == 4.<3->* || $ZSH_VERSION == <5->* ]] && return 0
    return 1
}

is43(){
    [[ $ZSH_VERSION == 4.<3->* || $ZSH_VERSION == <5->* ]] && return 0
    return 1
}

is433(){
    [[ $ZSH_VERSION == 4.3.<3->* || $ZSH_VERSION == 4.<4->* \
                                 || $ZSH_VERSION == <5->* ]] && return 0
    return 1
}

is439(){
    [[ $ZSH_VERSION == 4.3.<9->* || $ZSH_VERSION == 4.<4->* \
                                 || $ZSH_VERSION == <5->* ]] && return 0
    return 1
}

#f1# Checks whether or not you're running grml
isgrml(){
    [[ -f /etc/grml_version ]] && return 0
    return 1
}

#f1# Checks whether or not you're running a grml cd
isgrmlcd(){
    [[ -f /etc/grml_cd ]] && return 0
    return 1
}

if isgrml ; then
#f1# Checks whether or not you're running grml-small
    isgrmlsmall() {
        if [[ ${${${(f)"$(</etc/grml_version)"}%% *}##*-} == 'small' ]]; then
            return 0
        fi
        return 1
    }
else
    isgrmlsmall() { return 1 }
fi

isdarwin(){
    [[ $OSTYPE == darwin* ]] && return 0
    return 1
}

isfreebsd(){
    [[ $OSTYPE == freebsd* ]] && return 0
    return 1
}

#f1# are we running within an utf environment?
isutfenv() {
    case "$LANG $CHARSET $LANGUAGE" in
        *utf*) return 0 ;;
        *UTF*) return 0 ;;
        *)     return 1 ;;
    esac
}

# check for user, if not running as root set $SUDO to sudo
(( EUID != 0 )) && SUDO='sudo' || SUDO=''

# change directory to home on first invocation of zsh
# important for rungetty -> autologin
# Thanks go to Bart Schaefer!
isgrml && checkhome() {
    if [[ -z "$ALREADY_DID_CD_HOME" ]] ; then
        export ALREADY_DID_CD_HOME=$HOME
        cd
    fi
}

# check for zsh v3.1.7+

if ! [[ ${ZSH_VERSION} == 3.1.<7->*      \
     || ${ZSH_VERSION} == 3.<2->.<->*    \
     || ${ZSH_VERSION} == <4->.<->*   ]] ; then

    printf '-!-\n'
    printf '-!- In this configuration we try to make use of features, that only\n'
    printf '-!- require version 3.1.7 of the shell; That way this setup can be\n'
    printf '-!- used with a wide range of zsh versions, while using fairly\n'
    printf '-!- advanced features in all supported versions.\n'
    printf '-!-\n'
    printf '-!- However, you are running zsh version %s.\n' "$ZSH_VERSION"
    printf '-!-\n'
    printf '-!- While this *may* work, it might as well fail.\n'
    printf '-!- Please consider updating to at least version 3.1.7 of zsh.\n'
    printf '-!-\n'
    printf '-!- DO NOT EXPECT THIS TO WORK FLAWLESSLY!\n'
    printf '-!- If it does today, you'\''ve been lucky.\n'
    printf '-!-\n'
    printf '-!- Ye been warned!\n'
    printf '-!-\n'

    function zstyle() { : }
fi

# autoload wrapper - use this one instead of autoload directly
# We need to define this function as early as this, because autoloading
# 'is-at-least()' needs it.
function zrcautoload() {
    emulate -L zsh
    setopt extended_glob
    local fdir ffile
    local -i ffound

    ffile=$1
    (( ffound = 0 ))
    for fdir in ${fpath} ; do
        [[ -e ${fdir}/${ffile} ]] && (( ffound = 1 ))
    done

    (( ffound == 0 )) && return 1
    if [[ $ZSH_VERSION == 3.1.<6-> || $ZSH_VERSION == <4->* ]] ; then
        autoload -U ${ffile} || return 1
    else
        autoload ${ffile} || return 1
    fi
    return 0
}

# Load is-at-least() for more precise version checks Note that this test will
# *always* fail, if the is-at-least function could not be marked for
# autoloading.
zrcautoload is-at-least || is-at-least() { return 1 }

# set some important options (as early as possible)

# append history list to the history file; this is the default but we make sure
# because it's required for share_history.
setopt append_history

# import new commands from the history file also in other zsh-session
is4 && setopt share_history

# save each command's beginning timestamp and the duration to the history file
setopt extended_history

# If a new command line being added to the history list duplicates an older
# one, the older command is removed from the list
is4 && setopt histignorealldups

# remove command lines from the history list when the first character on the
# line is a space
setopt histignorespace

# if a command is issued that can't be executed as a normal command, and the
# command is the name of a directory, perform the cd command to that directory.
setopt auto_cd

# in order to use #, ~ and ^ for filename generation grep word
# *~(*.gz|*.bz|*.bz2|*.zip|*.Z) -> searches for word not in compressed files
# don't forget to quote '^', '~' and '#'!
setopt extended_glob

# display PID when suspending processes as well
setopt longlistjobs

# try to avoid the 'zsh: no matches found...'
setopt nonomatch

# report the status of backgrounds jobs immediately
setopt notify

# whenever a command completion is attempted, make sure the entire command path
# is hashed first.
setopt hash_list_all

# not just at the end
setopt completeinword

# Don't send SIGHUP to background processes when the shell exits.
setopt nohup

# make cd push the old directory onto the directory stack.
setopt auto_pushd

# avoid "beep"ing
setopt nobeep

# don't push the same dir twice.
setopt pushd_ignore_dups

# * shouldn't match dotfiles. ever.
setopt noglobdots

# use zsh style word splitting
setopt noshwordsplit

# don't error out when unset parameters are used
setopt unset

# setting some default values
NOCOR=${NOCOR:-0}
NOMENU=${NOMENU:-0}
NOPRECMD=${NOPRECMD:-0}
COMMAND_NOT_FOUND=${COMMAND_NOT_FOUND:-0}
GRML_ZSH_CNF_HANDLER=${GRML_ZSH_CNF_HANDLER:-/usr/share/command-not-found/command-not-found}
BATTERY=${BATTERY:-0}
GRMLSMALL_SPECIFIC=${GRMLSMALL_SPECIFIC:-1}
ZSH_NO_DEFAULT_LOCALE=${ZSH_NO_DEFAULT_LOCALE:-0}

typeset -ga ls_options
typeset -ga grep_options
if ls --help 2> /dev/null | grep -q GNU; then
    ls_options=( --color=auto )
elif [[ $OSTYPE == freebsd* ]]; then
    ls_options=( -G )
fi
if grep --help 2> /dev/null | grep -q GNU || \
   [[ $OSTYPE == freebsd* ]]; then
    grep_options=( --color=auto )
fi

# utility functions
# this function checks if a command exists and returns either true
# or false. This avoids using 'which' and 'whence', which will
# avoid problems with aliases for which on certain weird systems. :-)
# Usage: check_com [-c|-g] word
#   -c  only checks for external commands
#   -g  does the usual tests and also checks for global aliases
check_com() {
    emulate -L zsh
    local -i comonly gatoo

    if [[ $1 == '-c' ]] ; then
        (( comonly = 1 ))
        shift
    elif [[ $1 == '-g' ]] ; then
        (( gatoo = 1 ))
    else
        (( comonly = 0 ))
        (( gatoo = 0 ))
    fi

    if (( ${#argv} != 1 )) ; then
        printf 'usage: check_com [-c] <command>\n' >&2
        return 1
    fi

    if (( comonly > 0 )) ; then
        [[ -n ${commands[$1]}  ]] && return 0
        return 1
    fi

    if   [[ -n ${commands[$1]}    ]] \
      || [[ -n ${functions[$1]}   ]] \
      || [[ -n ${aliases[$1]}     ]] \
      || [[ -n ${reswords[(r)$1]} ]] ; then

        return 0
    fi

    if (( gatoo > 0 )) && [[ -n ${galiases[$1]} ]] ; then
        return 0
    fi

    return 1
}

# creates an alias and precedes the command with
# sudo if $EUID is not zero.
salias() {
    emulate -L zsh
    local only=0 ; local multi=0
    while [[ $1 == -* ]] ; do
        case $1 in
            (-o) only=1 ;;
            (-a) multi=1 ;;
            (--) shift ; break ;;
            (-h)
                printf 'usage: salias [-h|-o|-a] <alias-expression>\n'
                printf '  -h      shows this help text.\n'
                printf '  -a      replace '\'' ; '\'' sequences with '\'' ; sudo '\''.\n'
                printf '          be careful using this option.\n'
                printf '  -o      only sets an alias if a preceding sudo would be needed.\n'
                return 0
                ;;
            (*) printf "unkown option: '%s'\n" "$1" ; return 1 ;;
        esac
        shift
    done

    if (( ${#argv} > 1 )) ; then
        printf 'Too many arguments %s\n' "${#argv}"
        return 1
    fi

    key="${1%%\=*}" ;  val="${1#*\=}"
    if (( EUID == 0 )) && (( only == 0 )); then
        alias -- "${key}=${val}"
    elif (( EUID > 0 )) ; then
        (( multi > 0 )) && val="${val// ; / ; sudo }"
        alias -- "${key}=sudo ${val}"
    fi

    return 0
}

# a "print -l ${(u)foo}"-workaround for pre-4.2.0 shells
# usage: uprint foo
#   Where foo is the *name* of the parameter you want printed.
#   Note that foo is no typo; $foo would be wrong here!
if ! is42 ; then
    uprint () {
        emulate -L zsh
        local -a u
        local w
        local parameter=$1

        if [[ -z ${parameter} ]] ; then
            printf 'usage: uprint <parameter>\n'
            return 1
        fi

        for w in ${(P)parameter} ; do
            [[ -z ${(M)u:#$w} ]] && u=( $u $w )
        done

        builtin print -l $u
    }
fi

# Check if we can read given files and source those we can.
xsource() {
    if (( ${#argv} < 1 )) ; then
        printf 'usage: xsource FILE(s)...\n' >&2
        return 1
    fi

    while (( ${#argv} > 0 )) ; do
        [[ -r "$1" ]] && source "$1"
        shift
    done
    return 0
}

# Check if we can read a given file and 'cat(1)' it.
xcat() {
    emulate -L zsh
    if (( ${#argv} != 1 )) ; then
        printf 'usage: xcat FILE\n' >&2
        return 1
    fi

    [[ -r $1 ]] && cat $1
    return 0
}

# Remove these functions again, they are of use only in these
# setup files. This should be called at the end of .zshrc.
xunfunction() {
    emulate -L zsh
    local -a funcs
    funcs=(salias xcat xsource xunfunction zrcautoload)

    for func in $funcs ; do
        [[ -n ${functions[$func]} ]] \
            && unfunction $func
    done
    return 0
}

# this allows us to stay in sync with grml's zshrc and put own
# modifications in ~/.zshrc.local
zrclocal() {
    xsource "/etc/zsh/zshrc.local"
    xsource "${HOME}/.zshrc.local"
    return 0
}

# locale setup
if (( ZSH_NO_DEFAULT_LOCALE == 0 )); then
    xsource "/etc/default/locale"
fi

for var in LANG LC_ALL LC_MESSAGES ; do
    [[ -n ${(P)var} ]] && export $var
done

xsource "/etc/sysconfig/keyboard"

TZ=$(xcat /etc/timezone)

# set some variables
if check_com -c vim ; then
#v#
    export EDITOR=${EDITOR:-vim}
else
    export EDITOR=${EDITOR:-vi}
fi

#v#
export PAGER=${PAGER:-less}

#v#
export MAIL=${MAIL:-/var/mail/$USER}

# if we don't set $SHELL then aterm, rxvt,.. will use /bin/sh or /bin/bash :-/
export SHELL='/bin/zsh'

# color setup for ls:
check_com -c dircolors && eval $(dircolors -b)
# color setup for ls on OS X / FreeBSD:
isdarwin && export CLICOLOR=1
isfreebsd && export CLICOLOR=1

# do MacPorts setup on darwin
if isdarwin && [[ -d /opt/local ]]; then
    # Note: PATH gets set in /etc/zprofile on Darwin, so this can't go into
    # zshenv.
    PATH="/opt/local/bin:/opt/local/sbin:$PATH"
    MANPATH="/opt/local/share/man:$MANPATH"
fi

# load our function and completion directories
for fdir in /usr/share/grml/zsh/completion /usr/share/grml/zsh/functions; do
    fpath=( ${fdir} ${fdir}/**/*(/N) ${fpath} )
    if [[ ${fdir} == '/usr/share/grml/zsh/functions' ]] ; then
        for func in ${fdir}/**/[^_]*[^~](N.) ; do
            zrcautoload ${func:t}
        done
    fi
done
unset fdir func

# support colors in less
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# mailchecks
MAILCHECK=30

# report about cpu-/system-/user-time of command if running longer than
# 5 seconds
REPORTTIME=5

# watch for everyone but me and root
watch=(notme root)

# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath

# Remove zle-line-{init,finish} if it looks like it turns smkx. This would be
# better fixed by working with those modes too, but we use way too many
# hardcoded bindings for now.
function remove_widget () {
    local name=$1
    local cap=$2
    if (( ${+functions[$name]} )) && [[ ${functions[$name]} == *${cap}* ]]; then
        local w=${widgets[$name]}
        zle -D $name
        [[ $w == user:* ]] && unfunction ${w#*:}
    fi
}
remove_widget zle-line-init smkx
remove_widget zle-line-finish rmkx
unfunction remove_widget

# keybindings
if [[ "$TERM" != emacs ]] ; then
    [[ -z "$terminfo[kdch1]" ]] || bindkey -M emacs "$terminfo[kdch1]" delete-char
    [[ -z "$terminfo[khome]" ]] || bindkey -M emacs "$terminfo[khome]" beginning-of-line
    [[ -z "$terminfo[kend]"  ]] || bindkey -M emacs "$terminfo[kend]"  end-of-line
    [[ -z "$terminfo[kdch1]" ]] || bindkey -M vicmd "$terminfo[kdch1]" vi-delete-char
    [[ -z "$terminfo[khome]" ]] || bindkey -M vicmd "$terminfo[khome]" vi-beginning-of-line
    [[ -z "$terminfo[kend]"  ]] || bindkey -M vicmd "$terminfo[kend]"  vi-end-of-line
    [[ -z "$terminfo[cuu1]"  ]] || bindkey -M viins "$terminfo[cuu1]"  vi-up-line-or-history
    [[ -z "$terminfo[cuf1]"  ]] || bindkey -M viins "$terminfo[cuf1]"  vi-forward-char
    [[ -z "$terminfo[kcuu1]" ]] || bindkey -M viins "$terminfo[kcuu1]" vi-up-line-or-history
    [[ -z "$terminfo[kcud1]" ]] || bindkey -M viins "$terminfo[kcud1]" vi-down-line-or-history
    [[ -z "$terminfo[kcuf1]" ]] || bindkey -M viins "$terminfo[kcuf1]" vi-forward-char
    [[ -z "$terminfo[kcub1]" ]] || bindkey -M viins "$terminfo[kcub1]" vi-backward-char
    # ncurses stuff:
    [[ "$terminfo[kcuu1]" == $'\eO'* ]] && bindkey -M viins "${terminfo[kcuu1]/O/[}" vi-up-line-or-history
    [[ "$terminfo[kcud1]" == $'\eO'* ]] && bindkey -M viins "${terminfo[kcud1]/O/[}" vi-down-line-or-history
    [[ "$terminfo[kcuf1]" == $'\eO'* ]] && bindkey -M viins "${terminfo[kcuf1]/O/[}" vi-forward-char
    [[ "$terminfo[kcub1]" == $'\eO'* ]] && bindkey -M viins "${terminfo[kcub1]/O/[}" vi-backward-char
    [[ "$terminfo[khome]" == $'\eO'* ]] && bindkey -M viins "${terminfo[khome]/O/[}" beginning-of-line
    [[ "$terminfo[kend]"  == $'\eO'* ]] && bindkey -M viins "${terminfo[kend]/O/[}"  end-of-line
    [[ "$terminfo[khome]" == $'\eO'* ]] && bindkey -M emacs "${terminfo[khome]/O/[}" beginning-of-line
    [[ "$terminfo[kend]"  == $'\eO'* ]] && bindkey -M emacs "${terminfo[kend]/O/[}"  end-of-line
fi

## keybindings (run 'bindkeys' for details, more details via man zshzle)
# use emacs style per default:
bindkey -e
# use vi style:
# bindkey -v

## beginning-of-line OR beginning-of-buffer OR beginning of history
## by: Bart Schaefer <schaefer@brasslantern.com>, Bernhard Tittelbach
beginning-or-end-of-somewhere() {
    local hno=$HISTNO
    if [[ ( "${LBUFFER[-1]}" == $'\n' && "${WIDGET}" == beginning-of* ) || \
      ( "${RBUFFER[1]}" == $'\n' && "${WIDGET}" == end-of* ) ]]; then
        zle .${WIDGET:s/somewhere/buffer-or-history/} "$@"
    else
        zle .${WIDGET:s/somewhere/line-hist/} "$@"
        if (( HISTNO != hno )); then
            zle .${WIDGET:s/somewhere/buffer-or-history/} "$@"
        fi
    fi
}
zle -N beginning-of-somewhere beginning-or-end-of-somewhere
zle -N end-of-somewhere beginning-or-end-of-somewhere


#if [[ "$TERM" == screen ]] ; then

## with HOME/END, move to beginning/end of line (on multiline) on first keypress
## to beginning/end of buffer on second keypress
## and to beginning/end of history on (at most) the third keypress
# terminator & non-debian xterm
bindkey '\eOH' beginning-of-somewhere  # home
bindkey '\eOF' end-of-somewhere        # end
# freebsd console
bindkey '\e[H' beginning-of-somewhere   # home
bindkey '\e[F' end-of-somewhere         # end
# xterm,gnome-terminal,quake,etc
bindkey '^[[1~' beginning-of-somewhere  # home
bindkey '^[[4~' end-of-somewhere        # end
# if terminal type is set to 'rxvt':
bindkey '\e[7~' beginning-of-somewhere  # home
bindkey '\e[8~' end-of-somewhere        # end
#fi

bindkey '\e[A'  up-line-or-search       # cursor up
bindkey '\e[B'  down-line-or-search     # <ESC>-

## use Ctrl-left-arrow and Ctrl-right-arrow for jumping to word-beginnings on the CL
bindkey "\e[5C" forward-word
bindkey "\e[5D" backward-word
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word
## the same for alt-left-arrow and alt-right-arrow
bindkey '^[[1;3C' forward-word
bindkey '^[[1;3D' backward-word

# Search backward in the history for a line beginning with the current
# line up to the cursor and move the cursor to the end of the line then
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end  history-search-end
#k# search history backward for entry beginning with typed text
bindkey '^xp'   history-beginning-search-backward-end
#k# search history forward for entry beginning with typed text
bindkey '^xP'   history-beginning-search-forward-end
#k# search history backward for entry beginning with typed text
bindkey "\e[5~" history-beginning-search-backward-end # PageUp
#k# search history forward for entry beginning with typed text
bindkey "\e[6~" history-beginning-search-forward-end  # PageDown

# bindkey -s '^l' "|less\n"             # ctrl-L pipes to less
# bindkey -s '^b' " &\n"                # ctrl-B runs it in the background

# insert unicode character
# usage example: 'ctrl-x i' 00A7 'ctrl-x i' will give you an �
# See for example http://unicode.org/charts/ for unicode characters code
zrcautoload insert-unicode-char
zle -N insert-unicode-char
#k# Insert Unicode character
bindkey '^xi' insert-unicode-char

#m# k Shift-tab Perform backwards menu completion
if [[ -n "$terminfo[kcbt]" ]]; then
    bindkey "$terminfo[kcbt]" reverse-menu-complete
elif [[ -n "$terminfo[cbt]" ]]; then # required for GNU screen
    bindkey "$terminfo[cbt]"  reverse-menu-complete
fi

## toggle the ,. abbreviation feature on/off
# NOABBREVIATION: default abbreviation-state
#                 0 - enabled (default)
#                 1 - disabled
NOABBREVIATION=${NOABBREVIATION:-0}

grml_toggle_abbrev() {
    if (( ${NOABBREVIATION} > 0 )) ; then
        NOABBREVIATION=0
    else
        NOABBREVIATION=1
    fi
}

#k# Toggle abbreviation expansion on/off
zle -N grml_toggle_abbrev
bindkey '^xA' grml_toggle_abbrev

# add a command line to the shells history without executing it
commit-to-history() {
    print -s ${(z)BUFFER}
    zle send-break
}
zle -N commit-to-history
bindkey "^x^h" commit-to-history

# only slash should be considered as a word separator:
slash-backward-kill-word() {
    local WORDCHARS="${WORDCHARS:s@/@}"
    # zle backward-word
    zle backward-kill-word
}
zle -N slash-backward-kill-word

#k# Kill left-side word or everything up to next slash
bindkey '\ev' slash-backward-kill-word
#k# Kill left-side word or everything up to next slash
bindkey '\e^h' slash-backward-kill-word
#k# Kill left-side word or everything up to next slash
bindkey '\e^?' slash-backward-kill-word

# use the new *-pattern-* widgets for incremental history search
if is439 ; then
    bindkey '^r' history-incremental-pattern-search-backward
    bindkey '^s' history-incremental-pattern-search-forward
fi

# a generic accept-line wrapper

# This widget can prevent unwanted autocorrections from command-name
# to _command-name, rehash automatically on enter and call any number
# of builtin and user-defined widgets in different contexts.
#
# For a broader description, see:
# <http://bewatermyfriend.org/posts/2007/12-26.11-50-38-tooltime.html>
#
# The code is imported from the file 'zsh/functions/accept-line' from
# <http://ft.bewatermyfriend.org/comp/zsh/zsh-dotfiles.tar.bz2>, which
# distributed under the same terms as zsh itself.

# A newly added command will may not be found or will cause false
# correction attempts, if you got auto-correction set. By setting the
# following style, we force accept-line() to rehash, if it cannot
# find the first word on the command line in the $command[] hash.
zstyle ':acceptline:*' rehash true

function Accept-Line() {
    setopt localoptions noksharrays
    local -a subs
    local -xi aldone
    local sub
    local alcontext=${1:-$alcontext}

    zstyle -a ":acceptline:${alcontext}" actions subs

    (( ${#subs} < 1 )) && return 0

    (( aldone = 0 ))
    for sub in ${subs} ; do
        [[ ${sub} == 'accept-line' ]] && sub='.accept-line'
        zle ${sub}

        (( aldone > 0 )) && break
    done
}

function Accept-Line-getdefault() {
    emulate -L zsh
    local default_action

    zstyle -s ":acceptline:${alcontext}" default_action default_action
    case ${default_action} in
        ((accept-line|))
            printf ".accept-line"
            ;;
        (*)
            printf ${default_action}
            ;;
    esac
}

function Accept-Line-HandleContext() {
    zle Accept-Line

    default_action=$(Accept-Line-getdefault)
    zstyle -T ":acceptline:${alcontext}" call_default \
        && zle ${default_action}
}

function accept-line() {
    setopt localoptions noksharrays
    local -ax cmdline
    local -x alcontext
    local buf com fname format msg default_action

    alcontext='default'
    buf="${BUFFER}"
    cmdline=(${(z)BUFFER})
    com="${cmdline[1]}"
    fname="_${com}"

    Accept-Line 'preprocess'

    zstyle -t ":acceptline:${alcontext}" rehash \
        && [[ -z ${commands[$com]} ]]           \
        && rehash

    if    [[ -n ${com}               ]] \
       && [[ -n ${reswords[(r)$com]} ]] \
       || [[ -n ${aliases[$com]}     ]] \
       || [[ -n ${functions[$com]}   ]] \
       || [[ -n ${builtins[$com]}    ]] \
       || [[ -n ${commands[$com]}    ]] ; then

        # there is something sensible to execute, just do it.
        alcontext='normal'
        Accept-Line-HandleContext

        return
    fi

    if    [[ -o correct              ]] \
       || [[ -o correctall           ]] \
       && [[ -n ${functions[$fname]} ]] ; then

        # nothing there to execute but there is a function called
        # _command_name; a completion widget. Makes no sense to
        # call it on the commandline, but the correct{,all} options
        # will ask for it nevertheless, so warn the user.
        if [[ ${LASTWIDGET} == 'accept-line' ]] ; then
            # Okay, we warned the user before, he called us again,
            # so have it his way.
            alcontext='force'
            Accept-Line-HandleContext

            return
        fi

        if zstyle -t ":acceptline:${alcontext}" nocompwarn ; then
            alcontext='normal'
            Accept-Line-HandleContext
        else
            # prepare warning message for the user, configurable via zstyle.
            zstyle -s ":acceptline:${alcontext}" compwarnfmt msg

            if [[ -z ${msg} ]] ; then
                msg="%c will not execute and completion %f exists."
            fi

            zformat -f msg "${msg}" "c:${com}" "f:${fname}"

            zle -M -- "${msg}"
        fi
        return
    elif [[ -n ${buf//[$' \t\n']##/} ]] ; then
        # If we are here, the commandline contains something that is not
        # executable, which is neither subject to _command_name correction
        # and is not empty. might be a variable assignment
        alcontext='misc'
        Accept-Line-HandleContext

        return
    fi

    # If we got this far, the commandline only contains whitespace, or is empty.
    alcontext='empty'
    Accept-Line-HandleContext
}

zle -N accept-line
zle -N Accept-Line
zle -N Accept-Line-HandleContext

# power completion - abbreviation expansion
# power completion / abbreviation expansion / buffer expansion
# see http://zshwiki.org/home/examples/zleiab for details
# less risky than the global aliases but powerful as well
# just type the abbreviation key and afterwards ',.' to expand it
declare -A abk
setopt extendedglob
setopt interactivecomments
abk=(
#   key   # value                  (#d additional doc string)
#A# start
    '...'  '../..'
    '....' '../../..'
    'BG'   '& exit'
    'C'    '| wc -l'
    'G'    '|& grep '${grep_options:+"${grep_options[*]}"}
    'H'    '| head'
    'Hl'   ' --help |& less -r'    #d (Display help in pager)
    'L'    '| less'
    'LL'   '|& less -r'
    'M'    '| most'
    'N'    '&>/dev/null'           #d (No Output)
    'R'    '| tr A-z N-za-m'       #d (ROT13)
    'SL'   '| sort | less'
    'S'    '| sort -u'
    'T'    '| tail'
    'V'    '|& vim -'
#A# end
    'co'   './configure && make && sudo make install'
)

zleiab() {
    emulate -L zsh
    setopt extendedglob
    local MATCH

    if (( NOABBREVIATION > 0 )) ; then
        LBUFFER="${LBUFFER},."
        return 0
    fi

    matched_chars='[.-|_a-zA-Z0-9]#'
    LBUFFER=${LBUFFER%%(#m)[.-|_a-zA-Z0-9]#}
    LBUFFER+=${abk[$MATCH]:-$MATCH}
}

zle -N zleiab && bindkey ",." zleiab

#f# display contents of assoc array $abk
help-show-abk()
{
  zle -M "$(print "Type ,. after these abbreviations to expand them:"; print -a -C 2 ${(kv)abk})"
}
#k# Display list of abbreviations that expand when followed by ,.
zle -N help-show-abk && bindkey '^xb' help-show-abk

# autoloading
zrcautoload zmv    # who needs mmv or rename?
zrcautoload history-search-end

# we don't want to quote/espace URLs on our own...
# if autoload -U url-quote-magic ; then
#    zle -N self-insert url-quote-magic
#    zstyle ':url-quote-magic:*' url-metas '*?[]^()~#{}='
# else
#    print 'Notice: no url-quote-magic available :('
# fi
alias url-quote='autoload -U url-quote-magic ; zle -N self-insert url-quote-magic'

#m# k ESC-h Call \kbd{run-help} for the 1st word on the command line
alias run-help >&/dev/null && unalias run-help
for rh in run-help{,-git,-svk,-svn}; do
    zrcautoload $rh
done; unset rh

# completion system
if zrcautoload compinit ; then
    compinit || print 'Notice: no compinit available :('
else
    print 'Notice: no compinit available :('
    function compdef { }
fi

is4 && zrcautoload zed # use ZLE editor to edit a file or function

is4 && \
for mod in complist deltochar mathfunc ; do
    zmodload -i zsh/${mod} 2>/dev/null || print "Notice: no ${mod} available :("
done

# autoload zsh modules when they are referenced
if is4 ; then
    zmodload -a  zsh/stat    zstat
    zmodload -a  zsh/zpty    zpty
    zmodload -ap zsh/mapfile mapfile
fi

if is4 && zrcautoload insert-files && zle -N insert-files ; then
    #k# Insert files and test globbing
    bindkey "^xf" insert-files # C-x-f
fi

bindkey ' '   magic-space    # also do history expansion on space
#k# Trigger menu-complete
bindkey '\ei' menu-complete  # menu completion via esc-i

# press esc-e for editing command line in $EDITOR or $VISUAL
if is4 && zrcautoload edit-command-line && zle -N edit-command-line ; then
    #k# Edit the current line in \kbd{\$EDITOR}
    bindkey '\ee' edit-command-line
fi

if is4 && [[ -n ${(k)modules[zsh/complist]} ]] ; then
    #k# menu selection: pick item but stay in the menu
    bindkey -M menuselect '\e^M' accept-and-menu-complete
    # also use + and INSERT since it's easier to press repeatedly
    bindkey -M menuselect "+" accept-and-menu-complete
    bindkey -M menuselect "^[[2~" accept-and-menu-complete

    # accept a completion and try to complete again by using menu
    # completion; very useful with completing directories
    # by using 'undo' one's got a simple file browser
    bindkey -M menuselect '^o' accept-and-infer-next-history
fi

# press "ctrl-e d" to insert the actual date in the form yyyy-mm-dd
insert-datestamp() { LBUFFER+=${(%):-'%D{%Y-%m-%d}'}; }
zle -N insert-datestamp

#k# Insert a timestamp on the command line (yyyy-mm-dd)
bindkey '^ed' insert-datestamp

# press esc-m for inserting last typed word again (thanks to caphuso!)
insert-last-typed-word() { zle insert-last-word -- 0 -1 };
zle -N insert-last-typed-word;

#k# Insert last typed word
bindkey "\em" insert-last-typed-word

function grml-zsh-fg() {
  if (( ${#jobstates} )); then
    zle .push-input
    [[ -o hist_ignore_space ]] && BUFFER=' ' || BUFFER=''
    BUFFER="${BUFFER}fg"
    zle .accept-line
  else
    zle -M 'No background jobs. Doing nothing.'
  fi
}
zle -N grml-zsh-fg
#k# A smart shortcut for \kbd{fg<enter>}
bindkey '^z' grml-zsh-fg

# run command line as user root via sudo:
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    if [[ $BUFFER != sudo\ * ]]; then
        BUFFER="sudo $BUFFER"
        CURSOR=$(( CURSOR+5 ))
    fi
}
zle -N sudo-command-line

#k# prepend the current command with "sudo"
bindkey "^os" sudo-command-line

### jump behind the first word on the cmdline.
### useful to add options.
function jump_after_first_word() {
    local words
    words=(${(z)BUFFER})

    if (( ${#words} <= 1 )) ; then
        CURSOR=${#BUFFER}
    else
        CURSOR=${#${words[1]}}
    fi
}
zle -N jump_after_first_word
#k# jump to after first word (for adding options)
bindkey '^x1' jump_after_first_word

# complete word from history with menu (from Book: ZSH, OpenSource-Press)
zle -C hist-complete complete-word _generic
zstyle ':completion:hist-complete:*' completer _history
#k# complete word from history with menu
bindkey "^x^x" hist-complete

## complete word from currently visible Screen or Tmux buffer.
if check_com -c screen || check_com -c tmux; then
    _complete_screen_display() {
        [[ "$TERM" != "screen" ]] && return 1

        local TMPFILE=$(mktemp)
        local -U -a _screen_display_wordlist
        trap "rm -f $TMPFILE" EXIT

        # fill array with contents from screen hardcopy
        if ((${+TMUX})); then
            #works, but crashes tmux below version 1.4
            #luckily tmux -V option to ask for version, was also added in 1.4
            tmux -V &>/dev/null || return
            tmux -q capture-pane \; save-buffer -b 0 $TMPFILE \; delete-buffer -b 0
        else
            screen -X hardcopy $TMPFILE
            # screen sucks, it dumps in latin1, apparently always. so recode it
            # to system charset
            check_com recode && recode latin1 $TMPFILE
        fi
        _screen_display_wordlist=( ${(QQ)$(<$TMPFILE)} )
        # remove PREFIX to be completed from that array
        _screen_display_wordlist[${_screen_display_wordlist[(i)$PREFIX]}]=""
        compadd -a _screen_display_wordlist
    }
    #k# complete word from currently visible GNU screen buffer
    bindkey -r "^xS"
    compdef -k _complete_screen_display complete-word '^xS'
fi

# history

ZSHDIR=$HOME/.zsh

#v#
HISTFILE=$HOME/.zsh_history
isgrmlcd && HISTSIZE=500  || HISTSIZE=5000
isgrmlcd && SAVEHIST=1000 || SAVEHIST=10000 # useful for setopt append_history

# dirstack handling

DIRSTACKSIZE=${DIRSTACKSIZE:-20}
DIRSTACKFILE=${DIRSTACKFILE:-${HOME}/.zdirs}

if [[ -f ${DIRSTACKFILE} ]] && [[ ${#dirstack[*]} -eq 0 ]] ; then
    dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
    # "cd -" won't work after login by just setting $OLDPWD, so
    [[ -d $dirstack[1] ]] && cd $dirstack[1] && cd $OLDPWD
fi

chpwd() {
    local -ax my_stack
    my_stack=( ${PWD} ${dirstack} )
    if is42 ; then
        builtin print -l ${(u)my_stack} >! ${DIRSTACKFILE}
    else
        uprint my_stack >! ${DIRSTACKFILE}
    fi
}

# directory based profiles

if is433 ; then

# chpwd_profiles(): Directory Profiles, Quickstart:
#
# In .zshrc.local:
#
#   zstyle ':chpwd:profiles:/usr/src/grml(|/|/*)'   profile grml
#   zstyle ':chpwd:profiles:/usr/src/debian(|/|/*)' profile debian
#   chpwd_profiles
#
# For details see the `grmlzshrc.5' manual page.
function chpwd_profiles() {
    local profile context
    local -i reexecute

    context=":chpwd:profiles:$PWD"
    zstyle -s "$context" profile profile || profile='default'
    zstyle -T "$context" re-execute && reexecute=1 || reexecute=0

    if (( ${+parameters[CHPWD_PROFILE]} == 0 )); then
        typeset -g CHPWD_PROFILE
        local CHPWD_PROFILES_INIT=1
        (( ${+functions[chpwd_profiles_init]} )) && chpwd_profiles_init
    elif [[ $profile != $CHPWD_PROFILE ]]; then
        (( ${+functions[chpwd_leave_profile_$CHPWD_PROFILE]} )) \
            && chpwd_leave_profile_${CHPWD_PROFILE}
    fi
    if (( reexecute )) || [[ $profile != $CHPWD_PROFILE ]]; then
        (( ${+functions[chpwd_profile_$profile]} )) && chpwd_profile_${profile}
    fi

    CHPWD_PROFILE="${profile}"
    return 0
}

chpwd_functions=( ${chpwd_functions} chpwd_profiles )

fi # is433

# display battery status on right side of prompt via running 'BATTERY=1 zsh'
if [[ $BATTERY -gt 0 ]] ; then
    if ! check_com -c acpi ; then
        BATTERY=0
    fi
fi

battery() {
if [[ $BATTERY -gt 0 ]] ; then
    PERCENT="${${"$(acpi 2>/dev/null)"}/(#b)[[:space:]]#Battery <->: [^0-9]##, (<->)%*/${match[1]}}"
    if [[ -z "$PERCENT" ]] ; then
        PERCENT='acpi not present'
    else
        if [[ "$PERCENT" -lt 20 ]] ; then
            PERCENT="warning: ${PERCENT}%%"
        else
            PERCENT="${PERCENT}%%"
        fi
    fi
fi
}
# set colors for use in prompts
if zrcautoload colors && colors 2>/dev/null ; then
    BLUE="%{${fg[blue]}%}"
    RED="%{${fg_bold[red]}%}"
    GREEN="%{${fg[green]}%}"
    CYAN="%{${fg[cyan]}%}"
    MAGENTA="%{${fg[magenta]}%}"
    YELLOW="%{${fg[yellow]}%}"
    WHITE="%{${fg[white]}%}"
    NO_COLOUR="%{${reset_color}%}"
else
    BLUE=$'%{\e[1;34m%}'
    RED=$'%{\e[1;31m%}'
    GREEN=$'%{\e[1;32m%}'
    CYAN=$'%{\e[1;36m%}'
    WHITE=$'%{\e[1;37m%}'
    MAGENTA=$'%{\e[1;35m%}'
    YELLOW=$'%{\e[1;33m%}'
    NO_COLOUR=$'%{\e[0m%}'
fi

# gather version control information for inclusion in a prompt

if zrcautoload vcs_info; then
    # `vcs_info' in zsh versions 4.3.10 and below have a broken `_realpath'
    # function, which can cause a lot of trouble with our directory-based
    # profiles. So:
    if [[ ${ZSH_VERSION} == 4.3.<-10> ]] ; then
        function VCS_INFO_realpath () {
            setopt localoptions NO_shwordsplit chaselinks
            ( builtin cd -q $1 2> /dev/null && pwd; )
        }
    fi

    zstyle ':vcs_info:*' max-exports 2

    if [[ -o restricted ]]; then
        zstyle ':vcs_info:*' enable NONE
    fi
fi

# Change vcs_info formats for the grml prompt. The 2nd format sets up
# $vcs_info_msg_1_ to contain "zsh: repo-name" used to set our screen title.
# TODO: The included vcs_info() version still uses $VCS_INFO_message_N_.
#       That needs to be the use of $VCS_INFO_message_N_ needs to be changed
#       to $vcs_info_msg_N_ as soon as we use the included version.
if [[ "$TERM" == dumb ]] ; then
    zstyle ':vcs_info:*' actionformats "(%s%)-[%b|%a] " "zsh: %r"
    zstyle ':vcs_info:*' formats       "(%s%)-[%b] "    "zsh: %r"
else
    # these are the same, just with a lot of colours:
    zstyle ':vcs_info:*' actionformats "${MAGENTA}(${NO_COLOUR}%s${MAGENTA})${YELLOW}-${MAGENTA}[${GREEN}%b${YELLOW}|${RED}%a${MAGENTA}]${NO_COLOUR} " \
                                       "zsh: %r"
    zstyle ':vcs_info:*' formats       "${MAGENTA}(${NO_COLOUR}%s${MAGENTA})${YELLOW}-${MAGENTA}[${GREEN}%b${MAGENTA}]${NO_COLOUR}%} " \
                                       "zsh: %r"
    zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat "%b${RED}:${YELLOW}%r"
fi

# command not found handling

(( ${COMMAND_NOT_FOUND} == 1 )) &&
function command_not_found_handler() {
    emulate -L zsh
    if [[ -x ${GRML_ZSH_CNF_HANDLER} ]] ; then
        ${GRML_ZSH_CNF_HANDLER} $1
    fi
    return 1
}

# set prompt
if zrcautoload promptinit && promptinit 2>/dev/null ; then
    promptinit # people should be able to use their favourite prompt
else
    print 'Notice: no promptinit available :('
fi

setopt prompt_subst

# make sure to use right prompt only when not running a command
is41 && setopt transient_rprompt


function ESC_print () {
    info_print $'\ek' $'\e\\' "$@"
}
function set_title () {
    info_print  $'\e]0;' $'\a' "$@"
}

function info_print () {
    local esc_begin esc_end
    esc_begin="$1"
    esc_end="$2"
    shift 2
    printf '%s' ${esc_begin}
    printf '%s' "$*"
    printf '%s' "${esc_end}"
}

# TODO: revise all these NO* variables and especially their documentation
#       in zsh-help() below.
is4 && [[ $NOPRECMD -eq 0 ]] && precmd () {
    [[ $NOPRECMD -gt 0 ]] && return 0
    # update VCS information
    (( ${+functions[vcs_info]} )) && vcs_info

    if [[ $TERM == screen* ]] ; then
        if [[ -n ${vcs_info_msg_1_} ]] ; then
            ESC_print ${vcs_info_msg_1_}
        else
            ESC_print "zsh"
        fi
    fi
    # just use DONTSETRPROMPT=1 to be able to overwrite RPROMPT
    if [[ ${DONTSETRPROMPT:-} -eq 0 ]] ; then
        if [[ $BATTERY -gt 0 ]] ; then
            # update battery (dropped into $PERCENT) information
            battery
            RPROMPT="%(?..:() ${PERCENT}"
        else
            RPROMPT="%(?..:() "
        fi
    fi
    # adjust title of xterm
    # see http://www.faqs.org/docs/Linux-mini/Xterm-Title.html
    [[ ${NOTITLE:-} -gt 0 ]] && return 0
    case $TERM in
        (xterm*|rxvt*)
            set_title ${(%):-"%n@%m: %~"}
            ;;
    esac
}

# preexec() => a function running before every command
is4 && [[ $NOPRECMD -eq 0 ]] && \
preexec () {
    [[ $NOPRECMD -gt 0 ]] && return 0
# set hostname if not running on host with name 'grml'
    if [[ -n "$HOSTNAME" ]] && [[ "$HOSTNAME" != $(hostname) ]] ; then
       NAME="@$HOSTNAME"
    fi
# get the name of the program currently running and hostname of local machine
# set screen window title if running in a screen
    if [[ "$TERM" == screen* ]] ; then
        # local CMD=${1[(wr)^(*=*|sudo|ssh|-*)]}       # don't use hostname
        local CMD="${1[(wr)^(*=*|sudo|ssh|-*)]}$NAME" # use hostname
        ESC_print ${CMD}
    fi
# adjust title of xterm
    [[ ${NOTITLE} -gt 0 ]] && return 0
    case $TERM in
        (xterm*|rxvt*)
            set_title "${(%):-"%n@%m:"}" "$1"
            ;;
    esac
}

EXITCODE="%(?..%?%1v )"
# secondary prompt, printed when the shell needs more information to complete a
# command.
PS2='\`%_> '
# selection prompt used within a select loop.
PS3='?# '
# the execution trace prompt (setopt xtrace). default: '+%N:%i>'
PS4='+%N:%i:%_> '

# set variable debian_chroot if running in a chroot with /etc/debian_chroot
if [[ -z "$debian_chroot" ]] && [[ -r /etc/debian_chroot ]] ; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# don't use colors on dumb terminals (like emacs):
if [[ "$TERM" == dumb ]] ; then
    PROMPT="${EXITCODE}${debian_chroot:+($debian_chroot)}%n@%m %40<...<%B%~%b%<< "
else
    # only if $GRMLPROMPT is set (e.g. via 'GRMLPROMPT=1 zsh') use the extended
    # prompt set variable identifying the chroot you work in (used in the
    # prompt below)
    if [[ $GRMLPROMPT -gt 0 ]] ; then
        PROMPT="${RED}${EXITCODE}${CYAN}[%j running job(s)] ${GREEN}{history#%!} ${RED}%(3L.+.) ${BLUE}%* %D
${BLUE}%n${NO_COLOUR}@%m %40<...<%B%~%b%<< "
    else
        # This assembles the primary prompt string
        if (( EUID != 0 )); then
            PROMPT="${RED}${EXITCODE}${WHITE}${debian_chroot:+($debian_chroot)}${BLUE}%n${NO_COLOUR}@%m %40<...<%B%~%b%<< "
        else
            PROMPT="${BLUE}${EXITCODE}${WHITE}${debian_chroot:+($debian_chroot)}${RED}%n${NO_COLOUR}@%m %40<...<%B%~%b%<< "
        fi
    fi
fi

PROMPT="${PROMPT}"'${vcs_info_msg_0_}'"%# "

# if we are inside a grml-chroot set a specific prompt theme
if [[ -n "$GRML_CHROOT" ]] ; then
    PROMPT="%{$fg[red]%}(CHROOT) %{$fg_bold[red]%}%n%{$fg_no_bold[white]%}@%m %40<...<%B%~%b%<< %\# "
fi

# 'hash' some often used directories
#d# start
hash -d deb=/var/cache/apt/archives
hash -d doc=/usr/share/doc
hash -d linux=/lib/modules/$(command uname -r)/build/
hash -d log=/var/log
hash -d slog=/var/log/syslog
hash -d src=/usr/src
hash -d templ=/usr/share/doc/grml-templates
hash -d tt=/usr/share/doc/texttools-doc
hash -d www=/var/www
#d# end

# some aliases
if check_com -c screen ; then
    if [[ $UID -eq 0 ]] ; then
        if [[ -r /etc/grml/screenrc ]]; then
            alias screen="${commands[screen]} -c /etc/grml/screenrc"
        fi
    elif [[ -r $HOME/.screenrc ]] ; then
        alias screen="${commands[screen]} -c $HOME/.screenrc"
    else
        if [[ -r /etc/grml/screenrc_grml ]]; then
            alias screen="${commands[screen]} -c /etc/grml/screenrc_grml"
        else
            if [[ -r /etc/grml/screenrc ]]; then
                alias screen="${commands[screen]} -c /etc/grml/screenrc"
            fi
        fi
    fi
fi

# do we have GNU ls with color-support?
if [[ "$TERM" != dumb ]]; then
    #a1# execute \kbd{@a@}:\quad ls with colors
    alias ls='ls -b -CF '${ls_options:+"${ls_options[*]}"}
    #a1# execute \kbd{@a@}:\quad list all files, with colors
    alias la='ls -la '${ls_options:+"${ls_options[*]}"}
    #a1# long colored list, without dotfiles (@a@)
    alias ll='ls -l '${ls_options:+"${ls_options[*]}"}
    #a1# long colored list, human readable sizes (@a@)
    alias lh='ls -hAl '${ls_options:+"${ls_options[*]}"}
    #a1# List files, append qualifier to filenames \\&\quad(\kbd{/} for directories, \kbd{@} for symlinks ...)
    alias l='ls -lF '${ls_options:+"${ls_options[*]}"}
else
    alias ls='ls -b -CF'
    alias la='ls -la'
    alias ll='ls -l'
    alias lh='ls -hAl'
    alias l='ls -lF'
fi

alias mdstat='cat /proc/mdstat'
alias ...='cd ../../'

# generate alias named "$KERNELVERSION-reboot" so you can use boot with kexec:
if [[ -x /sbin/kexec ]] && [[ -r /proc/cmdline ]] ; then
    alias "$(uname -r)-reboot"="kexec -l --initrd=/boot/initrd.img-"$(uname -r)" --command-line=\"$(cat /proc/cmdline)\" /boot/vmlinuz-"$(uname -r)""
fi

# see http://www.cl.cam.ac.uk/~mgk25/unicode.html#term for details
alias term2iso="echo 'Setting terminal to iso mode' ; print -n '\e%@'"
alias term2utf="echo 'Setting terminal to utf-8 mode'; print -n '\e%G'"

# make sure it is not assigned yet
[[ -n ${aliases[utf2iso]} ]] && unalias utf2iso
utf2iso() {
    if isutfenv ; then
        for ENV in $(env | command grep -i '.utf') ; do
            eval export "$(echo $ENV | sed 's/UTF-8/iso885915/ ; s/utf8/iso885915/')"
        done
    fi
}

# make sure it is not assigned yet
[[ -n ${aliases[iso2utf]} ]] && unalias iso2utf
iso2utf() {
    if ! isutfenv ; then
        for ENV in $(env | command grep -i '\.iso') ; do
            eval export "$(echo $ENV | sed 's/iso.*/UTF-8/ ; s/ISO.*/UTF-8/')"
        done
    fi
}

# especially for roadwarriors using GNU screen and ssh:
if ! check_com asc &>/dev/null ; then
  asc() { autossh -t "$@" 'screen -RdU' }
  compdef asc=ssh
fi

#f1# Hints for the use of zsh on grml
zsh-help() {
    print "$bg[white]$fg[black]
zsh-help - hints for use of zsh on grml
=======================================$reset_color"

    print '
Main configuration of zsh happens in /etc/zsh/zshrc.
That file is part of the package grml-etc-core, if you want to
use them on a non-grml-system just get the tar.gz from
http://deb.grml.org/ or (preferably) get it from the git repository:

  http://git.grml.org/f/grml-etc-core/etc/zsh/zshrc

This version of grml'\''s zsh setup does not use skel/.zshrc anymore.
The file is still there, but it is empty for backwards compatibility.

For your own changes use these two files:
    $HOME/.zshrc.pre
    $HOME/.zshrc.local

The former is sourced very early in our zshrc, the latter is sourced
very lately.

System wide configuration without touching configuration files of grml
can take place in /etc/zsh/zshrc.local.

For information regarding zsh start at http://grml.org/zsh/

Take a look at grml'\''s zsh refcard:
% xpdf =(zcat /usr/share/doc/grml-docs/zsh/grml-zsh-refcard.pdf.gz)

Check out the main zsh refcard:
% '$BROWSER' http://www.bash2zsh.com/zsh_refcard/refcard.pdf

And of course visit the zsh-lovers:
% man zsh-lovers

You can adjust some options through environment variables when
invoking zsh without having to edit configuration files.
Basically meant for bash users who are not used to the power of
the zsh yet. :)

  "NOCOR=1    zsh" => deactivate automatic correction
  "NOMENU=1   zsh" => do not use auto menu completion
                      (note: use ctrl-d for completion instead!)
  "NOPRECMD=1 zsh" => disable the precmd + preexec commands (set GNU screen title)
  "NOTITLE=1  zsh" => disable setting the title of xterms without disabling
                      preexec() and precmd() completely
  "BATTERY=1  zsh" => activate battery status (via acpi) on right side of prompt
  "COMMAND_NOT_FOUND=1 zsh"
                   => Enable a handler if an external command was not found
                      The command called in the handler can be altered by setting
                      the GRML_ZSH_CNF_HANDLER variable, the default is:
                      "/usr/share/command-not-found/command-not-found"

A value greater than 0 is enables a feature; a value equal to zero
disables it. If you like one or the other of these settings, you can
add them to ~/.zshrc.pre to ensure they are set when sourcing grml'\''s
zshrc.'

    print "
$bg[white]$fg[black]
Please report wishes + bugs to the grml-team: http://grml.org/bugs/
Enjoy your grml system with the zsh!$reset_color"
}

# debian stuff
if [[ -r /etc/debian_version ]] ; then
    #a3# Execute \kbd{apt-cache search}
    alias acs='apt-cache search'
    #a3# Execute \kbd{apt-cache show}
    alias acsh='apt-cache show'
    #a3# Execute \kbd{apt-cache policy}
    alias acp='apt-cache policy'
    #a3# Execute \kbd{apt-get dist-upgrade}
    salias adg="apt-get dist-upgrade"
    #a3# Execute \kbd{apt-get install}
    salias agi="apt-get install"
    #a3# Execute \kbd{aptitude install}
    salias ati="aptitude install"
    #a3# Execute \kbd{apt-get upgrade}
    salias ag="apt-get upgrade"
    #a3# Execute \kbd{apt-get update}
    salias au="apt-get update"
    #a3# Execute \kbd{aptitude update ; aptitude safe-upgrade}
    salias -a up="aptitude update ; aptitude safe-upgrade"
    #a3# Execute \kbd{dpkg-buildpackage}
    alias dbp='dpkg-buildpackage'
    #a3# Execute \kbd{grep-excuses}
    alias ge='grep-excuses'

    # get a root shell as normal user in live-cd mode:
    if isgrmlcd && [[ $UID -ne 0 ]] ; then
       alias su="sudo su"
     fi

    #a1# Take a look at the syslog: \kbd{\$PAGER /var/log/syslog}
    salias llog="$PAGER /var/log/syslog"     # take a look at the syslog
    #a1# Take a look at the syslog: \kbd{tail -f /var/log/syslog}
    salias tlog="tail -f /var/log/syslog"    # follow the syslog
fi

# sort installed Debian-packages by size
if check_com -c dpkg-query ; then
    #a3# List installed Debian-packages sorted by size
    alias debs-by-size="dpkg-query -Wf 'x \${Installed-Size} \${Package} \${Status}\n' | sed -ne '/^x  /d' -e '/^x \(.*\) install ok installed$/s//\1/p' | sort -nr"
fi

# if cdrecord is a symlink (to wodim) or isn't present at all warn:
if [[ -L /usr/bin/cdrecord ]] || ! check_com -c cdrecord; then
    if check_com -c wodim; then
        cdrecord() {
            cat <<EOMESS
cdrecord is not provided under its original name by Debian anymore.
See #377109 in the BTS of Debian for more details.

Please use the wodim binary instead
EOMESS
            return 1
        }
    fi
fi

# Use hard limits, except for a smaller stack and no core dumps
unlimit
is425 && limit stack 8192
isgrmlcd && limit core 0 # important for a live-cd-system
limit -s

# completion system

# called later (via is4 && grmlcomp)
# note: use 'zstyle' for getting current settings
#         press ^xh (control-x h) for getting tags in context; ^x? (control-x ?) to run complete_debug with trace output
grmlcomp() {
    # TODO: This could use some additional information

    # allow one error for every three characters typed in approximate completer
    zstyle ':completion:*:approximate:'    max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'

    # don't complete backup files as executables
    zstyle ':completion:*:complete:-command-::commands' ignored-patterns '(aptitude-*|*\~)'

    # start menu completion only if it could find no unambiguous initial string
    zstyle ':completion:*:correct:*'       insert-unambiguous true
    zstyle ':completion:*:corrections'     format $'%{\e[0;31m%}%d (errors: %e)%{\e[0m%}'
    zstyle ':completion:*:correct:*'       original true

    # activate color-completion
    zstyle ':completion:*:default'         list-colors ${(s.:.)LS_COLORS}

    # format on completion
    zstyle ':completion:*:descriptions'    format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'

    # automatically complete 'cd -<tab>' and 'cd -<ctrl-d>' with menu
    # zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

    # insert all expansions for expand completer
    zstyle ':completion:*:expand:*'        tag-order all-expansions
    zstyle ':completion:*:history-words'   list false

    # activate menu
    zstyle ':completion:*:history-words'   menu yes

    # ignore duplicate entries
    zstyle ':completion:*:history-words'   remove-all-dups yes
    zstyle ':completion:*:history-words'   stop yes

    # match uppercase from lowercase
    zstyle ':completion:*'                 matcher-list 'm:{a-z}={A-Z}'

    # separate matches into groups
    zstyle ':completion:*:matches'         group 'yes'
    zstyle ':completion:*'                 group-name ''

    if [[ "$NOMENU" -eq 0 ]] ; then
        # if there are more than 5 options allow selecting from a menu
        zstyle ':completion:*'               menu select=5
    else
        # don't use any menus at all
        setopt no_auto_menu
    fi

    zstyle ':completion:*:messages'        format '%d'
    zstyle ':completion:*:options'         auto-description '%d'

    # describe options in full
    zstyle ':completion:*:options'         description 'yes'

    # on processes completion complete all user processes
    zstyle ':completion:*:processes'       command 'ps -au$USER'

    # offer indexes before parameters in subscripts
    zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

    # provide verbose completion information
    zstyle ':completion:*'                 verbose true

    # recent (as of Dec 2007) zsh versions are able to provide descriptions
    # for commands (read: 1st word in the line) that it will list for the user
    # to choose from. The following disables that, because it's not exactly fast.
    zstyle ':completion:*:-command-:*:'    verbose false

    # set format for warnings
    zstyle ':completion:*:warnings'        format $'%{\e[0;31m%}No matches for:%{\e[0m%} %d'

    # define files to ignore for zcompile
    zstyle ':completion:*:*:zcompile:*'    ignored-patterns '(*~|*.zwc)'
    zstyle ':completion:correct:'          prompt 'correct to: %e'

    # Ignore completion functions for commands you don't have:
    zstyle ':completion::(^approximate*):*:functions' ignored-patterns '_*'

    # Provide more processes in completion of programs like killall:
    zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

    # complete manual by their section
    zstyle ':completion:*:manuals'    separate-sections true
    zstyle ':completion:*:manuals.*'  insert-sections   true
    zstyle ':completion:*:man:*'      menu yes select

    # Search path for sudo completion
    zstyle ':completion:*:sudo:*' command-path /usr/local/sbin \
                                               /usr/local/bin  \
                                               /usr/sbin       \
                                               /usr/bin        \
                                               /sbin           \
                                               /bin            \
                                               /usr/X11R6/bin

    # provide .. as a completion
    zstyle ':completion:*' special-dirs ..

    # run rehash on completion so new installed program are found automatically:
    _force_rehash() {
        (( CURRENT == 1 )) && rehash
        return 1
    }

    ## correction
    # some people don't like the automatic correction - so run 'NOCOR=1 zsh' to deactivate it
    if [[ "$NOCOR" -gt 0 ]] ; then
        zstyle ':completion:*' completer _oldlist _expand _force_rehash _complete _files _ignored
        setopt nocorrect
    else
        # try to be smart about when to use what completer...
        setopt correct
        zstyle -e ':completion:*' completer '
            if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]] ; then
                _last_try="$HISTNO$BUFFER$CURSOR"
                reply=(_complete _match _ignored _prefix _files)
            else
                if [[ $words[1] == (rm|mv) ]] ; then
                    reply=(_complete _files)
                else
                    reply=(_oldlist _expand _force_rehash _complete _ignored _correct _approximate _files)
                fi
            fi'
    fi

    # command for process lists, the local web server details and host completion
    zstyle ':completion:*:urls' local 'www' '/var/www/' 'public_html'

    # caching
    [[ -d $ZSHDIR/cache ]] && zstyle ':completion:*' use-cache yes && \
                            zstyle ':completion::complete:*' cache-path $ZSHDIR/cache/

    # host completion
    if is42 ; then
        [[ -r ~/.ssh/known_hosts ]] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
        [[ -r /etc/hosts ]] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()
    else
        _ssh_hosts=()
        _etc_hosts=()
    fi
    hosts=(
        $(hostname)
        "$_ssh_hosts[@]"
        "$_etc_hosts[@]"
        grml.org
        localhost
    )
    zstyle ':completion:*:hosts' hosts $hosts
    # TODO: so, why is this here?
    #  zstyle '*' hosts $hosts

    # use generic completion system for programs not yet defined; (_gnu_generic works
    # with commands that provide a --help option with "standard" gnu-like output.)
    for compcom in cp deborphan df feh fetchipac head hnb ipacsum mv \
                   pal stow tail uname ; do
        [[ -z ${_comps[$compcom]} ]] && compdef _gnu_generic ${compcom}
    done; unset compcom

    # see upgrade function in this file
    compdef _hosts upgrade
}

# grmlstuff
grmlstuff() {
# people should use 'grml-x'!
    if check_com -c 915resolution; then
        855resolution() {
            echo "Please use 915resolution as resolution modifying tool for Intel \
graphic chipset."
            return -1
        }
    fi

    #a1# Output version of running grml
    alias grml-version='cat /etc/grml_version'

    if check_com -c rebuildfstab ; then
        #a1# Rebuild /etc/fstab
        alias grml-rebuildfstab='rebuildfstab -v -r -config'
    fi

    if check_com -c grml-debootstrap ; then
        debian2hd() {
            echo "Installing debian to harddisk is possible by using grml-debootstrap."
            return 1
        }
    fi
}

# now run the functions
isgrml && checkhome
is4    && isgrml    && grmlstuff
is4    && grmlcomp

# keephack
is4 && xsource "/etc/zsh/keephack"

# wonderful idea of using "e" glob qualifier by Peter Stephenson
# You use it as follows:
# $ NTREF=/reference/file
# $ ls -l *(e:nt:)
# This lists all the files in the current directory newer than the reference file.
# You can also specify the reference file inline; note quotes:
# $ ls -l *(e:'nt ~/.zshenv':)
is4 && nt() {
    if [[ -n $1 ]] ; then
        local NTREF=${~1}
    fi
    [[ $REPLY -nt $NTREF ]]
}

# shell functions

#f1# Reload an autoloadable function
freload() { while (( $# )); do; unfunction $1; autoload -U $1; shift; done }
compdef _functions freload

#f1# List symlinks in detail (more detailed version of 'readlink -f' and 'whence -s')
sll() {
    [[ -z "$1" ]] && printf 'Usage: %s <file(s)>\n' "$0" && return 1
    for file in "$@" ; do
        while [[ -h "$file" ]] ; do
            ls -l $file
            file=$(readlink "$file")
        done
    done
}

# TODO: Is it supported to use pager settings like this?
#   PAGER='less -Mr' - If so, the use of $PAGER here needs fixing
# with respect to wordsplitting. (ie. ${=PAGER})
if check_com -c $PAGER ; then
    #f1# View Debian's changelog of a given package
    dchange() {
        emulate -L zsh
        if [[ -r /usr/share/doc/$1/changelog.Debian.gz ]] ; then
            $PAGER /usr/share/doc/$1/changelog.Debian.gz
        elif [[ -r /usr/share/doc/$1/changelog.gz ]] ; then
            $PAGER /usr/share/doc/$1/changelog.gz
        else
            if check_com -c aptitude ; then
                echo "No changelog for package $1 found, using aptitude to retrieve it."
                if isgrml ; then
                    aptitude -t unstable changelog $1
                else
                    aptitude changelog $1
                fi
            else
                echo "No changelog for package $1 found, sorry."
                return 1
            fi
        fi
    }
    _dchange() { _files -W /usr/share/doc -/ }
    compdef _dchange dchange

    #f1# View Debian's NEWS of a given package
    dnews() {
        emulate -L zsh
        if [[ -r /usr/share/doc/$1/NEWS.Debian.gz ]] ; then
            $PAGER /usr/share/doc/$1/NEWS.Debian.gz
        else
            if [[ -r /usr/share/doc/$1/NEWS.gz ]] ; then
                $PAGER /usr/share/doc/$1/NEWS.gz
            else
                echo "No NEWS file for package $1 found, sorry."
                return 1
            fi
        fi
    }
    _dnews() { _files -W /usr/share/doc -/ }
    compdef _dnews dnews

    #f1# View upstream's changelog of a given package
    uchange() {
        emulate -L zsh
        if [[ -r /usr/share/doc/$1/changelog.gz ]] ; then
            $PAGER /usr/share/doc/$1/changelog.gz
        else
            echo "No changelog for package $1 found, sorry."
            return 1
        fi
    }
    _uchange() { _files -W /usr/share/doc -/ }
    compdef _uchange uchange
fi

# zsh profiling
profile() {
    ZSH_PROFILE_RC=1 $SHELL "$@"
}

#f1# Edit an alias via zle
edalias() {
    [[ -z "$1" ]] && { echo "Usage: edalias <alias_to_edit>" ; return 1 } || vared aliases'[$1]' ;
}
compdef _aliases edalias

#f1# Edit a function via zle
edfunc() {
    [[ -z "$1" ]] && { echo "Usage: edfunc <function_to_edit>" ; return 1 } || zed -f "$1" ;
}
compdef _functions edfunc

# use it e.g. via 'Restart apache2'
#m# f6 Start() \kbd{/etc/init.d/\em{process}}\quad\kbd{start}
#m# f6 Restart() \kbd{/etc/init.d/\em{process}}\quad\kbd{restart}
#m# f6 Stop() \kbd{/etc/init.d/\em{process}}\quad\kbd{stop}
#m# f6 Reload() \kbd{/etc/init.d/\em{process}}\quad\kbd{reload}
#m# f6 Force-Reload() \kbd{/etc/init.d/\em{process}}\quad\kbd{force-reload}
#m# f6 Status() \kbd{/etc/init.d/\em{process}}\quad\kbd{status}
if [[ -d /etc/init.d || -d /etc/service ]] ; then
    __start_stop() {
        local action_="${1:l}"  # e.g Start/Stop/Restart
        local service_="$2"
        local param_="$3"

        local service_target_="$(readlink /etc/init.d/$service_)"
        if [[ $service_target_ == "/usr/bin/sv" ]]; then
            # runit
            case "${action_}" in
                start) if [[ ! -e /etc/service/$service_ ]]; then
                           $SUDO ln -s "/etc/sv/$service_" "/etc/service/"
                       else
                           $SUDO "/etc/init.d/$service_" "${action_}" "$param_"
                       fi ;;
                # there is no reload in runits sysv emulation
                reload) $SUDO "/etc/init.d/$service_" "force-reload" "$param_" ;;
                *) $SUDO "/etc/init.d/$service_" "${action_}" "$param_" ;;
            esac
        else
            # sysvinit
            $SUDO "/etc/init.d/$service_" "${action_}" "$param_"
        fi
    }

    _grmlinitd() {
        local -a scripts
        scripts=( /etc/init.d/*(x:t) )
        _describe "service startup script" scripts
    }

    for i in Start Restart Stop Force-Reload Reload Status ; do
        eval "$i() { __start_stop $i \"\$1\" \"\$2\" ; }"
        compdef _grmlinitd $i
    done
fi

#f1# Provides useful information on globbing
H-Glob() {
    echo -e "
    /      directories
    .      plain files
    @      symbolic links
    =      sockets
    p      named pipes (FIFOs)
    *      executable plain files (0100)
    %      device files (character or block special)
    %b     block special files
    %c     character special files
    r      owner-readable files (0400)
    w      owner-writable files (0200)
    x      owner-executable files (0100)
    A      group-readable files (0040)
    I      group-writable files (0020)
    E      group-executable files (0010)
    R      world-readable files (0004)
    W      world-writable files (0002)
    X      world-executable files (0001)
    s      setuid files (04000)
    S      setgid files (02000)
    t      files with the sticky bit (01000)

  print *(m-1)          # Files modified up to a day ago
  print *(a1)           # Files accessed a day ago
  print *(@)            # Just symlinks
  print *(Lk+50)        # Files bigger than 50 kilobytes
  print *(Lk-50)        # Files smaller than 50 kilobytes
  print **/*.c          # All *.c files recursively starting in \$PWD
  print **/*.c~file.c   # Same as above, but excluding 'file.c'
  print (foo|bar).*     # Files starting with 'foo' or 'bar'
  print *~*.*           # All Files that do not contain a dot
  chmod 644 *(.^x)      # make all plain non-executable files publically readable
  print -l *(.c|.h)     # Lists *.c and *.h
  print **/*(g:users:)  # Recursively match all files that are owned by group 'users'
  echo /proc/*/cwd(:h:t:s/self//) # Analogous to >ps ax | awk '{print $1}'<"
}
alias help-zshglob=H-Glob

#v1# set number of lines to display per page
HELP_LINES_PER_PAGE=20
#v1# set location of help-zle cache file
HELP_ZLE_CACHE_FILE=~/.cache/zsh_help_zle_lines.zsh
#f1# helper function for help-zle, actually generates the help text
help_zle_parse_keybindings()
{
    emulate -L zsh
    setopt extendedglob
    unsetopt ksharrays  #indexing starts at 1

    #v1# choose files that help-zle will parse for keybindings
    ((${+HELPZLE_KEYBINDING_FILES})) || HELPZLE_KEYBINDING_FILES=( /etc/zsh/zshrc ~/.zshrc.pre ~/.zshrc ~/.zshrc.local )

    if [[ -r $HELP_ZLE_CACHE_FILE ]]; then
        local load_cache=0
        for f ($HELPZLE_KEYBINDING_FILES) [[ $f -nt $HELP_ZLE_CACHE_FILE ]] && load_cache=1
        [[ $load_cache -eq 0 ]] && . $HELP_ZLE_CACHE_FILE && return
    fi

    #fill with default keybindings, possibly to be overwriten in a file later
    #Note that due to zsh inconsistency on escaping assoc array keys, we encase the key in '' which we will remove later
    local -A help_zle_keybindings
    help_zle_keybindings['<Ctrl>@']="set MARK"
    help_zle_keybindings['<Ctrl>x<Ctrl>j']="vi-join lines"
    help_zle_keybindings['<Ctrl>x<Ctrl>b']="jump to matching brace"
    help_zle_keybindings['<Ctrl>x<Ctrl>u']="undo"
    help_zle_keybindings['<Ctrl>_']="undo"
    help_zle_keybindings['<Ctrl>x<Ctrl>f<c>']="find <c> in cmdline"
    help_zle_keybindings['<Ctrl>a']="goto beginning of line"
    help_zle_keybindings['<Ctrl>e']="goto end of line"
    help_zle_keybindings['<Ctrl>t']="transpose charaters"
    help_zle_keybindings['<Alt>t']="transpose words"
    help_zle_keybindings['<Alt>s']="spellcheck word"
    help_zle_keybindings['<Ctrl>k']="backward kill buffer"
    help_zle_keybindings['<Ctrl>u']="forward kill buffer"
    help_zle_keybindings['<Ctrl>y']="insert previously killed word/string"
    help_zle_keybindings["<Alt>'"]="quote line"
    help_zle_keybindings['<Alt>"']="quote from mark to cursor"
    help_zle_keybindings['<Alt><arg>']="repeat next cmd/char <arg> times (<Alt>-<Alt>1<Alt>0a -> -10 times 'a')"
    help_zle_keybindings['<Alt>u']="make next word Uppercase"
    help_zle_keybindings['<Alt>l']="make next word lowercase"
    help_zle_keybindings['<Ctrl>xd']="preview expansion under cursor"
    help_zle_keybindings['<Alt>q']="push current CL into background, freeing it. Restore on next CL"
    help_zle_keybindings['<Alt>.']="insert (and interate through) last word from prev CLs"
    help_zle_keybindings['<Alt>,']="complete word from newer history (consecutive hits)"
    help_zle_keybindings['<Alt>m']="repeat last typed word on current CL"
    help_zle_keybindings['<Ctrl>v']="insert next keypress symbol literally (e.g. for bindkey)"
    help_zle_keybindings['!!:n*<Tab>']="insert last n arguments of last command"
    help_zle_keybindings['!!:n-<Tab>']="insert arguments n..N-2 of last command (e.g. mv s s d)"
    help_zle_keybindings['<Alt>h']="show help/manpage for current command"

    #init global variables
    unset help_zle_lines help_zle_sln
    typeset -g -a help_zle_lines
    typeset -g help_zle_sln=1

    local k v
    local lastkeybind_desc contents     #last description starting with #k# that we found
    local num_lines_elapsed=0            #number of lines between last description and keybinding
    #search config files in the order they a called (and thus the order in which they overwrite keybindings)
    for f in $HELPZLE_KEYBINDING_FILES; do
        [[ -r "$f" ]] || continue   #not readable ? skip it
        contents="$(<$f)"
        for cline in "${(f)contents}"; do
            #zsh pattern: matches lines like: #k# ..............
            if [[ "$cline" == (#s)[[:space:]]#\#k\#[[:space:]]##(#b)(*)[[:space:]]#(#e) ]]; then
                lastkeybind_desc="$match[*]"
                num_lines_elapsed=0
            #zsh pattern: matches lines that set a keybinding using bindkey or compdef -k
            #             ignores lines that are commentend out
            #             grabs first in '' or "" enclosed string with length between 1 and 6 characters
            elif [[ "$cline" == [^#]#(bindkey|compdef -k)[[:space:]](*)(#b)(\"((?)(#c1,6))\"|\'((?)(#c1,6))\')(#B)(*)  ]]; then
                #description prevously found ? description not more than 2 lines away ? keybinding not empty ?
                if [[ -n $lastkeybind_desc && $num_lines_elapsed -lt 2 && -n $match[1] ]]; then
                    #substitute keybinding string with something readable
                    k=${${${${${${${match[1]/\\e\^h/<Alt><BS>}/\\e\^\?/<Alt><BS>}/\\e\[5~/<PageUp>}/\\e\[6~/<PageDown>}//(\\e|\^\[)/<Alt>}//\^/<Ctrl>}/3~/<Alt><Del>}
                    #put keybinding in assoc array, possibly overwriting defaults or stuff found in earlier files
                    #Note that we are extracting the keybinding-string including the quotes (see Note at beginning)
                    help_zle_keybindings[${k}]=$lastkeybind_desc
                fi
                lastkeybind_desc=""
            else
              ((num_lines_elapsed++))
            fi
        done
    done
    unset contents
    #calculate length of keybinding column
    local kstrlen=0
    for k (${(k)help_zle_keybindings[@]}) ((kstrlen < ${#k})) && kstrlen=${#k}
    #convert the assoc array into preformated lines, which we are able to sort
    for k v in ${(kv)help_zle_keybindings[@]}; do
        #pad keybinding-string to kstrlen chars and remove outermost characters (i.e. the quotes)
        help_zle_lines+=("${(r:kstrlen:)k[2,-2]}${v}")
    done
    #sort lines alphabetically
    help_zle_lines=("${(i)help_zle_lines[@]}")
    [[ -d ${HELP_ZLE_CACHE_FILE:h} ]] || mkdir -p "${HELP_ZLE_CACHE_FILE:h}"
    echo "help_zle_lines=(${(q)help_zle_lines[@]})" >| $HELP_ZLE_CACHE_FILE
    zcompile $HELP_ZLE_CACHE_FILE
}
typeset -g help_zle_sln
typeset -g -a help_zle_lines

#f1# Provides (partially autogenerated) help on keybindings and the zsh line editor
help-zle()
{
    emulate -L zsh
    unsetopt ksharrays  #indexing starts at 1
    #help lines already generated ? no ? then do it
    [[ ${+functions[help_zle_parse_keybindings]} -eq 1 ]] && {help_zle_parse_keybindings && unfunction help_zle_parse_keybindings}
    #already displayed all lines ? go back to the start
    [[ $help_zle_sln -gt ${#help_zle_lines} ]] && help_zle_sln=1
    local sln=$help_zle_sln
    #note that help_zle_sln is a global var, meaning we remember the last page we viewed
    help_zle_sln=$((help_zle_sln + HELP_LINES_PER_PAGE))
    zle -M "${(F)help_zle_lines[sln,help_zle_sln-1]}"
}
#k# display help for keybindings and ZLE (cycle pages with consecutive use)
zle -N help-zle && bindkey '^xz' help-zle

# grep for running process, like: 'any vim'
any() {
    emulate -L zsh
    unsetopt KSH_ARRAYS
    if [[ -z "$1" ]] ; then
        echo "any - grep for process(es) by keyword" >&2
        echo "Usage: any <keyword>" >&2 ; return 1
    else
        ps xauwww | grep -i "${grep_options[@]}" "[${1[1]}]${1[2,-1]}"
    fi
}


# After resuming from suspend, system is paging heavily, leading to very bad interactivity.
# taken from $LINUX-KERNELSOURCE/Documentation/power/swsusp.txt
[[ -r /proc/1/maps ]] && \
deswap() {
    print 'Reading /proc/[0-9]*/maps and sending output to /dev/null, this might take a while.'
    cat $(sed -ne 's:.* /:/:p' /proc/[0-9]*/maps | sort -u | grep -v '^/dev/')  > /dev/null
    print 'Finished, running "swapoff -a; swapon -a" may also be useful.'
}

# a wrapper for vim, that deals with title setting
#   VIM_OPTIONS
#       set this array to a set of options to vim you always want
#       to have set when calling vim (in .zshrc.local), like:
#           VIM_OPTIONS=( -p )
#       This will cause vim to send every file given on the
#       commandline to be send to it's own tab (needs vim7).
if check_com vim; then
    vim() {
        VIM_PLEASE_SET_TITLE='yes' command vim ${VIM_OPTIONS} "$@"
    }
fi

# make a backup of a file
bk() {
    cp -a "$1" "${1}_$(date --iso-8601=seconds)"
}

ssl_hashes=( sha512 sha256 sha1 md5 )

for sh in ${ssl_hashes}; do
    eval 'ssl-cert-'${sh}'() {
        emulate -L zsh
        if [[ -z $1 ]] ; then
            printf '\''usage: %s <file>\n'\'' "ssh-cert-'${sh}'"
            return 1
        fi
        openssl x509 -noout -fingerprint -'${sh}' -in $1
    }'
done; unset sh

ssl-cert-fingerprints() {
    emulate -L zsh
    local i
    if [[ -z $1 ]] ; then
        printf 'usage: ssl-cert-fingerprints <file>\n'
        return 1
    fi
    for i in ${ssl_hashes}
        do ssl-cert-$i $1;
    done
}

ssl-cert-info() {
    emulate -L zsh
    if [[ -z $1 ]] ; then
        printf 'usage: ssl-cert-info <file>\n'
        return 1
    fi
    openssl x509 -noout -text -in $1
    ssl-cert-fingerprints $1
}

# make sure our environment is clean regarding colors
for color in BLUE RED GREEN CYAN YELLOW MAGENTA WHITE ; unset $color

# "persistent history"
# just write important commands you always need to ~/.important_commands
if [[ -r ~/.important_commands ]] ; then
    fc -R ~/.important_commands
fi

# load the lookup subsystem if it's available on the system
zrcautoload lookupinit && lookupinit

# variables

# set terminal property (used e.g. by msgid-chooser)
export COLORTERM="yes"

# aliases

# general
#a2# Execute \kbd{du -sch}
alias da='du -sch'
#a2# Execute \kbd{jobs -l}
alias j='jobs -l'

# listing stuff
#a2# Execute \kbd{ls -lSrah}
alias dir="ls -lSrah"
#a2# Only show dot-directories
alias lad='ls -d .*(/)'
#a2# Only show dot-files
alias lsa='ls -a .*(.)'
#a2# Only files with setgid/setuid/sticky flag
alias lss='ls -l *(s,S,t)'
#a2# Only show symlinks
alias lsl='ls -l *(@)'
#a2# Display only executables
alias lsx='ls -l *(*)'
#a2# Display world-{readable,writable,executable} files
alias lsw='ls -ld *(R,W,X.^ND/)'
#a2# Display the ten biggest files
alias lsbig="ls -flh *(.OL[1,10])"
#a2# Only show directories
alias lsd='ls -d *(/)'
#a2# Only show empty directories
alias lse='ls -d *(/^F)'
#a2# Display the ten newest files
alias lsnew="ls -rtlh *(D.om[1,10])"
#a2# Display the ten oldest files
alias lsold="ls -rtlh *(D.Om[1,10])"
#a2# Display the ten smallest files
alias lssmall="ls -Srl *(.oL[1,10])"
#a2# Display the ten newest directories and ten newest .directories
alias lsnewdir="ls -rthdl *(/om[1,10]) .*(D/om[1,10])"
#a2# Display the ten oldest directories and ten oldest .directories
alias lsolddir="ls -rthdl *(/Om[1,10]) .*(D/Om[1,10])"

# some useful aliases
#a2# Remove current empty directory. Execute \kbd{cd ..; rmdir $OLDCWD}
alias rmcdir='cd ..; rmdir $OLDPWD || cd $OLDPWD'

#a2# ssh with StrictHostKeyChecking=no \\&\quad and UserKnownHostsFile unset
alias insecssh='ssh -o "StrictHostKeyChecking=no" -o "UserKnownHostsFile=/dev/null"'
alias insecscp='scp -o "StrictHostKeyChecking=no" -o "UserKnownHostsFile=/dev/null"'

# work around non utf8 capable software in utf environment via $LANG and luit
if check_com isutfenv && check_com luit ; then
    if check_com -c mrxvt ; then
        isutfenv && [[ -n "$LANG" ]] && \
            alias mrxvt="LANG=${LANG/(#b)(*)[.@]*/$match[1].iso885915} luit mrxvt"
    fi

    if check_com -c aterm ; then
        isutfenv && [[ -n "$LANG" ]] && \
            alias aterm="LANG=${LANG/(#b)(*)[.@]*/$match[1].iso885915} luit aterm"
    fi

    if check_com -c centericq ; then
        isutfenv && [[ -n "$LANG" ]] && \
            alias centericq="LANG=${LANG/(#b)(*)[.@]*/$match[1].iso885915} luit centericq"
    fi
fi

# useful functions

#f5# Backup \kbd{file {\rm to} file\_timestamp}
bk() {
    emulate -L zsh
    cp -b $1 $1_`date --iso-8601=m`
}

#f5# cd to directoy and list files
cl() {
    emulate -L zsh
    cd $1 && ls -a
}

# smart cd function, allows switching to /etc when running 'cd /etc/fstab'
cd() {
    if (( ${#argv} == 1 )) && [[ -f ${1} ]]; then
        [[ ! -e ${1:h} ]] && return 1
        print "Correcting ${1} to ${1:h}"
        builtin cd ${1:h}
    else
        builtin cd "$@"
    fi
}

#f5# Create Directoy and \kbd{cd} to it
mkcd() {
    if (( ARGC != 1 )); then
        printf 'usage: mkcd <new-directory>\n'
        return 1;
    fi
    if [[ ! -d "$1" ]]; then
        command mkdir -p "$1"
    else
        printf '`%s'\'' already exists: cd-ing.\n' "$1"
    fi
    builtin cd "$1"
}

#f5# Create temporary directory and \kbd{cd} to it
cdt() {
    local t
    t=$(mktemp -d)
    echo "$t"
    builtin cd "$t"
}

#f5# Create directory under cursor or the selected area
# Press ctrl-xM to create the directory under the cursor or the selected area.
# To select an area press ctrl-@ or ctrl-space and use the cursor.
# Use case: you type "mv abc ~/testa/testb/testc/" and remember that the
# directory does not exist yet -> press ctrl-XM and problem solved
inplaceMkDirs() {
    local PATHTOMKDIR
    if ((REGION_ACTIVE==1)); then
        local F=$MARK T=$CURSOR
        if [[ $F -gt $T ]]; then
            F=${CURSOR}
            T=${MARK}
        fi
        # get marked area from buffer and eliminate whitespace
        PATHTOMKDIR=${BUFFER[F+1,T]%%[[:space:]]##}
        PATHTOMKDIR=${PATHTOMKDIR##[[:space:]]##}
    else
        local bufwords iword
        bufwords=(${(z)LBUFFER})
        iword=${#bufwords}
        bufwords=(${(z)BUFFER})
        PATHTOMKDIR="${(Q)bufwords[iword]}"
    fi
    [[ -z "${PATHTOMKDIR}" ]] && return 1
    PATHTOMKDIR=${~PATHTOMKDIR}
    if [[ -e "${PATHTOMKDIR}" ]]; then
        zle -M " path already exists, doing nothing"
    else
        zle -M "$(mkdir -p -v "${PATHTOMKDIR}")"
        zle end-of-line
    fi
}
#k# mkdir -p <dir> from string under cursor or marked area
zle -N inplaceMkDirs && bindkey '^xM' inplaceMkDirs

#f5# List files which have been accessed within the last {\it n} days, {\it n} defaults to 1
accessed() {
    emulate -L zsh
    print -l -- *(a-${1:-1})
}

#f5# List files which have been changed within the last {\it n} days, {\it n} defaults to 1
changed() {
    emulate -L zsh
    print -l -- *(c-${1:-1})
}

#f5# List files which have been modified within the last {\it n} days, {\it n} defaults to 1
modified() {
    emulate -L zsh
    print -l -- *(m-${1:-1})
}
# modified() was named new() in earlier versions, add an alias for backwards compatibility
check_com new || alias new=modified

# use colors when GNU grep with color-support
#a2# Execute \kbd{grep -{}-color=auto}
(( $#grep_options > 0 )) && alias grep='grep '${grep_options:+"${grep_options[*]}"}

# Translate DE<=>EN
# 'translate' looks up fot a word in a file with language-to-language
# translations (field separator should be " : "). A typical wordlist looks
# like at follows:
#  | english-word : german-transmission
# It's also only possible to translate english to german but not reciprocal.
# Use the following oneliner to turn back the sort order:
#  $ awk -F ':' '{ print $2" : "$1" "$3 }' \
#    /usr/local/lib/words/en-de.ISO-8859-1.vok > ~/.translate/de-en.ISO-8859-1.vok
#f5# Translates a word
trans() {
    emulate -L zsh
    case "$1" in
        -[dD]*)
            translate -l de-en $2
            ;;
        -[eE]*)
            translate -l en-de $2
            ;;
        *)
            echo "Usage: $0 { -D | -E }"
            echo "         -D == German to English"
            echo "         -E == English to German"
    esac
}

# Usage: simple-extract <file>
# Using option -d deletes the original archive file.
#f5# Smart archive extractor
simple-extract() {
    emulate -L zsh
    setopt extended_glob noclobber
    local DELETE_ORIGINAL DECOMP_CMD USES_STDIN USES_STDOUT GZTARGET WGET_CMD
    local RC=0
    zparseopts -D -E "d=DELETE_ORIGINAL"
    for ARCHIVE in "${@}"; do
        case $ARCHIVE in
            *(tar.bz2|tbz2|tbz))
                DECOMP_CMD="tar -xvjf -"
                USES_STDIN=true
                USES_STDOUT=false
                ;;
            *(tar.gz|tgz))
                DECOMP_CMD="tar -xvzf -"
                USES_STDIN=true
                USES_STDOUT=false
                ;;
            *(tar.xz|txz|tar.lzma))
                DECOMP_CMD="tar -xvJf -"
                USES_STDIN=true
                USES_STDOUT=false
                ;;
            *tar)
                DECOMP_CMD="tar -xvf -"
                USES_STDIN=true
                USES_STDOUT=false
                ;;
            *rar)
                DECOMP_CMD="unrar x"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *lzh)
                DECOMP_CMD="lha x"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *7z)
                DECOMP_CMD="7z x"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *(zip|jar))
                DECOMP_CMD="unzip"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *deb)
                DECOMP_CMD="ar -x"
                USES_STDIN=false
                USES_STDOUT=false
                ;;
            *bz2)
                DECOMP_CMD="bzip2 -d -c -"
                USES_STDIN=true
                USES_STDOUT=true
                ;;
            *(gz|Z))
                DECOMP_CMD="gzip -d -c -"
                USES_STDIN=true
                USES_STDOUT=true
                ;;
            *(xz|lzma))
                DECOMP_CMD="xz -d -c -"
                USES_STDIN=true
                USES_STDOUT=true
                ;;
            *)
                print "ERROR: '$ARCHIVE' has unrecognized archive type." >&2
                RC=$((RC+1))
                continue
                ;;
        esac

        if ! check_com ${DECOMP_CMD[(w)1]}; then
            echo "ERROR: ${DECOMP_CMD[(w)1]} not installed." >&2
            RC=$((RC+2))
            continue
        fi

        GZTARGET="${ARCHIVE:t:r}"
        if [[ -f $ARCHIVE ]] ; then

            print "Extracting '$ARCHIVE' ..."
            if $USES_STDIN; then
                if $USES_STDOUT; then
                    ${=DECOMP_CMD} < "$ARCHIVE" > $GZTARGET
                else
                    ${=DECOMP_CMD} < "$ARCHIVE"
                fi
            else
                if $USES_STDOUT; then
                    ${=DECOMP_CMD} "$ARCHIVE" > $GZTARGET
                else
                    ${=DECOMP_CMD} "$ARCHIVE"
                fi
            fi
            [[ $? -eq 0 && -n "$DELETE_ORIGINAL" ]] && rm -f "$ARCHIVE"

        elif [[ "$ARCHIVE" == (#s)(https|http|ftp)://* ]] ; then
            if check_com curl; then
                WGET_CMD="curl -L -k -s -o -"
            elif check_com wget; then
                WGET_CMD="wget -q -O - --no-check-certificate"
            else
                print "ERROR: neither wget nor curl is installed" >&2
                RC=$((RC+4))
                continue
            fi
            print "Downloading and Extracting '$ARCHIVE' ..."
            if $USES_STDIN; then
                if $USES_STDOUT; then
                    ${=WGET_CMD} "$ARCHIVE" | ${=DECOMP_CMD} > $GZTARGET
                    RC=$((RC+$?))
                else
                    ${=WGET_CMD} "$ARCHIVE" | ${=DECOMP_CMD}
                    RC=$((RC+$?))
                fi
            else
                if $USES_STDOUT; then
                    ${=DECOMP_CMD} =(${=WGET_CMD} "$ARCHIVE") > $GZTARGET
                else
                    ${=DECOMP_CMD} =(${=WGET_CMD} "$ARCHIVE")
                fi
            fi

        else
            print "ERROR: '$ARCHIVE' is neither a valid file nor a supported URI." >&2
            RC=$((RC+8))
        fi
    done
    return $RC
}

__archive_or_uri()
{
    _alternative \
        'files:Archives:_files -g "*.(#l)(tar.bz2|tbz2|tbz|tar.gz|tgz|tar.xz|txz|tar.lzma|tar|rar|lzh|7z|zip|jar|deb|bz2|gz|Z|xz|lzma)"' \
        '_urls:Remote Archives:_urls'
}

_simple_extract()
{
    _arguments \
        '-d[delete original archivefile after extraction]' \
        '*:Archive Or Uri:__archive_or_uri'
}
compdef _simple_extract simple-extract
alias se=simple-extract

#f5# Set all ulimit parameters to \kbd{unlimited}
allulimit() {
    ulimit -c unlimited
    ulimit -d unlimited
    ulimit -f unlimited
    ulimit -l unlimited
    ulimit -n unlimited
    ulimit -s unlimited
    ulimit -t unlimited
}

#f5# Change the xterm title from within GNU-screen
xtrename() {
    emulate -L zsh
    if [[ $1 != "-f" ]] ; then
        if [[ -z ${DISPLAY} ]] ; then
            printf 'xtrename only makes sense in X11.\n'
            return 1
        fi
    else
        shift
    fi
    if [[ -z $1 ]] ; then
        printf 'usage: xtrename [-f] "title for xterm"\n'
        printf '  renames the title of xterm from _within_ screen.\n'
        printf '  also works without screen.\n'
        printf '  will not work if DISPLAY is unset, use -f to override.\n'
        return 0
    fi
    print -n "\eP\e]0;${1}\C-G\e\\"
    return 0
}

# Create small urls via http://goo.gl using curl(1).
# API reference: https://code.google.com/apis/urlshortener/
function zurl() {
    emulate -L zsh
    if [[ -z $1 ]]; then
        print "USAGE: zurl <URL>"
        return 1
    fi

    local PN url prog api json data
    PN=$0
    url=$1

    # Prepend 'http://' to given URL where necessary for later output.
    if [[ ${url} != http(s|)://* ]]; then
        url='http://'${url}
    fi

    if check_com -c curl; then
        prog=curl
    else
        print "curl is not available, but mandatory for ${PN}. Aborting."
        return 1
    fi
    api='https://www.googleapis.com/urlshortener/v1/url'
    contenttype="Content-Type: application/json"
    json="{\"longUrl\": \"${url}\"}"
    data=$($prog --silent -H ${contenttype} -d ${json} $api)
    # Match against a regex and print it
    if [[ $data =~ '"id": "(http://goo.gl/[[:alnum:]]+)"' ]]; then
        print $match;
    fi
}

#f2# Find history events by search pattern and list them by date.
whatwhen()  {
    emulate -L zsh
    local usage help ident format_l format_s first_char remain first last
    usage='USAGE: whatwhen [options] <searchstring> <search range>'
    help='Use `whatwhen -h'\'' for further explanations.'
    ident=${(l,${#${:-Usage: }},, ,)}
    format_l="${ident}%s\t\t\t%s\n"
    format_s="${format_l//(\\t)##/\\t}"
    # Make the first char of the word to search for case
    # insensitive; e.g. [aA]
    first_char=[${(L)1[1]}${(U)1[1]}]
    remain=${1[2,-1]}
    # Default search range is `-100'.
    first=${2:-\-100}
    # Optional, just used for `<first> <last>' given.
    last=$3
    case $1 in
        ("")
            printf '%s\n\n' 'ERROR: No search string specified. Aborting.'
            printf '%s\n%s\n\n' ${usage} ${help} && return 1
        ;;
        (-h)
            printf '%s\n\n' ${usage}
            print 'OPTIONS:'
            printf $format_l '-h' 'show help text'
            print '\f'
            print 'SEARCH RANGE:'
            printf $format_l "'0'" 'the whole history,'
            printf $format_l '-<n>' 'offset to the current history number; (default: -100)'
            printf $format_s '<[-]first> [<last>]' 'just searching within a give range'
            printf '\n%s\n' 'EXAMPLES:'
            printf ${format_l/(\\t)/} 'whatwhen grml' '# Range is set to -100 by default.'
            printf $format_l 'whatwhen zsh -250'
            printf $format_l 'whatwhen foo 1 99'
        ;;
        (\?)
            printf '%s\n%s\n\n' ${usage} ${help} && return 1
        ;;
        (*)
            # -l list results on stout rather than invoking $EDITOR.
            # -i Print dates as in YYYY-MM-DD.
            # -m Search for a - quoted - pattern within the history.
            fc -li -m "*${first_char}${remain}*" $first $last
        ;;
    esac
}

# mercurial related stuff
if check_com -c hg ; then
    # gnu like diff for mercurial
    # http://www.selenic.com/mercurial/wiki/index.cgi/TipsAndTricks
    #f5# GNU like diff for mercurial
    hgdi() {
        emulate -L zsh
        for i in $(hg status -marn "$@") ; diff -ubwd <(hg cat "$i") "$i"
    }

    # build debian package
    #a2# Alias for \kbd{hg-buildpackage}
    alias hbp='hg-buildpackage'

    # execute commands on the versioned patch-queue from the current repos
    alias mq='hg -R $(readlink -f $(hg root)/.hg/patches)'

    # diffstat for specific version of a mercurial repository
    #   hgstat      => display diffstat between last revision and tip
    #   hgstat 1234 => display diffstat between revision 1234 and tip
    #f5# Diffstat for specific version of a mercurial repos
    hgstat() {
        emulate -L zsh
        [[ -n "$1" ]] && hg diff -r $1 -r tip | diffstat || hg export tip | diffstat
    }

fi # end of check whether we have the 'hg'-executable

# grml-small cleanups

# The following is used to remove zsh-config-items that do not work
# in grml-small by default.
# If you do not want these adjustments (for whatever reason), set
# $GRMLSMALL_SPECIFIC to 0 in your .zshrc.pre file (which this configuration
# sources if it is there).

if (( GRMLSMALL_SPECIFIC > 0 )) && isgrmlsmall ; then

    unset abk[V]
    unalias    'V'      &> /dev/null
    unfunction vman     &> /dev/null
    unfunction viless   &> /dev/null
    unfunction 2html    &> /dev/null

    # manpages are not in grmlsmall
    unfunction manzsh   &> /dev/null
    unfunction man2     &> /dev/null

fi

zrclocal

## genrefcard.pl settings

### doc strings for external functions from files
#m# f5 grml-wallpaper() Sets a wallpaper (try completion for possible values)

### example: split functions-search 8,16,24,32
#@# split functions-search 8

## END OF FILE #################################################################
# vim:filetype=zsh foldmethod=marker autoindent expandtab shiftwidth=4
# Local variables:
# mode: sh
# End:
