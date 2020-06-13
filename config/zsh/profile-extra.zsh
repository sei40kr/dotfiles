# -*- mode: sh -*-

# zprofile
# author: Seong Yong-ju <sei40kr@gmail.com>

if [[ "$OSTYPE" == darwin* && -x /usr/libexec/path_helper ]]; then
    eval "$(/usr/libexec/path_helper -s)"
fi


## Homebrew

if [[ "$OSTYPE" == darwin* && "${+commands[brew]}" == 1 ]]; then
    __brew_prefix="$(brew --prefix)"

    path=( "${__brew_prefix}/bin" "${__brew_prefix}/sbin" "${path[@]}" )
    fpath=( "${__brew_prefix}/share/zsh/site-functions" "${fpath[@]}" )
    manpath=( "${__brew_prefix}/share/man" "${manpath[@]}" )
fi


## Perl

# Perlbrew
export PERLBREW_ROOT="${HOME}/perl5/perlbrew"
export PERL5LIB="${HOME}/perl5/lib/perl5:${PERL5LIB:+:${PERL5LIB}}"
export PERL_LOCAL_LIB_ROOT="${HOME}/perl5:${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_MB_OPT="--install_base '${HOME}/perl5'"
export PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"
path=( "${HOME}/perl5/bin" "${PERLBREW_ROOT}/bin" "${path[@]}" )
manpath=( "${HOME}/perl5/man" "${manpath[@]}" )


path=( "${HOME}/.local/bin" "${path[@]}" )
fpath=( "${ZDOTDIR}/functions"
        "${ZDOTDIR}/completions"
        "${fpath[@]}" )


#
# Others

WORKSPACE_DIR="${HOME}/projects"


export PATH
export FPATH
export MANPATH
