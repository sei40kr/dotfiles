#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju ( @sei40kr )

export TERM='xterm-256color-italic'

if [[ "${+commands[tmux]}" == 1 ]] && [[ -z "$TMUX" ]]
then
  env \
      FZF_TMUX=1 \
      FZF_TMUX_HEIGHT='25%' \
      tmux new-session
  exit
fi

# zmodload zsh/zprof
zmodload zsh/zpty

export EDITOR="$(which nvim)"
export HISTFILE="${HOME}/.histfile"
export HISTSIZE=1000
export KEYTIMEOUT=1
export SAVEHIST=1000

export XDG_CONFIG_HOME="${HOME}/.config"

if [[ -d "${HOME}/.linuxbrew" ]]
then
  export XDG_DATA_DIRS="${HOME}/.linuxbrew/share:$XDG_DATA_DIRS";
fi

if [[ -e "${HOME}/.zsh_secret" ]]
then
  . "${HOME}/.zsh_secret"
fi

autoload -Uz add-zsh-hook
autoload -Uz cdr
autoload -Uz chpwd_recent_dirs
autoload -Uz zmv

setopt append_history
setopt auto_cd
setopt auto_list
setopt auto_menu
setopt auto_pushd
setopt extended_history
setopt glob_dots
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt interactive_comments
setopt no_beep
setopt print_eight_bit
setopt prompt_subst
setopt pushd_ignore_dups
setopt share_history
unsetopt list_beep

export GOPATH="${HOME}/.go"
PYENV_ROOT="${HOME}/.pyenv"
RBENV_ROOT="${HOME}/.rbenv"
ZPLUG_HOME="${HOME}/.zplug"

path=(
  '/usr/local/opt/coreutils/libexec/gnubin'
  '/usr/local/share/git-core/contrib'
  "${HOME}/.cabal/bin"
  "${HOME}/.cargo/bin"
  "${GOPATH}/bin"
  "${PYENV_ROOT}/bin"
  "${RBENV_ROOT}/bin"
  "${path[@]}"
)

source /dev/stdin <<EOM
$(pyenv init - zsh --no-rehash)
$(rbenv init - zsh --no-rehash)
EOM

. "${HOME}/.zplugin/bin/zplugin.zsh"

autoload -Uz _zplugin

if [[ "${+_comps}" == 1 ]]
then
  _comps[zplugin]=_zplugin
fi

# zplugin: Commands {{{
zplugin ice from'gh-r' as'command' mv'gotcha_* -> gotcha'; zplugin light 'b4b4r07/gotcha'
zplugin ice as'command' cp'httpstat.sh -> httpstat' pick'httpstat'; zplugin light 'b4b4r07/httpstat'
# }}}

# zplugin: Libraries {{{
zplugin snippet 'OMZ::lib/completion.zsh'
zplugin snippet 'OMZ::lib/compfix.zsh'
zplugin snippet 'OMZ::lib/git.zsh'
zplugin snippet 'OMZ::lib/clipboard.zsh'
zplugin snippet 'OMZ::lib/key-bindings.zsh'
# }}}

# zplugin: Plugins {{{
zplugin snippet "${HOME}/.zsh/rc/30_aliases.zsh"
zplugin light 'b4b4r07/emoji-cli'
zplugin light 'b4b4r07/enhancd'
zplugin light 'djui/alias-tips'
zplugin light 'mollifier/anyframe'
zplugin light 'mollifier/cd-gitroot'
zplugin light 'zdharma/fast-syntax-highlighting'
zplugin light 'zsh-users/zsh-autosuggestions'
zplugin ice pick'k.sh'; zplugin light 'supercrabtree/k'
zplugin snippet 'OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh'
zplugin snippet 'OMZ::plugins/dotenv/dotenv.plugin.zsh'
zplugin snippet 'OMZ::plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh'
zplugin snippet 'OMZ::plugins/git/git.plugin.zsh'
zplugin snippet 'OMZ::plugins/gitignore/gitignore.plugin.zsh'
zplugin snippet 'OMZ::plugins/jsontools/jsontools.plugin.zsh'
zplugin ice if'[[ "${+commands[apt-get]}" ]]'; zplugin snippet 'OMZ::plugins/ubuntu/ubuntu.plugin.zsh'
zplugin snippet 'OMZ::plugins/zsh_reload/zsh_reload.plugin.zsh'
zplugin snippet 'https://raw.githubusercontent.com/junegunn/fzf/master/shell/completion.zsh'
zplugin snippet 'https://raw.githubusercontent.com/junegunn/fzf/master/shell/key-bindings.zsh'
# }}}

# zplugin: Completions {{{
zplugin ice blockf; zplugin light 'zsh-users/zsh-completions'
# }}}

autoload -Uz compinit; compinit

# zplugin: Theme {{{
zplugin ice pick'spaceship.zsh'; zplugin light 'denysdovhan/spaceship-zsh-theme'
# }}}
