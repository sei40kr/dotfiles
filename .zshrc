#!/usr/bin/env zsh

# .zshrc
# author: Seong Yong-ju ( @sei40kr )

if [[ -z "$TMUX" ]] && [[ "${+commands[tmux]}" == 1 ]]
then
  tmux new-session
  exit
fi

zmodload zsh/zpty

autoload -Uz add-zsh-hook
autoload -Uz cdr
autoload -Uz chpwd_recent_dirs
autoload -Uz zmv

path=(
  "${HOME}/.cabal/bin"
  "${HOME}/.cargo/bin"
  "${GOPATH}/bin"
  "${PYENV_ROOT}/bin"
  "${RBENV_ROOT}/bin"
  "${path[@]}"
)

eval "
$(pyenv init - zsh --no-rehash)
$(rbenv init - zsh --no-rehash)"

. "${HOME}/.zplugin/bin/zplugin.zsh"

autoload -Uz _zplugin

if [[ "${+_comps}" == 1 ]]
then
  _comps[zplugin]=_zplugin
fi

# zplugin: Utilities {{{
zplugin snippet 'OMZ::lib/git.zsh'
zplugin snippet 'OMZ::lib/clipboard.zsh'
zplugin snippet "${HOME}/.zsh/rc/10_utilities.zsh"

zplugin snippet 'OMZ::lib/key-bindings.zsh'
zplugin snippet "${HOME}/.zsh/rc/20_key-bindings.zsh"

zplugin snippet "${HOME}/.zsh/rc/30_aliases.zsh"

zplugin snippet 'OMZ::lib/completion.zsh'
zplugin snippet 'OMZ::lib/compfix.zsh'
zplugin snippet "${HOME}/.zsh/rc/50_options.zsh"

zplugin snippet "${HOME}/.zsh/rc/70_misc.zsh"

zplugin snippet "${HOME}/.zsh/rc/80_custom.zsh"

zplugin ice pick'spaceship.zsh'; zplugin light 'denysdovhan/spaceship-zsh-theme'
# }}}

# zplugin: Plugins {{{
zplugin light 'b4b4r07/emoji-cli'
zplugin light 'b4b4r07/enhancd'
zplugin light 'davidparsson/zsh-nvm-lazy'
zplugin light 'davidparsson/zsh-pyenv-lazy'
zplugin light 'djui/alias-tips'
zplugin light 'mollifier/anyframe'
zplugin light 'mollifier/cd-gitroot'
zplugin ice pick'k.sh'; zplugin light 'supercrabtree/k'
zplugin light 'zdharma/fast-syntax-highlighting'
zplugin light 'zsh-users/zsh-autosuggestions'
zplugin snippet 'OMZ::plugins/colored-man-pages/colored-man-pages.plugin.zsh'
zplugin snippet 'OMZ::plugins/dotenv/dotenv.plugin.zsh'
zplugin snippet 'OMZ::plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh'
zplugin snippet 'OMZ::plugins/git/git.plugin.zsh'
zplugin snippet 'OMZ::plugins/gitignore/gitignore.plugin.zsh'
zplugin snippet 'OMZ::plugins/gnu-utils/gnu-utils.plugin.zsh'
zplugin snippet 'OMZ::plugins/jsontools/jsontools.plugin.zsh'
zplugin snippet 'OMZ::plugins/zsh_reload/zsh_reload.plugin.zsh'

case "${(L):-$(uname -a)}" in
  *arch*)   zplugin snippet 'OMZ::plugins/archlinux/archlinux.plugin.zsh' ;;
  *fedora*) zplugin snippet 'OMZ::plugins/fedora/fedora.plugin.zsh'       ;;
  *darwin*) zplugin snippet 'OMZ::plugins/osx/osx.plugin.zsh'             ;;
  *ubuntu*) zplugin snippet 'OMZ::plugins/ubuntu/ubuntu.plugin.zsh'       ;;
esac

if [[ -d '/usr/local/opt/fzf' ]]
then
  zplugin snippet '/usr/local/opt/fzf/shell/completion.zsh'
  zplugin snippet '/usr/local/opt/fzf/shell/key-bindings.zsh'
fi
# }}}

# zplugin: Commands {{{
zplugin ice from'gh-r' as'command' mv'gotcha_* -> gotcha'; zplugin light 'b4b4r07/gotcha'
zplugin ice as'command' cp'httpstat.sh -> httpstat' pick'httpstat'; zplugin light 'b4b4r07/httpstat'

if [[ -d '/usr/local/share/git-core' ]]
then
  zplugin ice as'command'; zplugin snippet '/usr/local/share/git-core/contrib/diff-highlight/diff-highlight'
fi
# }}}

# zplugin: Completions {{{
zplugin ice blockf; zplugin light 'zsh-users/zsh-completions'
# }}}

autoload -Uz compinit; compinit
