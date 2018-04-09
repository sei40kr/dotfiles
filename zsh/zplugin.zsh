## Commands

zplugin ice from'gh-r' as'program' mv'gotcha_* -> gotcha'
zplugin light b4b4r07/gotcha

zplugin ice as'program' pick'httpstat' mv'httpstat.sh -> httpstat'
zplugin light b4b4r07/httpstat

zplugin ice as'program' pick'bin/ssh-keyreg'; zplugin light b4b4r07/ssh-keyreg

zplugin ice from'gh-r' as'program' mv'direnv* -> direnv' atload'eval "$(direnv hook zsh)"'
zplugin light direnv/direnv

zplugin ice as'program' pick'bin/fzf-tmux' src'shell/key-bindings.zsh'
zplugin light junegunn/fzf

zplugin ice from'gh-r' as'program'; zplugin light junegunn/fzf-bin


## ZSH functions

zplugin ice atload'alias U="cd-gitroot"'
zplugin light mollifier/cd-gitroot



## Prezto

() {
  local modules=(
    # Prezto
    helper
    # ZSH
    completion
    directory
    editor
    environment
    history
    utility
    # Haskell
    haskell
    # Perl
    perl
    # Python
    python
    # Ruby
    rails
    ruby
    # Tools
    docker
    git
    gnu-utility
    homebrew
    rsync
    ssh
  )

  # Load OS-specified modules.
  if [[ "$OSTYPE" == darwin* ]]; then
    modules=( osx $modules )
  elif [[ "${+commands[pacman]}" == 1 ]]; then
    modules=( pacman $modules )
  fi

  # Load tmux module unless the session is Xfce or XMonad.
  if [[ ! "$XDG_SESSION_DESKTOP" =~ ^(xfce|xmonad)$ ]]; then
    zstyle ':prezto:module:tmux:auto-start' local 'yes'
    zstyle ':prezto:module:tmux:session' name 'default'

    modules=( tmux $modules )
  fi

  for module in $modules; do
    zplugin ice svn pick'init.zsh'; zplugin snippet PZT::"modules/${module}"
  done
}


## Oh My Zsh

() {
  local plugins=(
    # ZSH
    common-aliases
    fancy-ctrl-z
    # Go
    golang
    # Java
    mvn
    # Node
    npm
    npx
    yarn
    react-native
    # Python
    autopep8
    pylint
    pip
    # Ruby
    capistrano
    gem
    rake
    # Scala
    sbt
    # Database & other middlewares
    postgres
    # Tools
    colored-man-pages
    extract
    nmap
  )

  # Load OS-specified plugins.
  if [[ "${+commands[aptitude]}" == 1 ]] \
         || [[ "${+commands[apt-get]}" == 1 ]]; then
    plugins=( debian $plugins )
  fi

  # Load plugins.
  for plugin in $plugins; do
    zplugin ice svn; zplugin snippet OMZ::"plugins/${plugin}"
  done

  local snippets=()

  # Load snippets.
  for snippet in $snippets; do
    zplugin snippet OMZ::"plugins/${snippet}/${snippet}.plugin.zsh"
  done

  local completions=(
    # Haskell
    stack
    # Java
    ant
    gradle
    # Ruby
    rake-fast
    # Tools
    mosh
  )

  # Load completions lazily.
  for completion in $completions; do
    zplugin ice svn wait''; zplugin snippet OMZ::"plugins/${completion}"
  done

  # Unalias rm='rm -i', cp='cp -i', mv='mv -i'.
  unalias rm cp mv
}


## Local completions

() {
  local paths=(
    "${GOENV_ROOT}/completions/goenv.zsh"
    "${PYENV_ROOT}/completions/pyenv.zsh"
    "${RBENV_ROOT}/completions/rbenv.zsh"
  )

  # Load local completions.
  for local_path in $paths; do
    if [[ -s "$local_path" ]]; then
      zplugin ice wait''; zplugin snippet "$local_path"
    fi
  done
}


## Completions

zplugin ice blockf pick'' wait''; zplugin light zsh-users/zsh-completions

# jsforce
zplugin ice pick'' wait''; zplugin light jsforce/jsforce-zsh-completions

# sdkman
zplugin ice pick'' wait''; zplugin light nobeans/zsh-sdkman


## Other plugins

# k
k() {
  zplugin ice pick'k.sh'; zplugin light supercrabtree/k
  unhash -f k

  alias k='k -Ah --no-vcs'

  k
}

# zsh-nvm
if [[ -n "$NVM_DIR" ]]; then
  export NVM_SYMLINK_CURRENT=true
  zplugin light lukechilds/zsh-nvm
fi


## ZSH theme

zplugin ice pick'async.zsh' lucid src'pure.zsh' wait'!0'
zplugin light sindresorhus/pure


## ZSH enhancements

export ENHANCD_FILTER="fzf --height 50% --reverse"
export ENHANCD_DOT_SHOW_FULLPATH=1
zplugin ice pick'init.sh'; zplugin light b4b4r07/enhancd

export ZSH_PLUGINS_ALIAS_TIPS_TEXT="alias-tips: "
zplugin ice lucid wait'1'; zplugin light djui/alias-tips

zplugin ice lucid wait'1' load'[[ -n "$TMUX" ]]' unload'[[ -z "$TMUX" ]]'
zplugin load sei40kr/zsh-tmux-rename

zplugin ice lucid atload'_zsh_autosuggest_start' wait'0'
zplugin light zsh-users/zsh-autosuggestions

zplugin ice lucid atload'zpcompinit' wait'0'
zplugin light zdharma/fast-syntax-highlighting

fpath=( "${HOME}/.zsh/plugins/zsh-better-run-help" $fpath )
. "${HOME}/.zsh/plugins/zsh-better-run-help/zsh-better-run-help.plugin.zsh"
