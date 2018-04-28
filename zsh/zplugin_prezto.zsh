# zplugin_prezto.zsh --- ZSH plugins from Prezto
# author: Seong Yong-ju <sei40kr@gmail.com>

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
    gnu-utility
    homebrew
    rsync
    ssh
  )

  # Load OS-specified modules
  if [[ "$OSTYPE" == darwin* ]]; then
    modules=( osx $modules )
  elif [[ "${+commands[pacman]}" == 1 ]]; then
    modules=( pacman $modules )
  fi

  if [[ ! "$XDG_SESSION_DESKTOP" =~ ^(xfce|xmonad)$ ]]; then
    modules=( tmux $modules )
    zstyle ':prezto:module:tmux:auto-start' local 'yes'
    zstyle ':prezto:module:tmux:session' name 'default'
  fi

  for module in $modules; do
    zplugin ice svn pick'init.zsh'
    zplugin snippet PZT::"modules/${module}"
  done

  unsetopt CORRECT
  unalias e
}
