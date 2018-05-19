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
    tmux
  )

  # Load OS-specified modules
  if [[ "$OSTYPE" == darwin* ]]; then
    modules=( osx $modules )
  fi

  zstyle ':prezto:module:editor' key-bindings 'vi'
  zstyle ':prezto:module:tmux:session' name 'default'

  for module in $modules; do
    zplugin ice svn pick'init.zsh'
    zplugin snippet PZT::"modules/${module}"
  done

  unsetopt CORRECT
  unalias e
}
