# zplugin_completions.zsh --- ZSH plugins for add command completions
# author: Seong Yong-ju <sei40kr@gmail.com>

# @b4b4r07's dotfiles

zplugin ice svn pick'' wait''
zplugin snippet 'https://github.com/b4b4r07/dotfiles/trunk/.zsh/Completion'

# @zchee's zsh-completions

zplugin ice svn pick'' wait''
zplugin snippet 'https://github.com/zchee/zsh-completions/trunk/src'

# zsh-completions

zplugin ice svn pick'' wait''
zplugin snippet 'https://github.com/zsh-users/zsh-completions/trunk/src'

# docker-ce

zplugin ice svn pick'' wait''
zplugin snippet 'https://github.com/docker/docker-ce/trunk/components/cli/contrib/completion/zsh'

# jsforce

zplugin ice pick'' wait''
zplugin light jsforce/jsforce-zsh-completions

# sdkman

zplugin ice pick'' wait''
zplugin light nobeans/zsh-sdkman

() {
  local paths=(
    "${GOENV_ROOT}/completions/goenv.zsh"
    "${PYENV_ROOT}/completions/pyenv.zsh"
    "${RBENV_ROOT}/completions/rbenv.zsh"
  )

  for local_path in $paths; do
    if [[ -s "$local_path" ]]; then
      zplugin ice wait''
      zplugin snippet "$local_path"
    fi
  done
}
