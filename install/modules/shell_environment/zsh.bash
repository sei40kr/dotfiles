# author: Seong Yong-ju <sei40kr@gmail.com>

install_zsh() {
  if is_macos; then
    brew_install zsh subversion fzf
  elif is_archlinux; then
    pacman_sync zsh subversion fzf
  else
    unsupported_platform_error
  fi

  ln -fs "${HOME}/.dotfiles/zsh/zshenv" "${HOME}/.zshenv"
  mkdir -p "${ZDOTDIR}"
  ln -fs "${HOME}/.dotfiles/zsh/zshenv" "${ZDOTDIR}/.zshenv"
  ln -fs "${HOME}/.dotfiles/zsh/zprofile" "${ZDOTDIR}/.zprofile"
  ln -fs "${HOME}/.dotfiles/zsh/zshrc" "${ZDOTDIR}/.zshrc"

  ln -fs "${HOME}/.dotfiles/zsh/aliases.zsh" "${ZDOTDIR}/aliases.zsh"
  ln -fs "${HOME}/.dotfiles/zsh/custom-history.zsh" \
    "${ZDOTDIR}/custom-history.zsh"
  ln -fs "${HOME}/.dotfiles/zsh/secrets.zsh" "${ZDOTDIR}/secrets.zsh"

  ln -fsT "${HOME}/.dotfiles/zsh/functions" "${ZDOTDIR}/functions"
  ln -fsT "${HOME}/.dotfiles/zsh/completions" "${ZDOTDIR}/completions"

  mkdir -p "${ZINIT_HOME_DIR}"
  git_clone zdharma/zinit "${ZINIT_BIN_DIR}"

  # Install starship config
  ln -fs "${HOME}/.dotfiles/zsh/starship.toml" \
    "${XDG_CONFIG_HOME}/starship.toml"

  sudo chsh -s "$(command -v zsh)" "$USER"
}
