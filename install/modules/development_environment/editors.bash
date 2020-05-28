# author: Seong Yong-ju <sei40kr@gmail.com>

install_editors() {
  while true; do
    print_title 'Editors'

    tui_add_options \
      'Doom Emacs (requires fd, ripgrep)' install_doom_emacs \
      'IntelliJ IDEA Ultimate Edition' install_intellij_idea_ue
    tui_set_quit_option d 'Done'

    if ! tui_select_option 'Enter your option'; then
      break
    fi
  done
}

install_doom_emacs() {
  if is_macos; then
    brew_install \
      d12frosted/emacs-plus/emacs-plus --with-emacs-27-branch --with-xwidgets --without-spacemacs-icon \
      libvterm cmake
  elif is_archlinux; then
    pacman_sync \
      emacs \
      libvterm cmake
  else
    unsupported_platform_error
  fi

  git_clone sei40kr/doom-emacs "${HOME}/.emacs.d"
  git_clone sei40kr/doom.d "$DOOMDIR"

  "${HOME}/.emacs.d/bin/doom" -y --doomdir "$DOOMDIR" install
}

install_intellij_idea_ue() {
  if is_macos; then
    brew_cask_install intellij-idea
  elif is_archlinux; then
    trizen_sync intellij-idea-ultimate-edition
  else
    unsupported_platform_error
  fi

  git_clone MarcoIeni/intellimacs "${HOME}/.intellimacs"

  ln -fs "${HOME}/.dotfiles/intellij-idea/ideavimrc" "${HOME}/.ideavimrc"
}
