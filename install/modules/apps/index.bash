# author: Seong Yong-ju <sei40kr@gmail.com>

# shellcheck source=instant_messengers.bash
use_lazy_modules instant_messengers install_instant_messengers

install_apps() {
  while true; do
    print_title 'Apps'

    tui_add_options 'Google Chrome' install_google_chrome
    if is_macos; then
      tui_add_options 'Notion' install_notion
    elif is_archlinux; then
      tui_add_options \
        'Geary' install_geary \
        'Thunar' install_thunar \
        'GNOME Calendar' install_gnome_calendar \
        'GNOME Pomodoro' install_gnome_pomodoro \
        'GoldenDict' install_goldendict
    fi
    tui_add_options 'Bitwarden' install_bitwarden
    if is_macos; then
      tui_add_options \
        'iTerm2' install_iterm2 \
        'Alfred' install_alfred \
        'Google Japanese IME' install_google_japanese_ime
    elif is_archlinux; then
      tui_add_options \
        'Alacritty' install_alacritty \
        'Rofi' install_rofi \
        'Fcitx' install_fcitx
    fi
    tui_add_options 'Instant Messengers' install_instant_messengers
    tui_set_quit_option d 'Done'

    if ! tui_select_options 'Enter your option'; then
      break
    fi
  done
}

install_google_chrome() {
  if is_macos; then
    brew_cask_install google-chrome
  elif is_archlinux; then
    trizen_sync google-chrome
  else
    unsupported_platform_error
  fi
}

install_geary() {
  assert_archlinux
  pacman_sync geary
}

install_thunar() {
  assert_archlinux
  pacman_sync thunar
}

install_gnome_calendar() {
  assert_archlinux
  pacman_sync gnome-calendar
}

install_gnome_pomodoro() {
  assert_archlinux
  trizen_sync gnome-shell-pomodoro
}

install_goldendict() {
  assert_archlinux
  pacman_sync goldendict
}

install_notion() {
  assert_macos
  brew_cask_install notion
}

install_bitwarden() {
  if is_macos; then
    brew_cask_install bitwarden
  elif is_archlinux; then
    trizen_sync bitwarden-bin
  else
    unsupported_platform_error
  fi
}

install_iterm2() {
  assert_macos
  brew_cask_install iterm2
}

install_alacritty() {
  assert_archlinux
  pacman_sync alacritty

  mkdir -p "${XDG_CONFIG_HOME}/alacritty"
  ln -fs "${HOME}/.dotfiles/alacritty/alacritty.yml" \
    "${XDG_CONFIG_HOME}/alacritty/alacritty.yml"
}

install_alfred() {
  assert_macos
  brew_cask_install alfred
}

install_rofi() {
  assert_archlinux
  pacman_sync rofi

  mkdir -p "${XDG_CONFIG_HOME}/rofi"
  ln -fs "${HOME}/.dotfiles/rofi/config.rasi" \
    "${XDG_CONFIG_HOME}/rofi/config.rasi"
  ln -fs "${HOME}/.dotfiles/rofi/onedark.rasi" \
    "${XDG_CONFIG_HOME}/rofi/onedark.rasi"

  ln -fsT "${XDG_CONFIG_HOME}/rofi/scripts" "${HOME}/rofi-scripts"
}

install_google_japanese_ime() {
  assert_macos
  brew_cask_install google-japanese-ime
}

install_fcitx() {
  assert_archlinux
  pacman_sync \
    fcitx \
    fcitx-configtool \
    fcitx-gtk2 fcitx-gtk3 fcitx-qt5 \
    fcitx-mozc

  mkdir -p "${XDG_CONFIG_HOME}/fcitx/conf"
  ln -fs "${HOME}/.dotfiles/fcitx/config" "${XDG_CONFIG_HOME}/fcitx/config"
  ln -fs "${HOME}/.dotfiles/fcitx/conf/fcitx-classic-ui.conf" \
    "${XDG_CONFIG_HOME}/fcitx/conf/fcitx-classic-ui.conf"
}
