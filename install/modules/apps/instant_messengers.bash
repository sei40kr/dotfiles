# author: Seong Yong-ju <sei40kr@gmail.com>

install_instant_messengers() {
  while true; do
    print_title 'Instant Messengers'

    tui_add_options \
      'Skype' install_skype \
      'Slack' install_slack \
      'Discord' install_discord
    tui_set_quit_option d 'Done'

    if ! tui_select_options 'Enter your option'; then
      break
    fi
  done
}

install_skype() {
  if is_macos; then
    brew_cask_install skype
  elif is_archlinux; then
    trizen_sync skypeforlinux-stable-bin
  else
    unsupported_platform_error
  fi
}

install_slack() {
  if is_macos; then
    brew_cask_install slack
  elif is_archlinux; then
    trizen_sync slack-desktop
  else
    unsupported_platform_error
  fi
}

install_discord() {
  if is_macos; then
    brew_cask_install discord
  elif is_archlinux; then
    pacman_sync discord
  else
    unsupported_platform_error
  fi
}
