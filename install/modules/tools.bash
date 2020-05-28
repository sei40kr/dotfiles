# author: Seong Yong-ju <sei40kr@gmail.com>

install_tools() {
  while true; do
    if is_archlinux; then
      tui_add_options 'Profile-sync-daemon' install_psd
    fi
    tui_add_options \
      'rclone' install_rclone \
      'Pandoc' install_pandoc
    tui_set_quit_option d 'Done'

    if ! tui_select_options 'Enter your option'; then
      break
    fi
  done
}

install_rclone() {
  if is_macos; then
    brew_install rclone
  elif is_archlinux; then
    pacman_sync rclone
  else
    unsupported_platform_error
  fi
}

install_psd() {
  assert_archlinux
  trizen_sync profile-sync-daemon
  systemctl_user_enable psd.service
}

install_pandoc() {
  if is_macos; then
    brew_install pandoc
    brew_cask_install wkhtmltopdf
  elif is_archlinux; then
    pacman_sync pandoc wkhtmltopdf
  else
    unsupported_platform_error
  fi
}
