# author: Seong Yong-ju <sei40kr@gmail.com>

system_setup() {
  while true; do
    if is_archlinux; then
      tui_add_options \
        'Trizen' install_trizen \
        'NetworkManager' install_networkmanager \
        'BlueZ' install_bluez \
        'fstrim' install_fstrim \
        'TLP' install_tlp \
        'Network Time Protocol daemon' install_ntpd
    fi
    tui_add_options 'OpenSSH' install_openssh
    tui_set_quit_option d 'Done'

    if ! tui_select_option 'Enter your option'; then
      break
    fi

  done
}

install_trizen() {
  assert_archlinux

  if ! command_exists trizen; then
    pacman_sync base-devel git perl

    assert_command_exists makepkg
    git_clone_build https://aur.archlinux.org/trizen.git \
      'makepkg -mis --noconfirm --neeeded'
  fi

  mkdir -p "${XDG_CONFIG_HOME}/trizen"
  ln -fs "${HOME}/.dotfiles/trizen/trizen.conf" \
    "${XDG_CONFIG_HOME}/trizen/trizen.conf"
}

install_networkmanager() {
  assert_archlinux
  pacman_sync networkmanager
  systemctl_enable NetworkManager.service
}

install_bluez() {
  assert_archlinux
  pacman_sync bluez bluez-utils
  systemctl_enable bluetooth.service
}

install_fstrim() {
  assert_archlinux
  sudo_systemctl_enable fstrim.timer
}

install_tlp() {
  assert_archlinux
  pacman_sync tlp
  systemctl_enable tlp.service
  systemctl_enable tlp-sleep.service
  systemctl_mask systemd-rfkill.service
  systemctl_mask systemd-rfkill.socket
}

install_ntpd() {
  pacman_sync ntp networkmanager-dispatcher-ntpd
  sudo timedatectl set-timezone Asia/Tokyo
}

install_openssh() {
  if is_macos; then
    brew_install openssh
  elif is_archlinux; then
    pacman_sync openssh
  else
    unsupported_platform_error
  fi

  # TODO Prohibit password authentication

  # shellcheck disable=SC2174
  mkdir -pm 700 "${HOME}/.ssh"
  ln -fs "${HOME}/.dotfiles/ssh/config" "${HOME}/.ssh/config"
}
