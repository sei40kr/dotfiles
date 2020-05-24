# author: Seong Yong-ju <sei40kr@gmail.com>

# systemctl_enable SERVICE
#
# Enable a system service with systemctl.
#
systemctl_enable() {
  assert_command_exists systemctl

  local service="$1"

  print-step "Enabling system service ${service}"

  sudo systemctl enable --now "$service"
}

# systemctl_enable SERVICE
#
# Mask a system service with systemctl.
#
systemctl_mask() {
  assert_command_exists systemctl

  local service="$1"

  print-step "Masking system service ${service}"

  sudo systemctl mask --now "$service"
}

# systemctl_user_enable SERVICE
#
# Enable a user service with systemctl.
#
systemctl_user_enable() {
  assert_command_exists systemctl

  local service="$1"

  print-step "Enabling user service ${service}"

  systemctl --user enable --now "$service"
}
