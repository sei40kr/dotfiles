# linux.bash --- Rofi installer for Linux
# author: Seong Yong-ju <sei40kr@gmail.com>

require_facades symlink

symlink_make rofi/config .config/rofi/config
symlink_make rofi-system-menu .local/bin/rofi-system-menu
