# linux.bash --- fcitx installer for Linux
# author: Seong Yong-ju <sei40kr@gmail.com>

require_facades symlink

symlink_make fcitx/conf/fcitx-classic-ui.config .config/fcitx/conf/fcitx-classic-ui.config
symlink_make fcitx/config .config/fcitx/config
