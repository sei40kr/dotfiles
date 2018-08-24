# linux.bash --- Fontconfig installer for Linux
# author: Seong Yong-ju <sei40kr@gmail.com>

require_facades symlink

symlink_make fontconfig/fonts.conf .config/fontconfig/fonts.conf
symlink_make fontconfig/conf.d/10-hinting-none.conf .config/fontconfig/conf.d/10-hinting-none.conf
symlink_make fontconfig/conf.d/10-sub-pixel-rgb.conf .config/fontconfig/conf.d/10-sub-pixel-rgb.conf
symlink_make fontconfig/conf.d/11-lcdfilter-default.conf .config/fontconfig/conf.d/11-lcdfilter-default.conf
symlink_make fontconfig/conf.d/66-courier-bitmap-ja.conf .config/fontconfig/conf.d/66-courier-bitmap-ja.conf
symlink_make fontconfig/conf.d/66-terminus-ja.conf .config/fontconfig/conf.d/66-terminus-ja.conf
