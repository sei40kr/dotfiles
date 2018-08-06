# linux.bash --- Fontconfig installer for Linux
# author: Seong Yong-ju <sei40kr@gmail.com>

make_symlink fontconfig/fonts.conf .config/fontconfig/fonts.conf
make_symlink fontconfig/conf.d/10-hinting-none.conf .config/fontconfig/conf.d/10-hinting-none.conf
make_symlink fontconfig/conf.d/10-sub-pixel-rgb.conf .config/fontconfig/conf.d/10-sub-pixel-rgb.conf
make_symlink fontconfig/conf.d/11-lcdfilter-default.conf .config/fontconfig/conf.d/11-lcdfilter-default.conf
make_symlink fontconfig/conf.d/66-courier-bitmap-ja.conf .config/fontconfig/conf.d/66-courier-bitmap-ja.conf
make_symlink fontconfig/conf.d/66-terminus-ja.conf .config/fontconfig/conf.d/66-terminus-ja.conf
