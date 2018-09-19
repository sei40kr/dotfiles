# fontconfig.bash --- Fontconfig
# author: Seong Yong-ju <sei40kr@gmail.com>

pacman_sync_facade fontconfig

ln_facade "${DOTFILES_PATH}/fontconfig/conf.d" "${XDG_CONFIG_HOME}/fontconfig/conf.d"
ln_facade "${DOTFILES_PATH}/fontconfig/fonts.conf" "${XDG_CONFIG_HOME}/fontconfig/fonts.conf"
