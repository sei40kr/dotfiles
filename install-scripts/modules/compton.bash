# compton.bash --- compton installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    trizen_sync_facade compton-no-blur-limit-git

    ln_facade "${DOTFILES_PATH}/compton/comp" "${HOME}/.local/bin/comp"
    ln_facade "${DOTFILES_PATH}/compton/compton.conf" "${XDG_CONFIG_HOME}/compton.conf"
fi
