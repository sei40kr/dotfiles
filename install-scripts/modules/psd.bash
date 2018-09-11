# psd.bash --- Profile-sync-daemon installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    trizen_sync_facade profile-sync-daemon

    ln_facade "${DOTFILES_PATH}/psd/psd.conf" "${XDG_CONFIG_HOME}/psd/psd.conf"

    systemctl_enable_user_facade psd.service
fi
