# redshift.bash --- Redshift installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade redshift

    ln_facade "${DOTFILES_PATH}/redshift/redshift.conf" "${XDG_CONFIG_HOME}/redshift/redshift.conf"
    ln_facade "${DOTFILES_PATH}/redshift/hooks/brightness.sh" "${XDG_CONFIG_HOME}/redshift/hooks/brightness.sh"

    systemctl_enable_user_facade redshift.service
fi
