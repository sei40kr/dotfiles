# docker.bash --- Docker installer
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_macos; then
    brew_cask_install_facade docker
elif is_arch; then
    pacman_sync_facade docker
    pacman_sync_facade docker-compose

    systemctl_enable_facade docker.service
fi
