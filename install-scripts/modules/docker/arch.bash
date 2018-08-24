# arch.bash --- Docker installer for Arch Linux
# author: Seong Yong-ju <sei40kr@gmail.com>

require_facades pacman systemctl

pacman_sync_pkg docker
pacman_sync_pkg docker-compose

systemctl_enable_svc docker.service
