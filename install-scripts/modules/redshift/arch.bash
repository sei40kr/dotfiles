# arch.bash --- Redshift installer for ArchLinux
# author: Seong Yong-ju <sei40kr@gmail.com>

require_facades pacman systemctl

pacman_sync_pkg redshift

systemctl_enable_usrsvc redshift.service
