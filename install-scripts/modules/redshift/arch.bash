# arch.bash --- Redshift installer for ArchLinux
# author: Seong Yong-ju <sei40kr@gmail.com>

pacman_sync_pkg redshift

systemctl_enable_usrsvc redshift
