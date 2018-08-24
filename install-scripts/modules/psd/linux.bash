# linux.bash --- Profile-sync-daemon installer for Linux
# author: Seong Yong-ju <sei40kr@gmail.com>

require_facades symlink

symlink_make psd/psd.conf .config/psd/psd.conf
