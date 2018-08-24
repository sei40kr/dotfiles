# linux.bash --- Alacritty installer for Linux
# author: Seong Yong-ju <sei40kr@gmail.com>

require_facades symlink

symlink_make alacritty/alacritty.yml .config/alacritty/alacritty.yml
