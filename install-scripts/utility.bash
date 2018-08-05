# utility.bash --- Utility functions for a facade/module
# author: Seong Yong-ju <sei40kr@gmail.com>

clear_last_line() {
    tput cuu 1 && tput el
}
