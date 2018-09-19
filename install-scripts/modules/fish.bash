# fish.bash --- fish shell
# author: Seong Yong-ju <sei40kr@gmail.com>

if is_arch; then
    pacman_sync_facade fish
    pacman_sync_facade fzf

    ln_facade "${DOTFILES_PATH}/fish/config.fish" "${XDG_CONFIG_HOME}/config.fish"

    # TODO Install other configurations

    # TODO Install abbreviations

    # TODO Install fisherman

    ln_facade "${DOTFILES_PATH}/fish/bash_profile" "${HOME}/.bash_profile"
    ln_facade "${DOTFILES_PATH}/fish/bashrc" "${HOME}/.bashrc"
fi
