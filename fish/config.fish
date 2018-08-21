# config.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

set -U fish_term24bit 1

## dotfiles

if [ -z "$DOTFILES_PATH" ]
    set -U DOTFILES_PATH (realpath (dirname (realpath (status --current-filename))/..))
end


## bobthefish

# Font options
set -g theme_powerline_fonts no
set -g theme_nerd_fonts yes
# Prompt options
set -g theme_newline_cursor yes
# Color scheme options
set -g theme_color_scheme base16-dark
# VCS options
set -g theme_display_git_dirty_verbose no
set -g theme_display_git_master_branch no
