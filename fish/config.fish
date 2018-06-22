# config.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

set -U fish_term24bit 1

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


## toggl

abbr -a tgs toggl stop


## fzf

set -U FZF_TMUX 0
set -U FZF_COMPLETE 0
