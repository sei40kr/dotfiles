# config.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

set -U fish_term24bit 1

# dotfiles
set -g EDITOR emacs
set -g DOTFILES_PATH (dirname (dirname (realpath (status --current-filename))))

# fish-ghq
set -g GHQ_SELECTOR fzf

# fzf
set -g FZF_DEFAULT_OPTS '--height 40'
set -g FZF_FIND_FILE_OPTS '--reverse --inline-info'
set -g FZF_TMUX 0
set -g FZF_COMPLETE 0
set -g FZF_ENABLE_OPEN_PREVIEW 0

# perlbrew
if [ -d "$PERLBREW_ROOT" ]
    . $PERLBREW_ROOT/etc/perlbrew.fish
end

# pure
set -g pure_color_normal (set_color 'c5c8c6')
set -g pure_color_red (set_color 'a54242')
set -g pure_color_green (set_color '8c9440')
set -g pure_color_yellow (set_color 'de935f')
set -g pure_color_blue (set_color '5f819d')
set -g pure_color_cyan (set_color '5e8d87')
set -g pure_color_gray (set_color '6c6c6c')
