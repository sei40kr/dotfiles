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
