# config.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

set -g EDITOR emacs

# direnv
eval (direnv hook fish)

# fzf
set -g FZF_FIND_FILE_OPTS '--reverse --inline-info'
set -g FZF_TMUX 0
set -g FZF_ENABLE_OPEN_PREVIEW 0

# perlbrew
if [ -d "$PERLBREW_ROOT" ]
    . $PERLBREW_ROOT/etc/perlbrew.fish
end

# pure
set pure_symbol_prompt "~>"
set pure_symbol_git_down_arrow "v"
set pure_symbol_git_up_arrow "^"
set pure_symbol_git_dirty !
set pure_symbol_horizontal_bar "_"
set pure_color_normal (set_color normal)
set pure_color_red (set_color red)
set pure_color_yellow (set_color yellow)
set pure_color_blue (set_color blue)
set pure_color_magenta (set_color magenta)
set pure_color_cyan (set_color cyan)
set pure_color_gray (set_color 5c6370)
set pure_color_white (set_color white)

# tmux
# Handle TMUX focus events
# cf https://github.com/fish-shell/fish-shell/issues/1917
bind \e\[I 'begin; end'
bind \e\[O 'begin; end'
