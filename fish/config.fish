# config.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

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


## tmux

function __tmux_rename -v PWD
    if [ -z "$TMUX" ]
        return
    end

    set -lx tmux_window_id (tmux display-message -p '#{window_id}')
    set -lx cwd $PWD

    # Don't load config.fish
    set -lx HOME (mktemp -d)
    set -lx XDG_CONFIG_HOME $HOME

    fish -c 'tmux rename-window -t $tmux_window_id (basename (git -C $cwd rev-parse --show-toplevel ^/dev/null; or echo $cwd)) &'
end

if [ -n "$TMUX" ]
    __tmux_rename
end


## toggl

abbr -a tgs toggl stop


## fzf

set -U FZF_TMUX 1
set -U FZF_COMPLETE 0
