# config.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

set -g EDITOR emacs

# direnv
eval (direnv hook fish)

# fzf
set -gx FZF_DEFAULT_OPTS '--reverse --inline-info --color=dark --color=fg:-1,bg:-1,hl:#c678dd,fg+:#ffffff,bg+:#4b5263,hl+:#d858fe --color=info:#98c379,prompt:#61afef,pointer:#be5046,marker:#e5c07b,spinner:#61afef,header:#61afef'
set -g FZF_TMUX 1
set -g FZF_ENABLE_OPEN_PREVIEW 0

# perlbrew
if [ -d "$PERLBREW_ROOT" ]
    . $PERLBREW_ROOT/etc/perlbrew.fish
end

# pure
set pure_symbol_prompt '~>'
set pure_symbol_git_arrow_down 'v'
set pure_symbol_git_arrow_up '^'
set pure_symbol_git_dirty !
set pure_symbol_horizontal_bar '*'
